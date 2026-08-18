/// Conversion functions from WrittenTypes to ProgramTypes.
module LibParser.WrittenTypesToProgramTypes

open Prelude
open LibExecution.ProgramTypes

module WT = WrittenTypes
module PT = LibExecution.ProgramTypes
module RT = LibExecution.RuntimeTypes
type NRE = PT.NameResolutionError
module NR = NameResolver

type Context =
  { // Fully qualified path of the function currently being lowered, used to
    // recognize self-recursive calls.
    currentFnName : List<string> option
    // Function parameters by name, used to lower references to `EArg`.
    argMap : Map<string, int>
    // Local bindings from lets and patterns, used before global name resolution.
    localBindings : Set<string> }

/// The builtin name sets that name resolution checks against.
///
/// It is the same set every time, and building `builtins.fns |> Map.keys |> Set` inline at each
/// resolution site rebuilds the whole builtin name set for every name in every expression of every
/// package fn loaded.
///
/// Keyed on the `Builtins` instance rather than held in a mutable, so tests building their own
/// builtin sets in parallel don't see each other's.
module private BuiltinNames =
  let private fnCache =
    System.Runtime.CompilerServices.ConditionalWeakTable<RT.Builtins, Set<RT.FQFnName.Builtin>>()

  let private valueCache =
    System.Runtime.CompilerServices.ConditionalWeakTable<RT.Builtins, Set<RT.FQValueName.Builtin>>()

  let fns (builtins : RT.Builtins) : Set<RT.FQFnName.Builtin> =
    fnCache.GetValue(builtins, (fun b -> b.fns.Keys |> Set.ofSeq))

  let values (builtins : RT.Builtins) : Set<RT.FQValueName.Builtin> =
    valueCache.GetValue(builtins, (fun b -> b.values.Keys |> Set.ofSeq))


let private bindLocalName (context : Context) (name : string) : Context =
  let currentFnName =
    match context.currentFnName with
    | Some qualifiedName ->
      match List.tryLast qualifiedName with
      | Some lastName when lastName = name -> None
      | _ -> context.currentFnName
    | None -> None
  { context with
      currentFnName = currentFnName
      argMap = Map.remove name context.argMap
      localBindings = Set.add name context.localBindings }

let private foldMapContext
  (f : 'ctx -> 'a -> 'ctx * 'b)
  (context : 'ctx)
  (items : List<'a>)
  : 'ctx * List<'b> =
  items
  |> List.fold
    (fun (ctx, acc) item ->
      let (newCtx, converted) = f ctx item
      (newCtx, converted :: acc))
    (context, [])
  |> fun (ctx, acc) -> (ctx, List.rev acc)

let private neListOrSingleton (fallback : 'a) (items : List<'a>) : NEList<'a> =
  match items with
  | h :: t -> NEList.ofList h t
  | [] -> NEList.singleton fallback

// `Module.Path.fn` becomes an unresolved name; name resolution runs later.
let private qualifiedFnName (q : WT.QualifiedFnIdentifier) : WT.Name =
  let parts = (q.modules |> List.map (fun (m, _) -> m.name)) @ [ q.fn.name ]
  match parts with
  | h :: t -> WT.Unresolved(NEList.ofList h t)
  | [] -> WT.Unresolved(NEList.singleton "_")

// `Module.Path.TypeName` becomes an unresolved name for record literals and type
// refs.
let private qualifiedTypeName (q : WT.QualifiedTypeIdentifier) : WT.Name =
  let parts = (q.modules |> List.map (fun (m, _) -> m.name)) @ [ q.typ.name ]
  match parts with
  | h :: t -> WT.Unresolved(NEList.ofList h t)
  | [] -> WT.Unresolved(NEList.singleton "_")

// Enum type names are a plain list. Empty means an unqualified case like `Ok`.
let private enumTypeName
  (q : WT.QualifiedTypeIdentifier)
  : WT.UnresolvedEnumTypeName =
  (q.modules |> List.map (fun (m, _) -> m.name)) @ [ q.typ.name ]
  |> List.filter (fun s -> s <> "")

// Option and Result constructors can be bare. Other enum constructors require
// `Type.Case` until expected-type inference is supported.
let private enumTypeNameForCase
  (q : WT.QualifiedTypeIdentifier)
  (caseName : string)
  : WT.UnresolvedEnumTypeName =
  match enumTypeName q, caseName with
  | [], ("Some" | "None") -> [ "Stdlib"; "Option"; "Option" ]
  | [], ("Ok" | "Error") -> [ "Stdlib"; "Result"; "Result" ]
  | names, _ -> names

module InfixFnName =
  let toPT (name : WT.InfixFnName) : PT.InfixFnName =
    match name with
    | WT.ArithmeticPlus -> PT.ArithmeticPlus
    | WT.ArithmeticMinus -> PT.ArithmeticMinus
    | WT.ArithmeticMultiply -> PT.ArithmeticMultiply
    | WT.ArithmeticDivide -> PT.ArithmeticDivide
    | WT.ArithmeticModulo -> PT.ArithmeticModulo
    | WT.ArithmeticPower -> PT.ArithmeticPower
    | WT.ComparisonGreaterThan -> PT.ComparisonGreaterThan
    | WT.ComparisonGreaterThanOrEqual -> PT.ComparisonGreaterThanOrEqual
    | WT.ComparisonLessThan -> PT.ComparisonLessThan
    | WT.ComparisonLessThanOrEqual -> PT.ComparisonLessThanOrEqual
    | WT.ComparisonEquals -> PT.ComparisonEquals
    | WT.ComparisonNotEquals -> PT.ComparisonNotEquals
    | WT.StringConcat -> PT.StringConcat

module TypeReference =
  let rec toPT
    (pm : PT.PackageManager)
    (onMissing : NR.OnMissing)
    (currentModule : List<string>)
    (t : WT.TypeReference)
    : Ply<PT.TypeReference> =
    let toPT = toPT pm onMissing currentModule
    uply {
      match t with
      | WT.TUnit _ -> return PT.TUnit
      | WT.TBool _ -> return PT.TBool
      | WT.TInt8 _ -> return PT.TInt8
      | WT.TUInt8 _ -> return PT.TUInt8
      | WT.TInt16 _ -> return PT.TInt16
      | WT.TUInt16 _ -> return PT.TUInt16
      | WT.TInt32 _ -> return PT.TInt32
      | WT.TUInt32 _ -> return PT.TUInt32
      | WT.TInt64 _ -> return PT.TInt64
      | WT.TUInt64 _ -> return PT.TUInt64
      | WT.TInt128 _ -> return PT.TInt128
      | WT.TUInt128 _ -> return PT.TUInt128
      | WT.TInt _ -> return PT.TInt
      | WT.TFloat _ -> return PT.TFloat
      | WT.TChar _ -> return PT.TChar
      | WT.TString _ -> return PT.TString
      | WT.TDateTime _ -> return PT.TDateTime
      | WT.TUuid _ -> return PT.TUuid
      | WT.TBlob _ -> return PT.TBlob

      | WT.TList(_, _, _, inner, _) -> return! toPT inner |> Ply.map PT.TList

      | WT.TDict(_, _, _, inner, _) -> return! toPT inner |> Ply.map PT.TDict

      | WT.TVariable(_, _, (_, name)) -> return PT.TVariable name

      | WT.TTuple(_, first, _, second, rest, _, _) ->
        let! firstType = toPT first
        let! secondType = toPT second
        let! otherTypes = Ply.List.mapSequentially (fun (_, t) -> toPT t) rest
        return PT.TTuple(firstType, secondType, otherTypes)

      | WT.TFn(_, args, ret) ->
        let! argTypes = Ply.List.mapSequentially (fun (t, _) -> toPT t) args
        let! returnType = toPT ret
        let paramTypes = neListOrSingleton PT.TUnit argTypes
        return PT.TFn(paramTypes, returnType)

      | WT.TCustom q ->
        // A few builtin types take a type arg and parse as a custom `Name<T>`,
        // but lower to a dedicated PT case (not a package type reference).
        match q.modules, q.typ.name, q.typeArgs with
        | [], "Stream", [ inner ] -> return! toPT inner |> Ply.map PT.TStream
        | [], "DB", [ inner ] -> return! toPT inner |> Ply.map PT.TDB
        | _ ->
          let! resolved =
            NR.resolveTypeName pm onMissing currentModule (qualifiedTypeName q)
          let! typeArgs = Ply.List.mapSequentially toPT q.typeArgs
          return PT.TCustomType(resolved, typeArgs)
    }

module BinaryOperation =
  let toPT (binop : WT.BinaryOperation) : PT.BinaryOperation =
    match binop with
    | WT.BinOpAnd -> PT.BinOpAnd
    | WT.BinOpOr -> PT.BinOpOr

module Infix =
  let toPT (infix : WT.Infix) : PT.Infix =
    match infix with
    | WT.InfixFnCall(fn) -> PT.InfixFnCall(InfixFnName.toPT fn)
    | WT.BinOp binop -> PT.BinOp(BinaryOperation.toPT binop)

module LetPattern =
  let rec toPT (context : Context) (p : WT.LetPattern) : (Context * PT.LetPattern) =
    match p with
    | WT.LPVariable(_, varName) ->
      let newContext = bindLocalName context varName
      (newContext, PT.LPVariable(gid (), varName))
    | WT.LPWildcard _ -> (context, PT.LPWildcard(gid ()))
    | WT.LPTuple(_, first, _, second, rest, _, _) ->
      let (context1, first') = toPT context first
      let (context2, second') = toPT context1 second
      let (finalContext, theRest') =
        rest |> foldMapContext (fun ctx (_, pat) -> toPT ctx pat) context2
      (finalContext, PT.LPTuple(gid (), first', second', theRest'))
    | WT.LPUnit _ -> (context, PT.LPUnit(gid ()))

module MatchPattern =
  let rec toPT
    (context : Context)
    (p : WT.MatchPattern)
    : (Context * PT.MatchPattern) =
    match p with
    | WT.MPVariable(_, varName) ->
      let newContext = bindLocalName context varName
      (newContext, PT.MPVariable(gid (), varName))
    | WT.MPEnum(_, (_, caseName), fieldPats) ->
      let (finalContext, convertedPats) = fieldPats |> foldMapContext toPT context
      (finalContext, PT.MPEnum(gid (), caseName, convertedPats))
    | WT.MPInt64(_, (_, i), _) -> (context, PT.MPInt64(gid (), i))
    | WT.MPUInt64(_, (_, i), _) -> (context, PT.MPUInt64(gid (), i))
    | WT.MPInt8(_, (_, i), _) -> (context, PT.MPInt8(gid (), i))
    | WT.MPUInt8(_, (_, i), _) -> (context, PT.MPUInt8(gid (), i))
    | WT.MPInt16(_, (_, i), _) -> (context, PT.MPInt16(gid (), i))
    | WT.MPUInt16(_, (_, i), _) -> (context, PT.MPUInt16(gid (), i))
    | WT.MPInt32(_, (_, i), _) -> (context, PT.MPInt32(gid (), i))
    | WT.MPUInt32(_, (_, i), _) -> (context, PT.MPUInt32(gid (), i))
    | WT.MPInt128(_, (_, i), _) -> (context, PT.MPInt128(gid (), i))
    | WT.MPUInt128(_, (_, i), _) -> (context, PT.MPUInt128(gid (), i))
    | WT.MPInt(_, (_, i)) -> (context, PT.MPInt(gid (), i))
    | WT.MPBool(_, b) -> (context, PT.MPBool(gid (), b))
    | WT.MPChar(_, contents, _, _) ->
      (context,
       PT.MPChar(
         gid (),
         // Normalize like strings so canonically equal chars match.
         (match contents with
          | Some(_, s) -> String.normalize s
          | None -> "")
       ))
    | WT.MPString(_, contents, _, _) ->
      (context,
       PT.MPString(
         gid (),
         (match contents with
          | Some(_, s) -> String.normalize s
          | None -> "")
       ))
    | WT.MPFloat(_, neg, w, f) ->
      (context, PT.MPFloat(gid (), (if neg then Negative else Positive), w, f))
    | WT.MPUnit _ -> (context, PT.MPUnit(gid ()))
    | WT.MPTuple(_, first, _, second, rest, _, _) ->
      let (context1, first') = toPT context first
      let (context2, second') = toPT context1 second
      let (finalContext, theRest') =
        rest |> foldMapContext (fun ctx (_, pat) -> toPT ctx pat) context2
      (finalContext, PT.MPTuple(gid (), first', second', theRest'))
    | WT.MPList(_, contents, _, _) ->
      let (finalContext, convertedPats) =
        contents |> foldMapContext (fun ctx (pat, _) -> toPT ctx pat) context
      (finalContext, PT.MPList(gid (), convertedPats))
    | WT.MPListCons(_, head, tail, _) ->
      let (context1, head') = toPT context head
      let (finalContext, tail') = toPT context1 tail
      (finalContext, PT.MPListCons(gid (), head', tail'))
    | WT.MPOr(_, pats) ->
      let (finalContext, convertedPats) = pats |> foldMapContext toPT context
      (finalContext,
       PT.MPOr(gid (), NEList.ofListUnsafe "MatchPattern.toPT" [] convertedPats))
    | WT.MPError _ ->
      // Parse-error holes cannot reach execution lowering. Callers reject
      // parses with diagnostics before this point.
      Exception.raiseInternal "parse-error hole (MPError) reached lowering" []


module Expr =
  let resolveTypeName
    (pm : PT.PackageManager)
    (onMissing : NR.OnMissing)
    (currentModule : List<string>)
    (names : List<string>)
    (caseName : string) // used for errors
    : Ply<PT.NameResolution<PT.FQTypeName.FQTypeName>> =
    match names with
    | [] -> Ply({ originalName = [ caseName ]; resolved = Error NRE.InvalidName })
    | head :: tail ->
      let name = NEList.ofList head tail |> WT.Unresolved
      NR.resolveTypeName pm onMissing currentModule name

  let rec toPT
    (builtins : RT.Builtins)
    (pm : PT.PackageManager)
    (onMissing : NR.OnMissing)
    (currentModule : List<string>)
    (context : Context)
    (e : WT.Expr)
    : Ply<PT.Expr> =
    let toPT ctx = toPT builtins pm onMissing currentModule ctx
    uply {
      match e with
      | WT.EChar(_, contents, _, _) ->
        return
          PT.EChar(
            gid (),
            // Normalize like strings so canonically equal chars compare equal.
            (match contents with
             | Some(_, s) -> String.normalize s
             | None -> "")
          )
      | WT.EInt64(_, (_, num), _) -> return PT.EInt64(gid (), num)
      | WT.EUInt64(_, (_, num), _) -> return PT.EUInt64(gid (), num)
      | WT.EInt8(_, (_, num), _) -> return PT.EInt8(gid (), num)
      | WT.EUInt8(_, (_, num), _) -> return PT.EUInt8(gid (), num)
      | WT.EInt16(_, (_, num), _) -> return PT.EInt16(gid (), num)
      | WT.EUInt16(_, (_, num), _) -> return PT.EUInt16(gid (), num)
      | WT.EInt32(_, (_, num), _) -> return PT.EInt32(gid (), num)
      | WT.EUInt32(_, (_, num), _) -> return PT.EUInt32(gid (), num)
      | WT.EInt128(_, (_, num), _) -> return PT.EInt128(gid (), num)
      | WT.EUInt128(_, (_, num), _) -> return PT.EUInt128(gid (), num)
      | WT.EInt(_, (_, num)) -> return PT.EInt(gid (), num)
      | WT.EString(_, _, segments, _, _) ->
        let! segments =
          Ply.List.mapSequentially
            (stringSegmentToPT builtins pm onMissing currentModule context)
            segments
        return PT.EString(gid (), segments)
      | WT.EFloat(_, neg, whole, fraction) ->
        return
          PT.EFloat(gid (), (if neg then Negative else Positive), whole, fraction)
      | WT.EBool(_, b) -> return PT.EBool(gid (), b)
      | WT.EUnit _ -> return PT.EUnit(gid ())
      | WT.EVariable(_, var) ->
        let id = gid ()
        // Function args and local bindings take precedence over package names.
        match Map.tryFind var context.argMap with
        | Some index -> return PT.EArg(id, index)
        | None when Set.contains var context.localBindings ->
          return PT.EVariable(id, var)
        | None ->
          // Bare names resolve value-first, then function, then local variable.
          let! value =
            NR.resolveValueName
              (BuiltinNames.values builtins)
              pm
              NR.OnMissing.Allow
              currentModule
              (WT.Unresolved(NEList.singleton var))
          match value.resolved with
          | Ok _ -> return PT.EValue(id, value)
          | Error _ ->
            let! fnResult =
              NR.resolveFnName
                (BuiltinNames.fns builtins)
                pm
                NR.OnMissing.Allow
                currentModule
                (WT.Unresolved(NEList.singleton var))
            match fnResult.resolved with
            | Ok _ -> return PT.EFnName(id, fnResult)
            | Error _ -> return PT.EVariable(id, var)
      | WT.ERecordFieldAccess(_, obj, (_, fieldname), _) ->
        let id = gid ()
        // When we have field access like `Module.fn`, try to resolve as qualified
        // function or value name first, since the parser treats dotted identifiers
        // as field access rather than qualified names.
        let rec extractPath (expr : WT.Expr) : Option<NEList<string>> =
          match expr with
          | WT.EVariable(_, name) -> Some(NEList.singleton name)
          | WT.ERecordFieldAccess(_, inner, (_, field), _) ->
            extractPath inner |> Option.map (fun path -> NEList.pushBack field path)
          | _ -> None

        match extractPath obj with
        | Some basePath ->
          // If the first path segment is local, this is field access, not a
          // package lookup.
          let firstPart = basePath.head
          if
            Set.contains firstPart context.localBindings
            || Map.containsKey firstPart context.argMap
          then
            let! obj = toPT context obj
            return PT.ERecordFieldAccess(id, obj, fieldname)
          else
            let fullPath = NEList.pushBack fieldname basePath
            // Qualified bare names resolve value-first, then function.
            let! valueResult =
              NR.resolveValueName
                (BuiltinNames.values builtins)
                pm
                NR.OnMissing.Allow
                currentModule
                (WT.Unresolved fullPath)
            match valueResult.resolved with
            | Ok _ -> return PT.EValue(id, valueResult)
            | Error _ ->
              let! fnResult =
                NR.resolveFnName
                  (BuiltinNames.fns builtins)
                  pm
                  NR.OnMissing.Allow
                  currentModule
                  (WT.Unresolved fullPath)
              match fnResult.resolved with
              | Ok _ -> return PT.EFnName(id, fnResult)
              | Error _ ->
                let! obj = toPT context obj
                return PT.ERecordFieldAccess(id, obj, fieldname)
        | None ->
          let! obj = toPT context obj
          return PT.ERecordFieldAccess(id, obj, fieldname)
      | WT.EApply(_, (WT.EFnName(_, q) as callee), typeArgs, args) ->
        let id = gid ()
        let name = qualifiedFnName q
        let! processedTypeArgs =
          Ply.List.mapSequentially
            (TypeReference.toPT pm onMissing currentModule)
            typeArgs
        // Every Darklang fn has at least one parameter, so a call always has at
        // least one arg. A type-args-only `f<T>` is really `f<T> ()`: seed the
        // implicit unit arg. Same in serializer lowering.
        let! processedArgs =
          match args with
          | [] -> Ply(NEList.singleton (PT.EUnit(gid ())))
          | h :: t -> Ply.NEList.mapSequentially (toPT context) (NEList.ofList h t)

        // Arguments and local bindings shadow package functions in calls, just
        // as they do for bare references. Only an unshadowed callee resolves as
        // self/package function first. The callee gets its own id, distinct from
        // the wrapping EApply's.
        match name with
        | WT.Unresolved { head = varName; tail = [] } ->
          match Map.tryFind varName context.argMap with
          | Some index ->
            return
              PT.EApply(id, PT.EArg(gid (), index), processedTypeArgs, processedArgs)
          | None when Set.contains varName context.localBindings ->
            return
              PT.EApply(
                id,
                PT.EVariable(gid (), varName),
                processedTypeArgs,
                processedArgs
              )
          | None ->
            let isSelfCall =
              match context.currentFnName with
              | Some currentFnName -> currentModule @ [ varName ] = currentFnName
              | None -> false
            if isSelfCall then
              return
                PT.EApply(id, PT.ESelf(gid ()), processedTypeArgs, processedArgs)
            else
              // Applied names resolve function-first, then value, after lexical
              // bindings and self-recursion have had a chance to shadow them.
              let! fnName =
                NR.resolveFnName
                  (BuiltinNames.fns builtins)
                  pm
                  NR.OnMissing.Allow
                  currentModule
                  name
              match fnName.resolved with
              | Ok _ ->
                return
                  PT.EApply(
                    id,
                    PT.EFnName(gid (), fnName),
                    processedTypeArgs,
                    processedArgs
                  )
              | Error _ ->
                let! valueName =
                  NR.resolveValueName
                    (BuiltinNames.values builtins)
                    pm
                    NR.OnMissing.Allow
                    currentModule
                    name
                let callee =
                  match valueName.resolved with
                  | Ok _ -> PT.EValue(gid (), valueName)
                  | Error _ -> PT.EVariable(gid (), varName)
                return PT.EApply(id, callee, processedTypeArgs, processedArgs)
        | _ ->
          // Qualified callees also resolve function-first. Fall back to the
          // value-first path when no function matches, preserving bare-reference
          // behavior while allowing `Mod.f x` to call the function even when a
          // same-named value exists.
          let! fnNameResolved =
            NR.resolveFnName
              (BuiltinNames.fns builtins)
              pm
              NR.OnMissing.Allow
              currentModule
              name
          let! expr =
            match fnNameResolved.resolved with
            | Ok _ -> Ply(PT.EFnName(gid (), fnNameResolved))
            | Error _ -> toPT context callee
          return PT.EApply(id, expr, processedTypeArgs, processedArgs)
      | WT.EApply(_, lhs, typeArgs, args) ->
        let id = gid ()
        let! name = toPT context lhs
        let! typeArgs =
          Ply.List.mapSequentially
            (TypeReference.toPT pm onMissing currentModule)
            typeArgs
        let! args =
          match args with
          | [] -> Ply(NEList.singleton (PT.EUnit(gid ())))
          | h :: t -> Ply.NEList.mapSequentially (toPT context) (NEList.ofList h t)
        return PT.EApply(id, name, typeArgs, args)
      | WT.EFnName(_, q) ->
        let id = gid ()
        let name = qualifiedFnName q
        // A bare qualified name like `Mod.Sub.foo` could be a package value or a
        // function. The parser uses a fn-name node so it can preserve module
        // ranges, but non-applied references still resolve value-first. Operator
        // `KnownBuiltin` names are always functions,
        // so skip the value lookup. Keep the failed resolution (don't discard it).
        let! valueResolved =
          match name with
          | WT.Unresolved _ ->
            uply {
              let! v =
                NR.resolveValueName
                  (BuiltinNames.values builtins)
                  pm
                  NR.OnMissing.Allow
                  currentModule
                  name
              return Some v
            }
          | WT.KnownBuiltin _ -> uply { return None }
        match valueResolved with
        | Some value when
          (match value.resolved with
           | Ok _ -> true
           | Error _ -> false)
          ->
          return PT.EValue(id, value)
        | _ ->
          let! fnName =
            NR.resolveFnName
              (BuiltinNames.fns builtins)
              pm
              NR.OnMissing.Allow
              currentModule
              name
          match fnName.resolved, valueResolved with
          | Ok _, _ -> return PT.EFnName(id, fnName)
          // A bare qualified name that resolves to neither value nor fn returns
          // EValue(Error),
          // so DeferredResolver can later refresh forward references to values.
          // KnownBuiltin operator names stay EFnName.
          | Error _, Some value -> return PT.EValue(id, value)
          | Error _, None -> return PT.EFnName(id, fnName)
      | WT.ELambda(_, pats, body, _, _) ->
        let id = gid ()
        // Lambda params do not inherit function arg slots. The enclosing
        // function's args become ordinary locals so the lambda closes over them.
        let lambdaContext =
          { context with
              argMap = Map.empty
              localBindings =
                Set.union context.localBindings (context.argMap |> Map.keys |> Set) }
        // Blank (`___`) params parse as empty-name vars; drop them.
        let kept =
          pats
          |> List.filter (fun p ->
            match p with
            | WT.LPVariable(_, "") -> false
            | _ -> true)
        let (finalContext, ptPats) =
          kept |> foldMapContext LetPattern.toPT lambdaContext
        let! body = toPT finalContext body
        let patsNel = neListOrSingleton (PT.LPUnit(gid ())) ptPats
        return PT.ELambda(id, patsNel, body)
      | WT.ELet(_, pat, rhs, body, _, _) ->
        let id = gid ()
        // A let binder is not in scope in its rhs. For a let-bound lambda, an
        // unresolved reference to the binding name therefore stays EVariable
        // (and becomes recursion at runtime), while a package fn/value resolves
        // normally so the runtime can report the ambiguity.
        //
        // The one inherited context that can pre-empt that distinction is ESelf:
        // when the nested binding has the enclosing package fn's name, resolve it
        // normally instead of treating it as an unconditional outer self-call.
        let rhsContext =
          match pat, rhs, context.currentFnName with
          | WT.LPVariable(_, name), WT.ELambda _, Some qualifiedName when
            List.tryLast qualifiedName = Some name
            ->
            { context with currentFnName = None }
          | _ -> context
        let! rhs = toPT rhsContext rhs
        let (newContext, ptPat) = LetPattern.toPT context pat
        let! body = toPT newContext body
        return PT.ELet(id, ptPat, rhs, body)
      | WT.EIf(_, cond, thenExpr, elseExpr, _, _, _) ->
        let id = gid ()
        let! cond = toPT context cond
        let! thenExpr = toPT context thenExpr
        let! elseExpr =
          uply {
            match elseExpr with
            | Some value ->
              let! newValue = toPT context value
              return Some newValue
            | None -> return None
          }
        return PT.EIf(id, cond, thenExpr, elseExpr)
      | WT.EList(_, contents, _, _) ->
        let id = gid ()
        let! exprs =
          contents |> List.map fst |> Ply.List.mapSequentially (toPT context)
        return PT.EList(id, exprs)
      | WT.ETuple(_, first, _, second, rest, _, _) ->
        let id = gid ()
        let! first = toPT context first
        let! second = toPT context second
        let! theRest = Ply.List.mapSequentially (fun (_, e) -> toPT context e) rest
        return PT.ETuple(id, first, second, theRest)
      | WT.EDict(_, contents, _, _, _) ->
        let id = gid ()
        let! pairs =
          contents
          |> Ply.List.mapSequentially (fun (_, (_, key), v) ->
            uply {
              let! v = toPT context v
              return (key, v)
            })
        return PT.EDict(id, pairs)
      | WT.ERecord(_, tn, fields, _, _) ->
        let id = gid ()
        let entries = fields |> List.map (fun (_, (_, name), v) -> (name, v))
        let! typeName =
          NR.resolveTypeName pm onMissing currentModule (qualifiedTypeName tn)
        let! flds =
          entries
          |> Ply.List.mapSequentially (fun (fieldName, fieldExpr) ->
            uply {
              let! fieldExpr = toPT context fieldExpr
              return (fieldName, fieldExpr)
            })
        let! typeArgs =
          Ply.List.mapSequentially
            (TypeReference.toPT pm onMissing currentModule)
            tn.typeArgs
        return PT.ERecord(id, typeName, typeArgs, flds)
      | WT.ERecordUpdate(_, record, updates, _, _, _) ->
        let id = gid ()
        let! record = toPT context record
        let lowered = updates |> List.map (fun ((_, name), _, v) -> (name, v))
        let! updatesPT =
          match lowered with
          | [] -> Ply(NEList.singleton ("_", PT.EUnit(gid ())))
          | h :: t ->
            (h :: t)
            |> Ply.List.mapSequentially (fun (name, e) ->
              uply {
                let! e = toPT context e
                return (name, e)
              })
            |> Ply.map (NEList.ofListUnsafe "record update" [])
        return PT.ERecordUpdate(id, record, updatesPT)
      | WT.EPipe(_, expr, pipeExprs) ->
        let id = gid ()
        let! expr = toPT context expr
        let! rest =
          pipeExprs
          |> List.map snd
          |> Ply.List.mapSequentially (
            pipeExprToPT builtins pm onMissing currentModule context
          )
        return PT.EPipe(id, expr, rest)
      // An unqualified uppercase name (`Red`, `XDB`) parses as a zero-field enum,
      // but it is ambiguous: it could be an enum case, a DB, or an uppercase-bound
      // variable (values/fns are lowercase, so never those). Lower it to an
      // EVariable so name resolution disambiguates. Left as an EEnum, a DB or
      // variable ref would be forced into enum-case resolution and fail.
      | WT.EEnum(_, tn, (_, caseName), fields, _) when
        List.isEmpty (enumTypeNameForCase tn caseName) && List.isEmpty fields
        ->
        return! toPT context (WT.EVariable(WT.synthRange, caseName))
      | WT.EEnum(_, tn, (_, caseName), fields, _) ->
        let id = gid ()
        let! typeName =
          resolveTypeName
            pm
            onMissing
            currentModule
            (enumTypeNameForCase tn caseName)
            caseName
        let! exprs = Ply.List.mapSequentially (toPT context) fields
        let! typeArgs =
          Ply.List.mapSequentially
            (TypeReference.toPT pm onMissing currentModule)
            tn.typeArgs
        return PT.EEnum(id, typeName, typeArgs, caseName, exprs)
      | WT.EMatch(_, mexpr, cases, _, _) ->
        let id = gid ()
        let! mexpr = toPT context mexpr
        let! cases =
          Ply.List.mapSequentially
            (fun (case : WT.MatchCase) ->
              uply {
                let (patternContext, mp) = MatchPattern.toPT context case.pat
                let! whenCondition =
                  uply {
                    match case.whenCondition with
                    | Some(_, whenExpr) ->
                      let! whenExpr = toPT patternContext whenExpr
                      return Some whenExpr
                    | None -> return None
                  }
                let! expr = toPT patternContext case.rhs
                let result : PT.MatchCase =
                  { pat = mp; whenCondition = whenCondition; rhs = expr }
                return result
              })
            cases

        return PT.EMatch(id, mexpr, cases)
      | WT.EInfix(_, (_, infixOp), arg1, arg2) ->
        let id = gid ()
        let! arg1 = toPT context arg1
        let! arg2 = toPT context arg2
        return PT.EInfix(id, Infix.toPT infixOp, arg1, arg2)
      | WT.EStatement(_, first, next) ->
        let! first = toPT context first
        let! next = toPT context next
        return PT.EStatement(gid (), first, next)
      | WT.EError _ ->
        return
          Exception.raiseInternal
            "parse-error hole (EError) reached lowering"
            [ "hint", box "validate the WrittenTypes tree before lowering" ]
    }

  and stringSegmentToPT
    (builtins : RT.Builtins)
    (pm : PT.PackageManager)
    (onMissing : NR.OnMissing)
    (currentModule : List<string>)
    (context : Context)
    (segment : WT.StringSegment)
    : Ply<PT.StringSegment> =
    match segment with
    // Normalize literal text to match string tokenization.
    | WT.StringText(_, text) -> Ply(PT.StringText(String.normalize text))
    | WT.StringInterpolation(_, expr, _, _) ->
      toPT builtins pm onMissing currentModule context expr
      |> Ply.map (fun interpolated -> PT.StringInterpolation interpolated)

  and pipeExprToPT
    (builtins : RT.Builtins)
    (pm : PT.PackageManager)
    (onMissing : NR.OnMissing)
    (currentModule : List<string>)
    (context : Context)
    (pipeExpr : WT.PipeExpr)
    : Ply<PT.PipeExpr> =
    let toPT ctx = toPT builtins pm onMissing currentModule ctx

    uply {
      match pipeExpr with
      | WT.EPipeVariableOrFnCall(_, name) ->
        let id = gid ()
        // Arguments and locals shadow package functions in pipe segments too.
        // Only an unshadowed bare name resolves function-first.
        if
          Map.containsKey name context.argMap
          || Set.contains name context.localBindings
        then
          return PT.EPipeVariable(id, name, [])
        else
          let! resolved =
            NR.resolveFnName
              (BuiltinNames.fns builtins)
              pm
              NR.OnMissing.Allow
              currentModule
              (WT.Name.Unresolved(NEList.singleton name))
          return
            match resolved.resolved with
            | Ok _ -> PT.EPipeFnCall(id, resolved, [], [])
            | Error _ -> PT.EPipeVariable(id, name, [])

      | WT.EPipeLambda(_, pats, body, _, _) ->
        let id = gid ()
        // Lambda params do not inherit function arg slots. The enclosing
        // function's args become ordinary locals so the lambda closes over them.
        let lambdaContext =
          { context with
              argMap = Map.empty
              localBindings =
                Set.union context.localBindings (context.argMap |> Map.keys |> Set) }
        let kept =
          pats
          |> List.filter (fun p ->
            match p with
            | WT.LPVariable(_, "") -> false
            | _ -> true)
        let (finalContext, ptPats) =
          kept |> foldMapContext LetPattern.toPT lambdaContext
        let! body = toPT finalContext body
        let patsNel = neListOrSingleton (PT.LPUnit(gid ())) ptPats
        return PT.EPipeLambda(id, patsNel, body)

      | WT.EPipeInfix(_, (_, infixOp), first) ->
        let id = gid ()
        let! first = toPT context first
        return PT.EPipeInfix(id, Infix.toPT infixOp, first)

      | WT.EPipeFnCall(_, q, typeArgs, args) ->
        let id = gid ()
        let name = qualifiedFnName q
        match name, typeArgs with
        | WT.Unresolved { head = varName; tail = [] }, [] ->
          // A bare pipe segment with args might be a function call or a variable
          // application. Resolve function-first, then fall back to variable.
          let! args = Ply.List.mapSequentially (toPT context) args
          if
            Map.containsKey varName context.argMap
            || Set.contains varName context.localBindings
          then
            return PT.EPipeVariable(id, varName, args)
          else
            let! fnName =
              NR.resolveFnName
                (BuiltinNames.fns builtins)
                pm
                NR.OnMissing.Allow
                currentModule
                name
            match fnName.resolved with
            | Ok _ -> return PT.EPipeFnCall(id, fnName, [], args)
            | Error _ -> return PT.EPipeVariable(id, varName, args)
        | _ ->
          // Missing names use Allow here, like other fn-name lowering. Package
          // loading can continue and unresolved names are handled later.
          let! fnName =
            NR.resolveFnName
              (BuiltinNames.fns builtins)
              pm
              NR.OnMissing.Allow
              currentModule
              name
          let! typeArgs =
            Ply.List.mapSequentially
              (TypeReference.toPT pm onMissing currentModule)
              typeArgs
          let! args = Ply.List.mapSequentially (toPT context) args
          return PT.EPipeFnCall(id, fnName, typeArgs, args)

      | WT.EPipeEnum(_, tn, (_, caseName), fields, _) ->
        let id = gid ()
        let! typeName =
          resolveTypeName
            pm
            onMissing
            currentModule
            (enumTypeNameForCase tn caseName)
            caseName
        let! fields = Ply.List.mapSequentially (toPT context) fields
        return PT.EPipeEnum(id, typeName, caseName, fields)
    }


module TypeDeclaration =
  module RecordField =
    let toPT
      (pm : PT.PackageManager)
      (onMissing : NR.OnMissing)
      (currentModule : List<string>)
      (f : WT.TypeDeclaration.RecordField)
      : Ply<PT.TypeDeclaration.RecordField> =
      uply {
        let! typ = TypeReference.toPT pm onMissing currentModule f.typ
        return { name = f.name; typ = typ; description = f.description }
      }

  module EnumField =
    let toPT
      (pm : PT.PackageManager)
      (onMissing : NR.OnMissing)
      (currentModule : List<string>)
      (f : WT.TypeDeclaration.EnumField)
      : Ply<PT.TypeDeclaration.EnumField> =
      uply {
        let! typ = TypeReference.toPT pm onMissing currentModule f.typ
        return { typ = typ; label = f.label; description = f.description }
      }

  module EnumCase =
    let toPT
      (pm : PT.PackageManager)
      (onMissing : NR.OnMissing)
      (currentModule : List<string>)
      (c : WT.TypeDeclaration.EnumCase)
      : Ply<PT.TypeDeclaration.EnumCase> =
      uply {
        let! fields =
          Ply.List.mapSequentially
            (EnumField.toPT pm onMissing currentModule)
            c.fields
        return { name = c.name; fields = fields; description = c.description }
      }

  module Definition =
    let toPT
      (pm : PT.PackageManager)
      (onMissing : NR.OnMissing)
      (currentModule : List<string>)
      (d : WT.TypeDeclaration.Definition)
      : Ply<PT.TypeDeclaration.Definition> =
      uply {
        match d with
        | WT.TypeDeclaration.Alias typ ->
          let! typ = TypeReference.toPT pm onMissing currentModule typ
          return PT.TypeDeclaration.Alias typ

        | WT.TypeDeclaration.Record fields ->
          let! fields =
            Ply.NEList.mapSequentially
              (RecordField.toPT pm onMissing currentModule)
              fields
          return PT.TypeDeclaration.Record fields

        | WT.TypeDeclaration.Enum cases ->
          let! cases =
            Ply.NEList.mapSequentially
              (EnumCase.toPT pm onMissing currentModule)
              cases
          return PT.TypeDeclaration.Enum cases
      }


  let toPT
    (pm : PT.PackageManager)
    (onMissing : NR.OnMissing)
    (currentModule : List<string>)
    (d : WT.TypeDeclaration.T)
    : Ply<PT.TypeDeclaration.T> =
    uply {
      let! def = Definition.toPT pm onMissing currentModule d.definition
      return { typeParams = d.typeParams; definition = def }
    }


module PackageRefs = LibExecution.PackageRefs

// --- lower package declarations (types / values / fns) to ProgramTypes ---
//
// Each is stamped with a placeholder `Hash ""` here; the real content-addressed
// hash is computed downstream by stabilization (`LibDB.HashStabilization` / the
// `pmStabilizeHashes` builtin), not at lowering time. `Name.toLocation` /
// `toModules` derive the owner-qualified location the graft keys on.

module PackageType =
  module Name =
    let toLocation (name : WT.PackageType.Name) : PT.PackageLocation =
      { owner = name.owner; modules = name.modules; name = name.name }

    let toModules (name : WT.PackageType.Name) : List<string> =
      name.owner :: name.modules

  let toPT
    (pm : PT.PackageManager)
    (onMissing : NR.OnMissing)
    (currentModule : List<string>)
    (pt : WT.PackageType.PackageType)
    : Ply<PT.PackageType.PackageType> =
    uply {
      let! declaration =
        TypeDeclaration.toPT pm onMissing currentModule pt.declaration
      return
        { hash = Hash ""; description = pt.description; declaration = declaration }
    }

module PackageValue =
  module Name =
    let toLocation (name : WT.PackageValue.Name) : PT.PackageLocation =
      { owner = name.owner; modules = name.modules; name = name.name }

    let toModules (name : WT.PackageValue.Name) : List<string> =
      name.owner :: name.modules

  let toPT
    (builtins : RT.Builtins)
    (pm : PT.PackageManager)
    (onMissing : NR.OnMissing)
    (currentModule : List<string>)
    (c : WT.PackageValue.PackageValue)
    : Ply<PT.PackageValue.PackageValue> =
    uply {
      let context =
        { currentFnName = None; argMap = Map.empty; localBindings = Set.empty }
      let! body = Expr.toPT builtins pm onMissing currentModule context c.body
      return { hash = Hash ""; description = c.description; body = body }
    }


module PackageFn =
  module Name =
    let toLocation (name : WT.PackageFn.Name) : PT.PackageLocation =
      { owner = name.owner; modules = name.modules; name = name.name }

    let toModules (name : WT.PackageFn.Name) : List<string> =
      name.owner :: name.modules

  module Parameter =
    let toPT
      (pm : PT.PackageManager)
      (onMissing : NR.OnMissing)
      (currentModule : List<string>)
      (p : WT.PackageFn.Parameter)
      : Ply<PT.PackageFn.Parameter> =
      uply {
        let! typ = TypeReference.toPT pm onMissing currentModule p.typ
        return { name = p.name; typ = typ; description = p.description }
      }

  /// Collect TVariable names from a PT TypeReference.
  /// Used to discover implicit function type parameters from declared
  /// param/return types. For example, `let f (x: List<'a>) : Stream<'a> = ...`
  /// may not declare `<'a>` explicitly, but callers still need `'a` registered
  /// when inference cannot fill it in, such as with empty literals.
  let rec private collectTVars
    (acc : List<string>)
    (tr : PT.TypeReference)
    : List<string> =
    match tr with
    | PT.TVariable name when not (List.contains name acc) -> acc @ [ name ]
    | PT.TVariable _ -> acc
    | PT.TList inner
    | PT.TStream inner
    | PT.TDict inner
    | PT.TDB inner -> collectTVars acc inner
    | PT.TTuple(a, b, rest) ->
      let acc = collectTVars acc a
      let acc = collectTVars acc b
      rest |> List.fold collectTVars acc
    | PT.TCustomType(_, args) -> args |> List.fold collectTVars acc
    | PT.TFn(args, ret) ->
      let acc = NEList.toList args |> List.fold collectTVars acc
      collectTVars acc ret
    | _ -> acc

  let toPT
    (builtins : RT.Builtins)
    (pm : PT.PackageManager)
    (onMissing : NR.OnMissing)
    (currentModule : List<string>)
    (fn : WT.PackageFn.PackageFn)
    : Ply<PT.PackageFn.PackageFn> =
    uply {
      let! parameters =
        Ply.NEList.mapSequentially
          (Parameter.toPT pm onMissing currentModule)
          fn.parameters
      let! returnType = TypeReference.toPT pm onMissing currentModule fn.returnType
      let argMap =
        fn.parameters
        |> NEList.toList
        |> List.mapi (fun i param -> param.name, i)
        |> Map.ofList
      let context =
        { currentFnName = Some(currentModule @ [ fn.name.name ])
          argMap = argMap
          localBindings = Set.empty }
      let! body = Expr.toPT builtins pm onMissing currentModule context fn.body

      // Explicit typeParams stay first because callers pass positional type
      // args in that order. Discovered names append in first-seen order.
      let explicitTypeParams = fn.typeParams
      let implicitTypeParams =
        let fromParams =
          parameters
          |> NEList.toList
          |> List.fold (fun acc p -> collectTVars acc p.typ) []
        let withReturn = collectTVars fromParams returnType
        withReturn |> List.filter (fun n -> not (List.contains n explicitTypeParams))
      let allTypeParams = explicitTypeParams @ implicitTypeParams

      return
        { hash = Hash ""
          parameters = parameters
          returnType = returnType
          description = fn.description
          body = body
          typeParams = allTypeParams }
    }




module DB =
  let toPT
    (pm : PT.PackageManager)
    (onMissing : NR.OnMissing)
    (currentModule : List<string>)
    (db : WT.DB.T)
    : Ply<PT.DB.T> =
    uply {
      let! typ = TypeReference.toPT pm onMissing currentModule db.typ
      return { tlid = gid (); name = db.name; version = db.version; typ = typ }
    }


// Handler removed — see notes/wrap-up/handler-toplevel-deletion-plan.md
// for the cutover. Worker / Cron / REPL had no production constructors;
// HTTP went earlier with the BwdServer rewrite.
