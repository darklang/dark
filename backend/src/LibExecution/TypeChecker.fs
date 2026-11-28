/// The run-time type-checker
/// Called by the interpreter, and a few other places
module LibExecution.TypeChecker

open Prelude
open RuntimeTypes
module VT = ValueType
module RTE = RuntimeError


type TypeCheckPathPart = RuntimeError.TypeChecking.TypeCheckPathPart
type ReverseTypeCheckPath = RuntimeError.TypeChecking.ReverseTypeCheckPath

/// The answer for the unifications that need no recursion and no type lookup: a concrete scalar
/// against its own ValueType, or anything against `Unknown`. That is the overwhelming majority of
/// calls, and type-checking arguments and results is a large share of what the interpreter
/// allocates, so answering them without the general unifier is worth the separate path.
///
/// `TVariable` is deliberately excluded rather than folded into the `Unknown` case. The full matcher tries
/// `TVariable name, _` FIRST, so a type variable meeting `Unknown` binds the variable to `Unknown`; short
/// -circuiting that here would skip the binding and silently change inference.
let private unifiesTrivially (expected : TypeReference) (actual : ValueType) : bool =
  // Matches on `expected` and then on `actual` rather than on the pair. F# allocates the tuple for a
  // two-value match here rather than eliding it -- the same change to `inferTVarsFromArg` moved total
  // allocation 3.3% -- and this runs on every unification.
  //
  // `TVariable` is deliberately excluded rather than folded into the `Unknown` case: the full matcher
  // tries `TVariable name, _` FIRST, so a type variable meeting `Unknown` binds the variable to
  // `Unknown`, and short-circuiting that here would skip the binding and change inference.
  match expected with
  | TVariable _ -> false
  | _ ->
    match actual with
    | ValueType.Unknown -> true
    | ValueType.Known kt ->
      match expected with
      | TUnit ->
        (match kt with
         | KTUnit -> true
         | _ -> false)
      | TBool ->
        (match kt with
         | KTBool -> true
         | _ -> false)
      | TInt8 ->
        (match kt with
         | KTInt8 -> true
         | _ -> false)
      | TUInt8 ->
        (match kt with
         | KTUInt8 -> true
         | _ -> false)
      | TInt16 ->
        (match kt with
         | KTInt16 -> true
         | _ -> false)
      | TUInt16 ->
        (match kt with
         | KTUInt16 -> true
         | _ -> false)
      | TInt32 ->
        (match kt with
         | KTInt32 -> true
         | _ -> false)
      | TUInt32 ->
        (match kt with
         | KTUInt32 -> true
         | _ -> false)
      | TInt64 ->
        (match kt with
         | KTInt64 -> true
         | _ -> false)
      | TUInt64 ->
        (match kt with
         | KTUInt64 -> true
         | _ -> false)
      | TInt128 ->
        (match kt with
         | KTInt128 -> true
         | _ -> false)
      | TUInt128 ->
        (match kt with
         | KTUInt128 -> true
         | _ -> false)
      | TBigInt ->
        (match kt with
         | KTBigInt -> true
         | _ -> false)
      | TFloat64 ->
        (match kt with
         | KTFloat64 -> true
         | _ -> false)
      | TFloat32 ->
        (match kt with
         | KTFloat32 -> true
         | _ -> false)
      | TDecimal ->
        (match kt with
         | KTDecimal -> true
         | _ -> false)
      | TChar ->
        (match kt with
         | KTChar -> true
         | _ -> false)
      | TString ->
        (match kt with
         | KTString -> true
         | _ -> false)
      | TUuid ->
        (match kt with
         | KTUuid -> true
         | _ -> false)
      | TDateTime ->
        (match kt with
         | KTDateTime -> true
         | _ -> false)
      | TBytes ->
        (match kt with
         | KTBytes -> true
         | _ -> false)
      | _ -> false


/// Answer a unification for the cases that need no type lookup and no recursion into a compound type.
///
/// `Undecided` means "this one goes to the full check"; it is never a type error, so callers must
/// fall back rather than treat it as a failure.
///
/// A struct DU rather than `Result<_, _> voption`: this runs per argument per call, and the `Ok` wrapper
/// alone was one of the more frequently allocated objects in the interpreter.
[<Struct>]
type SyncUnification =
  /// Unified, with the symbol table it produced.
  | Unified of tst : TypeSymbolTable
  /// Genuinely doesn't unify. Carries no path to where: `tryUnify`, the only way in, answers any
  /// failure by re-running the whole check on the full route, which builds its own path for the error
  /// message. Threading one through here meant a cons per container level per argument per call, for a
  /// value that was discarded on failure and never read on success.
  | Mismatched
  /// Needs the full check -- a type lookup, or an error message worth building properly.
  | Undecided

let rec unifyValueTypeSync
  (tst : TypeSymbolTable)
  (expected : TypeReference)
  (actual : ValueType)
  : SyncUnification =
  if unifiesTrivially expected actual then
    Unified tst
  else
    match expected with
    // Same reasoning as the hoisted case in `unifyValueType`, and deliberately the same logic: most
    // arguments bind a type variable, and most rebind it to what it already held.
    | TVariable name ->
      // A `voption` lookup, so finding a bound type variable -- the common case here, not the
      // exception -- costs no `Some`.
      match tst.TryFind name with
      | ValueNone -> Unified(TST.add name actual tst)
      | ValueSome bound ->
        if bound = actual then
          Unified tst
        else
          match ValueType.merge bound actual with
          | Ok merged ->
            if merged = bound then Unified tst else Unified(TST.add name merged tst)
          | Error() -> Mismatched

    // Containers recurse but never need a type lookup, so they stay on this path. Without these a
    // `List<Int64>` argument -- which is most of what the standard library passes around -- would fall to
    // the full check purely for being a container, which is the common case rather than a rare one.
    | TList innerT ->
      match actual with
      | ValueType.Known(KTList innerV) -> unifyValueTypeSync tst innerT innerV
      | _ -> Undecided
    | TStream innerT ->
      match actual with
      | ValueType.Known(KTStream innerV) -> unifyValueTypeSync tst innerT innerV
      | _ -> Undecided
    | TDict(keyT, valueT) ->
      match actual with
      | ValueType.Known(KTDict(keyV, valueV)) ->
        match unifyValueTypeSync tst keyT keyV with
        | Unified tst -> unifyValueTypeSync tst valueT valueV
        | notUnified -> notUnified
      | _ -> Undecided

    | TTuple(tFirst, tSecond, tRest) ->
      match actual with
      | ValueType.Known(KTTuple(vFirst, vSecond, vRest)) ->
        // Element by element, the rest pairwise: consing the first two onto the rest of each
        // side, to walk one list, allocated four cells on every check of a tuple, and a list of
        // tuples is checked on every call that passes one.
        match unifyValueTypeSync tst tFirst vFirst with
        | Unified tst ->
          match unifyValueTypeSync tst tSecond vSecond with
          | Unified tst -> unifyTypeArgsSyncVT tst tRest vRest
          | other -> other
        | other -> other
      | _ -> Undecided

    // A custom type of exactly the declared name. `Types.find` would only be needed to ask whether
    // the declared type is an alias, and it can't be: an alias's name is the alias, but a value
    // built through one carries the underlying type's name, so equal names mean it resolved to
    // itself. Same argument as the `Dval` version in `tryUnify`.
    //
    // This case is what makes a *nested* custom type stay on the fast path. Without it,
    // `Result<'a, ParseError>` fell to the full unifier over the `ParseError`, which is to say
    // every function in the language that can fail.
    | TCustomType({ resolved = Ok declared }, declaredArgs) ->
      match actual with
      | ValueType.Known(KTCustomType(actualName, actualArgs)) when
        actualName = declared
        ->
        unifyTypeArgsSyncVT tst declaredArgs actualArgs
      | _ -> Undecided

    // Everything else can need `Types.find`.
    | _ -> Undecided

/// Type arguments unified pairwise, synchronously. Top-level and mutually recursive with
/// `unifyValueTypeSync` so neither captures anything.
and private unifyTypeArgsSyncVT
  (tst : TypeSymbolTable)
  (declared : List<TypeReference>)
  (actual : List<ValueType>)
  : SyncUnification =
  // Nested matches, not `match declared, actual with`: the tuple form allocates the pair, once per
  // type argument on the path every parameterised value takes.
  match declared with
  | [] ->
    match actual with
    | [] -> Unified tst
    | _ -> Undecided
  | d :: dRest ->
    match actual with
    | [] -> Undecided
    | a :: aRest ->
      match unifyValueTypeSync tst d a with
      | Unified tst -> unifyTypeArgsSyncVT tst dRest aRest
      | other -> other


/// Alias unwrapping without a CE. Only a resolved custom type can be an alias, and only that case needs
/// `Types.find`, so everything else is already the answer.
let unwrapAliasSync (typ : TypeReference) : TypeReference voption =
  match typ with
  | TCustomType({ resolved = Ok _ }, _) -> ValueNone
  | _ -> ValueSome typ


/// What each alias the full unifier has unwrapped stands for, so the fast check can
/// see through it next time instead of going back to `Types.find`.
///
/// Process-wide and never cleared, which is safe because type names are content hashes: an alias's
/// name fixes its declaration, and so the type it unwraps to. Filled only by the full check, so it
/// holds exactly the answers that path gave.
///
/// Without it every value checked against an alias takes the full check, a lookup and a handful
/// of closures per call, for example each `Int64` returned as an `ID`.
let aliasTargets =
  System.Collections.Concurrent.ConcurrentDictionary<FQTypeName.FQTypeName, TypeReference>()

let rec unifyValueType
  (types : Types)
  (tst : TypeSymbolTable)
  (pathSoFar : ReverseTypeCheckPath)
  (expected : TypeReference)
  (actual : ValueType)
  : Result<TypeSymbolTable, ReverseTypeCheckPath> =
  if unifiesTrivially expected actual then
    (Ok tst)
  else

    match expected with
    // Hoisted out of the match below because it is the hot path. ~88% of calls are into functions
    // whose signature has a type variable, and `unifiesTrivially` never claims one, so nearly every
    // non-trivial unification lands here. Answering it first skips the `(expected, actual)` pair the
    // tuple match allocates.
    | TVariable name ->
      (match TST.tryFind name tst |> ValueOption.toOption with
       | None -> (Ok(TST.add name actual tst))
       | Some t ->
         // Already bound to exactly this: `Map.add` would rebuild the symbol table's spine to store the
         // value it already holds. Dark's stdlib is heavily polymorphic, so this is the *common* path --
         // nearly every argument check binds a type variable, and most rebind it to what it already was.
         if t = actual then
           (Ok tst)
         else
           match ValueType.merge t actual with
           | Ok merged ->
             if merged = t then (Ok tst) else (Ok(TST.add name merged tst))
           | Error() -> (Error pathSoFar))

    | _ ->

      // Bound after the fast paths: this is a partial application, so it allocates a closure on every call
      // that reaches it, and most calls no longer do.
      let r = unifyValueType types

      match expected, actual with

      | TVariable _, _ ->
        // Unreachable: handled above. Kept so the match stays exhaustive over TypeReference.
        Ok tst

      | _, ValueType.Unknown -> Ok tst


      | TUnit, ValueType.Known KTUnit -> Ok tst
      | TBool, ValueType.Known KTBool -> Ok tst

      | TInt8, ValueType.Known KTInt8 -> Ok tst
      | TUInt8, ValueType.Known KTUInt8 -> Ok tst
      | TInt16, ValueType.Known KTInt16 -> Ok tst
      | TUInt16, ValueType.Known KTUInt16 -> Ok tst
      | TInt32, ValueType.Known KTInt32 -> Ok tst
      | TUInt32, ValueType.Known KTUInt32 -> Ok tst
      | TInt64, ValueType.Known KTInt64 -> Ok tst
      | TUInt64, ValueType.Known KTUInt64 -> Ok tst
      | TInt128, ValueType.Known KTInt128 -> Ok tst
      | TUInt128, ValueType.Known KTUInt128 -> Ok tst
      | TBigInt, ValueType.Known KTBigInt -> Ok tst

      | TFloat64, ValueType.Known KTFloat64 -> Ok tst
      | TFloat32, ValueType.Known KTFloat32 -> Ok tst
      | TDecimal, ValueType.Known KTDecimal -> Ok tst

      | TChar, ValueType.Known KTChar -> Ok tst
      | TString, ValueType.Known KTString -> Ok tst

      | TUuid, ValueType.Known KTUuid -> Ok tst
      | TDateTime, ValueType.Known KTDateTime -> Ok tst

      | TBytes, ValueType.Known KTBytes -> Ok tst

      | TStream innerT, ValueType.Known(KTStream innerV) ->
        r tst (TypeCheckPathPart.ListType :: pathSoFar) innerT innerV

      | TList innerT, ValueType.Known(KTList innerV) ->
        r tst (TypeCheckPathPart.ListType :: pathSoFar) innerT innerV

      | TDict(keyT, valueT), ValueType.Known(KTDict(keyV, valueV)) ->
        match r tst (TypeCheckPathPart.DictKeyType :: pathSoFar) keyT keyV with
        | Ok tst ->
          r tst (TypeCheckPathPart.DictValueType :: pathSoFar) valueT valueV
        | error -> error

      | TTuple(tFirst, tSecond, tRest),
        ValueType.Known(KTTuple(vFirst, vSecond, vRest)) ->
        // first, make sure that the tuple lengths match
        let expectedLen = 2 + List.length tRest
        let actualLen = 2 + List.length vRest
        if expectedLen <> actualLen then
          Error(TypeCheckPathPart.TupleLength(expectedLen, actualLen) :: pathSoFar)
        else
          // then, make sure that the tuple elements match
          let expected = tFirst :: tSecond :: tRest
          let actual = vFirst :: vSecond :: vRest
          List.foldWithIndex
            (fun i acc (e, a) ->
              match acc with
              | Error _ -> acc
              | Ok tst -> r tst (TypeCheckPathPart.TupleAtIndex i :: pathSoFar) e a)
            (Ok tst)
            (List.zip expected actual)

      | TCustomType({ originalName = names; resolved = Error err }, _), _ ->
        RTE.ParseTimeNameResolution(names, err) |> raiseRTE

      | TCustomType({ resolved = Ok typeNameT }, typeArgsT), actual ->
        // CLEANUP can't we assume aliases are already unwrapped?
        // if so, we can tidy this case quite a bit
        match Types.find types typeNameT with
        | None -> Error pathSoFar
        | Some declaration ->
          match declaration, actual with
          | { definition = TypeDeclaration.Alias _ }, _ ->
            // Unwrapped with its type arguments: the alias's own parameters are not the
            // caller's type variables, so one alias used at two types is two types.
            let target = TypeReference.unwrapAlias types expected
            // Keyed by name alone, so only an alias with no parameters is the same type each time.
            if List.isEmpty declaration.typeParams then
              aliasTargets.TryAdd(typeNameT, target) |> ignore<bool>
            r tst pathSoFar target actual

          | _, ValueType.Known(KTCustomType(typeNameV, typeArgsV)) ->
            if typeNameV <> typeNameT then
              Error pathSoFar
            else if List.length typeArgsT <> List.length typeArgsV then
              // (this is really unexpected -- interpreter should prevent this)
              Error(
                TypeCheckPathPart.TypeArgLength(
                  typeNameT,
                  List.length typeArgsT,
                  List.length typeArgsV
                )
                :: pathSoFar
              )
            elif List.isEmpty typeArgsT then
              // No type arguments to walk, which is nearly every record and enum. Worth saying
              // outright rather than folding over an empty list: `foldSequentiallyWithIndex`
              // builds its `(0, state)` tuple and its lambda's closure before it discovers there
              // is nothing to iterate, and none of that is free.
              Ok tst
            else
              let typeArgCount = List.length typeArgsT
              List.zip typeArgsT typeArgsV
              |> List.foldWithIndex
                (fun i acc (e, a) ->
                  match acc with
                  | Error _path -> acc
                  | Ok tst ->
                    let path =
                      TypeCheckPathPart.TypeArg(typeNameT, i, typeArgCount)
                      :: pathSoFar
                    match r tst path e a with
                    | Error path -> Error path
                    | Ok tst -> Ok tst)
                (Ok tst)

          | _, _ -> Error pathSoFar

      | TFn(argTypes, returnType), ValueType.Known(KTFn(vArgs, vRet)) ->
        if NEList.length argTypes <> NEList.length vArgs then
          Error pathSoFar
        else
          List.zip
            (returnType :: (NEList.toList argTypes))
            (vRet :: (NEList.toList vArgs))
          |> List.fold
            (fun acc (e, a) ->
              match acc with
              | Error _path -> acc
              | Ok tst -> r tst pathSoFar e a)
            (Ok tst)

      | _, _ -> Error pathSoFar


let unify
  (types : Types)
  (tst : TypeSymbolTable)
  (expected : TypeReference)
  (actual : Dval)
  : Result<TypeSymbolTable, ReverseTypeCheckPath> =
  let actualType = Dval.toValueType actual
  if unifiesTrivially expected actualType then
    (Ok tst)
  else
    match unifyValueType types tst [] expected actualType with
    | Error path -> path |> Error
    | Ok updatedTst -> Ok updatedTst

/// Resolved declarations for types with no type arguments, which is nearly all of them.
///
/// Type-checking a record or enum argument resolves its declaration, which otherwise goes to the
/// package manager on every call. Anything that passes records around pays it constantly.
///
/// Keyed on the `Types` instance as well as the name. Keying on the name alone looks safe -- an
/// `FQTypeName` is a content hash, so the same name should mean the same declaration -- but it is
/// not: tests mint their own package managers and reuse names across different declarations.
/// Scoping per `Types` costs nothing in practice, since a server has one for the life of the
/// process.
///
/// An alias is cached only when it has no type params of its own: one that has them resolves
/// through the caller's type symbol table, so its answer isn't a function of the name.
let private resolvedTypeCache =
  System.Runtime.CompilerServices.ConditionalWeakTable<Types, System.Collections.Concurrent.ConcurrentDictionary<FQTypeName.FQTypeName, FQTypeName.FQTypeName *
  List<string * ValueType> *
  TypeDeclaration.Definition>>()

let private cacheFor (types : Types) =
  let mutable d = Unchecked.defaultof<_>
  if resolvedTypeCache.TryGetValue(types, &d) then
    d
  else
    let fresh =
      System.Collections.Concurrent.ConcurrentDictionary<FQTypeName.FQTypeName, FQTypeName.FQTypeName *
      List<string * ValueType> *
      TypeDeclaration.Definition>()
    resolvedTypeCache.AddOrUpdate(types, fresh)
    fresh

// CLEANUP I wonder if this can/should happen in PT2RT instead of during interpretation
let rec resolveType
  (types : Types)
  (threadID : ThreadID)
  (tst : TypeSymbolTable)
  (typeName : FQTypeName.FQTypeName)
  (typeArgs : List<ValueType>)
  // : (typeName * typeArgs * def)
  : FQTypeName.FQTypeName * List<string * ValueType> * TypeDeclaration.Definition =
  let mutable cached = Unchecked.defaultof<_>
  if List.isEmpty typeArgs && (cacheFor types).TryGetValue(typeName, &cached) then
    // Once warm this is where nearly every call lands.
    cached
  else

    match Types.find types typeName with
    | None -> RTE.TypeNotFound typeName |> raiseRTE
    | Some decl ->
      match decl.definition with
      | TypeDeclaration.Alias aliasedType ->
        let resolvedType = TypeReference.unwrapAlias types aliasedType
        match resolvedType with
        | TCustomType({ resolved = Ok innerTypeName }, innerTypeArgs) ->
          match Types.find types innerTypeName with
          | None -> RTE.TypeNotFound innerTypeName |> raiseRTE
          | Some targetDecl ->
            // Create mapping from original type params to provided args/unknowns
            let typeArgsMap =
              if List.isEmpty typeArgs && not (List.isEmpty decl.typeParams) then
                decl.typeParams
                |> List.map (fun p -> p, ValueType.Unknown)
                |> Map.ofList
              else
                List.zip decl.typeParams typeArgs |> Map.ofList

            // Map inner type args using target type's param names
            let mappedInnerArgsVT =
              List.zip targetDecl.typeParams innerTypeArgs
              |> List.map (fun (targetParam, typeRef) ->
                let vt = TypeReference.toVT types tst typeRef
                match typeRef with
                | TVariable name ->
                  match Map.tryFind name typeArgsMap with
                  | Some vt -> (targetParam, vt)
                  | None -> (targetParam, vt)
                | _ -> (targetParam, vt))

            let result =
              resolveType types threadID tst innerTypeName []
              |> (fun (resolvedName, _, def) ->
                (resolvedName, mappedInnerArgsVT, def))
            if List.isEmpty typeArgs && List.isEmpty decl.typeParams then
              (cacheFor types)[typeName] <- result
            result

        | _ -> RTE.TypeNotFound typeName |> raiseRTE

      | definition ->
        let typeArgsVT =
          if List.isEmpty typeArgs && not (List.isEmpty decl.typeParams) then
            decl.typeParams |> List.map (fun p -> (p, ValueType.Unknown))
          else
            List.zip decl.typeParams typeArgs

        let result = (typeName, typeArgsVT, definition)
        if List.isEmpty typeArgs then (cacheFor types)[typeName] <- result
        result



/// `unifyValueTypeSync` against a `Dval`, without building the dval's ValueType when the shape can be
/// compared directly.
///
/// `Dval.toValueType` on a container returns `Known(KTList t)` -- two allocations -- and the unifier's
/// very next move is to take it apart again. Scalars are cached singletons and cost nothing, but a
/// container argument is the common case in this standard library, and this runs per argument per call
/// and on every frame return.
///
/// Only the shapes that carry their element type are special-cased. Everything else, including binding a
/// type variable (which genuinely needs a ValueType to bind), falls through to the original path.
let private unifyDval
  (tst : TypeSymbolTable)
  (expected : TypeReference)
  (actual : Dval)
  : SyncUnification =
  // Nested matches, not `match expected, actual with`: the tuple form allocates the pair.
  match expected with
  | TList innerT ->
    match actual with
    | DList(innerV, _) -> unifyValueTypeSync tst innerT innerV
    | _ -> unifyValueTypeSync tst expected (Dval.toValueType actual)
  | TDict(keyT, valueT) ->
    match actual with
    | DDict(keyV, valueV, _) ->
      match unifyValueTypeSync tst keyT keyV with
      | Unified tst -> unifyValueTypeSync tst valueT valueV
      | notUnified -> notUnified
    | _ -> unifyValueTypeSync tst expected (Dval.toValueType actual)
  | _ -> unifyValueTypeSync tst expected (Dval.toValueType actual)


/// The unification behind both the parameter and the result checks, answered here when it needs no
/// type lookup.
///
/// Runs on every argument of every call and on every frame return, so the point is that the ordinary case
/// allocates nothing at all.
///
/// `ValueNone` means "go to the full check". That covers an aliased type, a compound type needing recursion,
/// and *any* failure: building the error message resolves the expected type, which can hit the package
/// store. Failures are rare and already the slow path.
/// The type arguments of a custom type, unified pairwise.
///
/// Parameterised types are not a corner case: `Option` and `Result` are the two most common types in
/// the language. Without this, their type arguments send the check down the full check, and
/// returning either costs several times a plain record.
///
/// `ValueNone` on anything unresolved *or* mismatched, so the full check still produces the error
/// message and the path that describes where the mismatch was.
let rec private unifyTypeArgs
  (tst : TypeSymbolTable)
  (declared : List<TypeReference>)
  (actual : List<ValueType>)
  : TypeSymbolTable voption =
  match unifyTypeArgsSyncVT tst declared actual with
  | Unified tst -> ValueSome tst
  | Mismatched
  | Undecided -> ValueNone


/// Whether <param actual> is a value of the primitive type <param expected>, for `tryUnify`.
///
/// Nested matches, not `match expected, actual with`: the tuple form allocates the pair.
let private primitiveFits (expected : TypeReference) (actual : Dval) : bool =
  match expected with
  | TUnit ->
    (match actual with
     | DUnit -> true
     | _ -> false)
  | TBool ->
    (match actual with
     | DBool _ -> true
     | _ -> false)
  | TBigInt ->
    (match actual with
     | DBigInt _ -> true
     | _ -> false)
  | TInt8 ->
    (match actual with
     | DInt8 _ -> true
     | _ -> false)
  | TUInt8 ->
    (match actual with
     | DUInt8 _ -> true
     | _ -> false)
  | TInt16 ->
    (match actual with
     | DInt16 _ -> true
     | _ -> false)
  | TUInt16 ->
    (match actual with
     | DUInt16 _ -> true
     | _ -> false)
  | TInt32 ->
    (match actual with
     | DInt32 _ -> true
     | _ -> false)
  | TUInt32 ->
    (match actual with
     | DUInt32 _ -> true
     | _ -> false)
  | TInt64 ->
    (match actual with
     | DInt64 _ -> true
     | _ -> false)
  | TUInt64 ->
    (match actual with
     | DUInt64 _ -> true
     | _ -> false)
  | TInt128 ->
    (match actual with
     | DInt128 _ -> true
     | _ -> false)
  | TUInt128 ->
    (match actual with
     | DUInt128 _ -> true
     | _ -> false)
  | TFloat64 ->
    (match actual with
     | DFloat64 _ -> true
     | _ -> false)
  | TFloat32 ->
    (match actual with
     | DFloat32 _ -> true
     | _ -> false)
  | TDecimal ->
    (match actual with
     | DDecimal _ -> true
     | _ -> false)
  | TChar ->
    (match actual with
     | DChar _ -> true
     | _ -> false)
  | TString ->
    (match actual with
     | DStr _ -> true
     | _ -> false)
  | TUuid ->
    (match actual with
     | DUuid _ -> true
     | _ -> false)
  | TDateTime ->
    (match actual with
     | DDateTime _ -> true
     | _ -> false)
  | _ -> false


/// Whether <param actual> is a value of <param expected> with nothing to bind: a primitive, or a type
/// variable <param tst> already binds to exactly its type. `true` means a unify would pass and hand
/// back <param tst> as it is; `false` means ask it. A lambda's result is checked this way on every
/// call, and for `List.fold`'s `'b` the variable is bound from the first element on.
let fitsWithNothingToBind
  (tst : TypeSymbolTable)
  (expected : TypeReference)
  (actual : Dval)
  : bool =
  match expected with
  | TVariable name ->
    match tst.TryFind name with
    | ValueSome bound -> Dval.hasValueType bound actual
    | ValueNone -> false
  | _ -> primitiveFits expected actual


/// Whether <param known> is the primitive <param expected>, `Bytes` included.
let private knownIs (expected : TypeReference) (known : KnownType) : bool =
  match known with
  | KTUnit -> expected.IsTUnit
  | KTBool -> expected.IsTBool
  | KTInt8 -> expected.IsTInt8
  | KTUInt8 -> expected.IsTUInt8
  | KTInt16 -> expected.IsTInt16
  | KTUInt16 -> expected.IsTUInt16
  | KTInt32 -> expected.IsTInt32
  | KTUInt32 -> expected.IsTUInt32
  | KTInt64 -> expected.IsTInt64
  | KTUInt64 -> expected.IsTUInt64
  | KTInt128 -> expected.IsTInt128
  | KTUInt128 -> expected.IsTUInt128
  | KTBigInt -> expected.IsTBigInt
  | KTFloat64 -> expected.IsTFloat64
  | KTFloat32 -> expected.IsTFloat32
  | KTDecimal -> expected.IsTDecimal
  | KTChar -> expected.IsTChar
  | KTString -> expected.IsTString
  | KTUuid -> expected.IsTUuid
  | KTDateTime -> expected.IsTDateTime
  | KTBytes -> expected.IsTBytes
  | _ -> false


/// Whether a value whose type is known to be <param known> is a value of <param expected> with
/// nothing to bind: the same type all the way down, every part of it known. A record or enum
/// matches by name, which is enough for the reason given at `argFitsWithNothingToBind`. A type
/// variable fits what <param tst> already binds it to, exactly, as `unifyValueTypeSync` has it.
let rec private knownFits
  (tst : TypeSymbolTable)
  (expected : TypeReference)
  (known : KnownType)
  : bool =
  match expected with
  | TVariable name ->
    match tst.TryFind name with
    | ValueSome(ValueType.Known bound) -> bound = known
    | _ -> false
  | _ ->
    match known with
    | KTList elem ->
      (match expected with
       | TList inner -> valueTypeFits tst inner elem
       | _ -> false)
    | KTTuple(a, b, rest) ->
      (match expected with
       | TTuple(ea, eb, eRest) ->
         valueTypeFits tst ea a
         && valueTypeFits tst eb b
         && valueTypesFit tst eRest rest
       | _ -> false)
    | KTCustomType(FQTypeName.Package(Hash actualName), actualArgs) ->
      (match expected with
       | TCustomType({ resolved = Ok(FQTypeName.Package(Hash declared)) },
                     declaredArgs) ->
         System.String.Equals(declared, actualName)
         && valueTypesFit tst declaredArgs actualArgs
       | _ -> false)
    | _ -> knownIs expected known

and private valueTypeFits
  (tst : TypeSymbolTable)
  (expected : TypeReference)
  (actual : ValueType)
  : bool =
  match actual with
  | ValueType.Known k -> knownFits tst expected k
  | ValueType.Unknown -> false

/// `valueTypeFits` for each of a type's arguments, or a tuple's parts after the second.
and private valueTypesFit
  (tst : TypeSymbolTable)
  (declared : List<TypeReference>)
  (actual : List<ValueType>)
  : bool =
  match declared with
  | [] -> List.isEmpty actual
  | d :: dRest ->
    match actual with
    | a :: aRest -> valueTypeFits tst d a && valueTypesFit tst dRest aRest
    | [] -> false


/// `fitsWithNothingToBind` for a package fn's argument, which is checked on every call. Also
/// `Bytes`, and a list whose element type is known and is the declared one (`List<(Int64 * Doc)>`):
/// the unifier reads a list's element type and not its items, so this answers as it would. One
/// whose element type is not known yet (`[]`), or names an alias, is left to the unifier.
///
/// Also a record or enum of exactly the declared type whose type arguments are known and declared
/// (`Option<Doc>`): equal names mean the declared type is not an alias (see `tryUnifyGeneral`).
/// And a tuple whose parts each fit.
let rec argFitsWithNothingToBind
  (tst : TypeSymbolTable)
  (expected : TypeReference)
  (actual : Dval)
  : bool =
  match expected with
  | TBytes ->
    (match actual with
     | DBytes _ -> true
     | _ -> false)
  | TList inner ->
    (match actual with
     | DList(ValueType.Known elem, _) -> knownFits tst inner elem
     | _ -> false)
  | TCustomType({ resolved = Ok(FQTypeName.Package(Hash declared)) }, declaredArgs) ->
    (match actual with
     | DEnum(_, FQTypeName.Package(Hash actualName), actualArgs, _, _)
     | DRecord(_, FQTypeName.Package(Hash actualName), actualArgs, _) ->
       System.String.Equals(declared, actualName)
       && valueTypesFit tst declaredArgs actualArgs
     | _ -> false)
  | TTuple(ea, eb, eRest) ->
    (match actual with
     | DTuple(a, b, rest) ->
       argFitsWithNothingToBind tst ea a
       && argFitsWithNothingToBind tst eb b
       && argsFitWithNothingToBind tst eRest rest
     | _ -> false)
  | TVariable _ -> fitsWithNothingToBind tst expected actual
  | _ -> primitiveFits expected actual

and private argsFitWithNothingToBind
  (tst : TypeSymbolTable)
  (expected : List<TypeReference>)
  (actual : List<Dval>)
  : bool =
  match expected with
  | [] -> List.isEmpty actual
  | e :: eRest ->
    match actual with
    | a :: aRest ->
      argFitsWithNothingToBind tst e a && argsFitWithNothingToBind tst eRest aRest
    | [] -> false


/// Whether <param actual> is a value of <param expected>, and the symbol table with whatever that
/// bound, or `ValueNone` if only the full unifier can say.
///
/// A primitive that fits is answered here, before the general case: every builtin call checks
/// each argument and its result this way, and most of those are primitives. The general case has
/// a frame of several hundred bytes, cleared on entry, which cost more than the check itself.
let rec tryUnify
  (tst : TypeSymbolTable)
  (expected : TypeReference)
  (actual : Dval)
  : TypeSymbolTable voption =
  if primitiveFits expected actual then
    ValueSome tst
  else
    tryUnifyGeneral tst expected actual

/// `tryUnify` for everything but a primitive that fits.
and private tryUnifyGeneral
  (tst : TypeSymbolTable)
  (expected : TypeReference)
  (actual : Dval)
  : TypeSymbolTable voption =
  match actual with
  // A read still in flight unifies with anything, by being unknown: `Promise<t>` is `t`. Only a
  // result ever arrives here as one (a wrapper handing back a builtin's result unlooked-at; a
  // builtin that combined reads into one); arguments and stored values are forced first. The
  // value was checked against the builtin's own return type when made, or is when it lands.
  | DPromise _ -> ValueSome tst
  | _ ->
    match expected with
    // A record or enum of exactly the declared type, with no type arguments. This is what a program
    // that models anything passes around all day, and until now every one of them fell through to the
    // full unifier purely to ask `Types.find` whether the declared type was an alias.
    //
    // It can't be. An alias's *name* is the alias, but a value built through it carries the underlying
    // type's name, so if the two names are equal the declared type resolved to itself -- and names are
    // content hashes, so that means it is the type, not an alias to something else.
    //
    // Type arguments are excluded rather than compared: with them the answer depends on unifying each
    // one, which is what the full check is for.
    | TCustomType({ resolved = Ok declared }, declaredArgs) ->
      match actual with
      | DRecord(_, actualName, actualArgs, _) when actualName = declared ->
        unifyTypeArgs tst declaredArgs actualArgs
      | DEnum(_, actualName, actualArgs, _, _) when actualName = declared ->
        unifyTypeArgs tst declaredArgs actualArgs
      | _ ->
        // An alias the full check has already unwrapped: answer against what it stands for, as
        // that route would. Its type arguments are not consulted there either.
        let mutable target = Unchecked.defaultof<TypeReference>
        if aliasTargets.TryGetValue(declared, &target) then
          tryUnify tst target actual
        else
          ValueNone

    // A primitive expected type, answered here rather than through `unwrapAliasSync` and
    // `unifyDval`. Neither can do anything for a type with no variables in it: a primitive is
    // never an alias, and there is nothing to bind. One that fits never gets here
    // (`tryUnify`), so this is a mismatch.
    //
    // One arm for all of them, and the check in a function of its own: an arm per type, each with
    // its own `ValueSome tst`, gave this function a 2.7 KB stack frame, cleared on every call.
    | TUnit
    | TBool
    | TBigInt
    | TInt8
    | TUInt8
    | TInt16
    | TUInt16
    | TInt32
    | TUInt32
    | TInt64
    | TUInt64
    | TInt128
    | TUInt128
    | TFloat64
    | TFloat32
    | TDecimal
    | TChar
    | TString
    | TUuid
    | TDateTime -> if primitiveFits expected actual then ValueSome tst else ValueNone

    | _ ->

      match unwrapAliasSync expected with
      | ValueNone -> ValueNone
      | ValueSome expected ->
        match unifyDval tst expected actual with
        | Unified updatedTst -> ValueSome updatedTst
        | Mismatched
        | Undecided -> ValueNone


/// Check against an alias's cached underlying type when available.
/// Leave the caller's original type reference unchanged so error messages can
/// still identify the declared type.
let tryUnifyWithAliases
  (types : Types)
  (tst : TypeSymbolTable)
  (expected : TypeReference)
  (actual : Dval)
  : TypeSymbolTable voption =
  match tryUnify tst expected actual with
  | ValueSome _ as result -> result
  | ValueNone ->
    match expected with
    | TCustomType _ ->
      match TypeReference.tryResolvedAlias types expected with
      | ValueSome resolved -> tryUnify tst resolved actual
      | ValueNone -> ValueNone
    | _ -> ValueNone


/// `expected` unified against `actual`, the shared body of `checkFnParam` and `checkFnResult`. A
/// mismatch carries the expected type as a value type, resolved only then: it exists only to
/// describe the failure, and this runs on every parameter and result of every call.
let private unifyDeclared
  (types : Types)
  (tst : TypeSymbolTable)
  (expected : TypeReference)
  (actual : Dval)
  : Result<TypeSymbolTable, ValueType> =
  let unwrapped = TypeReference.unwrapAlias types expected
  match unify types tst unwrapped actual with
  | Ok updatedTst -> Ok updatedTst
  | Error _path -> Error(TypeReference.toVT types tst unwrapped)


// The errors carry the reference as written, from before `unwrapAlias`, so an alias reports the
// alias.
let checkFnParam
  (types : Types)
  (fnName : FQFnName.FQFnName)
  (tst : TypeSymbolTable)
  (paramIndex : int)
  (paramName : string)
  (expected : TypeReference)
  (actual : Dval)
  : Result<TypeSymbolTable, RTE.Error> =
  match unifyDeclared types tst expected actual with
  | Ok updatedTst -> Ok updatedTst
  | Error expectedVT ->
    RTE.Applications.FnParameterNotExpectedType(
      fnName,
      paramIndex,
      paramName,
      Some expected,
      expectedVT,
      Dval.toValueType actual,
      actual
    )
    |> RTE.Apply
    |> Error


let checkFnResult
  (types : Types)
  (fnName : FQFnName.FQFnName)
  (tst : TypeSymbolTable)
  (expected : TypeReference)
  (actual : Dval)
  : Result<TypeSymbolTable, RTE.Error> =
  match unifyDeclared types tst expected actual with
  | Ok updatedTst -> Ok updatedTst
  | Error expectedVT ->
    RTE.Applications.FnResultNotExpectedType(
      fnName,
      Some expected,
      expectedVT,
      Dval.toValueType actual,
      actual
    )
    |> RTE.Apply
    |> Error


/// Helpers for creating type-checked Dvals
/// (lists, records, enums, etc.)
///
/// Dvals should be created carefully,:
/// - to have the correct `ValueType`s, where appropriate
///  i.e. we should not have `DList(Known KTInt64, [ DString("hi") ])`
///
/// - similarly, we should fail when trying to merge `Dval`s with conflicting `ValueType`s
///   i.e. `List.concat [1] ["hi"]` should fail
///   because we can't merge `Known KTInt64` and `Known KTString`
///
/// These functions are intended to help with both of these,
/// in cases where the functions in `Dval.fs` are insufficient
/// (i.e. we don't know the Dark sub-types of a Dval in some F# code).
///
/// Doing this at-construction is important to ensure efficient run-time type-checking.
module DvalCreator =
  /// The element type <param remaining> agrees on, merged into <param typ>, raising at the first
  /// element that disagrees. Only the type: the list itself is used as it is. Top-level recursion
  /// rather than a fold, whose lambda and accumulator would each allocate per element.
  let rec private listElementType
    (threadID : ThreadID)
    (i : int)
    (typ : ValueType)
    (remaining : List<Dval>)
    : struct (ValueType * int) =
    match remaining with
    | [] -> struct (typ, i)
    | dv :: rest ->
      match VT.mergeDval typ dv with
      | Ok newType -> listElementType threadID (i + 1) newType rest
      | Error() ->
        RTE.Lists.Error.TriedToAddMismatchedData(i, typ, Dval.toValueType dv, dv)
        |> RTE.Error.List
        |> raiseRTE

  /// The key and value types `remaining` agree on; raises at the first entry that
  /// does not.
  let rec private dictEntryTypes
    (threadID : ThreadID)
    (keyType : ValueType)
    (valueType : ValueType)
    (remaining : List<Dval * Dval>)
    : struct (ValueType * ValueType) =
    match remaining with
    | [] -> struct (keyType, valueType)
    | (k, v) :: rest ->
      Dval.assertUsableDictKey k
      let kt = Dval.toValueType k
      let keyType =
        match VT.merge keyType kt with
        | Ok merged -> merged
        | Error() ->
          RTE.Dicts.Error.MismatchedKeyType(keyType, kt, k)
          |> RTE.Error.Dict
          |> raiseRTE

      let vt = Dval.toValueType v

      match VT.merge valueType vt with
      | Ok merged -> dictEntryTypes threadID keyType merged rest
      | Error() ->
        RTE.Dicts.Error.TriedToAddMismatchedData(k, valueType, vt, v)
        |> RTE.Error.Dict
        |> raiseRTE

  /// A dict's entries, checked, and the types they agree on. Built in one go rather
  /// than an insert per entry. Later entries replace earlier ones with the same key;
  /// the at-rest checker reports duplicate constant keys (`DuplicateDictKey`).
  let dictEntries
    (threadID : ThreadID)
    (keyType : ValueType)
    (valueType : ValueType)
    (entries : List<Dval * Dval>)
    : struct (ValueType * ValueType * DictMap) =
    let struct (kt, vt) = dictEntryTypes threadID keyType valueType entries
    struct (kt, vt, DictMap.ofList entries)

  // CLEANUP consider skipping type-checking after N elements or after the type args are fully resolved, whichever comes last.
  //   In order to support this^, add another param or two so that direct [] interpretation is differentiated from calls to this from Builtins and other places.
  //   (or split this into 2 separate fns with clearer names)
  /// A list of <param items>, typed by what they agree on. The items are checked, not copied.
  ///
  /// Holding a read in flight, the list is a promise of its own (`Pending.built`), built
  /// again from the values once they land: the element type is worked out then, and an
  /// element of the wrong type raises then. The same holds for every creator here.
  let rec list (threadID : ThreadID) (typ : ValueType) (items : List<Dval>) : Dval =
    let struct (elementType, count) = listElementType threadID 0 typ items
    let made = DList(elementType, DvalList.ofListN count items)
    if List.exists Pending.isPromise items then
      made
      |> Pending.built (fun _ ->
        (list threadID typ (List.map Pending.settled items)))
    else
      made


  // CLEANUP see notes in `list` above
  let rec dict
    (threadID : ThreadID)
    (keyType : ValueType)
    (valueType : ValueType)
    (entries : List<Dval * Dval>)
    : Dval =
    let struct (kt, vt, made) = dictEntries threadID keyType valueType entries
    let made = DDict(kt, vt, made)
    if List.exists (fun (_, v) -> Pending.isPromise v) entries then
      made
      |> Pending.built (fun _ ->
        let landed = entries |> List.map (fun (k, v) -> (k, Pending.settled v))
        (dict threadID keyType valueType landed))
    else
      made

  /// A list or dict a builtin built around reads, built again from the landed values, so its
  /// element type is worked out and a value of the wrong type raises where it is waited for.
  let recheck (threadID : ThreadID) (filled : Dval) : Dval =
    match filled with
    | DList(vt, Items(items)) -> (list threadID vt items)
    | DDict(kt, vt, entries) -> (dict threadID kt vt (DictMap.toList entries))
    | other -> other

  /// `List.concat` of two lists whose element types merge. Both lists are already valid, so the
  /// merge is the whole check, and O(1): re-checking every element would make a list built by
  /// repeated concatenation O(n²). `ValueNone` when the types do not merge, where `list` names
  /// the element at fault.
  let tryConcat
    (vt1 : ValueType)
    (l1 : DvalList)
    (vt2 : ValueType)
    (l2 : DvalList)
    : Dval voption =
    match VT.merge vt1 vt2 with
    | Ok merged -> ValueSome(DList(merged, DvalList.append l1 l2))
    | Error() -> ValueNone

  /// `List.prepend`: <param value> on the front of a list whose element type is <param vt>.
  /// `ValueNone` when the types do not merge, where `list` names the element at fault.
  let tryPrepend (vt : ValueType) (l : DvalList) (value : Dval) : Dval voption =
    match VT.mergeDval vt value with
    | Ok merged -> ValueSome(DList(merged, DvalList.cons value l))
    | Error() -> ValueNone

  /// `Dict.set`: the dict with <param k> set to <param v>. `ValueNone` when the key cannot be a
  /// dict key or either type does not merge, where `dictAddEntry` raises.
  let tryDictAddEntry
    (keyType : ValueType)
    (valueType : ValueType)
    (entries : DictMap)
    (k : Dval)
    (v : Dval)
    : Dval voption =
    if not (Dval.isUsableDictKey k) then
      ValueNone
    else
      match VT.mergeDval valueType v with
      | Ok valueType ->
        match VT.mergeDval keyType k with
        | Ok keyType -> ValueSome(DDict(keyType, valueType, DictMap.add k v entries))
        | Error() -> ValueNone
      | Error() -> ValueNone

  let dictAddEntry
    (keyType : ValueType)
    (valueType : ValueType)
    (entries : DictMap)
    (k : Dval)
    (v : Dval)
    : struct (ValueType * ValueType * DictMap) =
    Dval.assertUsableDictKey k
    let kt = Dval.toValueType k
    let keyType =
      match VT.merge keyType kt with
      | Ok merged -> merged
      | Error() ->
        RTE.Dicts.Error.MismatchedKeyType(keyType, kt, k)
        |> RTE.Error.Dict
        |> raiseRTE

    let vt = Dval.toValueType v
    match VT.merge valueType vt with
    | Ok merged -> struct (keyType, merged, DictMap.add k v entries)
    | Error() ->
      RTE.Dicts.Error.TriedToAddMismatchedData(k, valueType, vt, v)
      |> RTE.Error.Dict
      |> raiseRTE

  /// The `None` an unknown-typed lookup returns, kept rather than rebuilt: a dict miss returns
  /// one, and it is the same value every time. Keyed by the type name object, which `optionType`
  /// renews when the package generation changes.
  let mutable private unknownNone : Dval = DUnit

  let optionNone (innerType : ValueType) : Dval =
    let typeName = Dval.optionType ()
    match innerType with
    | ValueType.Unknown ->
      match unknownNone with
      | DEnum(kept, _, _, _, _) when System.Object.ReferenceEquals(kept, typeName) ->
        unknownNone
      | _ ->
        let none = DEnum(typeName, typeName, [ innerType ], "None", [])
        unknownNone <- none
        none
    | ValueType.Known _ -> DEnum(typeName, typeName, [ innerType ], "None", [])

  let rec optionSome (expected : ValueType) (dv : Dval) : Dval =
    match dv with
    | DPromise _ ->
      optionSomeNow expected dv
      |> Pending.built (fun _ -> (optionSome expected (Pending.settled dv)))
    | _ -> optionSomeNow expected dv

  and private optionSomeNow (expected : ValueType) (dv : Dval) =
    let typeName = Dval.optionType ()

    let vt = Dval.toValueType dv

    // Merging into `Unknown` gives the value's own type, so skip the `Result` that says so.
    match expected with
    | ValueType.Unknown -> DEnum(typeName, typeName, [ vt ], "Some", [ dv ])
    | ValueType.Known _ ->
      match VT.merge expected vt with
      | Ok typ -> DEnum(typeName, typeName, [ typ ], "Some", [ dv ])
      | Error() ->
        RuntimeError.Enums.ConstructionFieldOfWrongType(
          "Some",
          0,
          None,
          expected,
          vt,
          dv
        )
        |> RuntimeError.Enum
        |> raiseRTE


  let option (expectedType : ValueType) (dv : Option<Dval>) : Dval =
    match dv with
    | Some dv -> optionSome expectedType dv
    | None -> optionNone expectedType


  module Result =
    let ok (okType : ValueType) (errorType : ValueType) (dvOk : Dval) : Dval =
      let typeName = Dval.resultType ()
      let dvalType = Dval.toValueType dvOk
      match VT.merge okType dvalType with
      | Ok typ -> DEnum(typeName, typeName, [ typ; errorType ], "Ok", [ dvOk ])
      | Error() ->
        RuntimeError.Enums.ConstructionFieldOfWrongType(
          "Ok",
          0,
          None,
          okType,
          dvalType,
          dvOk
        )
        |> RuntimeError.Enum
        |> raiseRTE

    let error (okType : ValueType) (errorType : ValueType) (dvError : Dval) : Dval =
      let typeName = Dval.resultType ()
      let dvalType = Dval.toValueType dvError
      match VT.merge errorType dvalType with
      | Ok typ -> DEnum(typeName, typeName, [ okType; typ ], "Error", [ dvError ])
      | Error() ->
        RuntimeError.Enums.ConstructionFieldOfWrongType(
          "Error",
          0,
          None,
          errorType,
          dvalType,
          dvError
        )
        |> RuntimeError.Enum
        |> raiseRTE

    let result
      (okType : ValueType)
      (errorType : ValueType)
      (dv : Result<Dval, Dval>)
      : Dval =
      match dv with
      | Ok dv -> ok okType errorType dv
      | Error dv -> error okType errorType dv


  /// Narrow a resolved definition to an enum's cases. Top-level so neither path allocates a closure.
  let private enumCasesOf
    (typeName : FQTypeName.FQTypeName)
    (resolvedName : FQTypeName.FQTypeName)
    (typeArgs : List<string * ValueType>)
    (definition : TypeDeclaration.Definition)
    : struct (FQTypeName.FQTypeName *
      List<string * ValueType> *
      NEList<TypeDeclaration.EnumCase>)
    =
    match definition with
    | TypeDeclaration.Enum cases -> struct (resolvedName, typeArgs, cases)
    | _ ->
      Exception.raiseInternal
        "Expected enum type but found other type"
        [ "typeName", typeName ]

  /// Narrow a resolved definition to a record's fields. Top-level, as above.
  let private recordFieldsOf
    (typeName : FQTypeName.FQTypeName)
    (resolvedName : FQTypeName.FQTypeName)
    (typeArgs : List<string * ValueType>)
    (definition : TypeDeclaration.Definition)
    : struct (FQTypeName.FQTypeName *
      List<string * ValueType> *
      NEList<TypeDeclaration.RecordField>)
    =
    match definition with
    | TypeDeclaration.Record fields -> struct (resolvedName, typeArgs, fields)
    | _ -> RTE.Records.CreationTypeNotRecord typeName |> RTE.Record |> raiseRTE

  let resolveEnumType
    (types : Types)
    (threadID : ThreadID)
    (typeName : FQTypeName.FQTypeName)
    (typeArgs : List<ValueType>)
    : struct (FQTypeName.FQTypeName *
      List<string * ValueType> *
      NEList<TypeDeclaration.EnumCase>)
    =
    let resolved = resolveType types threadID TST.empty typeName typeArgs

    let resolvedName, typeArgs, definition = resolved
    (enumCasesOf typeName resolvedName typeArgs definition)
  /// One field of an enum case being constructed. Same shape as `checkRecordFields`, and the same
  /// reason for existing: as a lambda over a three-element accumulator this costs a closure, a state
  /// machine and a tuple per field of every enum built.
  /// One type argument once a field has been checked: an unknown one takes what the field bound
  /// it to, a known one merges.
  let private bindTypeArg
    (newTST : TypeSymbolTable)
    (paramName : string)
    (vt : ValueType)
    : ValueType voption =
    match vt with
    | ValueType.Unknown ->
      match TST.tryFind paramName newTST with
      | ValueSome known -> ValueSome known
      | ValueNone -> ValueSome vt
    | known ->
      match TST.tryFind paramName newTST with
      | ValueNone -> ValueSome known
      | ValueSome bound ->
        match ValueType.merge known bound with
        | Ok merged -> ValueSome merged
        | Error() -> ValueNone

  /// The type-argument walk from the checked-field slow path, without building an error path.
  ///
  /// `ValueNone` when a merge fails, so the caller falls through to the full check and the
  /// error message is built in one place rather than two.
  ///
  /// An argument that does not change keeps its pair, and a tail that does not change is the
  /// same list, so a field that teaches nothing new allocates nothing.
  let rec private updateTypeArgs
    (newTST : TypeSymbolTable)
    (typeArgs : List<string * ValueType>)
    : List<string * ValueType> voption =
    match typeArgs with
    | [] -> ValueSome typeArgs
    | (paramName, vt) :: rest ->
      match bindTypeArg newTST paramName vt with
      | ValueNone -> ValueNone
      | ValueSome bound ->
        match updateTypeArgs newTST rest with
        | ValueNone -> ValueNone
        | ValueSome newRest ->
          if
            System.Object.ReferenceEquals(bound, vt)
            && System.Object.ReferenceEquals(newRest, rest)
          then
            ValueSome typeArgs
          else
            ValueSome((paramName, bound) :: newRest)

  /// `updateTypeArgs` for the values alone, as a `DEnum` holds them, so the pairs are never
  /// rebuilt. `values` are the ones so far, or empty before the first field, when they are still
  /// the ones in `typeArgs`.
  let rec private updateTypeArgValues
    (newTST : TypeSymbolTable)
    (typeArgs : List<string * ValueType>)
    (values : List<ValueType>)
    : List<ValueType> voption =
    match typeArgs with
    | [] -> ValueSome []
    | (paramName, own) :: rest ->
      let struct (vt, valuesRest) =
        match values with
        | v :: vs -> struct (v, vs)
        | [] -> struct (own, [])
      match bindTypeArg newTST paramName vt with
      | ValueNone -> ValueNone
      | ValueSome bound ->
        match updateTypeArgValues newTST rest valuesRest with
        | ValueNone -> ValueNone
        | ValueSome newRest ->
          match values with
          | v :: vs when
            System.Object.ReferenceEquals(bound, v)
            && System.Object.ReferenceEquals(newRest, vs)
            ->
            ValueSome values
          | _ -> ValueSome(bound :: newRest)


  let rec private checkEnumFields
    (types : Types)
    (threadID : ThreadID)
    (caseName : string)
    (fieldIndex : int)
    (remaining : List<TypeReference * Dval>)
    (typeArgs : List<string * ValueType>)
    (fieldsInReverse : List<Dval>)
    (tst : TypeSymbolTable)
    : struct (List<string * ValueType> * List<Dval> * TypeSymbolTable) =
    match remaining with
    | [] -> (struct (typeArgs, fieldsInReverse, tst))
    | (fieldDef, actualField) :: rest ->
      // Same fast path as `checkRecordFields`: most fields unify without needing the type store, and
      // a case with no type parameters has nothing to learn from them. Parameterised types take
      // this path too, which matters because `Option` and `Result` are the language's two most
      // common; only a genuine failure falls through to the full check.
      match tryUnify tst fieldDef actualField with
      | ValueSome newTST ->
        let updated =
          if List.isEmpty typeArgs then
            ValueSome typeArgs
          else
            updateTypeArgs newTST typeArgs

        match updated with
        | ValueSome newTypeArgs ->
          checkEnumFields
            types
            threadID
            caseName
            (fieldIndex + 1)
            rest
            newTypeArgs
            (actualField :: fieldsInReverse)
            newTST
        | ValueNone ->
          checkEnumFieldsSlow
            types
            threadID
            caseName
            fieldIndex
            remaining
            typeArgs
            fieldsInReverse
            tst

      | _ ->
        checkEnumFieldsSlow
          types
          threadID
          caseName
          fieldIndex
          remaining
          typeArgs
          fieldsInReverse
          tst

  /// The part of a field check that genuinely needs the type store, or that failed the fast
  /// unification. Split out so both fall-through points share one implementation rather than the
  /// body being duplicated.
  and private checkEnumFieldsSlow
    (types : Types)
    (threadID : ThreadID)
    (caseName : string)
    (fieldIndex : int)
    (remaining : List<TypeReference * Dval>)
    (typeArgs : List<string * ValueType>)
    (fieldsInReverse : List<Dval>)
    (tst : TypeSymbolTable)
    : struct (List<string * ValueType> * List<Dval> * TypeSymbolTable) =
    match remaining with
    | [] -> (struct (typeArgs, fieldsInReverse, tst))
    | (fieldDef, actualField) :: rest ->

      match unify types tst fieldDef actualField with
      | Error _path ->
        // Resolved here rather than before the check that almost always passes: it only exists
        // to describe the failure.
        let expected = TypeReference.toVT types tst fieldDef
        RTE.Enums.ConstructionFieldOfWrongType(
          caseName,
          fieldIndex,
          Some fieldDef,
          expected,
          Dval.toValueType actualField,
          actualField
        )
        |> RTE.Error.Enum
        |> raiseRTE

      | Ok newTST ->
        // A type with no parameters has nothing to learn from its fields, and that's the common
        // case. Skipping the walk skips a rebuilt list of pairs per field.
        let newTypeArgs =
          if List.isEmpty typeArgs then
            typeArgs
          else
            List.map
              (fun (paramName, vt) ->
                match vt with
                | ValueType.Unknown ->
                  match TST.tryFind paramName newTST with
                  | ValueSome known -> ((paramName, known))
                  | ValueNone -> ((paramName, vt))

                | known ->
                  match
                    ValueType.merge
                      known
                      (TST.tryFind paramName newTST
                       |> ValueOption.defaultValue known)
                  with
                  | Ok merged -> ((paramName, merged))
                  | Error() ->
                    let expected = TypeReference.toVT types tst fieldDef
                    RTE.Enums.ConstructionFieldOfWrongType(
                      caseName,
                      fieldIndex,
                      Some fieldDef,
                      expected,
                      Dval.toValueType actualField,
                      actualField
                    )
                    |> RTE.Enum
                    |> raiseRTE)
              typeArgs

        checkEnumFields
          types
          threadID
          caseName
          (fieldIndex + 1)
          rest
          newTypeArgs
          (actualField :: fieldsInReverse)
          newTST


  /// Bind the resolved type arguments into the symbol table, find the named case, and pair its
  /// declared field types with the values given. Raises on an unknown case or the wrong arity, so
  /// both of `enum`'s paths report those identically.
  let private enumCaseFields
    (tst : TypeSymbolTable)
    (resolvedTypeName : FQTypeName.FQTypeName)
    (typeArgs : List<string * ValueType>)
    (caseDefs : NEList<TypeDeclaration.EnumCase>)
    (caseName : string)
    (fields : List<Dval>)
    : struct (TypeSymbolTable * List<TypeReference * Dval>) =
    let tst = typeArgs |> List.fold (fun acc (name, vt) -> TST.add name vt acc) tst

    match caseDefs |> NEList.find (fun c -> c.name = caseName) with
    | None ->
      RTE.Enums.ConstructionCaseNotFound(resolvedTypeName, caseName)
      |> RTE.Error.Enum
      |> raiseRTE

    | Some case ->
      let expected, actual = (List.length case.fields, List.length fields)

      if expected <> actual then
        RTE.Enums.ConstructionWrongNumberOfFields(
          resolvedTypeName,
          caseName,
          expected,
          actual
        )
        |> RTE.Error.Enum
        |> raiseRTE
      else
        struct (tst, List.zip case.fields fields)

  /// Check each field against its declared type, walking the two lists side by side, and return
  /// the type arguments that leaves. ValueNone when any field needs the type store or fails to
  /// unify: the caller then takes the full path, which is where errors are built.
  ///
  /// Every enum in the language is constructed here, and checking a field never changes it, so
  /// this hands back the fields it was given instead of rebuilding them. Nested matches rather
  /// than a match on the pair, which allocates a tuple per field.
  let rec private enumFields
    (tst : TypeSymbolTable)
    (defs : List<TypeReference>)
    (fields : List<Dval>)
    (typeArgs : List<string * ValueType>)
    (values : List<ValueType>)
    : voption<List<ValueType>> =
    match defs with
    | [] ->
      match fields with
      | [] ->
        // No fields, or no type arguments: nothing was bound.
        if List.isEmpty values && not (List.isEmpty typeArgs) then
          ValueSome(typeArgs |> List.map Tuple2.second)
        else
          ValueSome values
      | _ -> ValueNone
    | fieldDef :: defsRest ->
      match fields with
      | [] -> ValueNone
      | actualField :: fieldsRest ->
        match tryUnify tst fieldDef actualField with
        | ValueSome newTST ->
          match updateTypeArgValues newTST typeArgs values with
          | ValueSome newValues ->
            enumFields newTST defsRest fieldsRest typeArgs newValues
          | ValueNone -> ValueNone
        | ValueNone -> ValueNone

  /// The case named <param caseName>, <param case> or one of <param rest>, when <param fields>
  /// has the right number of fields for it. Head and tail apart, so no list is made of the type's
  /// cases to look through them.
  let rec private caseWithArity
    (case : TypeDeclaration.EnumCase)
    (rest : List<TypeDeclaration.EnumCase>)
    (caseName : string)
    (fields : List<Dval>)
    : voption<TypeDeclaration.EnumCase> =
    if case.name = caseName then
      if List.length case.fields = List.length fields then
        ValueSome case
      else
        ValueNone
    else
      match rest with
      | [] -> ValueNone
      | next :: rest -> caseWithArity next rest caseName fields

  /// The half of enum construction after the type is known. A top-level function taking everything
  /// explicitly, not a local one: a local closing over `types` and friends is a closure allocated on
  /// every construction, which is what it cost when it was written that way.
  let private enumAfterResolve
    (types : Types)
    (threadID : ThreadID)
    (tst : TypeSymbolTable)
    (sourceTypeName : FQTypeName.FQTypeName)
    (caseName : string)
    (fields : List<Dval>)
    (resolvedTypeName : FQTypeName.FQTypeName)
    (typeArgs : List<string * ValueType>)
    (caseDefs : NEList<TypeDeclaration.EnumCase>)
    : Dval =
    let fast =
      match caseWithArity caseDefs.head caseDefs.tail caseName fields with
      | ValueSome case ->
        let tst =
          typeArgs |> List.fold (fun acc (name, vt) -> TST.add name vt acc) tst
        enumFields tst case.fields fields typeArgs []
      | ValueNone -> ValueNone

    match fast with
    | ValueSome typeArgValues ->
      (DEnum(sourceTypeName, resolvedTypeName, typeArgValues, caseName, fields))
    | ValueNone ->
      let struct (tst, fieldsZipped) =
        enumCaseFields tst resolvedTypeName typeArgs caseDefs caseName fields

      let checked' =
        checkEnumFields types threadID caseName 0 fieldsZipped typeArgs [] tst

      let struct (typeArgs, fieldsInReverse, _updatedTst) = checked'
      (DEnum(
        sourceTypeName,
        resolvedTypeName,
        typeArgs |> List.map Tuple2.second,
        caseName,
        List.rev fieldsInReverse
      ))
  let rec enum
    (types : Types)
    (threadID : ThreadID)
    (tst : TypeSymbolTable)
    (sourceTypeName : FQTypeName.FQTypeName)
    (typeArgs : List<ValueType>)
    (caseName : string)
    (fields : List<Dval>)
    : Dval =
    if List.exists Pending.isPromise fields then
      let again _ =
        enum
          types
          threadID
          tst
          sourceTypeName
          typeArgs
          caseName
          (List.map Pending.settled fields)
      enumNow types threadID tst sourceTypeName typeArgs caseName fields
      |> (Pending.built again)
    else
      enumNow types threadID tst sourceTypeName typeArgs caseName fields

  and private enumNow
    (types : Types)
    (threadID : ThreadID)
    (tst : TypeSymbolTable)
    (sourceTypeName : FQTypeName.FQTypeName)
    (typeArgs : List<ValueType>)
    (caseName : string)
    (fields : List<Dval>)
    : Dval =
    let resolved = resolveEnumType types threadID sourceTypeName typeArgs

    let struct (resolvedTypeName, typeArgs, caseDefs) = resolved
    enumAfterResolve
      types
      threadID
      tst
      sourceTypeName
      caseName
      fields
      resolvedTypeName
      typeArgs
      caseDefs
  // Resolve aliases and collect expected fields for a record type
  let resolveRecordType
    (types : Types)
    (threadID : ThreadID)
    (typeName : FQTypeName.FQTypeName)
    (typeArgs : List<ValueType>)
    : struct (FQTypeName.FQTypeName *
      List<string * ValueType> *
      NEList<TypeDeclaration.RecordField>)
    =
    let resolved = resolveType types threadID TST.empty typeName typeArgs

    let resolvedName, typeArgs, definition = resolved
    (recordFieldsOf typeName resolvedName typeArgs definition)
  /// The declared field with this name, or nothing.
  ///
  /// A loop rather than `NEList.find (fun f -> f.name = name)`: that lambda captures the name, so
  /// it's a closure allocated for every field of every record built.
  let rec private findFieldIn
    (fields : List<TypeDeclaration.RecordField>)
    (name : string)
    : TypeDeclaration.RecordField voption =
    match fields with
    | [] -> ValueNone
    | f :: rest -> if f.name = name then ValueSome f else findFieldIn rest name

  let private findExpectedField
    (fields : NEList<TypeDeclaration.RecordField>)
    (name : string)
    : TypeDeclaration.RecordField voption =
    if fields.head.name = name then
      ValueSome fields.head
    else
      findFieldIn fields.tail name


  /// One field of a record being constructed: validate it, unify it against the declared field
  /// type, and fold what that taught us back into the type arguments and the symbol table.
  ///
  /// `entries` are the fields in source order from `i` on, and sorted by name before it: each is
  /// moved into place as it is checked, so the errors come in source order and the array is the
  /// record's once the last field is in.
  ///
  /// Top-level and fully parameterised, threading its accumulators as arguments rather than as a
  /// tuple. As a lambda passed to `List.fold` it allocated a closure, a state
  /// machine and a three-element accumulator tuple *per field of every record ever built*.
  let rec private checkRecordFields
    (types : Types)
    (threadID : ThreadID)
    (expectedFields : NEList<TypeDeclaration.RecordField>)
    (entries : (struct (string * Dval))[])
    (i : int)
    (currentTypeArgs : List<string * ValueType>)
    (tst : TypeSymbolTable)
    : struct (List<string * ValueType> * TypeSymbolTable) =
    if i = entries.Length then
      (struct (currentTypeArgs, tst))
    else
      let struct (fieldName, fieldValue) = entries[i]
      if fieldName = "" then RTE.Records.CreationEmptyKey |> RTE.Record |> raiseRTE

      if not (RecordFields.insertSorted entries i) then
        RTE.Records.CreationDuplicateField fieldName |> RTE.Record |> raiseRTE

      match findExpectedField expectedFields fieldName with
      | ValueNone ->
        RTE.Records.CreationFieldNotExpected fieldName |> RTE.Record |> raiseRTE

      | ValueSome fieldDef ->
        // The overwhelming majority of fields unify without needing the type store, and a type with
        // no parameters has nothing to learn from them. When both hold, the whole field is handled
        // here. A struct option of a struct tuple, so the fast path allocates nothing to report itself.
        let fast =
          match tryUnify tst fieldDef.typ fieldValue with
          | ValueNone -> ValueNone
          | ValueSome newTST ->
            if List.isEmpty currentTypeArgs then
              ValueSome(struct (currentTypeArgs, newTST))
            else
              // Only a genuine merge failure falls through.
              match updateTypeArgs newTST currentTypeArgs with
              | ValueSome updated -> ValueSome(struct (updated, newTST))
              | ValueNone -> ValueNone

        match fast with
        | ValueSome(struct (newTypeArgs, newTST)) ->
          checkRecordFields
            types
            threadID
            expectedFields
            entries
            (i + 1)
            newTypeArgs
            newTST
        | ValueNone ->

          match unify types tst fieldDef.typ fieldValue with
          | Error _path ->
            // The declared type is only needed to describe the failure, so it's resolved here
            // rather than before the check that almost always passes.
            let expected = TypeReference.toVT types tst fieldDef.typ
            RTE.Records.CreationFieldOfWrongType(
              fieldName,
              Some fieldDef.typ,
              expected,
              Dval.toValueType fieldValue,
              fieldValue
            )
            |> RTE.Record
            |> raiseRTE

          | Ok newTST ->
            // Update the type args with anything this field pinned down. See the note in
            // `checkEnumFields`: no type parameters means nothing to learn, and no walk.
            let newTypeArgs =
              if List.isEmpty currentTypeArgs then
                currentTypeArgs
              else
                List.map
                  (fun (paramName, vt) ->
                    match vt with
                    | ValueType.Unknown ->
                      match TST.tryFind paramName newTST with
                      | ValueSome known -> ((paramName, known))
                      | ValueNone -> ((paramName, vt))

                    | known ->
                      match
                        ValueType.merge
                          known
                          (TST.tryFind paramName newTST
                           |> ValueOption.defaultValue known)
                      with
                      | Ok merged -> ((paramName, merged))
                      | Error() ->
                        let expected = TypeReference.toVT types newTST fieldDef.typ
                        RTE.Records.CreationFieldOfWrongType(
                          fieldName,
                          Some fieldDef.typ,
                          expected,
                          Dval.toValueType fieldValue,
                          fieldValue
                        )
                        |> RTE.Record
                        |> raiseRTE)
                  currentTypeArgs

            checkRecordFields
              types
              threadID
              expectedFields
              entries
              (i + 1)
              newTypeArgs
              newTST


  /// Whether one of `updates[0..i-1]` sets `name`.
  let rec private namedBefore
    (updates : (struct (string * Dval))[])
    (name : string)
    (i : int)
    : bool =
    if i = 0 then
      false
    else
      let struct (n, _) = updates[i - 1]
      n = name || namedBefore updates name (i - 1)

  /// One field of a record being updated. Same shape and the same reason for existing as
  /// `checkRecordFields`: as a lambda over a three-element accumulator it costs a closure, a state
  /// machine and a tuple per field of every record update.
  ///
  /// Updates differ from construction in their error cases -- an update may not name a field the
  /// type doesn't have, but it may legitimately leave fields alone -- so the messages differ and
  /// the two cannot share a body.
  let rec private updateRecordFields
    (types : Types)
    (threadID : ThreadID)
    (expectedFields : NEList<TypeDeclaration.RecordField>)
    (updates : (struct (string * Dval))[])
    (i : int)
    (fieldsSoFar : (struct (string * Dval))[])
    (currentTypeArgs : List<string * ValueType>)
    (tst : TypeSymbolTable)
    : struct ((struct (string * Dval))[] * List<string * ValueType> * TypeSymbolTable) =
    if i = updates.Length then
      (struct (fieldsSoFar, currentTypeArgs, tst))
    else
      let struct (fieldName, fieldValue) = updates[i]
      if fieldName = "" then RTE.Records.UpdateEmptyKey |> RTE.Record |> raiseRTE

      // `fieldsSoFar` starts with the original record's fields, so it can't tell a repeat.
      if namedBefore updates fieldName i then
        RTE.Records.UpdateDuplicateField fieldName |> RTE.Record |> raiseRTE

      match findExpectedField expectedFields fieldName with
      | ValueNone ->
        RTE.Records.UpdateFieldNotExpected fieldName |> RTE.Record |> raiseRTE

      | ValueSome fieldDef ->
        // The common case: the field unifies without the type store and the type has no parameters
        // to learn anything from, so the whole field is handled here.
        // A struct option of a struct tuple, so the fast path allocates nothing to report itself.
        let fast =
          match tryUnify tst fieldDef.typ fieldValue with
          | ValueNone -> ValueNone
          | ValueSome newTST ->
            if List.isEmpty currentTypeArgs then
              ValueSome(struct (currentTypeArgs, newTST))
            else
              // Only a genuine merge failure falls through.
              match updateTypeArgs newTST currentTypeArgs with
              | ValueSome updated -> ValueSome(struct (updated, newTST))
              | ValueNone -> ValueNone

        match fast with
        | ValueSome(struct (newTypeArgs, newTST)) ->
          updateRecordFields
            types
            threadID
            expectedFields
            updates
            (i + 1)
            (RecordFields.set fieldName fieldValue fieldsSoFar)
            newTypeArgs
            newTST
        | ValueNone ->

          match unify types tst fieldDef.typ fieldValue with
          | Error _path ->
            // Resolved here rather than before the check that almost always passes.
            let expected = TypeReference.toVT types tst fieldDef.typ
            RTE.Records.UpdateFieldOfWrongType(
              fieldName,
              Some fieldDef.typ,
              expected,
              Dval.toValueType fieldValue,
              fieldValue
            )
            |> RTE.Record
            |> raiseRTE

          | Ok newTST ->
            // No type parameters means nothing to learn, and no walk.
            let newTypeArgs =
              if List.isEmpty currentTypeArgs then
                currentTypeArgs
              else
                List.map
                  (fun (paramName, vt) ->
                    match vt with
                    | ValueType.Unknown ->
                      match TST.tryFind paramName newTST with
                      | ValueSome known -> ((paramName, known))
                      | ValueNone -> ((paramName, vt))

                    | known ->
                      match
                        ValueType.merge
                          known
                          (TST.tryFind paramName newTST
                           |> ValueOption.defaultValue known)
                      with
                      | Ok merged -> ((paramName, merged))
                      | Error() ->
                        let expected = TypeReference.toVT types newTST fieldDef.typ
                        RTE.Records.UpdateFieldOfWrongType(
                          fieldName,
                          Some fieldDef.typ,
                          expected,
                          Dval.toValueType fieldValue,
                          fieldValue
                        )
                        |> RTE.Record
                        |> raiseRTE)
                  currentTypeArgs

            updateRecordFields
              types
              threadID
              expectedFields
              updates
              (i + 1)
              (RecordFields.set fieldName fieldValue fieldsSoFar)
              newTypeArgs
              newTST


  /// Constructs a Dval.DRecord, ensuring that the fields match the expected shape
  ///
  /// note: if provided, the typeArgs must match the # of typeArgs expected by the type
  /// Reject a record that is missing a declared field, or build it. Top-level, as above.
  let private finishRecord
    (sourceTypeName : FQTypeName.FQTypeName)
    (resolvedTypeName : FQTypeName.FQTypeName)
    (expectedFields : NEList<TypeDeclaration.RecordField>)
    (entries : (struct (string * Dval))[])
    (finalTypeArgs : List<string * ValueType>)
    : Dval =
    // Every field given is a declared one and none is given twice, so none is missing exactly
    // when there are as many as were declared.
    if entries.Length = NEList.length expectedFields then
      DRecord(
        sourceTypeName,
        resolvedTypeName,
        finalTypeArgs |> List.map Tuple2.second,
        RecordFields entries
      )
    else
      match
        expectedFields
        |> NEList.find (fun f -> RecordFields.indexOf entries f.name < 0)
      with
      | Some missing ->
        RTE.Records.CreationMissingField missing.name |> RTE.Record |> raiseRTE
      | None ->
        Exception.raiseInternal "A record had more fields than its type declares" []

  /// The half of record construction after the type is known. Top-level, taking everything
  /// explicitly, for the same reason as `enumAfterResolve`.
  let private recordAfterResolve
    (types : Types)
    (threadID : ThreadID)
    (tst : TypeSymbolTable)
    (sourceTypeName : FQTypeName.FQTypeName)
    (fields : (struct (string * Dval))[])
    (resolvedTypeName : FQTypeName.FQTypeName)
    (resolvedTypeArgs : List<string * ValueType>)
    (expectedFields : NEList<TypeDeclaration.RecordField>)
    : Dval =
    let tst =
      resolvedTypeArgs |> List.fold (fun acc (name, vt) -> TST.add name vt acc) tst

    let checked' =
      checkRecordFields types threadID expectedFields fields 0 resolvedTypeArgs tst

    let struct (finalTypeArgs, _updatedTST) = checked'
    (finishRecord sourceTypeName resolvedTypeName expectedFields fields finalTypeArgs)
  let rec private holdsRead (fields : (struct (string * Dval))[]) (i : int) : bool =
    if i = fields.Length then
      false
    else
      let struct (_, v) = fields[i]
      Pending.isPromise v || holdsRead fields (i + 1)

  /// `fields` in source order, which become the record's own: they are sorted in place, so the
  /// caller hands over an array it will not use again.
  let rec record
    (types : Types)
    (threadID : ThreadID)
    (tst : TypeSymbolTable)
    (sourceTypeName : FQTypeName.FQTypeName)
    (typeArgs : List<ValueType>)
    (fields : (struct (string * Dval))[])
    : Dval =
    if holdsRead fields 0 then
      // `recordNow` sorts `fields`, so the check once the reads land runs over a copy taken
      // first, and reports in source order too.
      let source = Array.copy fields
      let again _ =
        source
        |> Array.map (fun (struct (n, v)) -> struct (n, Pending.settled v))
        |> record types threadID tst sourceTypeName typeArgs
      recordNow types threadID tst sourceTypeName typeArgs fields
      |> (Pending.built again)
    else
      recordNow types threadID tst sourceTypeName typeArgs fields

  and private recordNow
    (types : Types)
    (threadID : ThreadID)
    (tst : TypeSymbolTable)
    (sourceTypeName : FQTypeName.FQTypeName)
    (typeArgs : List<ValueType>)
    (fields : (struct (string * Dval))[])
    : Dval =
    let resolved = resolveRecordType types threadID sourceTypeName typeArgs

    let struct (resolvedTypeName, resolvedTypeArgs, expectedFields) = resolved
    recordAfterResolve
      types
      threadID
      tst
      sourceTypeName
      fields
      resolvedTypeName
      resolvedTypeArgs
      expectedFields
  /// Constructs a Dval.DRecord, ensuring that the fields match the expected shape
  ///
  /// note: if provided, the typeArgs must match the # of typeArgs expected by the type
  /// The half of a record update after the type is known. Top-level, as above.
  let private recordUpdateAfterResolve
    (types : Types)
    (threadID : ThreadID)
    (tst : TypeSymbolTable)
    (sourceTypeName : FQTypeName.FQTypeName)
    (resolvedTypeName : FQTypeName.FQTypeName)
    (typeArgsBeforeUpdate : List<ValueType>)
    (currentFields : RecordFields)
    (fieldUpdates : (struct (string * Dval))[])
    (resolvedTypeArgs : List<string * ValueType>)
    (expectedFields : NEList<TypeDeclaration.RecordField>)
    : Dval =
    // A record built outside the interpreter (by a builtin, or converted from an F# value) can
    // carry no type args for a generic type. Treat each as unknown and let the update infer it,
    // as constructing the record would; zipping the two lists used to raise an internal error.
    let resolvedTypeArgs =
      if List.length typeArgsBeforeUpdate = List.length resolvedTypeArgs then
        List.zip typeArgsBeforeUpdate resolvedTypeArgs
        |> List.map (fun (beforeUpdate, (name, _)) -> (name, beforeUpdate))
      else
        resolvedTypeArgs |> List.map (fun (name, _) -> (name, ValueType.Unknown))

    // The record type's parameters shadow the caller's, as in `recordAfterResolve`. Without this
    // a type parameter the function happens to share a name with fills the record's: an update
    // of a `NameResolution<_>` inside a frame where some caller bound `'a` came out typed with
    // that caller's `'a`, and the return check raised.
    let tst =
      resolvedTypeArgs |> List.fold (fun acc (name, vt) -> TST.add name vt acc) tst

    let updated =
      updateRecordFields
        types
        threadID
        expectedFields
        fieldUpdates
        0
        (Array.copy currentFields.Entries)
        resolvedTypeArgs
        tst

    let struct (updatedFields, finalTypeArgs, _updatedTST) = updated
    (DRecord(
      sourceTypeName,
      resolvedTypeName,
      finalTypeArgs |> List.map Tuple2.second,
      RecordFields updatedFields
    ))
  let rec recordUpdate
    (types : Types)
    (threadID : ThreadID)
    (tst : TypeSymbolTable)
    (sourceTypeName : FQTypeName.FQTypeName)
    (resolvedTypeName : FQTypeName.FQTypeName)
    (typeArgsBeforeUpdate : List<ValueType>)
    (currentFields : RecordFields)
    (fieldUpdates : (struct (string * Dval))[])
    : Dval =
    if holdsRead fieldUpdates 0 || holdsRead currentFields.Entries 0 then
      let holds (_, v) = Pending.isPromise v
      let updates =
        fieldUpdates |> Array.map (fun (struct (n, v)) -> (n, v)) |> List.ofArray
      // Every field that was a read is checked again once landed, as an update.
      let reads =
        RecordFields.toList currentFields @ updates
        |> List.filter holds
        |> List.map fst
      let again _ =
        let current = RecordFields.map Pending.settled currentFields
        let merged =
          updates
          |> List.fold
            (fun m (n, v) -> RecordFields.add n (Pending.settled v) m)
            current
        reads
        |> List.distinct
        |> List.choose (fun n ->
          RecordFields.tryFind n merged |> Option.map (fun v -> struct (n, v)))
        |> Array.ofList
        |> recordUpdate
          types
          threadID
          tst
          sourceTypeName
          resolvedTypeName
          typeArgsBeforeUpdate
          merged
      recordUpdateNow
        types
        threadID
        tst
        sourceTypeName
        resolvedTypeName
        typeArgsBeforeUpdate
        currentFields
        fieldUpdates
      |> (Pending.built again)
    else
      recordUpdateNow
        types
        threadID
        tst
        sourceTypeName
        resolvedTypeName
        typeArgsBeforeUpdate
        currentFields
        fieldUpdates

  and private recordUpdateNow
    (types : Types)
    (threadID : ThreadID)
    (tst : TypeSymbolTable)
    (sourceTypeName : FQTypeName.FQTypeName)
    (resolvedTypeName : FQTypeName.FQTypeName)
    (typeArgsBeforeUpdate : List<ValueType>)
    (currentFields : RecordFields)
    (fieldUpdates : (struct (string * Dval))[])
    : Dval =
    let resolved = resolveRecordType types threadID sourceTypeName []

    let struct (_resolvedTypeName, resolvedTypeArgs, expectedFields) = resolved
    recordUpdateAfterResolve
      types
      threadID
      tst
      sourceTypeName
      resolvedTypeName
      typeArgsBeforeUpdate
      currentFields
      fieldUpdates
      resolvedTypeArgs
      expectedFields
