/// The run-time type-checker
/// Called by the interpreter, and a few other places
module LibExecution.TypeChecker

open Prelude
open RuntimeTypes
module VT = ValueType
module RTE = RuntimeError


type TypeCheckPathPart = RuntimeError.TypeChecking.TypeCheckPathPart
type ReverseTypeCheckPath = RuntimeError.TypeChecking.ReverseTypeCheckPath

/// Indicates what action to take when key is already in dictionary
type OverwriteBehaviour =
  | ReplaceValue
  | ThrowIfDuplicate

/// Synchronous answer for the unifications that need no recursion, no type lookup and no await: a
/// concrete scalar against its own ValueType, or anything against `Unknown`. That is the overwhelming
/// majority of calls, and type-checking arguments and results is a large share of what the interpreter
/// allocates, so answering them without entering a computation expression is worth the separate path.
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
      | TInt ->
        (match kt with
         | KTInt -> true
         | _ -> false)
      | TFloat ->
        (match kt with
         | KTFloat -> true
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
      | TBlob ->
        (match kt with
         | KTBlob -> true
         | _ -> false)
      | _ -> false


/// Answer a unification without entering a computation expression, for the cases that need no type lookup
/// and no recursion into a compound type.
///
/// `unifyValueType` already answers these two cases without awaiting, but it returns a `Ply`, so every
/// caller still binds it and the Ply builder allocates a continuation closure for the bind. Returning the
/// answer directly is what lets the whole argument-check loop stay out of the CE.
///
/// `Undecided` means "this one has to go the async route"; it is never a type error, so callers must
/// fall back rather than treat it as a failure.
///
/// A struct DU rather than `Result<_, _> voption`: this runs per argument per call, and the `Ok` wrapper
/// alone was one of the more frequently allocated objects in the interpreter.
[<Struct>]
type SyncUnification =
  /// Unified, with the symbol table it produced.
  | Unified of tst : TypeSymbolTable
  /// Genuinely doesn't unify. Carries no path to where: `tryUnifySync`, the only way in, answers any
  /// failure by re-running the whole check on the async route, which builds its own path for the error
  /// message. Threading one through here meant a cons per container level per argument per call, for a
  /// value that was discarded on failure and never read on success.
  | Mismatched
  /// Needs the async path -- a type lookup, or an error message worth building properly.
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
    // `List<Int>` argument -- which is most of what the standard library passes around -- would fall to
    // the async route purely for being a container, which is the common case rather than a rare one.
    | TList innerT ->
      match actual with
      | ValueType.Known(KTList innerV) -> unifyValueTypeSync tst innerT innerV
      | _ -> Undecided
    | TStream innerT ->
      match actual with
      | ValueType.Known(KTStream innerV) -> unifyValueTypeSync tst innerT innerV
      | _ -> Undecided
    | TDict innerT ->
      match actual with
      | ValueType.Known(KTDict innerV) -> unifyValueTypeSync tst innerT innerV
      | _ -> Undecided

    | TTuple(tFirst, tSecond, tRest) ->
      match actual with
      | ValueType.Known(KTTuple(vFirst, vSecond, vRest)) ->
        if List.length tRest <> List.length vRest then
          Undecided // length mismatch: let the async path build the error
        else
          let rec go i tst (es : List<TypeReference>) (vs : List<ValueType>) =
            match es with
            | [] -> Unified tst
            | e :: eRest ->
              match vs with
              | [] -> Unified tst
              | v :: vRest ->
                match unifyValueTypeSync tst e v with
                | Unified tst' -> go (i + 1) tst' eRest vRest
                | other -> other
          go 0 tst (tFirst :: tSecond :: tRest) (vFirst :: vSecond :: vRest)
      | _ -> Undecided

    // A custom type of exactly the declared name. `Types.find` would only be needed to ask whether
    // the declared type is an alias, and it can't be: an alias's name is the alias, but a value
    // built through one carries the underlying type's name, so equal names mean it resolved to
    // itself. Same argument as the `Dval` version in `tryUnifySync`.
    //
    // This case is what makes a *nested* custom type stay on the synchronous path. Without it,
    // `Result<'a, ParseError>` fell to the async unifier over the `ParseError`, which is to say
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
  match declared, actual with
  | [], [] -> Unified tst
  | d :: dRest, a :: aRest ->
    match unifyValueTypeSync tst d a with
    | Unified tst -> unifyTypeArgsSyncVT tst dRest aRest
    | other -> other
  | _ -> Undecided


/// Alias unwrapping without a CE. Only a resolved custom type can be an alias, and only that case needs
/// `Types.find`, so everything else is already the answer.
let unwrapAliasSync (typ : TypeReference) : TypeReference voption =
  match typ with
  | TCustomType({ resolved = Ok _ }, _) -> ValueNone
  | _ -> ValueSome typ


let rec unifyValueType
  (types : Types)
  (tst : TypeSymbolTable)
  (pathSoFar : ReverseTypeCheckPath)
  (expected : TypeReference)
  (actual : ValueType)
  : Ply<Result<TypeSymbolTable, ReverseTypeCheckPath>> =
  if unifiesTrivially expected actual then
    Ply(Ok tst)
  else

    match expected with
    // Hoisted out of the match below, and out of the computation expression, because it is the hot path
    // and needs neither. ~88% of calls are into functions whose signature has a type variable, and
    // `unifiesTrivially` never claims one, so nearly every non-trivial unification lands here. Answering it
    // outside the CE skips both the state machine and the `(expected, actual)` pair the tuple match
    // allocates.
    | TVariable name ->
      (match TST.tryFind name tst |> ValueOption.toOption with
       | None -> Ply(Ok(TST.add name actual tst))
       | Some t ->
         // Already bound to exactly this: `Map.add` would rebuild the symbol table's spine to store the
         // value it already holds. Dark's stdlib is heavily polymorphic, so this is the *common* path --
         // nearly every argument check binds a type variable, and most rebind it to what it already was.
         if t = actual then
           Ply(Ok tst)
         else
           match ValueType.merge t actual with
           | Ok merged ->
             if merged = t then Ply(Ok tst) else Ply(Ok(TST.add name merged tst))
           | Error() -> Ply(Error pathSoFar))

    | _ ->

      // Bound after the fast paths: this is a partial application, so it allocates a closure on every call
      // that reaches it, and most calls no longer do.
      let r = unifyValueType types

      uply {
        match expected, actual with

        | TVariable _, _ ->
          // Unreachable: handled above. Kept so the match stays exhaustive over TypeReference.
          return Ok tst

        | _, ValueType.Unknown -> return Ok tst


        | TUnit, ValueType.Known KTUnit -> return Ok tst
        | TBool, ValueType.Known KTBool -> return Ok tst

        | TInt8, ValueType.Known KTInt8 -> return Ok tst
        | TUInt8, ValueType.Known KTUInt8 -> return Ok tst
        | TInt16, ValueType.Known KTInt16 -> return Ok tst
        | TUInt16, ValueType.Known KTUInt16 -> return Ok tst
        | TInt32, ValueType.Known KTInt32 -> return Ok tst
        | TUInt32, ValueType.Known KTUInt32 -> return Ok tst
        | TInt64, ValueType.Known KTInt64 -> return Ok tst
        | TUInt64, ValueType.Known KTUInt64 -> return Ok tst
        | TInt128, ValueType.Known KTInt128 -> return Ok tst
        | TUInt128, ValueType.Known KTUInt128 -> return Ok tst
        | TInt, ValueType.Known KTInt -> return Ok tst

        | TFloat, ValueType.Known KTFloat -> return Ok tst

        | TChar, ValueType.Known KTChar -> return Ok tst
        | TString, ValueType.Known KTString -> return Ok tst

        | TUuid, ValueType.Known KTUuid -> return Ok tst
        | TDateTime, ValueType.Known KTDateTime -> return Ok tst

        | TBlob, ValueType.Known KTBlob -> return Ok tst

        | TStream innerT, ValueType.Known(KTStream innerV) ->
          return! r tst (TypeCheckPathPart.ListType :: pathSoFar) innerT innerV

        | TList innerT, ValueType.Known(KTList innerV) ->
          return! r tst (TypeCheckPathPart.ListType :: pathSoFar) innerT innerV

        | TDict innerT, ValueType.Known(KTDict innerV) ->
          return! r tst (TypeCheckPathPart.DictValueType :: pathSoFar) innerT innerV

        | TTuple(tFirst, tSecond, tRest),
          ValueType.Known(KTTuple(vFirst, vSecond, vRest)) ->
          // first, make sure that the tuple lengths match
          let expectedLen = 2 + List.length tRest
          let actualLen = 2 + List.length vRest
          if expectedLen <> actualLen then
            return
              Error(
                TypeCheckPathPart.TupleLength(expectedLen, actualLen) :: pathSoFar
              )
          else
            // then, make sure that the tuple elements match
            let expected = tFirst :: tSecond :: tRest
            let actual = vFirst :: vSecond :: vRest
            return!
              Ply.List.foldSequentiallyWithIndex
                (fun i acc (e, a) ->
                  match acc with
                  | Error _ -> Ply acc
                  | Ok tst ->
                    r tst (TypeCheckPathPart.TupleAtIndex i :: pathSoFar) e a)
                (Ok tst)
                (List.zip expected actual)

        | TCustomType({ originalName = names; resolved = Error err }, _), _ ->
          return RTE.ParseTimeNameResolution(names, err) |> raiseUntargetedRTE

        | TCustomType({ resolved = Ok typeNameT }, typeArgsT), actual ->
          // CLEANUP can't we assume aliases are already unwrapped?
          // if so, we can tidy this case quite a bit
          match! Types.find types typeNameT with
          | None -> return Error pathSoFar
          | Some expected ->
            match expected, actual with
            | { definition = TypeDeclaration.Alias aliasType }, _ ->
              let! expected = TypeReference.unwrapAlias types aliasType
              return! r tst pathSoFar expected actual

            | _, ValueType.Known(KTCustomType(typeNameV, typeArgsV)) ->
              if typeNameV <> typeNameT then
                return Error pathSoFar
              else if List.length typeArgsT <> List.length typeArgsV then
                // (this is really unexpected -- interpreter should prevent this)
                return
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
                return Ok tst
              else
                let typeArgCount = List.length typeArgsT
                return!
                  List.zip typeArgsT typeArgsV
                  |> Ply.List.foldSequentiallyWithIndex
                    (fun i acc (e, a) ->
                      match acc with
                      | Error _path -> Ply acc
                      | Ok tst ->
                        uply {
                          let path =
                            TypeCheckPathPart.TypeArg(typeNameT, i, typeArgCount)
                            :: pathSoFar
                          match! r tst path e a with
                          | Error path -> return Error path
                          | Ok tst -> return Ok tst
                        })
                    (Ok tst)

            | _, _ -> return Error pathSoFar

        | TFn(argTypes, returnType), ValueType.Known(KTFn(vArgs, vRet)) ->
          if NEList.length argTypes <> NEList.length vArgs then
            return Error pathSoFar // CLEANUP include the lengths in the path
          else
            return!
              List.zip
                (returnType :: (NEList.toList argTypes))
                (vRet :: (NEList.toList vArgs))
              |> Ply.List.foldSequentially
                (fun acc (e, a) ->
                  match acc with
                  | Error _path -> Ply acc
                  | Ok tst -> r tst pathSoFar e a)
                (Ok tst)

        | TDB innerT, ValueType.Known(KTDB innerV) ->
          return! r tst pathSoFar innerT innerV

        | _, _ -> return Error pathSoFar
      }


let unify
  (types : Types)
  (tst : TypeSymbolTable)
  (expected : TypeReference)
  (actual : Dval)
  : Ply<Result<TypeSymbolTable, ReverseTypeCheckPath>> =
  let actualType = Dval.toValueType actual
  if unifiesTrivially expected actualType then
    Ply(Ok tst)
  else
    uply {
      match! unifyValueType types tst [] expected actualType with
      | Error path -> return path |> Error
      | Ok updatedTst -> return Ok updatedTst
    }

/// Resolved declarations for types with no type arguments, which is nearly all of them.
///
/// Type-checking a record or enum argument resolves its declaration, and that goes to the package
/// manager every time. It shows up as soon as you look at anything that passes records around: an
/// HTTP request costs 258 KB, and the type checker is most of the profile.
///
/// Keyed on the `Types` instance as well as the name. Keying on the name alone looks safe -- an
/// `FQTypeName` is a content hash, so the same name should be the same declaration -- and it is not:
/// 713 tests errored, because tests mint their own package managers and reuse names across different
/// declarations. Scoping per `Types` keeps essentially all of the win anyway, since a server has one
/// for the life of the process.
///
/// Only the non-alias branch is cached: resolving an alias depends on the caller's type symbol
/// table, so its answer isn't a function of the name.
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
  : Ply<FQTypeName.FQTypeName * List<string * ValueType> * TypeDeclaration.Definition> =
  let mutable cached = Unchecked.defaultof<_>
  if List.isEmpty typeArgs && (cacheFor types).TryGetValue(typeName, &cached) then
    // Outside the computation expression: once warm this is where nearly every call lands, and
    // entering the builder to hand back a value already in hand costs more than the lookup.
    Ply cached
  else

    uply {
      match! Types.find types typeName with
      | None -> return RTE.TypeNotFound typeName |> raiseRTE threadID
      | Some decl ->
        match decl.definition with
        | TypeDeclaration.Alias aliasedType ->
          let! resolvedType = TypeReference.unwrapAlias types aliasedType
          match resolvedType with
          | TCustomType({ resolved = Ok innerTypeName }, innerTypeArgs) ->
            match! Types.find types innerTypeName with
            | None -> return RTE.TypeNotFound innerTypeName |> raiseRTE threadID
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
              let! mappedInnerArgsVT =
                List.zip targetDecl.typeParams innerTypeArgs
                |> Ply.List.mapSequentially (fun (targetParam, typeRef) ->
                  uply {
                    let! vt = TypeReference.toVT types tst typeRef
                    return
                      match typeRef with
                      | TVariable name ->
                        match Map.tryFind name typeArgsMap with
                        | Some vt -> (targetParam, vt)
                        | None -> (targetParam, vt)
                      | _ -> (targetParam, vt)
                  })

              return!
                resolveType types threadID tst innerTypeName []
                |> Ply.map (fun (resolvedName, _, def) ->
                  (resolvedName, mappedInnerArgsVT, def))

          | _ -> return RTE.TypeNotFound typeName |> raiseRTE threadID

        | definition ->
          let typeArgsVT =
            if List.isEmpty typeArgs && not (List.isEmpty decl.typeParams) then
              decl.typeParams |> List.map (fun p -> (p, ValueType.Unknown))
            else
              List.zip decl.typeParams typeArgs

          let result = (typeName, typeArgsVT, definition)
          if List.isEmpty typeArgs then (cacheFor types)[typeName] <- result
          return result
    }



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
let private unifyDvalSync
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
  | TDict innerT ->
    match actual with
    | DDict(innerV, _) -> unifyValueTypeSync tst innerT innerV
    | _ -> unifyValueTypeSync tst expected (Dval.toValueType actual)
  | _ -> unifyValueTypeSync tst expected (Dval.toValueType actual)


/// The unification behind both the parameter and the result checks, answered without a computation
/// expression when it needs no type lookup.
///
/// Runs on every argument of every call and on every frame return, so the point is that the ordinary case
/// allocates nothing at all: no Ply, no continuation closure, no state machine.
///
/// `ValueNone` means "go the async route". That covers an aliased type, a compound type needing recursion,
/// and *any* failure: building the error message resolves the expected type, which can hit the package
/// store. Failures are rare and already the slow path, so they are not worth a sync variant.
/// The type arguments of a custom type, unified pairwise without awaiting.
///
/// Parameterised types are not a corner case: `Option` and `Result` are the two most common types in
/// the language, and until this existed, returning one cost about 4 KB against 1.3 KB for a plain
/// record, entirely because the type arguments sent the check down the asynchronous route.
///
/// `ValueNone` on anything unresolved *or* mismatched, so the async path still produces the error
/// message and the path that describes where the mismatch was.
let rec private unifyTypeArgsSync
  (tst : TypeSymbolTable)
  (declared : List<TypeReference>)
  (actual : List<ValueType>)
  : TypeSymbolTable voption =
  match unifyTypeArgsSyncVT tst declared actual with
  | Unified tst -> ValueSome tst
  | Mismatched
  | Undecided -> ValueNone


let tryUnifySync
  (tst : TypeSymbolTable)
  (expected : TypeReference)
  (actual : Dval)
  : TypeSymbolTable voption =
  match expected with
  // A record or enum of exactly the declared type, with no type arguments. This is what a program
  // that models anything passes around all day, and until now every one of them fell through to the
  // async unifier purely to ask `Types.find` whether the declared type was an alias.
  //
  // It can't be. An alias's *name* is the alias, but a value built through it carries the underlying
  // type's name, so if the two names are equal the declared type resolved to itself -- and names are
  // content hashes, so that means it is the type, not an alias to something else.
  //
  // Type arguments are excluded rather than compared: with them the answer depends on unifying each
  // one, which is what the async path is for.
  | TCustomType({ resolved = Ok declared }, declaredArgs) ->
    match actual with
    | DRecord(_, actualName, actualArgs, _) when actualName = declared ->
      unifyTypeArgsSync tst declaredArgs actualArgs
    | DEnum(_, actualName, actualArgs, _, _) when actualName = declared ->
      unifyTypeArgsSync tst declaredArgs actualArgs
    | _ -> ValueNone

  | _ ->

    match unwrapAliasSync expected with
    | ValueNone -> ValueNone
    | ValueSome expected ->
      match unifyDvalSync tst expected actual with
      | Unified updatedTst -> ValueSome updatedTst
      | Mismatched
      | Undecided -> ValueNone


let checkFnParam
  (types : Types)
  (fnName : FQFnName.FQFnName)
  (tst : TypeSymbolTable)
  (paramIndex : int)
  (paramName : string)
  (expected : TypeReference)
  (actual : Dval)
  : Ply<Result<TypeSymbolTable, RTE.Error>> =
  uply {
    let! expected = TypeReference.unwrapAlias types expected
    match! unify types tst expected actual with
    | Ok updatedTst -> return Ok updatedTst
    | Error _path ->
      let! expected = TypeReference.toVT types tst expected
      return
        RTE.Applications.FnParameterNotExpectedType(
          fnName,
          paramIndex,
          paramName,
          expected,
          Dval.toValueType actual,
          actual
        )
        |> RTE.Apply
        |> Error
  }


let checkFnResult
  (types : Types)
  (fnName : FQFnName.FQFnName)
  (tst : TypeSymbolTable)
  (expected : TypeReference)
  (actual : Dval)
  : Ply<Result<TypeSymbolTable, RTE.Error>> =
  uply {
    let! expected = TypeReference.unwrapAlias types expected
    match! unify types tst expected actual with
    | Ok updatedTst -> return Ok updatedTst
    | Error _path ->
      // Resolved here rather than before the unify: it exists only to render the error, and computing it
      // eagerly meant every successful return -- which is nearly all of them -- paid a full type
      // resolution (`toVT` walks the reference and can hit `Types.find`) to build a message nobody sees.
      let! expectedVT = TypeReference.toVT types tst expected
      return
        RTE.Applications.FnResultNotExpectedType(
          fnName,
          expectedVT,
          Dval.toValueType actual,
          actual
        )
        |> RTE.Apply
        |> Error
  }


/// Helpers for creating type-checked Dvals
/// (lists, records, enums, etc.)
///
/// Dvals should be created carefully,:
/// - to have the correct `ValueType`s, where appropriate
///  i.e. we should not have `DList(Known KTInt64, [ DString("hi") ])`
///
/// - similarly, we should fail when trying to merge `Dval`s with conflicting `ValueType`s
///   i.e. `List.append [1] ["hi"]` should fail
///   because we can't merge `Known KTInt64` and `Known KTString`
///
/// These functions are intended to help with both of these,
/// in cases where the functions in `Dval.fs` are insufficient
/// (i.e. we don't know the Dark sub-types of a Dval in some F# code).
///
/// Doing this at-construction is important to ensure efficient run-time type-checking.
module DvalCreator =
  // CLEANUP consider skipping type-checking after N elements or after the type args are fully resolved, whichever comes last.
  //   In order to support this^, add another param or two so that direct [] interpretation is differentiated from calls to this from Builtins and other places.
  //   (or split this into 2 separate fns with clearer names)
  let list (threadID : ThreadID) (typ : ValueType) (items : List<Dval>) : Dval =
    let (typ, items) =
      items
      |> List.foldWithIndex
        (fun i (typ, list) dv ->
          let dvalType = Dval.toValueType dv

          match VT.merge typ dvalType with
          | Ok newType -> newType, dv :: list
          | Error() ->
            RTE.Lists.Error.TriedToAddMismatchedData(i, typ, dvalType, dv)
            |> RTE.Error.List
            |> raiseRTE threadID)
        (typ, [])

    DList(typ, List.rev items)


  // CLEANUP see notes in `list` above
  let dict
    (threadID : ThreadID)
    (typ : ValueType)
    (entries : List<string * Dval>)
    : Dval =
    let (typ, entries) =
      List.fold
        (fun (typ, entries) (k, v) ->
          if Map.containsKey k entries then
            RTE.Dicts.Error.TriedToAddKeyAfterAlreadyPresent k
            |> RTE.Error.Dict
            |> raiseRTE threadID

          let vt = Dval.toValueType v

          match VT.merge typ vt with
          | Ok merged -> (merged, Map.add k v entries)
          | Error() ->
            RTE.Dicts.Error.TriedToAddMismatchedData(k, typ, vt, v)
            |> RTE.Error.Dict
            |> raiseRTE threadID)

        (typ, Map.empty)
        entries

    DDict(typ, entries)

  let dictAddEntry
    (threadID : ThreadID)
    (typ : ValueType)
    (entries : DvalMap)
    (newEntry : string * Dval)
    (overwrite : OverwriteBehaviour)
    : ValueType * DvalMap =
    let (k, v) = newEntry
    match overwrite with
    | ThrowIfDuplicate when Map.containsKey k entries ->
      RTE.Dicts.Error.TriedToAddKeyAfterAlreadyPresent k
      |> RTE.Error.Dict
      |> raiseRTE threadID
    | ReplaceValue
    | ThrowIfDuplicate ->
      let vt = Dval.toValueType v
      match VT.merge typ vt with
      | Ok merged -> (merged, Map.add k v entries)
      | Error() ->
        RTE.Dicts.Error.TriedToAddMismatchedData(k, typ, vt, v)
        |> RTE.Error.Dict
        |> raiseRTE threadID

  let optionNone (innerType : ValueType) : Dval =
    DEnum(Dval.optionType (), Dval.optionType (), [ innerType ], "None", [])

  let optionSome (threadID : ThreadID) (expected : ValueType) (dv : Dval) : Dval =
    let typeName = Dval.optionType ()

    let vt = Dval.toValueType dv

    match VT.merge expected vt with
    | Ok typ -> DEnum(typeName, typeName, [ typ ], "Some", [ dv ])
    | Error() ->
      RuntimeError.Enums.ConstructionFieldOfWrongType("Some", 0, expected, vt, dv)
      |> RuntimeError.Enum
      |> raiseRTE threadID


  let option
    (threadID : ThreadID)
    (expectedType : ValueType)
    (dv : Option<Dval>)
    : Dval =
    match dv with
    | Some dv -> optionSome threadID expectedType dv
    | None -> optionNone expectedType


  module Result =
    let ok
      (threadID : ThreadID)
      (okType : ValueType)
      (errorType : ValueType)
      (dvOk : Dval)
      : Dval =
      let typeName = Dval.resultType ()
      let dvalType = Dval.toValueType dvOk
      match VT.merge okType dvalType with
      | Ok typ -> DEnum(typeName, typeName, [ typ; errorType ], "Ok", [ dvOk ])
      | Error() ->
        RuntimeError.Enums.ConstructionFieldOfWrongType(
          "Ok",
          0,
          okType,
          dvalType,
          dvOk
        )
        |> RuntimeError.Enum
        |> raiseRTE threadID

    let error
      (threadID : ThreadID)
      (okType : ValueType)
      (errorType : ValueType)
      (dvError : Dval)
      : Dval =
      let typeName = Dval.resultType ()
      let dvalType = Dval.toValueType dvError
      match VT.merge errorType dvalType with
      | Ok typ -> DEnum(typeName, typeName, [ okType; typ ], "Error", [ dvError ])
      | Error() ->
        RuntimeError.Enums.ConstructionFieldOfWrongType(
          "Error",
          0,
          errorType,
          dvalType,
          dvError
        )
        |> RuntimeError.Enum
        |> raiseRTE threadID

    let result
      (threadID : ThreadID)
      (okType : ValueType)
      (errorType : ValueType)
      (dv : Result<Dval, Dval>)
      : Dval =
      match dv with
      | Ok dv -> ok threadID okType errorType dv
      | Error dv -> error threadID okType errorType dv


  let resolveEnumType
    (types : Types)
    (threadID : ThreadID)
    (typeName : FQTypeName.FQTypeName)
    (typeArgs : List<ValueType>)
    : Ply<FQTypeName.FQTypeName *
      List<string * ValueType> *
      NEList<TypeDeclaration.EnumCase>>
    =
    uply {
      let! (resolvedName, typeArgs, definition) =
        resolveType types threadID TST.empty typeName typeArgs

      match definition with
      | TypeDeclaration.Enum cases -> return (resolvedName, typeArgs, cases)
      | _ ->
        return
          Exception.raiseInternal
            "Expected enum type but found other type"
            [ "typeName", typeName ]
    }



  /// One field of an enum case being constructed. Same shape as `checkRecordFields`, and the same
  /// reason for existing: as a lambda over a three-element accumulator it cost a closure, a state
  /// machine and a tuple per field of every enum ever built.
  let rec private checkEnumFields
    (types : Types)
    (threadID : ThreadID)
    (caseName : string)
    (fieldIndex : int)
    (remaining : List<TypeReference * Dval>)
    (typeArgs : List<string * ValueType>)
    (fieldsInReverse : List<Dval>)
    (tst : TypeSymbolTable)
    : Ply<List<string * ValueType> * List<Dval> * TypeSymbolTable> =
    match remaining with
    | [] -> Ply((typeArgs, fieldsInReverse, tst))
    | (fieldDef, actualField) :: rest ->
      // Same fast path as `checkRecordFields`: most fields unify without needing the type store, a
      // case with no type parameters has nothing to learn from them, and Ply's builder is not
      // resumable code, so entering it costs a continuation in Release as much as in Debug.
      match tryUnifySync tst fieldDef actualField with
      | ValueSome newTST when List.isEmpty typeArgs ->
        checkEnumFields
          types
          threadID
          caseName
          (fieldIndex + 1)
          rest
          typeArgs
          (actualField :: fieldsInReverse)
          newTST
      | _ ->

        uply {
          match! unify types tst fieldDef actualField with
          | Error _path ->
            // Resolved here rather than before the check that almost always passes: it only exists
            // to describe the failure.
            let! expected = TypeReference.toVT types tst fieldDef
            return
              RTE.Enums.ConstructionFieldOfWrongType(
                caseName,
                fieldIndex,
                expected,
                Dval.toValueType actualField,
                actualField
              )
              |> RTE.Error.Enum
              |> raiseRTE threadID

          | Ok newTST ->
            // A type with no parameters has nothing to learn from its fields, and that's the common
            // case. Skipping the walk skips a rebuilt list of pairs and a builder entry per field.
            let! newTypeArgs =
              if List.isEmpty typeArgs then
                Ply typeArgs
              else
                Ply.List.mapSequentially
                  (fun (paramName, vt) ->
                    match vt with
                    | ValueType.Unknown ->
                      match TST.tryFind paramName newTST with
                      | ValueSome known -> Ply((paramName, known))
                      | ValueNone -> Ply((paramName, vt))

                    | known ->
                      match ValueType.merge known vt with
                      | Ok merged -> Ply((paramName, merged))
                      | Error() ->
                        uply {
                          let! expected = TypeReference.toVT types tst fieldDef
                          return
                            RTE.Enums.ConstructionFieldOfWrongType(
                              caseName,
                              fieldIndex,
                              expected,
                              Dval.toValueType actualField,
                              actualField
                            )
                            |> RTE.Enum
                            |> raiseRTE threadID
                        })
                  typeArgs

            return!
              checkEnumFields
                types
                threadID
                caseName
                (fieldIndex + 1)
                rest
                newTypeArgs
                (actualField :: fieldsInReverse)
                newTST
        }


  let enum
    (types : Types)
    (threadID : ThreadID)
    (tst : TypeSymbolTable)
    (sourceTypeName : FQTypeName.FQTypeName)
    (typeArgs : List<ValueType>)
    (caseName : string)
    (fields : List<Dval>)
    : Ply<Dval> =
    uply {
      // do basic resolution of aliases and type args
      let! (resolvedTypeName, typeArgs, caseDefs) =
        resolveEnumType types threadID sourceTypeName typeArgs

      let tst = typeArgs |> List.fold (fun acc (name, vt) -> TST.add name vt acc) tst

      // Find the case definition
      let foundCaseDef = caseDefs |> NEList.find (fun c -> c.name = caseName)

      match foundCaseDef with
      | None ->
        return
          RTE.Enums.ConstructionCaseNotFound(resolvedTypeName, caseName)
          |> RTE.Error.Enum
          |> raiseRTE threadID

      | Some case ->
        // Zip the fields, if we got the right # of them
        let fieldsZipped =
          let expected, actual = (List.length case.fields, List.length fields)

          if expected <> actual then
            RTE.Enums.ConstructionWrongNumberOfFields(
              resolvedTypeName,
              caseName,
              expected,
              actual
            )
            |> RTE.Error.Enum
            |> raiseRTE threadID
          else
            List.zip case.fields fields

        // Process each field, updating type args as we learn more
        let! (typeArgs, fieldsInReverse, _updatedTst) =
          checkEnumFields types threadID caseName 0 fieldsZipped typeArgs [] tst

        let typeArgs = typeArgs |> List.map Tuple2.second
        let fields = List.rev fieldsInReverse
        return DEnum(sourceTypeName, resolvedTypeName, typeArgs, caseName, fields)
    }


  // Resolve aliases and collect expected fields for a record type
  let resolveRecordType
    (types : Types)
    (threadID : ThreadID)
    (typeName : FQTypeName.FQTypeName)
    (typeArgs : List<ValueType>)
    : Ply<FQTypeName.FQTypeName *
      List<string * ValueType> *
      NEList<TypeDeclaration.RecordField>>
    =
    uply {
      let! (resolvedName, typeArgs, definition) =
        resolveType types threadID TST.empty typeName typeArgs

      match definition with
      | TypeDeclaration.Record fields -> return (resolvedName, typeArgs, fields)
      | _ ->
        return
          RTE.Records.CreationTypeNotRecord typeName
          |> RTE.Record
          |> raiseRTE threadID
    }



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
  /// Top-level and fully parameterised, and threading its accumulators as arguments rather than as
  /// a tuple. As a lambda passed to `Ply.List.foldSequentially` it allocated a closure, a state
  /// machine and a three-element accumulator tuple *per field of every record ever built*; the
  /// tuple alone was 2% of the profile for an HTTP request.
  let rec private checkRecordFields
    (types : Types)
    (threadID : ThreadID)
    (expectedFields : NEList<TypeDeclaration.RecordField>)
    (remaining : List<string * Dval>)
    (fieldsSoFar : Map<string, Dval>)
    (currentTypeArgs : List<string * ValueType>)
    (tst : TypeSymbolTable)
    : Ply<Map<string, Dval> * List<string * ValueType> * TypeSymbolTable> =
    match remaining with
    | [] -> Ply((fieldsSoFar, currentTypeArgs, tst))
    | (fieldName, fieldValue) :: rest ->
      // The validations and the lookup are synchronous, so they happen before the builder rather
      // than inside it.
      if fieldName = "" then
        RTE.Records.CreationEmptyKey |> RTE.Record |> raiseRTE threadID

      if Map.containsKey fieldName fieldsSoFar then
        RTE.Records.CreationDuplicateField fieldName
        |> RTE.Record
        |> raiseRTE threadID

      match findExpectedField expectedFields fieldName with
      | ValueNone ->
        RTE.Records.CreationFieldNotExpected fieldName
        |> RTE.Record
        |> raiseRTE threadID

      | ValueSome fieldDef ->
        // The overwhelming majority of fields unify without needing the type store, and a type with
        // no parameters has nothing to learn from them. When both hold, the whole field is handled
        // without entering the Ply builder at all -- and Ply's builder is not resumable code, so it
        // allocates a continuation in Release just as much as in Debug. This was 9% of the profile
        // of an HTTP server returning a constant string.
        match tryUnifySync tst fieldDef.typ fieldValue with
        | ValueSome newTST when List.isEmpty currentTypeArgs ->
          checkRecordFields
            types
            threadID
            expectedFields
            rest
            (Map.add fieldName fieldValue fieldsSoFar)
            currentTypeArgs
            newTST
        | _ ->

          uply {
            match! unify types tst fieldDef.typ fieldValue with
            | Error _path ->
              // The declared type is only needed to describe the failure, so it's resolved here
              // rather than before the check that almost always passes.
              let! expected = TypeReference.toVT types tst fieldDef.typ
              return
                RTE.Records.CreationFieldOfWrongType(
                  fieldName,
                  expected,
                  Dval.toValueType fieldValue,
                  fieldValue
                )
                |> RTE.Record
                |> raiseRTE threadID

            | Ok newTST ->
              // Update the type args with anything this field pinned down. See the note in
              // `checkEnumFields`: no type parameters means nothing to learn, and no walk.
              let! newTypeArgs =
                if List.isEmpty currentTypeArgs then
                  Ply currentTypeArgs
                else
                  Ply.List.mapSequentially
                    (fun (paramName, vt) ->
                      match vt with
                      | ValueType.Unknown ->
                        match TST.tryFind paramName newTST with
                        | ValueSome known -> Ply((paramName, known))
                        | ValueNone -> Ply((paramName, vt))

                      | known ->
                        match ValueType.merge known vt with
                        | Ok merged -> Ply((paramName, merged))
                        | Error() ->
                          uply {
                            let! expected =
                              TypeReference.toVT types newTST fieldDef.typ
                            return
                              RTE.Records.CreationFieldOfWrongType(
                                fieldName,
                                expected,
                                Dval.toValueType fieldValue,
                                fieldValue
                              )
                              |> RTE.Record
                              |> raiseRTE threadID
                          })
                    currentTypeArgs

              return!
                checkRecordFields
                  types
                  threadID
                  expectedFields
                  rest
                  (Map.add fieldName fieldValue fieldsSoFar)
                  newTypeArgs
                  newTST
          }


  /// Constructs a Dval.DRecord, ensuring that the fields match the expected shape
  ///
  /// note: if provided, the typeArgs must match the # of typeArgs expected by the type
  let record
    (types : Types)
    (threadID : ThreadID)
    (tst : TypeSymbolTable)
    (sourceTypeName : FQTypeName.FQTypeName)
    (typeArgs : List<ValueType>)
    (fields : List<string * Dval>)
    : Ply<Dval> =
    uply {
      let! (resolvedTypeName, resolvedTypeArgs, expectedFields) =
        resolveRecordType types threadID sourceTypeName typeArgs

      let tst =
        resolvedTypeArgs |> List.fold (fun acc (name, vt) -> TST.add name vt acc) tst

      let! (processedFields, finalTypeArgs, _updatedTST) =
        checkRecordFields
          types
          threadID
          expectedFields
          fields
          Map.empty
          resolvedTypeArgs
          tst

      // Check for missing fields
      match
        expectedFields
        |> NEList.find (fun f -> not (Map.containsKey f.name processedFields))
      with
      | Some missingField ->
        return
          RTE.Records.CreationMissingField missingField.name
          |> RTE.Record
          |> raiseRTE threadID

      | None ->
        return
          DRecord(
            sourceTypeName,
            resolvedTypeName,
            finalTypeArgs |> List.map Tuple2.second,
            processedFields
          )
    }


  /// Constructs a Dval.DRecord, ensuring that the fields match the expected shape
  ///
  /// note: if provided, the typeArgs must match the # of typeArgs expected by the type
  let recordUpdate
    (types : Types)
    (threadID : ThreadID)
    (tst : TypeSymbolTable)
    (sourceTypeName : FQTypeName.FQTypeName)
    (resolvedTypeName : FQTypeName.FQTypeName)
    (typeArgsBeforeUpdate : List<ValueType>)
    (currentFields : Map<string, Dval>)
    (fieldUpdates : List<string * Dval>)
    : Ply<Dval> =
    uply {
      let! (_resolvedTypeName, resolvedTypeArgs, expectedFields) =
        resolveRecordType types threadID sourceTypeName []

      let resolvedTypeArgs =
        List.zip typeArgsBeforeUpdate resolvedTypeArgs
        |> List.map (fun (beforeUpdate, (name, _)) -> (name, beforeUpdate))

      let! (updatedFields, finalTypeArgs, _updatedTST) =
        Ply.List.foldSequentially
          (fun (fieldsSoFar, currentTypeArgs, tst) (fieldName, fieldValue) ->
            uply {
              if fieldName = "" then
                return RTE.Records.UpdateEmptyKey |> RTE.Record |> raiseRTE threadID

              // CLEANUP if there are duplicate updates for the the same field, raise a `UpdateDuplicateField` RTE

              else
                match
                  expectedFields |> NEList.find (fun f -> f.name = fieldName)
                with
                | None ->
                  return
                    RTE.Records.UpdateFieldNotExpected fieldName
                    |> RTE.Record
                    |> raiseRTE threadID

                | Some fieldDef ->
                  let! expected = TypeReference.toVT types tst fieldDef.typ
                  match! unify types tst fieldDef.typ fieldValue with
                  | Error _path ->
                    // CLEANUP involve path, somehow
                    return
                      RTE.Records.UpdateFieldOfWrongType(
                        fieldName,
                        expected,
                        Dval.toValueType fieldValue,
                        fieldValue
                      )
                      |> RTE.Record
                      |> raiseRTE threadID
                  | Ok updatedTst ->
                    let! expected = TypeReference.toVT types updatedTst fieldDef.typ

                    // Update resultant typeArgs based on what we learned from this field
                    // , by checking the TST.
                    let newTypeArgs =
                      currentTypeArgs
                      |> List.map (fun (paramName, vt) ->
                        match vt with
                        | ValueType.Unknown ->
                          match
                            TST.tryFind paramName updatedTst
                            |> ValueOption.toOption
                          with
                          | Some known -> (paramName, known)
                          | None -> (paramName, vt)

                        | known ->
                          match ValueType.merge known vt with
                          | Ok merged -> (paramName, merged)
                          | Error() ->
                            RTE.Records.UpdateFieldOfWrongType(
                              fieldName,
                              expected,
                              Dval.toValueType fieldValue,
                              fieldValue
                            )
                            |> RTE.Record
                            |> raiseRTE threadID)

                    let fields = Map.add fieldName fieldValue fieldsSoFar

                    return (fields, newTypeArgs, updatedTst)
            })
          (currentFields, resolvedTypeArgs, tst)
          fieldUpdates

      let finalTypeArgs = finalTypeArgs |> List.map Tuple2.second

      return DRecord(sourceTypeName, resolvedTypeName, finalTypeArgs, updatedFields)
    }
