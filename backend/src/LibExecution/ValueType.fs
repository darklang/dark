/// helper functions related to RT.ValueType
[<RequireQualifiedAccess>]
module LibExecution.ValueType

open Prelude
open RuntimeTypes

// some helpers to reduce typing elsewhere
let unknown = ValueType.Unknown
let unknownTODO = ValueType.Unknown
let unknownDbTODO = ValueType.Unknown
let typeArgsTODO = []

let known inner = ValueType.Known inner

let unit = known KTUnit
let bool = known KTBool
let int8 = known KTInt8
let uint8 = known KTUInt8
let int16 = known KTInt16
let uint16 = known KTUInt16
let int32 = known KTInt32
let uint32 = known KTUInt32
let int64 = known KTInt64
let uint64 = known KTUInt64
let int128 = known KTInt128
let uint128 = known KTUInt128
let bigInt = known KTBigInt
let float = known KTFloat64
let float32 = known KTFloat32
let decimal = known KTDecimal
let char = known KTChar
let string = known KTString
let dateTime = known KTDateTime
let uuid = known KTUuid
let bytes = known KTBytes
let stream (inner : ValueType) : ValueType = known (KTStream inner)

let list (inner : ValueType) : ValueType = known (KTList inner)
let dict (key : ValueType) (value : ValueType) : ValueType =
  known (KTDict(key, value))
let set (element : ValueType) : ValueType = known (KTSet element)
let tuple
  (first : ValueType)
  (second : ValueType)
  (theRest : List<ValueType>)
  : ValueType =
  KTTuple(first, second, theRest) |> known

let customType
  (typeName : FQTypeName.FQTypeName)
  (typeArgs : List<ValueType>)
  : ValueType =
  KTCustomType(typeName, typeArgs) |> known


let rec private mergeKnownTypes
  (left : KnownType)
  (right : KnownType)
  : Result<KnownType, unit> =
  // Two references to the same type merge to themselves. Worth checking first because F# builds a
  // pair on the heap to evaluate the match below, and the scalar cases are nullary DU cases, which
  // are singletons -- so the overwhelmingly common "already the same type" case never builds one.
  if System.Object.ReferenceEquals(left, right) then
    Ok left
  else
    // Check stack space before recursive type merging so deep types raise a catchable
    // exception instead of overflowing the native stack. See Dval.equals.
    System.Runtime.CompilerServices.RuntimeHelpers.EnsureSufficientExecutionStack()

    match left, right with
    | KTUnit, KTUnit -> KTUnit |> Ok
    | KTBool, KTBool -> KTBool |> Ok
    | KTInt8, KTInt8 -> KTInt8 |> Ok
    | KTUInt8, KTUInt8 -> KTUInt8 |> Ok
    | KTInt16, KTInt16 -> KTInt16 |> Ok
    | KTUInt16, KTUInt16 -> KTUInt16 |> Ok
    | KTInt32, KTInt32 -> KTInt32 |> Ok
    | KTUInt32, KTUInt32 -> KTUInt32 |> Ok
    | KTInt64, KTInt64 -> KTInt64 |> Ok
    | KTUInt64, KTUInt64 -> KTUInt64 |> Ok
    | KTInt128, KTInt128 -> KTInt128 |> Ok
    | KTUInt128, KTUInt128 -> KTUInt128 |> Ok
    | KTBigInt, KTBigInt -> KTBigInt |> Ok
    | KTFloat64, KTFloat64 -> KTFloat64 |> Ok
    | KTFloat32, KTFloat32 -> KTFloat32 |> Ok
    | KTDecimal, KTDecimal -> KTDecimal |> Ok
    | KTChar, KTChar -> KTChar |> Ok
    | KTString, KTString -> KTString |> Ok
    | KTUuid, KTUuid -> KTUuid |> Ok
    | KTDateTime, KTDateTime -> KTDateTime |> Ok
    | KTBytes, KTBytes -> KTBytes |> Ok
    | KTStream left, KTStream right -> merge left right |> Result.map KTStream

    | KTList left, KTList right -> merge left right |> Result.map KTList
    | KTDict(lk, lv), KTDict(rk, rv) ->
      match merge lk rk, merge lv rv with
      | Ok k, Ok v -> Ok(KTDict(k, v))
      | _ -> Error()
    | KTSet left, KTSet right -> merge left right |> Result.map KTSet
    | KTTuple(l1, l2, ls), KTTuple(r1, r2, rs) ->
      let firstMerged = merge l1 r1
      let secondMerged = merge l2 r2
      if List.length ls <> List.length rs then
        Error()
      else
        let restMerged = List.map2 merge ls rs |> Result.collect

        match firstMerged, secondMerged, restMerged with
        | Ok first, Ok second, Ok rest -> Ok(KTTuple(first, second, rest))
        | _ -> Error()

    | KTCustomType(lName, lArgs), KTCustomType(rName, rArgs) ->
      if lName <> rName then
        Error()
      else if List.length lArgs <> List.length rArgs then
        Error()
      else
        List.map2 merge lArgs rArgs
        |> Result.collect
        |> Result.map (fun args -> KTCustomType(lName, args))

    | KTFn(lArgs, lRet), KTFn(rArgs, rRet) ->
      let argsMerged = NEList.map2 merge lArgs rArgs |> Result.collectNE
      let retMerged = merge lRet rRet

      match argsMerged, retMerged with
      | Ok args, Ok ret -> Ok(KTFn(args, ret))
      | _ -> Error()

    | _ -> Error()

and merge (left : ValueType) (right : ValueType) : Result<ValueType, unit> =
  if System.Object.ReferenceEquals(left, right) then
    Ok left
  else

    // Nested matches, not `match left, right with`: the tuple form allocates the pair, and this
    // runs once per list element, dict entry and record field.
    match left with
    | ValueType.Unknown -> Ok right
    | ValueType.Known l ->
      match right with
      | ValueType.Unknown -> Ok left
      | ValueType.Known r -> mergeKnownTypes l r |> Result.map ValueType.Known



/// Whether merging <param dv>'s type into <param vt> would give back <param vt>, answered without
/// building <param dv>'s type. Only `true` where that is certain: the same scalar, a list of the
/// same element type object, a custom type of the same name whose arguments are the same objects
/// or still unknown in the value, a tuple whose parts all keep theirs.
let rec private keepsType (vt : ValueType) (dv : Dval) : bool =
  match vt with
  | ValueType.Unknown -> false
  | ValueType.Known kt ->
    match kt with
    | KTTuple(first, second, rest) ->
      match dv with
      | DTuple(a, b, others) ->
        keepsType first a && keepsType second b && keepTypes rest others
      | _ -> false
    | KTCustomType(name, args) ->
      match dv with
      | DRecord(_, actualName, actualArgs, _)
      | DEnum(_, actualName, actualArgs, _, _) ->
        actualName = name && argsKept args actualArgs
      | _ -> false
    | _ ->
      // Scalars are shared singletons, and a list's type is memoized on its element type.
      match Dval.toValueType dv with
      | ValueType.Known actual -> System.Object.ReferenceEquals(actual, kt)
      | ValueType.Unknown -> false

and private keepTypes (vts : List<ValueType>) (dvs : List<Dval>) : bool =
  match vts with
  | [] -> List.isEmpty dvs
  | vt :: vtRest ->
    match dvs with
    | [] -> false
    | dv :: dvRest -> keepsType vt dv && keepTypes vtRest dvRest

and private argsKept (args : List<ValueType>) (actual : List<ValueType>) : bool =
  match args with
  | [] -> List.isEmpty actual
  | a :: aRest ->
    match actual with
    | [] -> false
    | b :: bRest ->
      (System.Object.ReferenceEquals(a, b) || (b.IsUnknown && not a.IsUnknown))
      && argsKept aRest bRest


/// `merge vt (Dval.toValueType dv)`: what a collection's element type becomes when <param dv> is
/// added to it. A value that already has that type leaves it as it is, without its type being
/// built and merged: a list of tuples gained a new tuple type on every element added, and a merge
/// of two tuple types rebuilds the tuple even when the two are the same.
let mergeDval (vt : ValueType) (dv : Dval) : Result<ValueType, unit> =
  if keepsType vt dv then Ok vt else merge vt (Dval.toValueType dv)
