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
let float = known KTFloat
let char = known KTChar
let string = known KTString
let dateTime = known KTDateTime
let uuid = known KTUuid

let list (inner : ValueType) : ValueType = known (KTList inner)
let dict (inner : ValueType) : ValueType = known (KTDict inner)
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

// let rec toString (vt : ValueType) : string =
//   match vt with
//   | ValueType.Unknown -> "_"
//   | ValueType.Known kt ->
//     match kt with
//     | KTUnit -> "Unit"
//     | KTBool -> "Bool"
//     | KTInt8 -> "Int8"
//     | KTUInt8 -> "UInt8"
//     | KTInt16 -> "Int16"
//     | KTUInt16 -> "UInt16"
//     | KTInt32 -> "Int32"
//     | KTUInt32 -> "UInt32"
//     | KTInt64 -> "Int64"
//     | KTUInt64 -> "UInt64"
//     | KTInt128 -> "Int128"
//     | KTUInt128 -> "UInt128"
//     | KTFloat -> "Float"
//     | KTChar -> "Char"
//     | KTString -> "String"
//     | KTUuid -> "Uuid"
//     | KTDateTime -> "DateTime"

//     | KTList inner -> $"List<{toString inner}>"
//     | KTDict inner -> $"Dict<{toString inner}>"
//     | KTTuple(first, second, theRest) ->
//       first :: second :: theRest
//       |> List.map toString
//       |> String.concat " * "
//       |> fun inner -> $"({inner})"

//     | KTCustomType(typeName, typeArgs) ->
//       let typeArgsPart =
//         match typeArgs with
//         | [] -> ""
//         | _ ->
//           typeArgs
//           |> List.map toString
//           |> String.concat ", "
//           |> fun inner -> $"<{inner}>"

//       $"{FQTypeName.toString typeName}{typeArgsPart}"

// // | KTFn(args, ret) ->
// //   NEList.toList args @ [ ret ] |> List.map toString |> String.concat " -> "

// //| KTDB inner -> $"DB<{toString inner}>"


let rec private mergeKnownTypes
  (left : KnownType)
  (right : KnownType)
  : Result<KnownType, unit> =
  let r = merge
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
  | KTFloat, KTFloat -> KTFloat |> Ok
  | KTChar, KTChar -> KTChar |> Ok
  | KTString, KTString -> KTString |> Ok
  | KTUuid, KTUuid -> KTUuid |> Ok
  | KTDateTime, KTDateTime -> KTDateTime |> Ok

  | KTList left, KTList right -> r left right |> Result.map KTList
  | KTDict left, KTDict right -> r left right |> Result.map KTDict
  | KTTuple(l1, l2, ls), KTTuple(r1, r2, rs) ->
    let firstMerged = r l1 r1
    let secondMerged = r l2 r2
    let restMerged = List.map2 r ls rs |> Result.collect

    match firstMerged, secondMerged, restMerged with
    | Ok first, Ok second, Ok rest -> Ok(KTTuple(first, second, rest))
    | _ -> Error()

  | KTCustomType(lName, lArgs), KTCustomType(rName, rArgs) ->
    if lName <> rName then
      Error()
    else if List.length lArgs <> List.length rArgs then
      Error()
    else
      List.map2 r lArgs rArgs
      |> Result.collect
      |> Result.map (fun args -> KTCustomType(lName, args))

  // | KTFn(lArgs, lRet), KTFn(rArgs, rRet) ->
  //   let argsMerged = NEList.map2 r lArgs rArgs |> Result.collectNE
  //   let retMerged = r lRet rRet

  //   match argsMerged, retMerged with
  //   | Ok args, Ok ret -> Ok(KTFn(args, ret))
  //   | _ -> Error()

  | _ -> Error()

and merge (left : ValueType) (right : ValueType) : Result<ValueType, unit> =
  match left, right with
  | ValueType.Unknown, v
  | v, ValueType.Unknown -> Ok v

  | ValueType.Known left, ValueType.Known right ->
    mergeKnownTypes left right |> Result.map ValueType.Known
