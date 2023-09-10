module LibExecution.Types

open System.Threading.Tasks
open FSharp.Control.Tasks

open Prelude

open RuntimeTypes


// maybe move this file back to RT
// module KnownType =
//   /// This assumes that TVariables have already been substituted
//   /// i.e. they're not allowed, and will raise an exception
//   let rec fromFullySubstitutedTypeReference (t : TypeReference) : KnownType =
//     let r = fromFullySubstitutedTypeReference
//     let k t = t |> r |> Known

//     match t with
//     | TUnit -> KTUnit
//     | TBool -> KTBool
//     | TInt -> KTInt
//     | TFloat -> KTFloat
//     | TChar -> KTChar
//     | TString -> KTString
//     | TUuid -> KTUuid
//     | TBytes -> KTBytes
//     | TDateTime -> KTDateTime
//     | TPassword -> KTPassword

//     | TList t -> KTList(Known(r t))
//     | TTuple(t1, t2, rest) -> KTTuple(k t1, k t2, List.map k rest)
//     | TDict t -> KTDict(k t)

//     | TFn(args, ret) -> KTFn(List.map k args, k ret)

//     | TDB typ -> KTDB(k typ)

//     | TCustomType(t, typeArgs) -> KTCustomType(t, List.map k typeArgs)

//     | TVariable _ ->
//       Exception.raiseInternal
//         "TVariable should have been substituted before calling toValueType"
//         []

module ValueType =
  /// This assumes that TVariables have already been substituted
  let rec fromTypeReference (tst : TypeSymbolTable) (t : TypeReference) : ValueType =
    let r = fromTypeReference

    match t with
    | TUnit -> ValueType.Known KTUnit
    | TBool -> ValueType.Known KTBool
    | TInt -> ValueType.Known KTInt
    | TFloat -> ValueType.Known KTFloat
    | TChar -> ValueType.Known KTChar
    | TString -> ValueType.Known KTString
    | TUuid -> ValueType.Known KTUuid
    | TBytes -> ValueType.Known KTBytes
    | TDateTime -> ValueType.Known KTDateTime
    | TPassword -> ValueType.Known KTPassword

    | TList t -> KTList(r tst t) |> ValueType.Known
    | TTuple(t1, t2, rest) ->
      KTTuple(r tst t1, r tst t2, List.map (r tst) rest) |> ValueType.Known
    | TDict t -> KTDict(r tst t) |> ValueType.Known

    | TFn(args, ret) -> KTFn(NEList.map (r tst) args, r tst ret) |> ValueType.Known

    | TDB typ -> KTDB(r tst typ) |> ValueType.Known

    | TCustomType(Error err, _typeArgs) ->
      //KTCustomType(t, List.map r typeArgs) |> ValueType.Known
      raiseRTE err

    | TCustomType(Ok t, typeArgs) ->
      KTCustomType(t, List.map (r tst) typeArgs) |> ValueType.Known

    | TVariable name ->
      match Map.get name tst with
      | Some t -> t
      | None -> ValueType.Unknown
