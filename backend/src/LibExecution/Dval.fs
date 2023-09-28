/// Dvals should be created carefully:
/// - to have the correct valueTypes, where appropriate
///  i.e. we should not have DList(Known KTInt, [ DString("hi") ])
///
/// - similarly, we should fail when trying to merge Dvals with conflicting valueTypes
///   i.e. `List.append [1] ["hi"]` should fail
///   because we can't merge `Known KTInt` and `Known KTString`
///
/// These functions are intended to help with both of these.
/// Direct construction of Dvals like `DList(Known KTInt, [ DString("hi") ])` would
/// ideally be impossible in all files (I think?) but this one (and tests), but
/// that's not possible right now, technically.
///
/// -----
///
/// Notes for WIP:
/// Soon, some of these are going to need a `Types` argument, which will be
/// needed to look up types (i.e. for DEnum construction, to ensure the fields
/// provided match the expected type, and/or help to fill in any Unknowns in the
/// type args
///
/// When that happens, I suspect:
/// - we'll need to pass in a `types: Types` argument to several of these fns
///   (Dval.enum, Dval.record, maybe others)
module LibExecution.Dval

open FSharp.Control.Tasks
open System.Threading.Tasks

open Prelude

open LibExecution.RuntimeTypes
module VT = ValueType


let int (i : int) = DInt(int64 i)


let rec mergeKnownTypes
  (left : KnownType)
  (right : KnownType)
  : Result<KnownType, unit> =
  let r = mergeValueTypes
  match left, right with
  | KTUnit, KTUnit -> KTUnit |> Ok
  | KTBool, KTBool -> KTBool |> Ok
  | KTInt, KTInt -> KTInt |> Ok
  | KTFloat, KTFloat -> KTFloat |> Ok
  | KTChar, KTChar -> KTChar |> Ok
  | KTString, KTString -> KTString |> Ok
  | KTUuid, KTUuid -> KTUuid |> Ok
  | KTBytes, KTBytes -> KTBytes |> Ok
  | KTDateTime, KTDateTime -> KTDateTime |> Ok

  | KTList left, KTList right -> r left right |> Result.map KTList
  | KTDict left, KTDict right -> r left right |> Result.map KTDict
  | KTTuple(l1, l2, ls), KTTuple(r1, r2, rs) ->
    let firstMerged = r l1 r1
    let secondMerged = r l2 r2
    let restMerged =
      List.map2 (fun left right -> r left right) ls rs |> Result.collect

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

  | KTFn(lArgs, lRet), KTFn(rArgs, rRet) ->
    let argsMerged = NEList.map2 r lArgs rArgs |> Result.collectNE
    let retMerged = r lRet rRet

    match argsMerged, retMerged with
    | Ok args, Ok ret -> Ok(KTFn(args, ret))
    | _ -> Error()

  | _ -> Error()

and mergeValueTypes
  (left : ValueType)
  (right : ValueType)
  : Result<ValueType, unit> =
  match left, right with
  | ValueType.Unknown, v
  | v, ValueType.Unknown -> Ok v

  | ValueType.Known left, ValueType.Known right ->
    mergeKnownTypes left right |> Result.map ValueType.Known


let rec toValueType (dv : Dval) : ValueType =
  match dv with
  | DUnit -> ValueType.Known KTUnit

  | DBool _ -> ValueType.Known KTBool
  | DInt _ -> ValueType.Known KTInt
  | DFloat _ -> ValueType.Known KTFloat
  | DChar _ -> ValueType.Known KTChar
  | DString _ -> ValueType.Known KTString
  | DDateTime _ -> ValueType.Known KTDateTime
  | DUuid _ -> ValueType.Known KTUuid
  | DBytes _ -> ValueType.Known KTBytes

  | DList(t, _) -> ValueType.Known(KTList t)
  | DDict(t, _) -> ValueType.Known(KTDict t)
  | DTuple(first, second, theRest) ->
    ValueType.Known(
      KTTuple(toValueType first, toValueType second, theRest |> List.map toValueType)
    )

  | DRecord(typeName, _, typeArgs, _fields) ->
    KTCustomType(typeName, typeArgs) |> ValueType.Known

  | DEnum(typeName, _, typeArgs, _caseName, _fields) ->
    KTCustomType(typeName, typeArgs) |> ValueType.Known

  | DFnVal fnImpl ->
    match fnImpl with
    | Lambda lambda ->
      KTFn(
        NEList.map (fun _ -> ValueType.Unknown) lambda.parameters,
        ValueType.Unknown
      )
      |> ValueType.Known

    // VTTODO look up type, etc
    | NamedFn _named -> ValueType.Unknown

  // CLEANUP follow up when DDB has a typeReference
  | DDB _ -> ValueType.Unknown



let mergeFailureRte (sourceId : Source) (vt1 : ValueType) (vt2 : ValueType) : 'a =
  RuntimeError.oldError
    $"Could not merge types {ValueType.toString vt1} and {ValueType.toString vt2}"
  |> fun e -> raiseRTE sourceId e



let private listPush
  (list : List<Dval>)
  (listType : ValueType)
  (dv : Dval)
  : ValueType * List<Dval> =
  let dvalType = toValueType dv
  let newType = mergeValueTypes listType dvalType

  match newType with
  | Ok newType -> newType, dv :: list
  | Error() ->
    mergeFailureRte
      None
      (ValueType.Known(KTList listType))
      (ValueType.Known(KTList dvalType))

let list (initialType : ValueType) (list : List<Dval>) : Dval =
  let (typ, dvs) =
    List.fold
      (fun (typ, dvs) dv -> listPush dvs typ dv)
      (initialType, [])
      (List.rev list)

  DList(typ, dvs)



// CLEANUP - this fn was unused so I commented it out
// remove? or will it be handy?
// let dict (fields : List<string * Dval>) : Dval =
//   // Give a warning for duplicate keys
//   List.fold
//     (DDict(Map.empty))
//     (fun m (k, v) ->
//       match m, k, v with
//       // TYPESCLEANUP: remove hacks
//       // If we're propagating a fakeval keep doing it. We handle it without this line but let's be certain
//       | m, _k, _v when isFake m -> m
//       // Errors should propagate (but only if we're not already propagating an error)
//       | DDict _, _, v when isFake v -> v
//       // Skip empty rows
//       | _, "", _ -> DError(SourceNone, $"Empty key: {k}")
//       // Error if the key appears twice
//       | DDict m, k, _v when Map.containsKey k m ->
//         DError(SourceNone, $"Duplicate key: {k}")
//       // Otherwise add it
//       | DDict m, k, v -> DDict(Map.add k v m)
//       // If we haven't got a DDict we're propagating an error so let it go
//       | m, _, _ -> m)
//     fields

// CLEANUP it'd probably be better to consolidate the two `dict` fns
// I can't decide which, though
let dict (valueType : ValueType) (entries : List<string * Dval>) : Dval =
  // TODO: use valueType in the same way that we use it for Lists
  // (some tests will likely break..)
  DDict(valueType, Map.ofList entries)

let dictFromMap (valueType : ValueType) (entries : Map<string, Dval>) : Dval =
  // TODO: use valueType in the same way that we use it for Lists
  // (some tests will likely break..)
  DDict(valueType, entries)



/// VTTODO
/// the interpreter "throws away" any valueTypes currently,
/// so while these .option and .result functions are great in that they
/// return the correct typeArgs, they conflict with what the interpreter will do
///
/// So, to make some tests happy, let's ignore these for now.
///
/// (might need better explanation^)
let ignoreAndUseEmpty (_ignoredForNow : List<ValueType>) = []





let optionType = TypeName.fqPackage "Darklang" [ "Stdlib"; "Option" ] "Option" 0

let optionSome (innerType : ValueType) (dv : Dval) : Dval =
  let dvalType = toValueType dv
  match mergeValueTypes innerType dvalType with
  | Ok typ ->
    DEnum(optionType, optionType, ignoreAndUseEmpty [ typ ], "Some", [ dv ])
  | Error() ->
    mergeFailureRte
      None
      (ValueType.Known(KTCustomType(optionType, [ innerType ])))
      (ValueType.Known(KTCustomType(optionType, [ dvalType ])))

let optionNone (innerType : ValueType) : Dval =
  DEnum(optionType, optionType, ignoreAndUseEmpty [ innerType ], "None", [])

let option (innerType : ValueType) (dv : Option<Dval>) : Dval =
  match dv with
  | Some dv -> optionSome innerType dv
  | None -> optionNone innerType



let resultType = TypeName.fqPackage "Darklang" [ "Stdlib"; "Result" ] "Result" 0

let resultOk (okType : ValueType) (errorType : ValueType) (dvOk : Dval) : Dval =
  let dvalType = toValueType dvOk
  match mergeValueTypes okType dvalType with
  | Ok typ ->
    DEnum(
      resultType,
      resultType,
      ignoreAndUseEmpty [ typ; errorType ],
      "Ok",
      [ dvOk ]
    )
  | Error() ->
    mergeFailureRte
      None
      (ValueType.Known(KTCustomType(resultType, [ okType; errorType ])))
      (ValueType.Known(KTCustomType(resultType, [ dvalType; errorType ])))

let resultError
  (okType : ValueType)
  (errorType : ValueType)
  (dvError : Dval)
  : Dval =
  let dvalType = toValueType dvError
  match mergeValueTypes errorType dvalType with
  | Ok typ ->
    DEnum(
      resultType,
      resultType,
      ignoreAndUseEmpty [ okType; typ ],
      "Error",
      [ dvError ]
    )
  | Error() ->
    mergeFailureRte
      None
      (ValueType.Known(KTCustomType(resultType, [ okType; errorType ])))
      (ValueType.Known(KTCustomType(resultType, [ okType; dvalType ])))

let result
  (okType : ValueType)
  (errorType : ValueType)
  (dv : Result<Dval, Dval>)
  : Dval =
  match dv with
  | Ok dv -> resultOk okType errorType dv
  | Error dv -> resultError okType errorType dv



// CLEANUP consider extracting a partial active pattern to match against common
// Dark-defined types
// e.g.
//
// let (|DOptionNone|_|) result =
//   match result with
//   | DEnum(FQName.Package { owner = "Darklang"
//                            modules = [ "Stdlib"; "Option" ]
//                            name = TypeName.TypeName "Option"
//                            version = 0 },
//           _,
//           _typeArgsDEnumTODO,
//           "None",
//           []) -> Some()
//   | _ -> None

// let (|DOptionSome|_|) result =
//   match result with
//   | DEnum(FQName.Package { owner = "Darklang"
//                            modules = [ "Stdlib"; "Option" ]
//                            name = TypeName.TypeName "Option"
//                            version = 0 },
//           _,
//           _typeArgsDEnumTODO,
//           "Some",
//           [ o ]) -> Some o

//   | _ -> None
