// -- Creating Dvals --
// Dvals should never be constructed that contain fakevals - the fakeval
// should always propagate (though, there are specific cases in the
// interpreter where they are discarded instead of propagated; still they are
// never put into other dvals). These static members check before creating the values.
// additionally, these functions fill in any ValueTypes relevant to a Dval,
// failing if they conflict with any expected valueType.

// Soon, some of thse are going to need a `Types` argument, which will be
// needed to look up types (i.e. for DEnum construction).
// When that happens, I suspect:
// - we'll need to pass in a `types: Types` argument to several of these fns
// - this whole module will become recursively defined

// These could just as well be renamed DvalCreators
// , but that name feels bad to me?
//
// maybe `CreateDval` or `DvalMaker` or something?
// i.e. `CreateDval.enum ...` seems reasonable-ish
module LibExecution.DvalUtils

open LibExecution.RuntimeTypes


// type DvalCreator = draft of an idea...
//   { list: List<Dval> -> Dval }


module private ValueType =
  // _just_ enough to make some tests less ugly - this should all be in Dark code soon
  // CLEANUP VTTODO ^
  let rec toString (vt : ValueType) : string =
    match vt with
    | ValueType.Unknown -> "_"
    | ValueType.Known kt ->
      match kt with
      | KTUnit -> "Unit"
      | KTBool -> "Bool"
      | KTInt -> "Int"
      | KTFloat -> "Float"
      | KTChar -> "Char"
      | KTString -> "String"
      | KTUuid -> "Uuid"
      | KTBytes -> "Bytes"
      | KTDateTime -> "DateTime"
      | KTPassword -> "Password"
      | KTList inner -> $"List<{toString inner}>"
      | KTTuple _ -> "Tuple (TODO)"
      | KTFn _ -> "Fn (TODO)"
      | KTDB _ -> "DB (TODO)"
      | KTCustomType _ -> "Custom Type (TODO)"
      | KTDict _ -> "Dict (TODO)"



let int (i : int) = DInt(int64 i)


let rec mergeKnownTypes
  (left : KnownType)
  (right : KnownType)
  : Result<KnownType, unit> =
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

  | KTList left, KTList right -> mergeValueTypes left right |> Result.map KTList
  | KTDict left, KTDict right -> mergeValueTypes left right |> Result.map KTDict
  | KTTuple(l1, l2, ls), KTTuple(r1, r2, rs) ->
    let firstMerged = mergeValueTypes l1 r1
    let secondMerged = mergeValueTypes l2 r2
    let restMerged =
      List.map2 (fun l r -> mergeValueTypes l r) ls rs |> Result.collect

    match firstMerged, secondMerged, restMerged with
    | Ok first, Ok second, Ok rest -> Ok(KTTuple(first, second, rest))
    | _ -> Error()

  | KTCustomType(lName, lArgs), KTCustomType(rName, rArgs) ->
    if lName <> rName then
      Error()
    else if List.length lArgs <> List.length rArgs then
      Error()
    else
      List.map2 mergeValueTypes lArgs rArgs
      |> Result.collect
      |> Result.map (fun args -> KTCustomType(lName, args))

  | KTFn(lArgs, lRet), KTFn(rArgs, rRet) ->
    let argsMerged = NEList.map2 mergeValueTypes lArgs rArgs |> Result.collectNE
    let retMerged = mergeValueTypes lRet rRet

    match argsMerged, retMerged with
    | Ok args, Ok ret -> Ok(KTFn(args, ret))
    | _ -> Error()


  | KTPassword, KTPassword -> KTPassword |> Ok

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
  | DUuid _ -> ValueType.Known KTUuid
  | DBytes _ -> ValueType.Known KTBytes
  | DDateTime _ -> ValueType.Known KTDateTime
  | DPassword _ -> ValueType.Known KTPassword

  | DList(t, _) -> ValueType.Known(KTList t)
  | DDict _t -> ValueType.Known(KTDict ValueType.Unknown) // VTTODO
  | DTuple(first, second, theRest) ->
    ValueType.Known(
      KTTuple(toValueType first, toValueType second, theRest |> List.map toValueType)
    )

  | DRecord(typeName, _, typeArgs, _fields) ->
    KTCustomType(typeName, typeArgs) |> ValueType.Known

  | DEnum(typeName, _, _caseName, _fields) ->
    let typeArgs =
      // TODO: somehow need to derive `typeArgs` from the `fields` (and `case`?)
      // we might need to look up the type...
      //fields |> List.map toValueType |> List.map Option.some
      []
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

  | DError _ -> Exception.raiseInternal "DError is being moved out of Dval" []



let private listPush
  (list : List<Dval>)
  (listType : ValueType)
  (dv : Dval)
  : Result<ValueType * List<Dval>, RuntimeError> =
  // what happens if we insert 5 into a list of strings? we should return an Error!

  // if we try to insert an `Error` (with the _error_ type known)
  // into a list of `Ok`s (with the _ok_ type known),
  // then we merge those types (result: `TCustomType` with both `OK` and `Error` types)

  // `KTCustomType("Result", [None, Some KTString])`
  // and
  // `KTCustomType("Result", [Some KTInt, None])`
  // merges to be come
  // `KTCustomType("Result", [Some KTInt, Some KTString])`

  let dvalType = toValueType dv
  let newType = mergeValueTypes listType dvalType

  match newType with
  | Ok newType -> Ok(newType, dv :: list)
  | Error() ->
    RuntimeError.oldError
      $"Could not merge types List<{ValueType.toString listType}> and List<{ValueType.toString dvalType}>"
    |> Error

let list (initialType : ValueType) (list : List<Dval>) : Dval =
  match List.find Dval.isFake list with
  | Some fake -> fake
  | None ->
    let result =
      List.fold
        (fun acc dv ->
          match acc with
          | Ok(typ, dvs) -> listPush dvs typ dv
          | Error e -> Error e)
        (Ok(initialType, []))
        (List.rev list)

    match result with
    | Ok(typ, dvs) -> DList(typ, dvs)
    | Error e -> DError(SourceNone, e)


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

let record
  (typeName : TypeName.TypeName)
  // TODO: (typeArgs: List<ValueType>)
  (fields : List<string * Dval>)
  : Dval =
  // Give a warning for duplicate keys
  List.fold
    (fun m (k, v) ->
      match m, k, v with
      // TYPESCLEANUP: remove hacks
      // If we're propagating a fakeval keep doing it. We handle it without this line but let's be certain
      | m, _k, _v when Dval.isFake m -> m
      // Errors should propagate (but only if we're not already propagating an error)
      | DRecord _, _, v when Dval.isFake v -> v
      // Skip empty rows
      | _, "", _ -> DError(SourceNone, RuntimeError.oldError $"Empty key {k}")
      // Error if the key appears twice
      | DRecord(_, _, _typeArgsTODO, m), k, _v when Map.containsKey k m ->
        DError(SourceNone, RuntimeError.oldError $"Duplicate key: {k}")
      // Otherwise add it
      | DRecord(tn, o, _typeArgsTODO, m), k, v ->
        DRecord(tn, o, valueTypesTODO, Map.add k v m)
      // If we haven't got a DDict we're propagating an error so let it go
      | m, _, _ -> m)
    (DRecord(typeName, typeName, valueTypesTODO, Map.empty))
    fields


let enum
  (typeName : TypeName.TypeName)
  // TODO: (typeArgs: List<ValueType>)
  (caseName : string)
  (fields : List<Dval>)
  : Dval =
  match List.find Dval.isFake fields with
  | Some v -> v
  | None -> DEnum(typeName, typeName, caseName, fields)


let optionType = TypeName.fqPackage "Darklang" [ "Stdlib"; "Option" ] "Option" 0

let optionSome (dv : Dval) : Dval =
  if Dval.isFake dv then dv else DEnum(optionType, optionType, "Some", [ dv ])

let optionNone : Dval = DEnum(optionType, optionType, "None", [])

// Wraps in an Option after checking that the value is not a fakeval
let option (dv : Option<Dval>) : Dval =
  match dv with
  | Some dv -> optionSome dv // checks isFake
  | None -> optionNone


let resultType = TypeName.fqPackage "Darklang" [ "Stdlib"; "Result" ] "Result" 0

let resultOk (dv : Dval) : Dval =
  if Dval.isFake dv then dv else DEnum(resultType, resultType, "Ok", [ dv ])
let resultError (dv : Dval) : Dval =
  if Dval.isFake dv then dv else DEnum(resultType, resultType, "Error", [ dv ])

// Wraps in a Result after checking that the value is not a fakeval
let result (dv : Result<Dval, Dval>) : Dval =
  match dv with
  | Ok dv -> resultOk dv
  | Error dv -> resultError dv
