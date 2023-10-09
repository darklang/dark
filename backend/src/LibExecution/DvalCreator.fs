/// Dvals should be created carefully:
/// - to have the correct valueTypes, where appropriate
///  i.e. we should not have DList(Known KTInt, [ DString("hi") ])
///
/// - similarly, we should fail when trying to merge Dvals with conflicting valueTypes
///   i.e. `List.append [1] ["hi"]` should fail
///   because we can't merge `Known KTInt` and `Known KTString`
///
/// These functions are intended to help with both of these, in cases where
/// the functions in Dval.fs are insufficient (i.e. we don't know the Dark sub-types
/// of a Dval in some F# code).
///
/// TODO needs a rename to indicate _safety_
/// - SafeDvalCreator (SDC), TypecheckedDvalCreator (TDC), etc.
module LibExecution.DvalCreator

open Prelude

open RuntimeTypes
module VT = ValueType

// TODO: maybe these should include `KnownType`s instead of `ValueType`s
// really this should be ErrorType
type DvalCreationError =

  /// When building up a list, we found a value that didn't match the list's type
  ///
  /// `[1; "2"]` the types of `1` and `"2"` don't match
  | ListAppend of vt : ValueType * item : Dval * vtOfDv : ValueType


  /// Record type declares a field that is not present in the record
  ///
  /// type Foo = { a: Int, b: Int }
  /// let x: Foo = { a: 1 }
  | RecordMissingField of
    typeName : TypeName.TypeName *
    fieldName : string *
    typ : TypeReference

  /// Record declaration has duplicate field names
  ///
  /// type Foo = { a: Int, a: Int }
  /// let x: Foo = { a: 1; b = 2; a: 1; }
  | RecordDuplicateField of typeName : TypeName.TypeName * fieldName : string


  // TODO add `Enum` case
  // The `Option` and `Result` cases below will likely be removeable once complete
  //| Enum
  | Option of vt : ValueType * item : Dval * vtOfDv : ValueType

  | ResultOk of
    okVt : ValueType *
    errVt : ValueType *
    okDv : Dval *
    vtOfDv : ValueType

  | ResultError of
    okVt : ValueType *
    errVt : ValueType *
    errDv : Dval *
    vtOfDv : ValueType


module RTE =
  module RT2DT = RuntimeTypesToDarkTypes
  let typeName = RuntimeError.name [ "DvalCreation" ] "Error" 0

  let toDT (error : DvalCreationError) : Dval =
    let (caseName, fields) =
      match error with
      | ListAppend(vt, item, vtOfDv) ->
        "ListAppend",
        [ RT2DT.Dval.ValueType.toDT vt
          RT2DT.Dval.toDT item
          RT2DT.Dval.ValueType.toDT vtOfDv ]

      | RecordMissingField(typeName, fieldName, typ) ->
        "RecordMissingField",
        [ RT2DT.TypeName.toDT typeName
          DString fieldName
          RT2DT.TypeReference.toDT typ ]

      | RecordDuplicateField(typeName, fieldName) ->
        "RecordDuplicateField", [ RT2DT.TypeName.toDT typeName; DString fieldName ]

      | Option(vt, item, vtOfDv) ->
        "Option",
        [ RT2DT.Dval.ValueType.toDT vt
          RT2DT.Dval.toDT item
          RT2DT.Dval.ValueType.toDT vtOfDv ]

      | ResultOk(okVt, errVt, okDv, vtOfDv) ->
        "ResultOk",
        [ RT2DT.Dval.ValueType.toDT okVt
          RT2DT.Dval.ValueType.toDT errVt
          RT2DT.Dval.toDT okDv
          RT2DT.Dval.ValueType.toDT vtOfDv ]

      | ResultError(okVt, errVt, errDv, vtOfDv) ->
        "ResultError",
        [ RT2DT.Dval.ValueType.toDT okVt
          RT2DT.Dval.ValueType.toDT errVt
          RT2DT.Dval.toDT errDv
          RT2DT.Dval.ValueType.toDT vtOfDv ]

    DEnum(typeName, typeName, [], caseName, fields)


let list (initialType : ValueType) (list : List<Dval>) : Dval =
  let (typ, dvs) =
    List.fold
      (fun (typ, list) dv ->
        let dvalType = Dval.toValueType dv

        match VT.merge typ dvalType with
        | Ok newType -> newType, dv :: list
        | Error() ->
          ListAppend(typ, dv, dvalType)
          |> RTE.toDT
          |> RuntimeError.dvalCreationError
          |> raiseRTE None)
      (initialType, [])
      list

  DList(typ, List.rev dvs)


let dict (typ : ValueType) (entries : List<string * Dval>) : Dval =
  // TODO: dictPush, etc.
  DDict(typ, Map entries)

let dictFromMap (typ : ValueType) (entries : Map<string, Dval>) : Dval =
  // TODO: dictPush, etc.
  DDict(typ, entries)

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
//       | _, "", _ -> DError(None, $"Empty key: {k}")
//       // Error if the key appears twice
//       | DDict m, k, _v when Map.containsKey k m ->
//         DError(None, $"Duplicate key: {k}")
//       // Otherwise add it
//       | DDict m, k, v -> DDict(Map.add k v m)
//       // If we haven't got a DDict we're propagating an error so let it go
//       | m, _, _ -> m)
//     fields



let optionSome (innerType : ValueType) (dv : Dval) : Dval =
  let typeName = Dval.optionType

  let dvalType = Dval.toValueType dv

  match VT.merge innerType dvalType with
  | Ok typ ->
    DEnum(typeName, typeName, Dval.ignoreAndUseEmpty [ typ ], "Some", [ dv ])
  | Error() ->
    RuntimeError.oldError
      $"Could not merge types {ValueType.toString (VT.customType typeName [ innerType ])} and {ValueType.toString (VT.customType typeName [ dvalType ])}"
    |> raiseRTE None

let optionNone (innerType : ValueType) : Dval =
  DEnum(
    Dval.optionType,
    Dval.optionType,
    Dval.ignoreAndUseEmpty [ innerType ],
    "None",
    []
  )

let option (innerType : ValueType) (dv : Option<Dval>) : Dval =
  match dv with
  | Some dv -> optionSome innerType dv
  | None -> optionNone innerType



let resultOk (okType : ValueType) (errorType : ValueType) (dvOk : Dval) : Dval =
  let dvalType = Dval.toValueType dvOk
  match VT.merge okType dvalType with
  | Ok typ ->
    DEnum(
      Dval.resultType,
      Dval.resultType,
      Dval.ignoreAndUseEmpty [ typ; errorType ],
      "Ok",
      [ dvOk ]
    )
  | Error() ->
    RuntimeError.oldError
      $"Could not merge types {ValueType.toString (VT.customType Dval.resultType [ okType; errorType ])} and {ValueType.toString (VT.customType Dval.resultType [ dvalType; errorType ])}"
    |> raiseRTE None

let resultError
  (okType : ValueType)
  (errorType : ValueType)
  (dvError : Dval)
  : Dval =
  let dvalType = Dval.toValueType dvError
  match VT.merge errorType dvalType with
  | Ok typ ->
    DEnum(
      Dval.resultType,
      Dval.resultType,
      Dval.ignoreAndUseEmpty [ okType; typ ],
      "Error",
      [ dvError ]
    )
  | Error() ->
    RuntimeError.oldError
      $"Could not merge types {ValueType.toString (VT.customType Dval.resultType [ okType; errorType ])} and {ValueType.toString (VT.customType Dval.resultType [ okType; dvalType ])}"
    |> raiseRTE None

let result
  (okType : ValueType)
  (errorType : ValueType)
  (dv : Result<Dval, Dval>)
  : Dval =
  match dv with
  | Ok dv -> resultOk okType errorType dv
  | Error dv -> resultError okType errorType dv


/// Constructs a Dval.DRecord, ensuring that the fields match the expected shape
///
/// note: if provided, the typeArgs must match the # of typeArgs expected by the type
let record
  (typeName : TypeName.TypeName)
  (fields : List<string * Dval>)
  : Ply<Dval> =
  let resolvedTypeName = typeName // TODO: alias lookup, etc.

  let fields =
    List.fold
      (fun fields (k, v) ->
        match fields, k, v with
        // skip empty rows
        | _, "", _ -> raiseUntargetedRTE (RuntimeError.oldError "Empty key")

        // error if the key appears twice
        | fields, k, _v when Map.containsKey k fields ->
          raiseUntargetedRTE (RuntimeError.oldError $"Duplicate key: {k}")

        // otherwise add it
        | fields, k, v -> Map.add k v fields)
      Map.empty
      fields

  // TODO:
  // - pass in a (types: Types) arg
  // - use it to determine type args of resultant Dval
  // - ensure fields match the expected shape (defined by type args and field defs)
  //   - this process should also effect the type args of the resultant Dval
  DRecord(resolvedTypeName, typeName, VT.typeArgsTODO, fields) |> Ply


let enum
  (resolvedTypeName : TypeName.TypeName) // todo: remove
  (sourceTypeName : TypeName.TypeName)
  (caseName : string)
  (fields : List<Dval>)
  : Ply<Dval> =
  // TODO:
  // - use passed-in Types to determine type args of resultant Dval
  // - ensure fields match the expected shape (defined by type args and field defs)
  //   - this process should also effect the type args of the resultant Dval

  DEnum(resolvedTypeName, sourceTypeName, VT.typeArgsTODO, caseName, fields)
  |> Ply
