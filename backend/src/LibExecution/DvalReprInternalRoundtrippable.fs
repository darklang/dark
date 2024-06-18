/// Ways of converting Dvals to/from strings, to be used exclusively internally.
///
/// That is, they should not be used in libraries, in the BwdServer, in HttpClient,
/// etc.
module LibExecution.DvalReprInternalRoundtrippable

open Prelude

// Note: we intentionally don't `open` RT here, so that we don't accidentally
// confuse the types defined here and the types defined in RT.
module RT = RuntimeTypes


module FormatV0 =
  // In the past, we used bespoke serialization formats, that were terrifying to
  // change. Going forward, if we want to use a format that we want to save and
  // reload, but don't need to search for, let's just use the simplest possible
  // format, using standard serializers.

  // We create our own format here because:
  // 1. we don't want to serialize some things, such as lambdas. Our own type allows
  //    us to be careful
  // 2. This needs to be backwards compatible, but we don't want to constrain how we
  //    change RT.Dval.

  module FQTypeName =
    type Package = uuid

    type FQTypeName = Package of Package

    let toRT (t : FQTypeName) : RT.FQTypeName.FQTypeName =
      match t with
      | Package id -> RT.FQTypeName.Package id

    let fromRT (t : RT.FQTypeName.FQTypeName) : FQTypeName =
      match t with
      | RT.FQTypeName.Package id -> FQTypeName.Package id



  module rec ValueType =
    module KnownType =
      type KnownType =
        | KTUnit
        | KTBool
        | KTInt64
        | KTUInt64
        | KTInt8
        | KTUInt8
        | KTInt16
        | KTUInt16
        | KTInt32
        | KTUInt32
        | KTInt128
        | KTUInt128
        | KTFloat
        | KTChar
        | KTString
        | KTUuid
        | KTDateTime

        | KTList of ValueType
        | KTTuple of ValueType * ValueType * List<ValueType>
        | KTDict of ValueType

        | KTFn of NEList<ValueType> * ValueType

        | KTCustomType of FQTypeName.FQTypeName * typeArgs : List<ValueType>

        | KTDB of ValueType

      let rec toRT (kt : KnownType) : RT.KnownType =
        match kt with
        | KTUnit -> RT.KTUnit
        | KTBool -> RT.KTBool
        | KTInt64 -> RT.KTInt64
        | KTUInt64 -> RT.KTUInt64
        | KTInt8 -> RT.KTInt8
        | KTUInt8 -> RT.KTUInt8
        | KTInt16 -> RT.KTInt16
        | KTUInt16 -> RT.KTUInt16
        | KTInt32 -> RT.KTInt32
        | KTUInt32 -> RT.KTUInt32
        | KTInt128 -> RT.KTInt128
        | KTUInt128 -> RT.KTUInt128
        | KTFloat -> RT.KTFloat
        | KTChar -> RT.KTChar
        | KTString -> RT.KTString
        | KTUuid -> RT.KTUuid
        | KTDateTime -> RT.KTDateTime

        | KTList vt -> RT.KTList(ValueType.toRT vt)
        | KTTuple(vt1, vt2, vts) ->
          RT.KTTuple(
            ValueType.toRT vt1,
            ValueType.toRT vt2,
            List.map ValueType.toRT vts
          )
        | KTDict vt -> RT.KTDict(ValueType.toRT vt)

        | KTFn(argTypes, returnType) ->
          RT.KTFn(NEList.map ValueType.toRT argTypes, ValueType.toRT returnType)

        | KTCustomType(typeName, typeArgs) ->
          RT.KTCustomType(FQTypeName.toRT typeName, List.map ValueType.toRT typeArgs)

        | KTDB vt -> RT.KTDB(ValueType.toRT vt)

      let rec fromRT (kt : RT.KnownType) : KnownType =
        match kt with
        | RT.KTUnit -> KTUnit
        | RT.KTBool -> KTBool
        | RT.KTInt64 -> KTInt64
        | RT.KTUInt64 -> KTUInt64
        | RT.KTInt8 -> KTInt8
        | RT.KTUInt8 -> KTUInt8
        | RT.KTInt16 -> KTInt16
        | RT.KTUInt16 -> KTUInt16
        | RT.KTInt32 -> KTInt32
        | RT.KTUInt32 -> KTUInt32
        | RT.KTInt128 -> KTInt128
        | RT.KTUInt128 -> KTUInt128
        | RT.KTFloat -> KTFloat
        | RT.KTChar -> KTChar
        | RT.KTString -> KTString
        | RT.KTUuid -> KTUuid
        | RT.KTDateTime -> KTDateTime

        | RT.KTList vt -> KTList(ValueType.fromRT vt)
        | RT.KTTuple(vt1, vt2, vts) ->
          KTTuple(
            ValueType.fromRT vt1,
            ValueType.fromRT vt2,
            List.map ValueType.fromRT vts
          )
        | RT.KTDict vt -> KTDict(ValueType.fromRT vt)

        | RT.KTFn(argTypes, returnType) ->
          KTFn(NEList.map ValueType.fromRT argTypes, ValueType.fromRT returnType)

        | RT.KTCustomType(typeName, typeArgs) ->
          KTCustomType(
            FQTypeName.fromRT typeName,
            List.map ValueType.fromRT typeArgs
          )

        | RT.KTDB vt -> KTDB(ValueType.fromRT vt)

    [<RequireQualifiedAccess>]
    type ValueType =
      | Unknown
      | Known of KnownType.KnownType

    let toRT (vt : ValueType) : RT.ValueType =
      match vt with
      | ValueType.Unknown -> RT.ValueType.Unknown
      | ValueType.Known kt -> RT.ValueType.Known(ValueType.KnownType.toRT kt)

    let fromRT (vt : RT.ValueType) : ValueType =
      match vt with
      | RT.ValueType.Unknown -> ValueType.ValueType.Unknown
      | RT.ValueType.Known kt ->
        ValueType.ValueType.Known(ValueType.KnownType.fromRT kt)

  let valueTypeTODO = ValueType.ValueType.Unknown


  type DvalMap = Map<string, Dval>

  and Dval =
    | DInt64 of int64
    | DUInt64 of uint64
    | DInt8 of int8
    | DUInt8 of uint8
    | DInt16 of int16
    | DUInt16 of uint16
    | DInt32 of int32
    | DUInt32 of uint32
    | DInt128 of System.Int128
    | DUInt128 of System.UInt128
    | DFloat of double
    | DBool of bool
    | DUnit
    | DString of string
    | DChar of string
    | DList of ValueType.ValueType * List<Dval>
    | DTuple of Dval * Dval * List<Dval>
    | DLambda // See docs/dblock-serialization.md
    | DDict of ValueType.ValueType * DvalMap
    | DDB of string
    | DDateTime of NodaTime.LocalDateTime
    | DUuid of System.Guid
    | DRecord of
      runtimeTypeName : FQTypeName.FQTypeName *
      sourceTypeName : FQTypeName.FQTypeName *
      typeArgs : List<ValueType.ValueType> *
      fields : DvalMap
    | DEnum of
      runtimeTypeName : FQTypeName.FQTypeName *
      sourceTypeName : FQTypeName.FQTypeName *
      typeArgs : List<ValueType.ValueType> *
      caseName : string *
      fields : List<Dval>


  let rec toRT (dv : Dval) : RT.Dval =
    match dv with
    | DString s -> RT.DString s
    | DChar c -> RT.DChar c
    | DInt64 i -> RT.DInt64 i
    | DUInt64 i -> RT.DUInt64 i
    | DInt8 i -> RT.DInt8 i
    | DUInt8 i -> RT.DUInt8 i
    | DInt16 i -> RT.DInt16 i
    | DUInt16 i -> RT.DUInt16 i
    | DInt32 i -> RT.DInt32 i
    | DUInt32 i -> RT.DUInt32 i
    | DInt128 i -> RT.DInt128 i
    | DUInt128 i -> RT.DUInt128 i
    | DBool b -> RT.DBool b
    | DFloat f -> RT.DFloat f
    | DUnit -> RT.DUnit
    | DLambda ->
      RT.DFnVal(
        RT.Lambda
          { typeSymbolTable = Map []
            symtable = Map []
            parameters = NEList.singleton (RT.LPVariable(gid (), "var"))
            body = RT.Expr.EUnit 0UL }
      )
    | DDateTime d -> RT.DDateTime d
    | DDB name -> RT.DDB name
    | DUuid uuid -> RT.DUuid uuid
    | DList(typ, l) -> RT.DList(ValueType.toRT typ, List.map toRT l)
    | DTuple(first, second, theRest) ->
      RT.DTuple(toRT first, toRT second, List.map toRT theRest)
    | DDict(typ, entries) -> RT.DDict(ValueType.toRT typ, Map.map toRT entries)
    | DRecord(typeName, original, typeArgs, o) ->
      RT.DRecord(
        FQTypeName.toRT typeName,
        FQTypeName.toRT original,
        List.map ValueType.toRT typeArgs,
        Map.map toRT o
      )
    | DEnum(typeName, original, typeArgs, caseName, fields) ->
      RT.DEnum(
        FQTypeName.toRT typeName,
        FQTypeName.toRT original,
        List.map ValueType.toRT typeArgs,
        caseName,
        List.map toRT fields
      )



  let rec fromRT (dv : RT.Dval) : Dval =
    match dv with
    | RT.DString s -> DString s
    | RT.DChar c -> DChar c
    | RT.DInt64 i -> DInt64 i
    | RT.DUInt64 i -> DUInt64 i
    | RT.DInt8 i -> DInt8 i
    | RT.DUInt8 i -> DUInt8 i
    | RT.DInt16 i -> DInt16 i
    | RT.DUInt16 i -> DUInt16 i
    | RT.DInt32 i -> DInt32 i
    | RT.DUInt32 i -> DUInt32 i
    | RT.DInt128 i -> DInt128 i
    | RT.DUInt128 i -> DUInt128 i
    | RT.DBool b -> DBool b
    | RT.DFloat f -> DFloat f
    | RT.DUnit -> DUnit
    | RT.DFnVal _ -> DLambda
    | RT.DDateTime d -> DDateTime d
    | RT.DDB name -> DDB name
    | RT.DUuid uuid -> DUuid uuid
    | RT.DList(typ, l) -> DList(ValueType.fromRT typ, List.map fromRT l)
    | RT.DTuple(first, second, theRest) ->
      DTuple(fromRT first, fromRT second, List.map fromRT theRest)
    | RT.DDict(typ, entries) -> DDict(ValueType.fromRT typ, Map.map fromRT entries)
    | RT.DRecord(typeName, original, typeArgs, o) ->
      DRecord(
        FQTypeName.fromRT typeName,
        FQTypeName.fromRT original,
        List.map ValueType.fromRT typeArgs,
        Map.map fromRT o
      )
    | RT.DEnum(typeName, original, typeArgs, caseName, fields) ->
      DEnum(
        FQTypeName.fromRT typeName,
        FQTypeName.fromRT original,
        List.map ValueType.fromRT typeArgs,
        caseName,
        List.map fromRT fields
      )


let toJsonV0 (dv : RT.Dval) : string =
  dv |> FormatV0.fromRT |> Json.Vanilla.serialize

let parseJsonV0 (json : string) : RT.Dval =
  json |> Json.Vanilla.deserialize<FormatV0.Dval> |> FormatV0.toRT

let toHashV2 (dvals : list<RT.Dval>) : string =
  dvals
  |> List.map FormatV0.fromRT
  |> fun items -> FormatV0.DList(FormatV0.valueTypeTODO, items)
  |> Json.Vanilla.serialize
  |> UTF8.toBytes
  |> System.IO.Hashing.XxHash64.Hash // fastest in .NET, does not need to be secure
  |> Base64.urlEncodeToString



module Test =
  let rec isRoundtrippableDval (dval : RT.Dval) : bool =
    match dval with
    | RT.DUnit
    | RT.DBool _
    | RT.DInt8 _
    | RT.DUInt8 _
    | RT.DInt16 _
    | RT.DUInt16 _
    | RT.DInt32 _
    | RT.DUInt32 _
    | RT.DInt64 _
    | RT.DUInt64 _
    | RT.DInt128 _
    | RT.DUInt128 _
    | RT.DFloat _
    | RT.DChar _
    | RT.DString _
    | RT.DUuid _
    | RT.DDateTime _ -> true

    | RT.DEnum(_typeName, _, _typeArgsDEnumTODO, _caseName, fields) ->
      List.all isRoundtrippableDval fields

    | RT.DList(_, dvals) -> List.all isRoundtrippableDval dvals

    | RT.DDict(_, map) -> map |> Map.values |> List.all isRoundtrippableDval

    | RT.DRecord(_, _, _, map) -> map |> Map.values |> List.all isRoundtrippableDval

    | RT.DTuple(v1, v2, rest) -> List.all isRoundtrippableDval (v1 :: v2 :: rest)

    | RT.DDB _ -> true

    | RT.DFnVal _ -> false // not supported
