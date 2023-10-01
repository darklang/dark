/// Ways of converting Dvals to/from strings, to be used exclusively internally.
///
/// That is, they should not be used in libraries, in the BwdServer, in HttpClient,
/// etc.
module LibExecution.DvalReprInternalRoundtrippable

open Prelude

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
  module FQName =

    type BuiltIn<'name> = { modules : List<string>; name : 'name; version : int }
    type UserProgram<'name> = { modules : List<string>; name : 'name; version : int }
    type Package<'name> =
      { owner : string; modules : List<string>; name : 'name; version : int }

    type FQName<'name> =
      | BuiltIn of BuiltIn<'name>
      | UserProgram of UserProgram<'name>
      | Package of Package<'name>


  module TypeName =
    type Name = TypeName of string
    type TypeName = FQName.FQName<Name>

    let toRT (t : TypeName) : RT.TypeName.TypeName =
      match t with
      | FQName.BuiltIn { modules = modules; name = TypeName name; version = version } ->
        RT.FQName.BuiltIn
          { modules = modules; name = RT.TypeName.TypeName name; version = version }
      | FQName.UserProgram { modules = modules
                             name = TypeName name
                             version = version } ->
        RT.FQName.UserProgram
          { modules = modules; name = RT.TypeName.TypeName name; version = version }
      | FQName.Package { owner = owner
                         modules = modules
                         name = TypeName name
                         version = version } ->
        RT.FQName.Package
          { owner = owner
            modules = modules
            name = RT.TypeName.TypeName name
            version = version }

    let fromRT (t : RT.TypeName.TypeName) : TypeName =
      match t with
      | RT.FQName.BuiltIn { modules = modules
                            name = RT.TypeName.TypeName name
                            version = version } ->
        FQName.BuiltIn { modules = modules; name = TypeName name; version = version }
      | RT.FQName.UserProgram { modules = modules
                                name = RT.TypeName.TypeName name
                                version = version } ->
        FQName.UserProgram
          { modules = modules; name = TypeName name; version = version }
      | RT.FQName.Package { owner = owner
                            modules = modules
                            name = RT.TypeName.TypeName name
                            version = version } ->
        FQName.Package
          { owner = owner
            modules = modules
            name = TypeName name
            version = version }

  module FnName =
    type Name = FnName of string
    type FnName = FQName.FQName<Name>

    let toRT (t : FnName) : RT.FnName.FnName =
      match t with
      | FQName.BuiltIn { modules = modules; name = FnName name; version = version } ->
        RT.FQName.BuiltIn
          { modules = modules; name = RT.FnName.FnName name; version = version }
      | FQName.UserProgram { modules = modules
                             name = FnName name
                             version = version } ->
        RT.FQName.UserProgram
          { modules = modules; name = RT.FnName.FnName name; version = version }
      | FQName.Package { owner = owner
                         modules = modules
                         name = FnName name
                         version = version } ->
        RT.FQName.Package
          { owner = owner
            modules = modules
            name = RT.FnName.FnName name
            version = version }

    let fromRT (t : RT.FnName.FnName) : FnName =
      match t with
      | RT.FQName.BuiltIn { modules = modules
                            name = RT.FnName.FnName name
                            version = version } ->
        FQName.BuiltIn { modules = modules; name = FnName name; version = version }
      | RT.FQName.UserProgram { modules = modules
                                name = RT.FnName.FnName name
                                version = version } ->
        FQName.UserProgram
          { modules = modules; name = FnName name; version = version }
      | RT.FQName.Package { owner = owner
                            modules = modules
                            name = RT.FnName.FnName name
                            version = version } ->
        FQName.Package
          { owner = owner; modules = modules; name = FnName name; version = version }





  module rec ValueType =
    module KnownType =
      type KnownType =
        | KTUnit
        | KTBool
        | KTInt
        | KTFloat
        | KTChar
        | KTString
        | KTUuid
        | KTBytes
        | KTDateTime

        | KTList of ValueType
        | KTTuple of ValueType * ValueType * List<ValueType>
        | KTDict of ValueType

        | KTFn of NEList<ValueType> * ValueType

        | KTCustomType of TypeName.TypeName * typeArgs : List<ValueType>

        | KTDB of ValueType

      let rec toRT (kt : KnownType) : RT.KnownType =
        match kt with
        | KTUnit -> RT.KTUnit
        | KTBool -> RT.KTBool
        | KTInt -> RT.KTInt
        | KTFloat -> RT.KTFloat
        | KTChar -> RT.KTChar
        | KTString -> RT.KTString
        | KTUuid -> RT.KTUuid
        | KTBytes -> RT.KTBytes
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
          RT.KTCustomType(TypeName.toRT typeName, List.map ValueType.toRT typeArgs)

        | KTDB vt -> RT.KTDB(ValueType.toRT vt)

      let rec fromRT (kt : RT.KnownType) : KnownType =
        match kt with
        | RT.KTUnit -> KTUnit
        | RT.KTBool -> KTBool
        | RT.KTInt -> KTInt
        | RT.KTFloat -> KTFloat
        | RT.KTChar -> KTChar
        | RT.KTString -> KTString
        | RT.KTUuid -> KTUuid
        | RT.KTBytes -> KTBytes
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
          KTCustomType(TypeName.fromRT typeName, List.map ValueType.fromRT typeArgs)

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
  and Source = Option<tlid * id>

  and RuntimeError = RuntimeError of Source * Dval

  and Dval =
    | DInt of int64
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
    | DBytes of byte array
    | DRecord of
      runtimeTypeName : TypeName.TypeName *
      sourceTypeName : TypeName.TypeName *
      typeArgs : List<ValueType.ValueType> *
      fields : DvalMap
    | DEnum of
      runtimeTypeName : TypeName.TypeName *
      sourceTypeName : TypeName.TypeName *
      typeArgs : List<ValueType.ValueType> *
      caseName : string *
      fields : List<Dval>


  let rec toRT (dv : Dval) : RT.Dval =
    match dv with
    | DString s -> RT.DString s
    | DChar c -> RT.DChar c
    | DInt i -> RT.DInt i
    | DBool b -> RT.DBool b
    | DFloat f -> RT.DFloat f
    | DUnit -> RT.DUnit
    | DLambda ->
      RT.DFnVal(
        RT.Lambda
          { typeSymbolTable = Map []
            tlid = 0UL
            symtable = Map []
            parameters = NEList.singleton (gid (), "var")
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
        TypeName.toRT typeName,
        TypeName.toRT original,
        List.map ValueType.toRT typeArgs,
        Map.map toRT o
      )
    | DBytes bytes -> RT.DBytes bytes
    | DEnum(typeName, original, typeArgs, caseName, fields) ->
      RT.DEnum(
        TypeName.toRT typeName,
        TypeName.toRT original,
        List.map ValueType.toRT typeArgs,
        caseName,
        List.map toRT fields
      )



  let rec fromRT (dv : RT.Dval) : Dval =
    match dv with
    | RT.DString s -> DString s
    | RT.DChar c -> DChar c
    | RT.DInt i -> DInt i
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
        TypeName.fromRT typeName,
        TypeName.fromRT original,
        List.map ValueType.fromRT typeArgs,
        Map.map fromRT o
      )
    | RT.DBytes bytes -> DBytes bytes
    | RT.DEnum(typeName, original, typeArgs, caseName, fields) ->
      DEnum(
        TypeName.fromRT typeName,
        TypeName.fromRT original,
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
    | RT.DFnVal _ -> false // not supported
    | RT.DString _
    | RT.DInt _
    | RT.DFloat _
    | RT.DUnit _
    | RT.DBool _
    | RT.DChar _
    | RT.DBytes _
    | RT.DDateTime _ -> true
    | RT.DEnum(_typeName, _, _typeArgsDEnumTODO, _caseName, fields) ->
      List.all isRoundtrippableDval fields
    | RT.DList(_, dvals) -> List.all isRoundtrippableDval dvals
    | RT.DDict(_, map) -> map |> Map.values |> List.all isRoundtrippableDval
    | RT.DRecord(_, _, _, map) -> map |> Map.values |> List.all isRoundtrippableDval
    | RT.DUuid _ -> true
    | RT.DTuple(v1, v2, rest) -> List.all isRoundtrippableDval (v1 :: v2 :: rest)
    | RT.DDB _ -> true
