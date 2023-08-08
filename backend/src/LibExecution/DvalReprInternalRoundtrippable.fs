/// Ways of converting Dvals to/from strings, to be used exclusively internally.
///
/// That is, they should not be used in libraries, in the BwdServer, in HttpClient,
/// etc.
module LibExecution.DvalReprInternalRoundtrippable

open Prelude
open VendoredTablecloth

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

    /// A name that is built into the runtime
    type BuiltIn<'name> = { modules : List<string>; name : 'name; version : int }

    /// Part of the user's program (eg canvas or cli)
    type UserProgram<'name> = { modules : List<string>; name : 'name; version : int }

    /// The name of a thing in the package manager
    // TODO: We plan to use UUIDs for this, but this is a placeholder
    type Package<'name> =
      { owner : string; modules : NEList<string>; name : 'name; version : int }

    type T<'name> =
      | BuiltIn of BuiltIn<'name>
      | UserProgram of UserProgram<'name>
      | Package of Package<'name>
      | Unknown of List<string>


  module TypeName =
    type Name = TypeName of string
    type T = FQName.T<Name>

    let toRT (t : T) : RT.TypeName.T =
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
      | FQName.Unknown names -> RT.FQName.Unknown names

    let fromRT (t : RT.TypeName.T) : T =
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
      | RT.FQName.Unknown names -> FQName.Unknown names

  module FnName =
    type Name = FnName of string
    type T = FQName.T<Name>

    let toRT (t : T) : RT.FnName.T =
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
      | FQName.Unknown names -> RT.FQName.Unknown names

    let fromRT (t : RT.FnName.T) : T =
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
      | RT.FQName.Unknown names -> FQName.Unknown names


  // the actual type of a dval
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
    | KTPassword

    | KTList of ValueType
    | KTTuple of ValueType * ValueType * List<ValueType>
    | KTDict of ValueType

    | KTFn of List<ValueType> * ValueType

    | KTDB of ValueType

    | KTCustomType of TypeName.T * typeArgs : List<ValueType>


  and ValueType =
    | Unknown
    | Known of KnownType

  module ValueType =
    let rec knownTypeToRT (t : KnownType) : RT.KnownType =
      let k = knownTypeToRT
      let v = toRT

      match t with
      | KTUnit -> RT.KTUnit
      | KTBool -> RT.KTBool
      | KTInt -> RT.KTInt
      | KTFloat -> RT.KTFloat
      | KTChar -> RT.KTChar
      | KTString -> RT.KTString
      | KTUuid -> RT.KTUuid
      | KTBytes -> RT.KTBytes
      | KTDateTime -> RT.KTDateTime
      | KTPassword -> RT.KTPassword
      | KTList typ -> RT.KTList(v typ)
      | KTTuple(first, second, theRest) ->
        RT.KTTuple(v first, v second, List.map v theRest)
      | KTFn(args, ret) -> RT.KTFn(List.map v args, v ret)
      | KTDB typ -> RT.KTDB(v typ)
      | KTCustomType(typeName, typeArgs) ->
        RT.KTCustomType(TypeName.toRT typeName, List.map v typeArgs)
      | KTDict typ -> RT.KTDict(v typ)

    and knownTypeFromRT (t : RT.KnownType) : KnownType =
      let k = knownTypeFromRT
      let v = fromRT

      match t with
      | RT.KTUnit -> KTUnit
      | RT.KTBool -> KTBool
      | RT.KTInt -> KTInt
      | RT.KTFloat -> KTFloat
      | RT.KTChar -> KTChar
      | RT.KTString -> KTString
      | RT.KTUuid -> KTUuid
      | RT.KTBytes -> KTBytes
      | RT.KTDateTime -> KTDateTime
      | RT.KTPassword -> KTPassword
      | RT.KTList typ -> KTList(v typ)
      | RT.KTTuple(first, second, theRest) ->
        KTTuple(v first, v second, List.map v theRest)
      | RT.KTFn(args, ret) -> KTFn(List.map v args, v ret)
      | RT.KTDB typ -> KTDB(v typ)
      | RT.KTCustomType(typeName, typeArgs) ->
        KTCustomType(TypeName.fromRT typeName, List.map v typeArgs)
      | RT.KTDict typ -> KTDict(v typ)

    and toRT (t : ValueType) : RT.ValueType =
      match t with
      | Unknown -> RT.Unknown
      | Known t -> RT.Known(knownTypeToRT t)

    and fromRT (t : RT.ValueType) : ValueType =
      match t with
      | RT.Unknown -> Unknown
      | RT.Known t -> Known(knownTypeFromRT t)

  type DvalMap = Map<string, Dval>
  and DvalSource =
    | SourceNone
    | SourceID of tlid * id

  and Dval =
    | DInt of int64
    | DFloat of double
    | DBool of bool
    | DUnit
    | DString of string
    | DChar of string
    | DList of ValueType * List<Dval>
    | DTuple of Dval * Dval * List<Dval>
    | DLambda // See docs/dblock-serialization.md
    | DDict of DvalMap
    | DError of DvalSource * string
    | DIncomplete of DvalSource
    | DDB of string
    | DDateTime of NodaTime.LocalDateTime
    | DPassword of byte array // We are allowed serialize this here, so don't use the Password type which doesn't deserialize
    | DUuid of System.Guid
    | DBytes of byte array
    | DRecord of runtimeTypeName : TypeName.T * sourceTypeName : TypeName.T * DvalMap
    | DEnum of
      runtimeTypeName : TypeName.T *
      sourceTypeName : TypeName.T *
      caseName : string *
      List<Dval>

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
            symtable = Map []
            parameters = []
            body = RT.Expr.EUnit 0UL }
      )
    | DIncomplete SourceNone -> RT.DIncomplete RT.SourceNone
    | DIncomplete(SourceID(tlid, id)) -> RT.DIncomplete(RT.SourceID(tlid, id))
    | DError(SourceNone, msg) -> RT.DError(RT.SourceNone, msg)
    | DError(SourceID(tlid, id), msg) -> RT.DError(RT.SourceID(tlid, id), msg)
    | DDateTime d -> RT.DDateTime d
    | DDB name -> RT.DDB name
    | DUuid uuid -> RT.DUuid uuid
    | DPassword pw -> RT.DPassword(Password pw)
    | DList(typ, l) -> RT.DList(ValueType.toRT typ, List.map toRT l)
    | DTuple(first, second, theRest) ->
      RT.DTuple(toRT first, toRT second, List.map toRT theRest)
    | DDict o -> RT.DDict(Map.map toRT o)
    | DRecord(typeName, original, o) ->
      RT.DRecord(TypeName.toRT typeName, TypeName.toRT original, Map.map toRT o)
    | DBytes bytes -> RT.DBytes bytes
    | DEnum(typeName, original, caseName, fields) ->
      RT.DEnum(
        TypeName.toRT typeName,
        TypeName.toRT original,
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
    | RT.DIncomplete RT.SourceNone -> DIncomplete SourceNone
    | RT.DIncomplete(RT.SourceID(tlid, id)) -> DIncomplete(SourceID(tlid, id))
    | RT.DError(RT.SourceNone, msg) -> DError(SourceNone, msg)
    | RT.DError(RT.SourceID(tlid, id), msg) -> DError(SourceID(tlid, id), msg)
    | RT.DDateTime d -> DDateTime d
    | RT.DDB name -> DDB name
    | RT.DUuid uuid -> DUuid uuid
    | RT.DPassword(Password pw) -> DPassword pw
    | RT.DList(typ, l) -> DList(ValueType.fromRT typ, List.map fromRT l)
    | RT.DTuple(first, second, theRest) ->
      DTuple(fromRT first, fromRT second, List.map fromRT theRest)
    | RT.DDict o -> DDict(Map.map fromRT o)
    | RT.DRecord(typeName, original, o) ->
      DRecord(TypeName.fromRT typeName, TypeName.fromRT original, Map.map fromRT o)
    | RT.DBytes bytes -> DBytes bytes
    | RT.DEnum(typeName, original, caseName, fields) ->
      DEnum(
        TypeName.fromRT typeName,
        TypeName.fromRT original,
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
  |> fun dv -> FormatV0.DList(FormatV0.Unknown, dv)
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
    | RT.DDateTime _
    | RT.DPassword _ -> true
    | RT.DEnum(_typeName, _, _caseName, fields) ->
      List.all isRoundtrippableDval fields
    | RT.DList(_, dvals) -> List.all isRoundtrippableDval dvals
    | RT.DDict map -> map |> Map.values |> List.all isRoundtrippableDval
    | RT.DRecord(_, _, map) -> map |> Map.values |> List.all isRoundtrippableDval
    | RT.DUuid _ -> true
    | RT.DTuple(v1, v2, rest) -> List.all isRoundtrippableDval (v1 :: v2 :: rest)
    | RT.DDB _
    | RT.DError _

    | RT.DIncomplete _ -> true
