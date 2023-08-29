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




  type DvalMap = Map<string, Dval>
  and DvalSource =
    | SourceNone
    | SourceID of tlid * id

  and RuntimeError = RuntimeError of DvalSource * Dval

  and Dval =
    | DInt of int64
    | DFloat of double
    | DBool of bool
    | DUnit
    | DString of string
    | DChar of string
    | DList of List<Dval>
    | DTuple of Dval * Dval * List<Dval>
    | DLambda // See docs/dblock-serialization.md
    | DDict of DvalMap
    | DDB of string
    | DDateTime of NodaTime.LocalDateTime
    | DPassword of byte array // We are allowed serialize this here, so don't use the Password type which doesn't deserialize
    | DUuid of System.Guid
    | DBytes of byte array
    | DRecord of
      runtimeTypeName : TypeName.TypeName *
      sourceTypeName : TypeName.TypeName *
      DvalMap
    | DEnum of
      runtimeTypeName : TypeName.TypeName *
      sourceTypeName : TypeName.TypeName *
      caseName : string *
      List<Dval>

    | DError of RuntimeError // CLEANUP remove

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
          { typeArgTable = Map []
            symtable = Map []
            parameters = NEList.singleton (gid (), "var")
            body = RT.Expr.EUnit 0UL }
      )
    | DDateTime d -> RT.DDateTime d
    | DDB name -> RT.DDB name
    | DUuid uuid -> RT.DUuid uuid
    | DPassword pw -> RT.DPassword(Password pw)
    | DList l -> RT.DList(List.map toRT l)
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

    | DError(RuntimeError(SourceID(tlid, id), rte)) ->
      RT.DError(RT.SourceID(tlid, id), RT.RuntimeError.fromDT (toRT rte))

    | DError(RuntimeError(SourceNone, rte)) ->
      RT.DError(RT.SourceNone, RT.RuntimeError.fromDT (toRT rte))



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
    | RT.DPassword(Password pw) -> DPassword pw
    | RT.DList l -> DList(List.map fromRT l)
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

    | RT.DError(RT.SourceID(tlid, id), rte) ->
      DError(RuntimeError(SourceID(tlid, id), rte |> RT.RuntimeError.toDT |> fromRT))

    | RT.DError(RT.SourceNone, rte) ->
      DError(RuntimeError(SourceNone, rte |> RT.RuntimeError.toDT |> fromRT))


let toJsonV0 (dv : RT.Dval) : string =
  dv |> FormatV0.fromRT |> Json.Vanilla.serialize

let parseJsonV0 (json : string) : RT.Dval =
  json |> Json.Vanilla.deserialize<FormatV0.Dval> |> FormatV0.toRT

let toHashV2 (dvals : list<RT.Dval>) : string =
  dvals
  |> List.map FormatV0.fromRT
  |> FormatV0.DList
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
    | RT.DList dvals -> List.all isRoundtrippableDval dvals
    | RT.DDict map -> map |> Map.values |> List.all isRoundtrippableDval
    | RT.DRecord(_, _, map) -> map |> Map.values |> List.all isRoundtrippableDval
    | RT.DUuid _ -> true
    | RT.DTuple(v1, v2, rest) -> List.all isRoundtrippableDval (v1 :: v2 :: rest)
    | RT.DDB _
    | RT.DError _ -> true
