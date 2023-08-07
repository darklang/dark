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
  type ValueType =
    | VTUnit
    | VTBool
    | VTInt
    | VTFloat
    | VTChar
    | VTString
    | VTUuid
    | VTBytes
    | VTDateTime
    | VTPassword
    | VTList of ConcreteType

    // let tpl = (None, 1, "true") // CTTuple(CTCustomType(...), CTInt, [CTString])
    // since every element of the tuple is a dval, we get a concrete type
    // reference for all of them
    | VTTuple of ValueType * ValueType * List<ValueType>

    // let y1 = (fun x -> x) //
    // let y2 = (fun (x: Int) -> x) // : CTFn([Some CTInt], None)
    // let y3 = (fun (x: Int) -> 5)
    // let z1 = (fun (x: String) -> 5)
    // let z2 = (fun (x: Int) -> "str")
    // z1 not compatible w/ any ys
    // z2 may be compatible with some ys for now, but not later? hm
    // [y; z] // : List<Fn<'a, 'a>>
    | VTFn of List<ConcreteType> * ConcreteType

    | VTDB of ValueType

    /// let n = None          // type args: [None]
    /// let s = Some(5)       // type args: [Some CTInt]
    /// let o = Ok (5)        // type args: [Some CTInt, None]
    /// let e = Error ("str") // type args: [None, Some CTString]
    | VTCustomType of TypeName.T * typeArgs : List<ConcreteType>

    // let myDict = {} // CTDIct(None)
    | VTDict of ConcreteType

  and ConcreteType = Option<ValueType>


  module ConcreteType =
    let rec valueTypeToRT (t: ValueType): RT.ValueType =
      let v = valueTypeToRT
      let c = toRT

      match t with
      | VTUnit -> RT.VTUnit
      | VTBool -> RT.VTBool
      | VTInt -> RT.VTInt
      | VTFloat -> RT.VTFloat
      | VTChar -> RT.VTChar
      | VTString -> RT.VTString
      | VTUuid -> RT.VTUuid
      | VTBytes -> RT.VTBytes
      | VTDateTime -> RT.VTDateTime
      | VTPassword -> RT.VTPassword
      | VTList typ -> RT.VTList(c typ)
      | VTTuple(first, second, theRest) ->
        RT.VTTuple(v first, v second, List.map v theRest)
      | VTFn(args, ret) -> RT.VTFn(List.map c args, c ret)
      | VTDB typ -> RT.VTDB(v typ)
      | VTCustomType(typeName, typeArgs) ->
        RT.VTCustomType(TypeName.toRT typeName, List.map c typeArgs)
      | VTDict typ -> RT.VTDict(c typ)

    and valueTypeFromRT (t: RT.ValueType): ValueType =
      let v = valueTypeFromRT
      let c = fromRT

      match t with
      | RT.VTUnit -> VTUnit
      | RT.VTBool -> VTBool
      | RT.VTInt -> VTInt
      | RT.VTFloat -> VTFloat
      | RT.VTChar -> VTChar
      | RT.VTString -> VTString
      | RT.VTUuid -> VTUuid
      | RT.VTBytes -> VTBytes
      | RT.VTDateTime -> VTDateTime
      | RT.VTPassword -> VTPassword
      | RT.VTList typ -> VTList(c typ)
      | RT.VTTuple(first, second, theRest) ->
        VTTuple(v first, v second, List.map v theRest)
      | RT.VTFn(args, ret) -> VTFn(List.map c args, c ret)
      | RT.VTDB typ -> VTDB(v typ)
      | RT.VTCustomType(typeName, typeArgs) ->
        VTCustomType(TypeName.fromRT typeName, List.map c typeArgs)
      | RT.VTDict typ -> VTDict(c typ)

    and toRT (t : ConcreteType) : RT.ConcreteType =
      Option.map valueTypeToRT t

    and fromRT (t : RT.ConcreteType) : ConcreteType =
      Option.map valueTypeFromRT t


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
    | DList of ConcreteType * List<Dval>
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
          { typeArgTable = Map []
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
    | DList(typ, l) -> RT.DList(ConcreteType.toRT typ, List.map toRT l)
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
    | RT.DList(typ, l) -> DList(ConcreteType.fromRT typ, List.map fromRT l)
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
  |> fun dv -> FormatV0.DList(None, dv)
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
    | RT.DList (_, dvals) -> List.all isRoundtrippableDval dvals
    | RT.DDict map -> map |> Map.values |> List.all isRoundtrippableDval
    | RT.DRecord(_, _, map) -> map |> Map.values |> List.all isRoundtrippableDval
    | RT.DUuid _ -> true
    | RT.DTuple(v1, v2, rest) -> List.all isRoundtrippableDval (v1 :: v2 :: rest)
    | RT.DDB _
    | RT.DError _

    | RT.DIncomplete _ -> true
