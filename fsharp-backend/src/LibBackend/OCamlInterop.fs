module LibBackend.OCamlInterop

// Interoperation functions with OCaml.

// Programs are stored using an OCaml-only serialization format, so we have to
// call OCaml code to fetch it and save it.  We send binary code which we get
// from the DB, convert it to OCaml types, then json convert it to get it back
// into F#. At that point we convert it to these types, and potentially convert
// it to the runtime types to run it.

// We also use these types to convert to the types the API uses, which are
// typically direct deserializations of these types.

open System.Runtime.InteropServices
open FSharpPlus

open Prelude
open Tablecloth

module PT = ProgramTypes
module RT = LibExecution.RuntimeTypes

module Binary =
  module Internal =
    // These allow us to call C functions from serialization_stubs.c, which in
    // turn call into the OCaml runtime.

    // FSTODO if we have segfaults, we might need to use this:
    // https://docs.microsoft.com/en-us/dotnet/standard/native-interop/best-practices#keeping-managed-objects-alive

    // ----------------
    // toplevels
    // ----------------
    [<DllImport("./libserialization.so",
                CallingConvention = CallingConvention.Cdecl,
                EntryPoint = "handler_bin2json")>]
    extern string handlerBin2Json(byte[] bytes, int length)

    [<DllImport("./libserialization.so",
                CallingConvention = CallingConvention.Cdecl,
                EntryPoint = "handler_json2bin")>]
    extern int handlerJson2Bin(string str, System.IntPtr& byteArray)

    [<DllImport("./libserialization.so",
                CallingConvention = CallingConvention.Cdecl,
                EntryPoint = "db_bin2json")>]
    extern string dbBin2Json(byte[] bytes, int length)

    [<DllImport("./libserialization.so",
                CallingConvention = CallingConvention.Cdecl,
                EntryPoint = "db_json2bin")>]
    extern int dbJson2Bin(string str, System.IntPtr& byteArray)

    [<DllImport("./libserialization.so",
                CallingConvention = CallingConvention.Cdecl,
                EntryPoint = "user_fn_bin2json")>]
    extern string userfnBin2Json(byte[] bytes, int length)

    [<DllImport("./libserialization.so",
                CallingConvention = CallingConvention.Cdecl,
                EntryPoint = "user_fn_json2bin")>]
    extern int userfnJson2Bin(string str, System.IntPtr& byteArray)

    [<DllImport("./libserialization.so",
                CallingConvention = CallingConvention.Cdecl,
                EntryPoint = "user_tipe_bin2json")>]
    extern string usertipeBin2Json(byte[] bytes, int length)

    [<DllImport("./libserialization.so",
                CallingConvention = CallingConvention.Cdecl,
                EntryPoint = "user_tipe_json2bin")>]
    extern int usertipeJson2Bin(string str, System.IntPtr& byteArray)

    // ----------------
    // expr/tlid pairs (used for packages)
    // ----------------
    [<DllImport("./libserialization.so",
                CallingConvention = CallingConvention.Cdecl,
                EntryPoint = "expr_tlid_pair_bin2json")>]
    extern string exprTLIDPairBin2Json(byte[] bytes, int length)

    [<DllImport("./libserialization.so",
                CallingConvention = CallingConvention.Cdecl,
                EntryPoint = "expr_tlid_pair_json2bin")>]
    extern int exprTLIDPairJson2Bin(string str, System.IntPtr& byteArray)

    // ----------------
    // oplists
    // ----------------
    [<DllImport("./libserialization.so",
                CallingConvention = CallingConvention.Cdecl,
                EntryPoint = "oplist_bin2json")>]
    extern string oplistBin2Json(byte[] bytes, int length)

    [<DllImport("./libserialization.so",
                CallingConvention = CallingConvention.Cdecl,
                EntryPoint = "oplist_json2bin")>]
    extern int oplistJson2Bin(string str, System.IntPtr& byteArray)

    // ----------------
    // dvals - we only need this for fuzzing, so we're just piggybacking on the
    // code that's already here
    // ----------------
    [<DllImport("./libserialization.so",
                CallingConvention = CallingConvention.Cdecl,
                EntryPoint = "to_internal_roundtrippable_v0")>]
    extern string toInternalRoundtrippableV0(string str)

    [<DllImport("./libserialization.so",
                CallingConvention = CallingConvention.Cdecl,
                EntryPoint = "of_internal_roundtrippable_v0")>]
    extern string ofInternalRoundtrippableV0(string str)

    [<DllImport("./libserialization.so",
                CallingConvention = CallingConvention.Cdecl,
                EntryPoint = "to_internal_queryable_v0")>]
    extern string toInternalQueryableV0(string str)

    [<DllImport("./libserialization.so",
                CallingConvention = CallingConvention.Cdecl,
                EntryPoint = "to_internal_queryable_v1")>]
    extern string toInternalQueryableV1(string str)

    [<DllImport("./libserialization.so",
                CallingConvention = CallingConvention.Cdecl,
                EntryPoint = "of_internal_queryable_v0")>]
    extern string ofInternalQueryableV0(string str)

    [<DllImport("./libserialization.so",
                CallingConvention = CallingConvention.Cdecl,
                EntryPoint = "of_internal_queryable_v1")>]
    extern string ofInternalQueryableV1(string str)

    [<DllImport("./libserialization.so",
                CallingConvention = CallingConvention.Cdecl,
                EntryPoint = "to_developer_repr_v0")>]
    extern string toDeveloperRepr(string str)

    [<DllImport("./libserialization.so",
                CallingConvention = CallingConvention.Cdecl,
                EntryPoint = "to_enduser_readable_text_v0")>]
    extern string toEnduserReadableTextV0(string str)

    [<DllImport("./libserialization.so",
                CallingConvention = CallingConvention.Cdecl,
                EntryPoint = "to_pretty_machine_json_v1")>]
    extern string toPrettyMachineJsonV1(string str)

    [<DllImport("./libserialization.so",
                CallingConvention = CallingConvention.Cdecl,
                EntryPoint = "to_url_string")>]
    extern string toUrlString(string str)

    [<DllImport("./libserialization.so",
                CallingConvention = CallingConvention.Cdecl,
                EntryPoint = "to_hashable_repr")>]
    extern string toHashableRepr(string str)

    [<DllImport("./libserialization.so",
                CallingConvention = CallingConvention.Cdecl,
                EntryPoint = "of_unknown_json_v1")>]
    extern string ofUnknownJsonV1(string str)

    [<DllImport("./libserialization.so",
                CallingConvention = CallingConvention.Cdecl,
                EntryPoint = "hash_v0")>]
    extern string hashV0(string str)

    [<DllImport("./libserialization.so",
                CallingConvention = CallingConvention.Cdecl,
                EntryPoint = "hash_v1")>]
    extern string hashV1(string str)


    // ----------------
    // serialization digest
    // ----------------
    [<DllImport("./libserialization.so",
                CallingConvention = CallingConvention.Cdecl,
                EntryPoint = "digest")>]
    extern string digest()

    // ----------------
    // OCaml runtime
    // ----------------
    [<DllImport("./libserialization.so",
                CallingConvention = CallingConvention.Cdecl,
                EntryPoint = "dark_init_ocaml")>]
    extern string darkInitOcaml()

    [<DllImport("./libserialization.so",
                CallingConvention = CallingConvention.Cdecl,
                EntryPoint = "register_thread")>]
    extern void registerThread()




  type OncePerThread private () =
    static let initializedTLS = new System.Threading.ThreadLocal<_>(fun () -> false)
    static member initialized : bool = initializedTLS.Value
    static member markInitialized() : unit = initializedTLS.Value <- true

  let registerThread () : unit =
    if not OncePerThread.initialized then
      OncePerThread.markInitialized ()
      Internal.registerThread ()
    else
      ()

  // Ideally we would pass in the actual internal function here, however F#
  // doesn't allow type signatures with byref arguments, so  we do
  let translateJson2Bin (internalFn : (System.IntPtr -> int)) : byte [] =
    registerThread ()
    let ptr = System.IntPtr()
    let length = internalFn ptr
    let (bytes : byte array) = Array.zeroCreate length
    Marshal.Copy(ptr, bytes, 0, length)
    bytes

  let startTranslateJson2Bin () : System.IntPtr =
    registerThread ()
    System.IntPtr()

  let finishTranslateJson2Bin (ptr : System.IntPtr) (length : int) : byte array =
    let (bytes : byte array) = Array.zeroCreate length
    Marshal.Copy(ptr, bytes, 0, length)
    bytes

  let translateBin2Json
    (internalFn : ((byte array * int) -> string))
    (bytes : byte [])
    : string =
    registerThread ()
    internalFn (bytes, bytes.Length)

  let handlerJson2Bin (json : string) : byte [] =
    let mutable ptr = startTranslateJson2Bin ()
    Internal.handlerJson2Bin (json, &ptr) |> finishTranslateJson2Bin ptr

  let handlerBin2Json (bytes : byte []) : string =
    translateBin2Json Internal.handlerBin2Json bytes

  let dbJson2Bin (json : string) : byte [] =
    let mutable ptr = startTranslateJson2Bin ()
    Internal.dbJson2Bin (json, &ptr) |> finishTranslateJson2Bin ptr

  let dbBin2Json (bytes : byte []) : string =
    translateBin2Json Internal.dbBin2Json bytes

  let userfnJson2Bin (json : string) : byte [] =
    let mutable ptr = startTranslateJson2Bin ()
    Internal.userfnJson2Bin (json, &ptr) |> finishTranslateJson2Bin ptr

  let userfnBin2Json (bytes : byte []) : string =
    translateBin2Json Internal.userfnBin2Json bytes

  let usertipeJson2Bin (json : string) : byte [] =
    let mutable ptr = startTranslateJson2Bin ()
    Internal.usertipeJson2Bin (json, &ptr) |> finishTranslateJson2Bin ptr

  let usertipeBin2Json (bytes : byte []) : string =
    translateBin2Json Internal.usertipeBin2Json bytes

  let exprTLIDPairJson2Bin (json : string) : byte [] =
    let mutable ptr = startTranslateJson2Bin ()
    Internal.exprTLIDPairJson2Bin (json, &ptr) |> finishTranslateJson2Bin ptr

  let exprTLIDPairBin2Json (bytes : byte []) : string =
    translateBin2Json Internal.exprTLIDPairBin2Json bytes

  let oplistJson2Bin (json : string) : byte [] =
    let mutable ptr = startTranslateJson2Bin ()
    Internal.oplistJson2Bin (json, &ptr) |> finishTranslateJson2Bin ptr

  let oplistBin2Json (bytes : byte []) : string =
    translateBin2Json Internal.oplistBin2Json bytes

  let digest () =
    registerThread ()
    Internal.digest ()

  let init () =
    printfn "serialization_init"
    let str = Internal.darkInitOcaml ()
    printfn "serialization_inited: %s" str
    ()

module OCamlTypes =
  // These types come directly from OCaml, and are used for automatic json
  // serializers, which match the Yojson derived serializers on the OCaml side.

  // fsharplint:disable FL0038

  type id = uint64

  type tlid = id

  type or_blank<'a> =
    | Blank of id
    | Filled of id * 'a
    | Partial of id * string

  type tipe =
    | TAny
    | TInt
    | TFloat
    | TBool
    | TNull
    | TDeprecated1
    | TStr
    | TList
    | TObj
    | TIncomplete
    | TError
    | TBlock
    | TResp
    | TDB
    | TDeprecated6
    | TDate
    | TDeprecated2
    | TDeprecated3
    | TDeprecated4 of string
    | TDeprecated5 of string
    | TDbList of tipe
    | TPassword
    | TUuid
    | TOption
    | TErrorRail
    | TCharacter
    | TResult
    | TUserType of string * int
    | TBytes


  module RuntimeT =
    type fnname = string
    type fieldname = string
    type varname = string
    type keyname = string
    type varbinding = varname or_blank
    type field = fieldname or_blank
    type key = keyname or_blank

    type sendToRail =
      | Rail
      | NoRail

    type FPString = { matchID : id; patternID : id; str : string }

    type fluidPattern =
      | FPVariable of id * id * string
      | FPConstructor of id * id * string * fluidPattern list
      | FPInteger of id * id * string
      | FPBool of id * id * bool
      | FPString of FPString
      | FPFloat of id * id * string * string
      | FPNull of id * id
      | FPBlank of id * id

    type fluidExpr =
      | EInteger of id * string
      | EBool of id * bool
      | EString of id * string
      | EFloat of id * string * string
      | ENull of id
      | EBlank of id
      | ELet of id * string * fluidExpr * fluidExpr
      | EIf of id * fluidExpr * fluidExpr * fluidExpr
      | EBinOp of id * string * fluidExpr * fluidExpr * sendToRail
      | ELambda of id * (id * string) list * fluidExpr
      | EFieldAccess of id * fluidExpr * string
      | EVariable of id * string
      | EFnCall of id * string * fluidExpr list * sendToRail
      | EPartial of id * string * fluidExpr
      | ERightPartial of id * string * fluidExpr
      | ELeftPartial of id * string * fluidExpr
      | EList of id * fluidExpr list
      | ERecord of id * (string * fluidExpr) list
      | EPipe of id * fluidExpr list
      | EConstructor of id * string * fluidExpr list
      | EMatch of id * fluidExpr * (fluidPattern * fluidExpr) list
      | EPipeTarget of id
      | EFeatureFlag of id * string * fluidExpr * fluidExpr * fluidExpr



    module DbT =
      type col = string or_blank * tipe or_blank

      type migration_kind = DeprecatedMigrationKind

      type db_migration_state =
        | DBMigrationAbandoned
        | DBMigrationInitialized

      type 'expr_type db_migration =
        { starting_version : int
          version : int
          state : db_migration_state
          rollforward : 'expr_type
          rollback : 'expr_type
          cols : col list }

      type 'expr_type db =
        { tlid : tlid
          name : string or_blank
          cols : col list
          version : int
          old_migrations : 'expr_type db_migration list
          active_migration : 'expr_type db_migration option }

    module HandlerT =
      type dtdeprecated = int or_blank

      type spec_types = { input : dtdeprecated; output : dtdeprecated }

      type spec =
        { ``module`` : string or_blank
          name : string or_blank
          modifier : string or_blank
          types : spec_types }

      type 'expr_type handler = { tlid : tlid; ast : 'expr_type; spec : spec }

    type ufn_param =
      { name : string or_blank
        tipe : tipe or_blank
        block_args : string list
        optional : bool
        description : string }

    type ufn_metadata =
      { name : string or_blank
        parameters : ufn_param list
        return_type : tipe or_blank
        description : string
        infix : bool }

    type 'expr_type user_fn =
      { tlid : tlid
        metadata : ufn_metadata
        ast : 'expr_type }

    type user_record_field = { name : string or_blank; tipe : tipe or_blank }

    type user_tipe_definition = UTRecord of user_record_field list

    type user_tipe =
      { tlid : tlid
        name : string or_blank
        version : int
        definition : user_tipe_definition }

    type tldata =
      | Handler of HandlerT.handler<fluidExpr>
      | DB of DbT.db<fluidExpr>

    type toplevel = { tlid : id; pos : pos; data : tldata }

    type toplevels = List<toplevel>

    type dval_map = Map<string, dval>

    and dhttp =
      | Redirect of string
      | Response of int * (string * string) list

    and optionT =
      | OptJust of dval
      | OptNothing

    and resultT =
      | ResOk of dval
      | ResError of dval

    and dval_source =
      | SourceNone
      | SourceId of tlid * id

    and dblock_args =
      { symtable : dval_map
        ``params`` : (id * string) list
        body : fluidExpr }

    and dval =
      (* basic types  *)
      | DInt of int64
      | DFloat of float
      | DBool of bool
      | DNull
      | DStr of string
      (* compound types *)
      | DList of dval list
      | DObj of dval_map
      (* special types - see notes above *)
      | DIncomplete of dval_source
      | DError of (dval_source * string)
      | DBlock of dblock_args
      | DErrorRail of dval
      (* user types: awaiting a better type system *)
      | DResp of (dhttp * dval)
      | DDB of string
      | DDate of System.DateTime
      // | DPassword of PasswordBytes.t
      | DUuid of System.Guid
      | DOption of optionT
      | DCharacter of string
      | DResult of resultT
      | DBytes of byte []

  module PackageManager =
    type parameter = { name : string; tipe : tipe; description : string }

    type fn =
      { user : string
        package : string
        ``module`` : string
        fnname : string
        version : int
        body : RuntimeT.fluidExpr
        parameters : parameter list
        return_type : tipe
        description : string
        author : string
        deprecated : bool
        tlid : id }

  type 'expr_type op =
    | SetHandler of tlid * pos * 'expr_type RuntimeT.HandlerT.handler
    | CreateDB of tlid * pos * string
    | AddDBCol of tlid * id * id
    | SetDBColName of tlid * id * string
    | SetDBColType of tlid * id * string
    | DeleteTL of tlid
    | MoveTL of tlid * pos
    | SetFunction of 'expr_type RuntimeT.user_fn
    | ChangeDBColName of tlid * id * string
    | ChangeDBColType of tlid * id * string
    | UndoTL of tlid
    | RedoTL of tlid
    | DeprecatedInitDbm of tlid * id * id * id * RuntimeT.DbT.migration_kind
    | SetExpr of tlid * id * 'expr_type
    | TLSavepoint of tlid
    | DeleteFunction of tlid
    | CreateDBMigration of tlid * id * id * (string or_blank * string or_blank) list
    | AddDBColToDBMigration of tlid * id * id
    | SetDBColNameInDBMigration of tlid * id * string
    | SetDBColTypeInDBMigration of tlid * id * string
    | AbandonDBMigration of tlid
    | DeleteColInDBMigration of tlid * id
    | DeleteDBCol of tlid * id
    | RenameDBname of tlid * string
    | CreateDBWithBlankOr of tlid * pos * id * string
    | DeleteTLForever of tlid
    | DeleteFunctionForever of tlid
    | SetType of RuntimeT.user_tipe
    | DeleteType of tlid
    | DeleteTypeForever of tlid

  type 'expr_type oplist = 'expr_type op list
  type 'expr_type tlid_oplist = (tlid * 'expr_type oplist)


module Convert =
  // This module converts back-and-forth between the F# ProgramTypes and OCaml
  // types. Prelude.Json.AutoSerialize generates JSON in the same format as
  // OCaml's Yojson so we can use these types directly to communicate with the
  // client (which also uses these types) and the OCaml libserialization
  // library.
  module OT = OCamlTypes
  module ORT = OT.RuntimeT

  // ----------------
  // OCaml to ProgramTypes
  // ----------------
  let bo2Option (bo : 'a OT.or_blank) : Option<'a> =
    match bo with
    | OT.Partial (_, s) -> None
    | OT.Filled (_, s) -> Some s
    | OT.Blank (_) -> None

  let bo2ID (bo : 'a OT.or_blank) : id =
    match bo with
    | OT.Partial (id, _) -> id
    | OT.Filled (id, _) -> id
    | OT.Blank (id) -> id

  let bo2String (bo : string OT.or_blank) : string =
    match bo with
    | OT.Partial (_, s) -> s
    | OT.Filled (_, s) -> s
    | OT.Blank (_) -> ""

  let rec ocamlPattern2PT (o : ORT.fluidPattern) : PT.Pattern =
    let r = ocamlPattern2PT

    match o with
    | ORT.FPVariable (_, id, str) -> PT.PVariable(id, str)
    | ORT.FPConstructor (_, id, name, pats) ->
        PT.PConstructor(id, name, List.map r pats)
    | ORT.FPInteger (_, id, i) -> PT.PInteger(id, parseBigint i)
    | ORT.FPBool (_, id, b) -> PT.PBool(id, b)
    | ORT.FPString fp -> PT.PString(fp.patternID, fp.str)
    | ORT.FPFloat (_, id, w, f) ->
        let whole = parseBigint w
        let sign = if w.[0] = '-' then Negative else Positive
        PT.PFloat(id, sign, System.Numerics.BigInteger.Abs whole, parseBigint f)
    | ORT.FPNull (_, id) -> PT.PNull id
    | ORT.FPBlank (_, id) -> PT.PBlank id

  let rec ocamlSter2PT (o : ORT.sendToRail) : PT.SendToRail =
    match o with
    | ORT.Rail -> PT.Rail
    | ORT.NoRail -> PT.NoRail

  let rec ocamlExpr2PT (o : ORT.fluidExpr) : PT.Expr =
    let r = ocamlExpr2PT

    match o with
    | ORT.EBlank id -> PT.EBlank id
    | ORT.EInteger (id, num) -> PT.EInteger(id, parseBigint num)
    | ORT.EString (id, str) -> PT.EString(id, str)
    | ORT.EFloat (id, w, f) ->
        let whole = parseBigint w
        let sign = if w.[0] = '-' then Negative else Positive
        PT.EFloat(id, sign, System.Numerics.BigInteger.Abs whole, parseBigint f)
    | ORT.EBool (id, b) -> PT.EBool(id, b)
    | ORT.ENull id -> PT.ENull id
    | ORT.EVariable (id, var) -> PT.EVariable(id, var)
    | ORT.EFieldAccess (id, obj, fieldname) -> PT.EFieldAccess(id, r obj, fieldname)
    | ORT.EFnCall (id, name, args, ster) ->
        PT.EFnCall(id, PT.FQFnName.parse name, List.map r args, ocamlSter2PT ster)
    | ORT.EBinOp (id, name, arg1, arg2, ster) ->
        PT.EBinOp(id, PT.FQFnName.parse name, r arg1, r arg2, ocamlSter2PT ster)
    | ORT.ELambda (id, vars, body) -> PT.ELambda(id, vars, r body)
    | ORT.ELet (id, lhs, rhs, body) -> PT.ELet(id, lhs, r rhs, r body)
    | ORT.EIf (id, cond, thenExpr, elseExpr) ->
        PT.EIf(id, r cond, r thenExpr, r elseExpr)
    | ORT.EPartial (id, str, oldExpr) -> PT.EPartial(id, str, r oldExpr)
    | ORT.ERightPartial (id, str, oldExpr) -> PT.ERightPartial(id, str, r oldExpr)
    | ORT.ELeftPartial (id, str, oldExpr) -> PT.ELeftPartial(id, str, r oldExpr)
    | ORT.EList (id, exprs) -> PT.EList(id, List.map r exprs)
    | ORT.ERecord (id, pairs) -> PT.ERecord(id, List.map (Tuple2.mapItem2 r) pairs)
    | ORT.EPipe (id, expr1 :: expr2 :: rest) ->
        PT.EPipe(id, r expr1, r expr2, List.map r rest)
    | ORT.EPipe (id, [ expr ]) -> r expr
    | ORT.EPipe (id, []) -> failwith "Invalid pipe {o}"
    | ORT.EConstructor (id, name, exprs) ->
        PT.EConstructor(id, name, List.map r exprs)
    | ORT.EMatch (id, mexpr, pairs) ->
        PT.EMatch(
          id,
          r mexpr,
          List.map ((Tuple2.mapItem1 ocamlPattern2PT) << (Tuple2.mapItem2 r)) pairs
        )
    | ORT.EPipeTarget id -> PT.EPipeTarget id
    | ORT.EFeatureFlag (id, name, cond, caseA, caseB) ->
        PT.EFeatureFlag(id, name, r cond, r caseA, r caseB)


  let ocamlexprTLIDPair2PT
    ((expr, tlid) : (ORT.fluidExpr * OT.tlid))
    : PT.Expr * tlid =
    (ocamlExpr2PT expr, tlid)

  let ocamlSpec2PT (o : ORT.HandlerT.spec) : PT.Handler.Spec =
    let ids : PT.Handler.ids =
      { moduleID = bo2ID o.``module``
        nameID = bo2ID o.name
        modifierID = bo2ID o.modifier }

    match bo2String o.``module``, bo2String o.name, bo2String o.modifier with
    | "HTTP", route, method -> PT.Handler.HTTP(route, method, ids)
    | "WORKER", name, _ -> PT.Handler.Worker(name, ids)
    | "CRON", name, interval -> PT.Handler.Cron(name, interval, ids)
    | "REPL", name, _ -> PT.Handler.REPL(name, ids)
    | workerName, name, _ -> PT.Handler.OldWorker(workerName, name, ids)

  let ocamlHandler2PT
    (pos : pos)
    (o : ORT.HandlerT.handler<ORT.fluidExpr>)
    : PT.Handler.T =
    { tlid = o.tlid
      ast = ocamlExpr2PT o.ast
      spec = ocamlSpec2PT o.spec
      pos = pos }


  let rec ocamlTipe2PT (o : OT.tipe) : PT.DType =
    match o with
    | OT.TAny -> PT.TAny
    | OT.TInt -> PT.TInt
    | OT.TFloat -> PT.TFloat
    | OT.TBool -> PT.TBool
    | OT.TNull -> PT.TNull
    | OT.TDeprecated1 -> PT.TAny
    | OT.TStr -> PT.TStr
    | OT.TList -> PT.TList PT.TAny
    | OT.TObj -> PT.TDict PT.TAny
    | OT.TIncomplete -> PT.TIncomplete
    | OT.TError -> PT.TError
    | OT.TBlock -> PT.TFn([ PT.TAny ], PT.TAny)
    | OT.TResp -> PT.THttpResponse PT.TAny
    | OT.TDB -> PT.TDB PT.TAny
    | OT.TDeprecated6 -> PT.TAny
    | OT.TDate -> PT.TDate
    | OT.TDeprecated2 -> PT.TAny
    | OT.TDeprecated3 -> PT.TAny
    | OT.TDeprecated4 string -> PT.TAny
    | OT.TDeprecated5 string -> PT.TAny
    | OT.TDbList tipe -> PT.TDbList(ocamlTipe2PT tipe)
    | OT.TPassword -> PT.TPassword
    | OT.TUuid -> PT.TUuid
    | OT.TOption -> PT.TOption PT.TAny
    | OT.TErrorRail -> PT.TErrorRail
    | OT.TCharacter -> PT.TChar
    | OT.TResult -> PT.TResult(PT.TAny, PT.TAny)
    | OT.TUserType (name, version) -> PT.TUserType(name, version)
    | OT.TBytes -> PT.TBytes

  let ocamlDBCol2PT ((name, tipe) : ORT.DbT.col) : PT.DB.Col =
    { nameID = bo2ID name
      name = bo2String name
      typ = bo2Option tipe |> Option.map ocamlTipe2PT
      typeID = bo2ID tipe }

  let ocamlDB2PT (pos : pos) (o : ORT.DbT.db<ORT.fluidExpr>) : PT.DB.T =
    { tlid = o.tlid
      name = bo2String o.name
      nameID = bo2ID o.name
      pos = pos
      cols = List.map ocamlDBCol2PT o.cols
      version = o.version }

  let ocamlUserType2PT (o : ORT.user_tipe) : PT.UserType.T =
    { tlid = o.tlid
      name = o.name |> bo2String
      nameID = bo2ID o.name
      version = o.version
      definition =
        match o.definition with
        | ORT.UTRecord fields ->
            PT.UserType.Record(
              List.map
                (fun (rf : ORT.user_record_field) ->
                  { name = rf.name |> bo2String
                    nameID = bo2ID rf.name
                    typ = rf.tipe |> bo2Option |> Option.map ocamlTipe2PT
                    typeID = bo2ID rf.tipe })
                fields
            ) }


  let ocamlUserFunction2PT (o : ORT.user_fn<ORT.fluidExpr>) : PT.UserFunction.T =
    { tlid = o.tlid
      name = o.metadata.name |> bo2String
      nameID = o.metadata.name |> bo2ID
      parameters = []
      returnType =
        o.metadata.return_type
        |> bo2Option
        |> Option.map ocamlTipe2PT
        |> Option.defaultValue PT.TAny
      returnTypeID = o.metadata.return_type |> bo2ID
      description = o.metadata.description
      infix = o.metadata.infix
      body = ocamlExpr2PT o.ast }

  let ocamlOp2PT (o : OT.op<ORT.fluidExpr>) : PT.Op =
    match o with
    | OT.SetHandler (tlid, pos, handler) ->
        PT.SetHandler(tlid, pos, ocamlHandler2PT pos handler)
    | OT.CreateDB (tlid, pos, name) -> PT.CreateDB(tlid, pos, name)
    | OT.AddDBCol (tlid, id1, id2) -> PT.AddDBCol(tlid, id1, id2)
    | OT.SetDBColName (tlid, id, name) -> PT.SetDBColName(tlid, id, name)
    | OT.SetDBColType (tlid, id, string) -> PT.SetDBColType(tlid, id, string)
    | OT.DeleteTL tlid -> PT.DeleteTL tlid
    | OT.MoveTL (tlid, pos) -> PT.MoveTL(tlid, pos)
    | OT.SetFunction fn -> PT.SetFunction(ocamlUserFunction2PT fn)
    | OT.ChangeDBColName (tlid, id, string) -> PT.ChangeDBColName(tlid, id, string)
    | OT.ChangeDBColType (tlid, id, string) -> PT.ChangeDBColType(tlid, id, string)
    | OT.UndoTL tlid -> PT.UndoTL tlid
    | OT.RedoTL tlid -> PT.RedoTL tlid
    | OT.DeprecatedInitDbm (tlid, id1, id2, id3, kind) ->
        PT.DeprecatedInitDBm(tlid, id1, id2, id3, PT.DeprecatedMigrationKind)
    | OT.SetExpr (tlid, id, e) -> PT.SetExpr(tlid, id, ocamlExpr2PT e)
    | OT.TLSavepoint tlid -> PT.TLSavepoint tlid
    | OT.DeleteFunction tlid -> PT.DeleteFunction tlid
    | OT.CreateDBMigration (tlid, id1, id2, rollingFns) ->
        PT.CreateDBMigration(
          tlid,
          id1,
          id2,
          List.map
            (fun (bo1, bo2) ->
              let s1 = bo2String bo1
              let id1 = bo2ID bo1
              let s2 = bo2String bo2
              let id2 = bo2ID bo2
              (s1, id1, s2, id2))
            rollingFns
        )
    | OT.AddDBColToDBMigration (tlid, id1, id2) ->
        PT.AddDBColToDBMigration(tlid, id1, id2)
    | OT.SetDBColNameInDBMigration (tlid, id, name) ->
        PT.SetDBColNameInDBMigration(tlid, id, name)
    | OT.SetDBColTypeInDBMigration (tlid, id, tipe) ->
        PT.SetDBColTypeInDBMigration(tlid, id, tipe)
    | OT.AbandonDBMigration tlid -> PT.AbandonDBMigration tlid
    | OT.DeleteColInDBMigration (tlid, id) -> PT.DeleteColInDBMigration(tlid, id)
    | OT.DeleteDBCol (tlid, id) -> PT.DeleteDBCol(tlid, id)
    | OT.RenameDBname (tlid, string) -> PT.RenameDBname(tlid, string)
    | OT.CreateDBWithBlankOr (tlid, pos, id, string) ->
        PT.CreateDBWithBlankOr(tlid, pos, id, string)
    | OT.DeleteTLForever tlid -> PT.DeleteTLForever tlid
    | OT.DeleteFunctionForever tlid -> PT.DeleteFunctionForever tlid
    | OT.SetType tipe -> PT.SetType(ocamlUserType2PT tipe)
    | OT.DeleteType tlid -> PT.DeleteType tlid
    | OT.DeleteTypeForever tlid -> PT.DeleteTypeForever tlid


  let ocamlOplist2PT (list : OT.oplist<ORT.fluidExpr>) : PT.Oplist =
    List.map ocamlOp2PT list

  let ocamlTLIDOplist2PT
    (tlidOplist : OT.tlid_oplist<ORT.fluidExpr>)
    : tlid * PT.Oplist =
    Tuple2.mapItem2 ocamlOplist2PT tlidOplist

  // ----------------
  // ProgramTypes to OCaml
  // ----------------
  let string2bo (id : id) (str : string) : (string OT.or_blank) =
    if str = "" then OT.Blank id else OT.Filled(id, str)

  let option2bo (id : id) (o : Option<'a>) : 'a OT.or_blank =
    match o with
    | None -> OT.Blank id
    | Some v -> OT.Filled(id, v)

  let rec pt2ocamlPattern (mid : id) (p : PT.Pattern) : ORT.fluidPattern =
    let r = pt2ocamlPattern mid

    match p with
    | PT.PVariable (id, str) -> ORT.FPVariable(mid, id, str)
    | PT.PConstructor (id, name, pats) ->
        ORT.FPConstructor(mid, id, name, List.map r pats)
    | PT.PInteger (id, i) -> ORT.FPInteger(mid, id, i.ToString())
    | PT.PCharacter (id, c) -> failwith "Character patterns not supported"
    | PT.PBool (id, b) -> ORT.FPBool(mid, id, b)
    | PT.PString (id, s) -> ORT.FPString { matchID = mid; patternID = id; str = s }
    | PT.PFloat (id, Positive, w, f) ->
        ORT.FPFloat(mid, id, w.ToString(), f.ToString())
    | PT.PFloat (id, Negative, w, f) -> ORT.FPFloat(mid, id, $"-{w}", f.ToString())
    | PT.PNull (id) -> ORT.FPNull(mid, id)
    | PT.PBlank (id) -> ORT.FPBlank(mid, id)

  let rec pt2ocamlSter (p : PT.SendToRail) : ORT.sendToRail =
    match p with
    | PT.Rail -> ORT.Rail
    | PT.NoRail -> ORT.NoRail

  let rec pt2ocamlExpr (p : PT.Expr) : ORT.fluidExpr =
    let r = pt2ocamlExpr

    match p with
    | PT.EBlank id -> ORT.EBlank id
    | PT.EInteger (id, num) -> ORT.EInteger(id, num.ToString())
    | PT.ECharacter (id, num) -> failwith "Characters not supported"
    | PT.EString (id, str) -> ORT.EString(id, str)
    | PT.EFloat (id, Positive, w, f) -> ORT.EFloat(id, w.ToString(), f.ToString())
    | PT.EFloat (id, Negative, w, f) -> ORT.EFloat(id, $"-{w}", f.ToString())
    | PT.EBool (id, b) -> ORT.EBool(id, b)
    | PT.ENull id -> ORT.ENull id
    | PT.EVariable (id, var) -> ORT.EVariable(id, var)
    | PT.EFieldAccess (id, obj, fieldname) -> ORT.EFieldAccess(id, r obj, fieldname)
    | PT.EFnCall (id, name, args, ster) ->
        ORT.EFnCall(id, name.ToString(), List.map r args, pt2ocamlSter ster)
    | PT.EBinOp (id, name, arg1, arg2, ster) ->
        ORT.EBinOp(id, name.ToString(), r arg1, r arg2, pt2ocamlSter ster)
    | PT.ELambda (id, vars, body) -> ORT.ELambda(id, vars, r body)
    | PT.ELet (id, lhs, rhs, body) -> ORT.ELet(id, lhs, r rhs, r body)
    | PT.EIf (id, cond, thenExpr, elseExpr) ->
        ORT.EIf(id, r cond, r thenExpr, r elseExpr)
    | PT.EPartial (id, str, oldExpr) -> ORT.EPartial(id, str, r oldExpr)
    | PT.ERightPartial (id, str, oldExpr) -> ORT.ERightPartial(id, str, r oldExpr)
    | PT.ELeftPartial (id, str, oldExpr) -> ORT.ELeftPartial(id, str, r oldExpr)
    | PT.EList (id, exprs) -> ORT.EList(id, List.map r exprs)
    | PT.ERecord (id, pairs) -> ORT.ERecord(id, List.map (Tuple2.mapItem2 r) pairs)
    | PT.EPipe (id, expr1, expr2, rest) ->
        ORT.EPipe(id, r expr1 :: r expr2 :: List.map r rest)
    | PT.EConstructor (id, name, exprs) ->
        ORT.EConstructor(id, name, List.map r exprs)
    | PT.EMatch (id, mexpr, pairs) ->
        ORT.EMatch(
          id,
          r mexpr,
          List.map
            ((Tuple2.mapItem1 (pt2ocamlPattern id)) << (Tuple2.mapItem2 r))
            pairs
        )
    | PT.EPipeTarget id -> ORT.EPipeTarget id
    | PT.EFeatureFlag (id, name, cond, caseA, caseB) ->
        ORT.EFeatureFlag(id, name, r cond, r caseA, r caseB)

  let pt2ocamlexprTLIDPair
    ((expr, tlid) : (PT.Expr * tlid))
    : ORT.fluidExpr * OT.tlid =
    (pt2ocamlExpr expr, tlid)


  let pt2ocamlSpec (p : PT.Handler.Spec) : ORT.HandlerT.spec =
    let types : ORT.HandlerT.spec_types =
      { input = OT.Blank(gid ()); output = OT.Blank(gid ()) }

    match p with
    | PT.Handler.HTTP (route, method, ids) ->
        { ``module`` = string2bo ids.moduleID "HTTP"
          name = string2bo ids.nameID route
          modifier = string2bo ids.modifierID method
          types = types }
    | PT.Handler.Worker (name, ids) ->
        { ``module`` = string2bo ids.moduleID "WORKER"
          name = string2bo ids.nameID name
          modifier = string2bo ids.modifierID "_"
          types = types }
    | PT.Handler.Cron (name, interval, ids) ->
        { ``module`` = string2bo ids.moduleID "CRON"
          name = string2bo ids.nameID name
          modifier = string2bo ids.modifierID interval
          types = types }
    | PT.Handler.REPL (name, ids) ->
        { ``module`` = string2bo ids.moduleID "REPL"
          name = string2bo ids.nameID name
          modifier = string2bo ids.modifierID "_"
          types = types }
    | PT.Handler.OldWorker (workerName, name, ids) ->
        { ``module`` = string2bo ids.moduleID workerName
          name = string2bo ids.nameID name
          modifier = string2bo ids.modifierID "_"
          types = types }

  let pt2ocamlHandler (p : PT.Handler.T) : ORT.HandlerT.handler<ORT.fluidExpr> =
    { tlid = p.tlid; ast = pt2ocamlExpr p.ast; spec = pt2ocamlSpec p.spec }

  let rec pt2ocamlTipe (p : PT.DType) : OT.tipe =
    match p with
    | PT.TVariable _ -> failwith "doesnt exist yet"
    | PT.TAny -> OT.TAny
    | PT.TInt -> OT.TInt
    | PT.TFloat -> OT.TFloat
    | PT.TLambda -> OT.TBlock
    | PT.TBool -> OT.TBool
    | PT.TNull -> OT.TNull
    | PT.TStr -> OT.TStr
    | PT.TList _ -> OT.TList
    | PT.TRecord _ -> OT.TObj
    | PT.TIncomplete -> OT.TIncomplete
    | PT.TError -> OT.TError
    | PT.TFn _ -> OT.TBlock
    | PT.THttpResponse _ -> OT.TResp
    | PT.TDB _ -> OT.TDB
    | PT.TDate -> OT.TDate
    | PT.TDict _ -> OT.TObj
    | PT.TDbList tipe -> OT.TDbList(pt2ocamlTipe tipe)
    | PT.TPassword -> OT.TPassword
    | PT.TUuid -> OT.TUuid
    | PT.TOption _ -> OT.TOption
    | PT.TErrorRail -> OT.TErrorRail
    | PT.TChar -> OT.TCharacter
    | PT.TResult _ -> OT.TResult
    | PT.TUserType (name, version) -> OT.TUserType(name, version)
    | PT.TBytes -> OT.TBytes

  let pt2ocamlDBCol (p : PT.DB.Col) : ORT.DbT.col =
    (string2bo p.nameID p.name, option2bo p.typeID (Option.map pt2ocamlTipe p.typ))

  let pt2ocamlDB (p : PT.DB.T) : ORT.DbT.db<ORT.fluidExpr> =
    { tlid = p.tlid
      name = string2bo p.nameID p.name
      cols = List.map pt2ocamlDBCol p.cols
      version = p.version
      old_migrations = []
      active_migration = None }


  let pt2ocamlUserType (p : PT.UserType.T) : ORT.user_tipe =
    { tlid = p.tlid
      name = p.name |> string2bo p.nameID
      version = p.version
      definition =
        match p.definition with
        | PT.UserType.Record fields ->
            ORT.UTRecord(
              List.map
                (fun (rf : PT.UserType.RecordField) ->
                  { name = string2bo rf.nameID rf.name
                    tipe = rf.typ |> Option.map pt2ocamlTipe |> option2bo rf.typeID })
                fields
            ) }

  let pt2ocamlUserFunction (p : PT.UserFunction.T) : ORT.user_fn<ORT.fluidExpr> =
    { tlid = p.tlid
      metadata =
        { name = string2bo p.nameID p.name
          parameters = []
          return_type =
            p.returnType |> pt2ocamlTipe |> Some |> option2bo p.returnTypeID
          description = p.description
          infix = p.infix }
      ast = pt2ocamlExpr p.body }

  let pt2ocamlOp (p : PT.Op) : OT.op<ORT.fluidExpr> =
    match p with
    | PT.SetHandler (tlid, pos, handler) ->
        OT.SetHandler(tlid, pos, pt2ocamlHandler handler)
    | PT.CreateDB (tlid, pos, name) -> OT.CreateDB(tlid, pos, name)
    | PT.AddDBCol (tlid, id1, id2) -> OT.AddDBCol(tlid, id1, id2)
    | PT.SetDBColName (tlid, id, name) -> OT.SetDBColName(tlid, id, name)
    | PT.SetDBColType (tlid, id, string) -> OT.SetDBColType(tlid, id, string)
    | PT.DeleteTL tlid -> OT.DeleteTL tlid
    | PT.MoveTL (tlid, pos) -> OT.MoveTL(tlid, pos)
    | PT.SetFunction fn -> OT.SetFunction(pt2ocamlUserFunction fn)
    | PT.ChangeDBColName (tlid, id, string) -> OT.ChangeDBColName(tlid, id, string)
    | PT.ChangeDBColType (tlid, id, string) -> OT.ChangeDBColType(tlid, id, string)
    | PT.UndoTL tlid -> OT.UndoTL tlid
    | PT.RedoTL tlid -> OT.RedoTL tlid
    | PT.SetExpr (tlid, id, e) -> OT.SetExpr(tlid, id, pt2ocamlExpr e)
    | PT.TLSavepoint tlid -> OT.TLSavepoint tlid
    | PT.DeleteFunction tlid -> OT.DeleteFunction tlid
    | PT.CreateDBMigration (tlid, id1, id2, rollingFns) ->
        OT.CreateDBMigration(
          tlid,
          id1,
          id2,
          List.map
            (fun (s1, id1, s2, id2) -> (string2bo id1 s1, string2bo id2 s2))
            rollingFns
        )
    | PT.AddDBColToDBMigration (tlid, id1, id2) ->
        OT.AddDBColToDBMigration(tlid, id1, id2)
    | PT.SetDBColNameInDBMigration (tlid, id, name) ->
        OT.SetDBColNameInDBMigration(tlid, id, name)
    | PT.SetDBColTypeInDBMigration (tlid, id, tipe) ->
        OT.SetDBColTypeInDBMigration(tlid, id, tipe)
    | PT.AbandonDBMigration tlid -> OT.AbandonDBMigration tlid
    | PT.DeleteColInDBMigration (tlid, id) -> OT.DeleteColInDBMigration(tlid, id)
    | PT.DeprecatedInitDBm (tlid, id1, id2, id3, kind) ->
        OT.DeprecatedInitDbm(
          tlid,
          id1,
          id2,
          id3,
          OT.RuntimeT.DbT.DeprecatedMigrationKind
        )
    | PT.DeleteDBCol (tlid, id) -> OT.DeleteDBCol(tlid, id)
    | PT.RenameDBname (tlid, string) -> OT.RenameDBname(tlid, string)
    | PT.CreateDBWithBlankOr (tlid, pos, id, string) ->
        OT.CreateDBWithBlankOr(tlid, pos, id, string)
    | PT.DeleteTLForever tlid -> OT.DeleteTLForever tlid
    | PT.DeleteFunctionForever tlid -> OT.DeleteFunctionForever tlid
    | PT.SetType tipe -> OT.SetType(pt2ocamlUserType tipe)
    | PT.DeleteType tlid -> OT.DeleteType tlid
    | PT.DeleteTypeForever tlid -> OT.DeleteTypeForever tlid


  let pt2ocamlOplist (list : PT.Oplist) : OT.oplist<ORT.fluidExpr> =
    List.map pt2ocamlOp list

  let pt2ocamlToplevels
    (toplevels : Map<tlid, PT.Toplevel>)
    : ORT.toplevels * ORT.user_fn<ORT.fluidExpr> list * ORT.user_tipe list =
    toplevels
    |> Map.values
    |> List.fold
         ([], [], [])
         (fun (tls, ufns, uts) tl ->
           match tl with
           | PT.TLHandler h ->
               let ocamlHandler = pt2ocamlHandler h

               let ocamlTL : ORT.toplevel =
                 { tlid = h.tlid; pos = h.pos; data = ORT.Handler ocamlHandler }

               ocamlTL :: tls, ufns, uts
           | PT.TLDB db ->
               let ocamlDB = pt2ocamlDB db

               let ocamlTL : ORT.toplevel =
                 { tlid = db.tlid; pos = db.pos; data = ORT.DB ocamlDB }

               ocamlTL :: tls, ufns, uts
           | PT.TLFunction f -> (tls, pt2ocamlUserFunction f :: ufns, uts)
           | PT.TLType t -> (tls, ufns, pt2ocamlUserType t :: uts))

  let ocamlPackageManagerParameter2PT
    (o : OT.PackageManager.parameter)
    : PT.PackageManager.Parameter =
    { name = o.name; description = o.description; typ = ocamlTipe2PT o.tipe }

  let pt2ocamlPackageManagerParameter
    (p : PT.PackageManager.Parameter)
    : OT.PackageManager.parameter =
    { name = p.name; description = p.description; tipe = pt2ocamlTipe p.typ }


  // let ocamlPackageManagerFn2PT (o : OT.PackageManager.fn) : PT.PackageManager.Fn =
  //   { user = fn.name.owner
  //     package = fn.name.package
  //     ``module`` = fn.name.module_
  //     fnname = fn.name.function_
  //     version = fn.name.version
  //     body = fn.body
  //     parameters = fn.parameters
  //     return_type = fn.returnType
  //     description = fn.description
  //     author = fn.author
  //     deprecated = fn.deprecated
  //     tlid = fn.tlid }
  //
  let pt2ocamlPackageManagerFn (p : PT.PackageManager.Fn) : OT.PackageManager.fn =
    { user = p.name.owner
      package = p.name.package
      ``module`` = p.name.module_
      fnname = p.name.function_
      version = p.name.version
      body = p.body |> pt2ocamlExpr
      parameters = p.parameters |> List.map pt2ocamlPackageManagerParameter
      return_type = p.returnType |> pt2ocamlTipe
      description = p.description
      author = p.author
      deprecated = p.deprecated
      tlid = p.tlid }

  let rec rt2ocamlDval (p : RT.Dval) : ORT.dval =
    let c = rt2ocamlDval

    match p with
    | RT.DStr s -> ORT.DStr s
    | RT.DChar c -> ORT.DCharacter c
    | RT.DInt i -> ORT.DInt(int64 i)
    | RT.DBool true -> ORT.DBool true
    | RT.DBool false -> ORT.DBool false
    | RT.DFloat f -> ORT.DFloat f
    | RT.DNull -> ORT.DNull
    | RT.DFnVal (RT.FnName _) -> failwith "not supported in ocaml"
    | RT.DFnVal (RT.Lambda args) ->
        ORT.DBlock
          { ``params`` = args.parameters
            symtable = Map.map c args.symtable
            body = ORT.EBlank 1UL }
    | RT.DIncomplete RT.SourceNone -> ORT.DIncomplete ORT.SourceNone
    | RT.DIncomplete (RT.SourceID (tlid, id)) ->
        ORT.DIncomplete(ORT.SourceId(tlid, id))
    | RT.DError (RT.SourceNone, msg) -> ORT.DError(ORT.SourceNone, msg)
    | RT.DError (RT.SourceID (tlid, id), msg) ->
        ORT.DError(ORT.SourceId(tlid, id), msg)
    | RT.DDate d -> ORT.DDate d
    | RT.DDB name -> ORT.DDB name
    | RT.DUuid uuid -> ORT.DUuid uuid
    | RT.DHttpResponse (RT.Redirect url, hdv) -> ORT.DResp(ORT.Redirect url, c hdv)
    | RT.DHttpResponse (RT.Response (code, headers), hdv) ->
        ORT.DResp(ORT.Response(code, headers), c hdv)
    | RT.DList l -> ORT.DList(List.map c l)
    | RT.DObj o -> ORT.DObj(Map.map c o)
    | RT.DOption None -> ORT.DOption ORT.OptNothing
    | RT.DOption (Some dv) -> ORT.DOption(ORT.OptJust(c dv))
    | RT.DResult (Ok dv) -> ORT.DResult(ORT.ResOk(c dv))
    | RT.DResult (Error dv) -> ORT.DResult(ORT.ResError(c dv))
    | RT.DErrorRail dv -> ORT.DErrorRail(c dv)
    | RT.DBytes bytes -> ORT.DBytes bytes

  let rec ocamlDval2rt (p : ORT.dval) : RT.Dval =
    let c = ocamlDval2rt

    match p with
    | ORT.DStr s -> RT.DStr s
    | ORT.DCharacter c -> RT.DChar c
    | ORT.DInt i -> RT.DInt(bigint i)
    | ORT.DBool true -> RT.DBool true
    | ORT.DBool false -> RT.DBool false
    | ORT.DFloat f -> RT.DFloat f
    | ORT.DNull -> RT.DNull
    | ORT.DBlock (args) ->
        RT.DFnVal(
          RT.Lambda
            { parameters = args.``params``
              symtable = Map.map c args.symtable
              body = RT.EBlank 1UL }
        )
    | ORT.DIncomplete ORT.SourceNone -> RT.DIncomplete RT.SourceNone
    | ORT.DIncomplete (ORT.SourceId (tlid, id)) ->
        RT.DIncomplete(RT.SourceID(tlid, id))
    | ORT.DError (ORT.SourceNone, msg) -> RT.DError(RT.SourceNone, msg)
    | ORT.DError (ORT.SourceId (tlid, id), msg) ->
        RT.DError(RT.SourceID(tlid, id), msg)
    | ORT.DDate d -> RT.DDate d
    | ORT.DDB name -> RT.DDB name
    | ORT.DUuid uuid -> RT.DUuid uuid
    | ORT.DResp (ORT.Redirect url, hdv) -> RT.DHttpResponse(RT.Redirect url, c hdv)
    | ORT.DResp (ORT.Response (code, headers), hdv) ->
        RT.DHttpResponse(RT.Response(code, headers), c hdv)
    | ORT.DList l -> RT.DList(List.map c l)
    | ORT.DObj o -> RT.DObj(Map.map c o)
    | ORT.DOption ORT.OptNothing -> RT.DOption None
    | ORT.DOption (ORT.OptJust dv) -> RT.DOption(Some(c dv))
    | ORT.DResult (ORT.ResOk dv) -> RT.DResult(Ok(c dv))
    | ORT.DResult (ORT.ResError dv) -> RT.DResult(Error(c dv))
    | ORT.DErrorRail dv -> RT.DErrorRail(c dv)
    | ORT.DBytes bytes -> RT.DBytes bytes



// ----------------
// Binary conversions
// ----------------
let toplevelOfCachedBinary
  ((data, pos) : (byte array * string option))
  : PT.Toplevel =
  let pos =
    pos
    |> Option.map Json.AutoSerialize.deserialize<pos>
    |> Option.unwrap { x = 0; y = 0 }

  let toplevelOfCachedHandler () =
    Binary.handlerBin2Json data
    |> Json.AutoSerialize.deserialize<OCamlTypes.RuntimeT.HandlerT.handler<OCamlTypes.RuntimeT.fluidExpr>>
    |> Convert.ocamlHandler2PT pos
    |> PT.TLHandler

  let toplevelOfCachedDB () =
    Binary.dbBin2Json data
    |> Json.AutoSerialize.deserialize<OCamlTypes.RuntimeT.DbT.db<OCamlTypes.RuntimeT.fluidExpr>>
    |> Convert.ocamlDB2PT pos
    |> PT.TLDB

  let toplevelOfCachedUserFunction () =
    Binary.userfnBin2Json data
    |> Json.AutoSerialize.deserialize<OCamlTypes.RuntimeT.user_fn<OCamlTypes.RuntimeT.fluidExpr>>
    |> Convert.ocamlUserFunction2PT
    |> PT.TLFunction

  let toplevelOfCachedUserTipe () =
    Binary.usertipeBin2Json data
    |> Json.AutoSerialize.deserialize<OCamlTypes.RuntimeT.user_tipe>
    |> Convert.ocamlUserType2PT
    |> PT.TLType

  try
    toplevelOfCachedHandler ()
  with e1 ->
    try
      toplevelOfCachedDB ()
    with e2 ->
      try
        toplevelOfCachedUserFunction ()
      with e3 ->
        try
          toplevelOfCachedUserTipe ()
        with e4 ->
          failwith
            $"could not parse binary toplevel {e1}\n\n{e2}\n\n{e3}\n\n{e4}\n\n"

let toplevelToCachedBinary (toplevel : PT.Toplevel) : byte array =
  match toplevel with
  | PT.TLHandler h ->
      h
      |> Convert.pt2ocamlHandler
      |> Json.AutoSerialize.serialize
      |> Binary.handlerJson2Bin

  | PT.TLDB db ->
      db |> Convert.pt2ocamlDB |> Json.AutoSerialize.serialize |> Binary.dbJson2Bin
  | PT.TLFunction db ->
      db
      |> Convert.pt2ocamlUserFunction
      |> Json.AutoSerialize.serialize
      |> Binary.userfnJson2Bin
  | PT.TLType db ->
      db
      |> Convert.pt2ocamlUserType
      |> Json.AutoSerialize.serialize
      |> Binary.usertipeJson2Bin


let oplistOfBinary (data : byte array) : PT.Oplist =
  Binary.oplistBin2Json data
  |> Json.AutoSerialize.deserialize<OCamlTypes.oplist<OCamlTypes.RuntimeT.fluidExpr>>
  |> Convert.ocamlOplist2PT

let oplistToBinary (oplist : PT.Oplist) : byte array =
  oplist
  |> Convert.pt2ocamlOplist
  |> Json.AutoSerialize.serialize
  |> Binary.oplistJson2Bin

let exprTLIDPairOfCachedBinary (data : byte array) : PT.Expr * tlid =
  Binary.exprTLIDPairBin2Json data
  |> Json.AutoSerialize.deserialize<OCamlTypes.RuntimeT.fluidExpr * OCamlTypes.tlid>
  |> Convert.ocamlexprTLIDPair2PT

let exprTLIDPairToCachedBinary ((expr, tlid) : (PT.Expr * tlid)) : byte array =
  (expr, tlid)
  |> Convert.pt2ocamlexprTLIDPair
  |> Json.AutoSerialize.serialize
  |> Binary.exprTLIDPairJson2Bin

// for fuzzing
let ofInternalQueryableV0 (str : string) : RT.Dval =
  Binary.Internal.registerThread ()

  str
  |> Binary.Internal.ofInternalQueryableV0
  |> Json.AutoSerialize.deserialize<OCamlTypes.RuntimeT.dval>
  |> Convert.ocamlDval2rt

let ofInternalQueryableV1 (str : string) : RT.Dval =
  Binary.Internal.registerThread ()

  str
  |> Binary.Internal.ofInternalQueryableV1
  |> Json.AutoSerialize.deserialize<OCamlTypes.RuntimeT.dval>
  |> Convert.ocamlDval2rt

let ofInternalRoundtrippableV0 (str : string) : RT.Dval =
  Binary.Internal.registerThread ()

  str
  |> Binary.Internal.ofInternalRoundtrippableV0
  |> Json.AutoSerialize.deserialize<OCamlTypes.RuntimeT.dval>
  |> Convert.ocamlDval2rt

let ofUnknownJson (str : string) : RT.Dval =
  Binary.Internal.registerThread ()

  str
  |> Binary.Internal.ofUnknownJsonV1
  |> Json.AutoSerialize.deserialize<OCamlTypes.RuntimeT.dval>
  |> Convert.ocamlDval2rt

let toDeveloperRepr (dv : RT.Dval) : string =
  Binary.Internal.registerThread ()

  dv
  |> Convert.rt2ocamlDval
  |> Json.AutoSerialize.serialize
  |> Binary.Internal.toDeveloperRepr

let toEnduserReadableTextV0 (dv : RT.Dval) : string =
  Binary.Internal.registerThread ()

  dv
  |> Convert.rt2ocamlDval
  |> Json.AutoSerialize.serialize
  |> Binary.Internal.toEnduserReadableTextV0

let toHashableRepr (dv : RT.Dval) : string =
  Binary.Internal.registerThread ()

  dv
  |> Convert.rt2ocamlDval
  |> Json.AutoSerialize.serialize
  |> Binary.Internal.toHashableRepr

let toInternalQueryableV0 (dv : RT.Dval) : string =
  Binary.Internal.registerThread ()

  dv
  |> Convert.rt2ocamlDval
  |> Json.AutoSerialize.serialize
  |> Binary.Internal.toInternalQueryableV0

let toInternalQueryableV1 (dv : RT.Dval) : string =
  Binary.Internal.registerThread ()

  dv
  |> Convert.rt2ocamlDval
  |> Json.AutoSerialize.serialize
  |> Binary.Internal.toInternalQueryableV1

let toInternalRoundtrippableV0 (dv : RT.Dval) : string =
  Binary.Internal.registerThread ()

  dv
  |> Convert.rt2ocamlDval
  |> Json.AutoSerialize.serialize
  |> Binary.Internal.toInternalRoundtrippableV0

let toPrettyMachineJsonV1 (dv : RT.Dval) : string =
  Binary.Internal.registerThread ()

  dv
  |> Convert.rt2ocamlDval
  |> Json.AutoSerialize.serialize
  |> Binary.Internal.toPrettyMachineJsonV1

let toUrlString (dv : RT.Dval) : string =
  Binary.Internal.registerThread ()

  dv
  |> Convert.rt2ocamlDval
  |> Json.AutoSerialize.serialize
  |> Binary.Internal.toUrlString

let hashV0 (dv : RT.Dval) : string =
  Binary.Internal.registerThread ()

  dv
  |> Convert.rt2ocamlDval
  |> Json.AutoSerialize.serialize
  |> Binary.Internal.hashV0

let hashV1 (dv : RT.Dval) : string =
  Binary.Internal.registerThread ()

  dv
  |> Convert.rt2ocamlDval
  |> Json.AutoSerialize.serialize
  |> Binary.Internal.hashV1
