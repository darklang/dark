module LibBackend.ProgramSerialization.OCamlInterop

// Interoperation functions with OCaml.

// Programs are stored using an OCaml-only serialization format, so we have to
// call OCaml code to fetch it and save it.  We send binary code which we get
// from the DB, convert it to OCaml types, then json convert it to get it back
// into F#. At that point we convert it to these types, and potentially convert
// it to the runtime types to run it.

// We also use these types to convert to the types the API uses, which are
// typically direct deserializations of these types.

open System.Runtime.InteropServices
open System.Threading.Tasks
open FSharp.Control.Tasks
open FSharpPlus
open System.Text.RegularExpressions

open Prelude
open Tablecloth
open LibBackend.Db

module PT = ProgramTypes

module Binary =
  module Internal =
    // These allow us to call C functions from serialization_stubs.c, which in
    // turn call into the OCaml runtime.

    // FSTODO if we have segfaults, we might need to use this:
    // https://docs.microsoft.com/en-us/dotnet/standard/native-interop/best-practices#keeping-managed-objects-alive

    // initialize OCaml runtime
    [<DllImport("./libserialization.so",
                CallingConvention = CallingConvention.Cdecl,
                EntryPoint = "dark_init_ocaml")>]
    extern string darkInitOcaml()

    // take a binary rep of a handler from the DB and convert it into JSON
    [<DllImport("./libserialization.so",
                CallingConvention = CallingConvention.Cdecl,
                EntryPoint = "handler_bin2json")>]
    extern string handlerBin2Json(byte[] bytes, int length)

    [<DllImport("./libserialization.so",
                CallingConvention = CallingConvention.Cdecl,
                EntryPoint = "handler_json2bin")>]
    extern int handlerJson2Bin(string str, System.IntPtr& byteArray)

    // take a binary rep of an expr&tlid from the DB and convert it into JSON
    [<DllImport("./libserialization.so",
                CallingConvention = CallingConvention.Cdecl,
                EntryPoint = "expr_tlid_pair_bin2json")>]
    extern string exprTLIDPairBin2Json(byte[] bytes, int length)

    [<DllImport("./libserialization.so",
                CallingConvention = CallingConvention.Cdecl,
                EntryPoint = "expr_tlid_pair_json2bin")>]
    extern int exprTLIDPairJson2Bin(string str, System.IntPtr& byteArray)

    // take a binary rep of an oplist from the DB and convert it into JSON
    [<DllImport("./libserialization.so",
                CallingConvention = CallingConvention.Cdecl,
                EntryPoint = "oplist_bin2json")>]
    extern string oplistBin2Json(byte[] bytes, int length)

    [<DllImport("./libserialization.so",
                CallingConvention = CallingConvention.Cdecl,
                EntryPoint = "oplist_json2bin")>]
    extern int oplistJson2Bin(string str, System.IntPtr& byteArray)

    [<DllImport("./libserialization.so",
                CallingConvention = CallingConvention.Cdecl,
                EntryPoint = "digest")>]
    extern string digest()

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

  let digest () = Internal.digest ()

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

    type fluidPattern =
      | FPVariable of id * id * string
      | FPConstructor of id * id * string * fluidPattern list
      | FPInteger of id * id * string
      | FPBool of id * id * bool
      | FPString of matchID : id * patternID : id * str : string
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

    type toplevels = Map<id, toplevel>

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
  module RT = OT.RuntimeT

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

  let rec ocamlPattern2PT (o : RT.fluidPattern) : PT.Pattern =
    let r = ocamlPattern2PT

    match o with
    | RT.FPVariable (_, id, str) -> PT.PVariable(id, str)
    | RT.FPConstructor (_, id, name, pats) ->
        PT.PConstructor(id, name, List.map r pats)
    | RT.FPInteger (_, id, i) -> PT.PInteger(id, parseBigint i)
    | RT.FPBool (_, id, b) -> PT.PBool(id, b)
    | RT.FPString (_, id, s) -> PT.PString(id, s)
    | RT.FPFloat (_, id, w, f) ->
        let whole = parseBigint w
        let sign = if w.[0] = '-' then Negative else Positive
        PT.PFloat(id, sign, System.Numerics.BigInteger.Abs whole, parseBigint f)
    | RT.FPNull (_, id) -> PT.PNull id
    | RT.FPBlank (_, id) -> PT.PBlank id

  let rec ocamlSter2PT (o : RT.sendToRail) : PT.SendToRail =
    match o with
    | RT.Rail -> PT.Rail
    | RT.NoRail -> PT.NoRail

  let rec ocamlExpr2PT (o : RT.fluidExpr) : PT.Expr =
    let r = ocamlExpr2PT

    match o with
    | RT.EBlank id -> PT.EBlank id
    | RT.EInteger (id, num) -> PT.EInteger(id, parseBigint num)
    | RT.EString (id, str) -> PT.EString(id, str)
    | RT.EFloat (id, w, f) ->
        let whole = parseBigint w
        let sign = if w.[0] = '-' then Negative else Positive
        PT.EFloat(id, sign, System.Numerics.BigInteger.Abs whole, parseBigint f)
    | RT.EBool (id, b) -> PT.EBool(id, b)
    | RT.ENull id -> PT.ENull id
    | RT.EVariable (id, var) -> PT.EVariable(id, var)
    | RT.EFieldAccess (id, obj, fieldname) -> PT.EFieldAccess(id, r obj, fieldname)
    | RT.EFnCall (id, name, args, ster) ->
        PT.EFnCall(id, PT.FQFnName.parse name, List.map r args, ocamlSter2PT ster)
    | RT.EBinOp (id, name, arg1, arg2, ster) ->
        PT.EBinOp(id, PT.FQFnName.parse name, r arg1, r arg2, ocamlSter2PT ster)
    | RT.ELambda (id, vars, body) -> PT.ELambda(id, vars, r body)
    | RT.ELet (id, lhs, rhs, body) -> PT.ELet(id, lhs, r rhs, r body)
    | RT.EIf (id, cond, thenExpr, elseExpr) ->
        PT.EIf(id, r cond, r thenExpr, r elseExpr)
    | RT.EPartial (id, str, oldExpr) -> PT.EPartial(id, str, r oldExpr)
    | RT.ERightPartial (id, str, oldExpr) -> PT.ERightPartial(id, str, r oldExpr)
    | RT.ELeftPartial (id, str, oldExpr) -> PT.ELeftPartial(id, str, r oldExpr)
    | RT.EList (id, exprs) -> PT.EList(id, List.map r exprs)
    | RT.ERecord (id, pairs) -> PT.ERecord(id, List.map (Tuple2.mapItem2 r) pairs)
    | RT.EPipe (id, expr1 :: expr2 :: rest) ->
        PT.EPipe(id, r expr1, r expr2, List.map r rest)
    | RT.EPipe (id, [ expr ]) -> r expr
    | RT.EPipe (id, []) -> failwith "Invalid pipe {o}"
    | RT.EConstructor (id, name, exprs) ->
        PT.EConstructor(id, name, List.map r exprs)
    | RT.EMatch (id, mexpr, pairs) ->
        PT.EMatch(
          id,
          r mexpr,
          List.map ((Tuple2.mapItem1 ocamlPattern2PT) << (Tuple2.mapItem2 r)) pairs
        )
    | RT.EPipeTarget id -> PT.EPipeTarget id
    | RT.EFeatureFlag (id, name, cond, caseA, caseB) ->
        PT.EFeatureFlag(id, name, r cond, r caseA, r caseB)


  let ocamlexprTLIDPair2PT
    ((expr, tlid) : (RT.fluidExpr * OT.tlid))
    : PT.Expr * tlid =
    (ocamlExpr2PT expr, tlid)

  let ocamlSpec2PT (o : RT.HandlerT.spec) : PT.Handler.Spec =
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
    (o : RT.HandlerT.handler<RT.fluidExpr>)
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
    | OT.TDB -> PT.TDB
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


  let ocamlUserType2PT (o : RT.user_tipe) : PT.UserType.T =
    { tlid = o.tlid
      name = o.name |> bo2String
      nameID = bo2ID o.name
      version = o.version
      definition =
        match o.definition with
        | RT.UTRecord fields ->
            PT.UserType.Record(
              List.map
                (fun (rf : RT.user_record_field) ->
                  { name = rf.name |> bo2String
                    nameID = bo2ID rf.name
                    typ = rf.tipe |> bo2Option |> Option.map ocamlTipe2PT
                    typeID = bo2ID rf.tipe })
                fields
            ) }


  let ocamlUserFunction2PT (o : RT.user_fn<RT.fluidExpr>) : PT.UserFunction.T =
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
      ast = ocamlExpr2PT o.ast }

  let ocamlOp2PT (o : OT.op<RT.fluidExpr>) : PT.Op =
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


  let ocamlOplist2PT (list : OT.oplist<RT.fluidExpr>) : PT.Oplist =
    List.map ocamlOp2PT list

  let ocamlTLIDOplist2PT
    (tlidOplist : OT.tlid_oplist<RT.fluidExpr>)
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

  let rec pt2ocamlPattern (mid : id) (p : PT.Pattern) : RT.fluidPattern =
    let r = pt2ocamlPattern mid

    match p with
    | PT.PVariable (id, str) -> RT.FPVariable(mid, id, str)
    | PT.PConstructor (id, name, pats) ->
        RT.FPConstructor(mid, id, name, List.map r pats)
    | PT.PInteger (id, i) -> RT.FPInteger(mid, id, i.ToString())
    | PT.PCharacter (id, c) -> failwith "Character patterns not supported"
    | PT.PBool (id, b) -> RT.FPBool(mid, id, b)
    | PT.PString (id, s) -> RT.FPString(mid, id, s)
    | PT.PFloat (id, Positive, w, f) ->
        RT.FPFloat(mid, id, w.ToString(), f.ToString())
    | PT.PFloat (id, Negative, w, f) -> RT.FPFloat(mid, id, $"-{w}", f.ToString())
    | PT.PNull (id) -> RT.FPNull(mid, id)
    | PT.PBlank (id) -> RT.FPBlank(mid, id)

  let rec pt2ocamlSter (p : PT.SendToRail) : RT.sendToRail =
    match p with
    | PT.Rail -> RT.Rail
    | PT.NoRail -> RT.NoRail

  let rec pt2ocamlExpr (p : PT.Expr) : RT.fluidExpr =
    let r = pt2ocamlExpr

    match p with
    | PT.EBlank id -> RT.EBlank id
    | PT.EInteger (id, num) -> RT.EInteger(id, num.ToString())
    | PT.ECharacter (id, num) -> failwith "Characters not supported"
    | PT.EString (id, str) -> RT.EString(id, str)
    | PT.EFloat (id, Positive, w, f) -> RT.EFloat(id, w.ToString(), f.ToString())
    | PT.EFloat (id, Negative, w, f) -> RT.EFloat(id, $"-{w}", f.ToString())
    | PT.EBool (id, b) -> RT.EBool(id, b)
    | PT.ENull id -> RT.ENull id
    | PT.EVariable (id, var) -> RT.EVariable(id, var)
    | PT.EFieldAccess (id, obj, fieldname) -> RT.EFieldAccess(id, r obj, fieldname)
    | PT.EFnCall (id, name, args, ster) ->
        RT.EFnCall(id, name.ToString(), List.map r args, pt2ocamlSter ster)
    | PT.EBinOp (id, name, arg1, arg2, ster) ->
        RT.EBinOp(id, name.ToString(), r arg1, r arg2, pt2ocamlSter ster)
    | PT.ELambda (id, vars, body) -> RT.ELambda(id, vars, r body)
    | PT.ELet (id, lhs, rhs, body) -> RT.ELet(id, lhs, r rhs, r body)
    | PT.EIf (id, cond, thenExpr, elseExpr) ->
        RT.EIf(id, r cond, r thenExpr, r elseExpr)
    | PT.EPartial (id, str, oldExpr) -> RT.EPartial(id, str, r oldExpr)
    | PT.ERightPartial (id, str, oldExpr) -> RT.ERightPartial(id, str, r oldExpr)
    | PT.ELeftPartial (id, str, oldExpr) -> RT.ELeftPartial(id, str, r oldExpr)
    | PT.EList (id, exprs) -> RT.EList(id, List.map r exprs)
    | PT.ERecord (id, pairs) -> RT.ERecord(id, List.map (Tuple2.mapItem2 r) pairs)
    | PT.EPipe (id, expr1, expr2, rest) ->
        RT.EPipe(id, r expr1 :: r expr2 :: List.map r rest)
    | PT.EConstructor (id, name, exprs) ->
        RT.EConstructor(id, name, List.map r exprs)
    | PT.EMatch (id, mexpr, pairs) ->
        RT.EMatch(
          id,
          r mexpr,
          List.map
            ((Tuple2.mapItem1 (pt2ocamlPattern id)) << (Tuple2.mapItem2 r))
            pairs
        )
    | PT.EPipeTarget id -> RT.EPipeTarget id
    | PT.EFeatureFlag (id, name, cond, caseA, caseB) ->
        RT.EFeatureFlag(id, name, r cond, r caseA, r caseB)

  let pt2ocamlexprTLIDPair
    ((expr, tlid) : (PT.Expr * tlid))
    : RT.fluidExpr * OT.tlid =
    (pt2ocamlExpr expr, tlid)


  let pt2ocamlSpec (p : PT.Handler.Spec) : RT.HandlerT.spec =
    let types : RT.HandlerT.spec_types =
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

  let pt2ocamlHandler (p : PT.Handler.T) : RT.HandlerT.handler<RT.fluidExpr> =
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
    | PT.TDB -> OT.TDB
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

  let pt2ocamlDBCol (p : PT.DB.Col) : RT.DbT.col =
    (string2bo p.nameID p.name, option2bo p.typeID (Option.map pt2ocamlTipe p.typ))

  let pt2ocamlDB (p : PT.DB.T) : RT.DbT.db<RT.fluidExpr> =
    { tlid = p.tlid
      name = string2bo p.nameID p.name
      cols = List.map pt2ocamlDBCol p.cols
      version = p.version
      old_migrations = []
      active_migration = None }


  let pt2ocamlUserType (p : PT.UserType.T) : RT.user_tipe =
    { tlid = p.tlid
      name = p.name |> string2bo p.nameID
      version = p.version
      definition =
        match p.definition with
        | PT.UserType.Record fields ->
            RT.UTRecord(
              List.map
                (fun (rf : PT.UserType.RecordField) ->
                  { name = string2bo rf.nameID rf.name
                    tipe = rf.typ |> Option.map pt2ocamlTipe |> option2bo rf.typeID })
                fields
            ) }

  let pt2ocamlUserFunction (p : PT.UserFunction.T) : RT.user_fn<RT.fluidExpr> =
    { tlid = p.tlid
      metadata =
        { name = string2bo p.nameID p.name
          parameters = []
          return_type =
            p.returnType |> pt2ocamlTipe |> Some |> option2bo p.returnTypeID
          description = p.description
          infix = p.infix }
      ast = pt2ocamlExpr p.ast }

  let pt2ocamlOp (p : PT.Op) : OT.op<RT.fluidExpr> =
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


  let pt2ocamlOplist (list : PT.Oplist) : OT.oplist<RT.fluidExpr> =
    List.map pt2ocamlOp list

  let pt2ocamlToplevels
    (toplevels : Map<tlid, PT.Toplevel>)
    : RT.toplevels * RT.user_fn<RT.fluidExpr> list * RT.user_tipe list =
    toplevels
    |> Map.values
    |> List.fold
         (Map.empty, [], [])
         (fun (tls, ufns, uts) tl ->
           match tl with
           | PT.TLHandler h ->
               let ocamlHandler = pt2ocamlHandler h

               let ocamlTL : RT.toplevel =
                 { tlid = h.tlid; pos = h.pos; data = RT.Handler ocamlHandler }

               Map.add h.tlid ocamlTL tls, ufns, uts
           | PT.TLDB db ->
               let ocamlDB = pt2ocamlDB db

               let ocamlTL : RT.toplevel =
                 { tlid = db.tlid; pos = db.pos; data = RT.DB ocamlDB }

               (Map.add db.tlid ocamlTL tls, ufns, uts)
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





// ----------------
// Binary conversions
// ----------------
let toplevelOfCachedBinary
  ((data, pos) : (byte array * string option))
  // FSTODO: incorporate pos
  // FSTODO: support tipes, dbs, functions
  : PT.Toplevel =
  let pos = { x = 0; y = 0 } // FSTODO

  Binary.handlerBin2Json data
  |> Json.AutoSerialize.deserialize<OCamlTypes.RuntimeT.HandlerT.handler<OCamlTypes.RuntimeT.fluidExpr>>
  |> Convert.ocamlHandler2PT pos
  |> PT.TLHandler

let toplevelToCachedBinary (toplevel : PT.Toplevel) : byte array =
  match toplevel with
  | PT.TLHandler h ->
      h
      |> Convert.pt2ocamlHandler
      |> Json.AutoSerialize.serialize
      |> Binary.handlerJson2Bin

  | _ -> failwith $"toplevel not supported yet {toplevel}"

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
  let json = Binary.exprTLIDPairBin2Json data
  printfn $"JSON {json}"

  let deserialized =
    Json.AutoSerialize.deserialize<OCamlTypes.RuntimeT.fluidExpr * OCamlTypes.tlid> (
      json
    )

  printfn $"DESERIALIZED {deserialized}"
  let converted = Convert.ocamlexprTLIDPair2PT deserialized
  printfn $"CONVERTED {converted}"
  converted

let exprTLIDPairToCachedBinary ((expr, tlid) : (PT.Expr * tlid)) : byte array =
  (expr, tlid)
  |> Convert.pt2ocamlexprTLIDPair
  |> Json.AutoSerialize.serialize
  |> Binary.exprTLIDPairJson2Bin
