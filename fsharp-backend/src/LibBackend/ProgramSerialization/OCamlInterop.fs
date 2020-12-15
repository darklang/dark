module LibBackend.ProgramSerialization.OCamlInterop

// Interoperation functions with OCaml.

// Programs are stored using an OCaml-only serialization format, so we have to
// call OCaml code to fetch it and save it.  We send binary code which we get
// from the DB, convert it to OCaml types, then json convert it to get it back
// into F#. At that point we convert it to these types, and potentially convert
// it to the runtime types to run it.

open System.Runtime.InteropServices
open System.Threading.Tasks
open FSharp.Control.Tasks
open FSharpPlus
open FSharp.Data
open System.Text.RegularExpressions

open Prelude
open LibBackend.Db

module PT = ProgramTypes
open LibExecution.SharedTypes

module Binary =
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

  [<DllImport("./libserialization.so",
              CallingConvention = CallingConvention.Cdecl,
              EntryPoint = "digest")>]
  extern string digest()

  let init () =
    printfn "serialization_init"
    let str = darkInitOcaml ()
    printfn "serialization_inited: %s" str
    ()

module OCamlTypes =

  // fsharplint:disable FL0038

  type pos = { x : int; y : int }

  type id = int64

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

    type 'expr_type package_fn = { metadata : ufn_metadata; ast : 'expr_type }

    type user_record_field = { name : string or_blank; tipe : tipe or_blank }

    type user_tipe_definition = UTRecord of user_record_field list

    type user_tipe =
      { tlid : tlid
        name : string or_blank
        version : int
        definition : user_tipe_definition }

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
  type 'expr_type tlid_oplists = (tlid * 'expr_type oplist) list

module Yojson =
  // Yojson is the OCaml automated JSON format. This module generates and
  // parses JSON in that format, so that it can be sent to/from OCaml.

  // Prelude.Json.AutoSerialize generated JSON in the same format as OCaml's
  // Yojson. We add the exact ocaml types so that we can serialize directly,
  // and then convert those types into ProgramTypes.
  module OT = OCamlTypes
  module RT = OT.RuntimeT

  // ----------------
  // OCaml to ProgramTypes
  // ----------------
  let bo2ID (bo : string OT.or_blank) : id =
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
    | RT.FPInteger (_, id, i) -> PT.PInteger(id, i)
    | RT.FPBool (_, id, b) -> PT.PBool(id, b)
    | RT.FPString (_, id, s) -> PT.PString(id, s)
    | RT.FPFloat (_, id, w, f) -> PT.PFloat(id, w, f)
    | RT.FPNull (_, id) -> PT.PNull id
    | RT.FPBlank (_, id) -> PT.PBlank id

  let rec ocamlSter2PT (o : RT.sendToRail) : PT.SendToRail =
    match o with
    | RT.Rail -> PT.Rail
    | RT.NoRail -> PT.Rail

  let rec ocamlExpr2PT (o : RT.fluidExpr) : PT.Expr =
    let r = ocamlExpr2PT

    match o with
    | RT.EBlank id -> PT.EBlank id
    | RT.EInteger (id, num) -> PT.EInteger(id, num)
    | RT.EString (id, str) -> PT.EString(id, str)
    | RT.EFloat (id, whole, fraction) -> PT.EFloat(id, whole, fraction)
    | RT.EBool (id, b) -> PT.EBool(id, b)
    | RT.ENull id -> PT.ENull id
    | RT.EVariable (id, var) -> PT.EVariable(id, var)
    | RT.EFieldAccess (id, obj, fieldname) -> PT.EFieldAccess(id, r obj, fieldname)
    | RT.EFnCall (id, name, args, ster) ->
        PT.EFnCall(id, PT.FQFnName.T.parse name, List.map r args, ocamlSter2PT ster)
    | RT.EBinOp (id, name, arg1, arg2, ster) ->
        PT.EBinOp(id, PT.FQFnName.T.parse name, r arg1, r arg2, ocamlSter2PT ster)
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


  let ocamlSpec2PT (o : RT.HandlerT.spec) : PT.Handler.Spec =
    let ids : PT.Handler.ids =
      { moduleID = bo2ID o.``module``
        nameID = bo2ID o.name
        modifierID = bo2ID o.modifier }

    match bo2String o.``module``, bo2String o.name, bo2String o.modifier with
    | "HTTP", route, method -> PT.Handler.HTTP(route, method, ids)
    | "Worker", name, _ -> PT.Handler.Worker(name, ids)
    | "CRON", name, interval -> PT.Handler.Cron(name, interval, ids)
    | "REPL", name, _ -> PT.Handler.REPL(name, ids)
    | workerName, name, _ -> PT.Handler.OldWorker(workerName, name, ids)

  let ocamlHandler2PT (o : RT.HandlerT.handler<RT.fluidExpr>) : PT.Handler.T =
    { tlid = o.tlid; ast = ocamlExpr2PT o.ast; spec = ocamlSpec2PT o.spec }

  // ----------------
  // ProgramTypes to OCaml
  // ----------------
  let pt2bo (id : id) (str : string) : (string OT.or_blank) =
    if str = "" then OT.Blank id else OT.Filled(id, str)

  let rec pt2ocamlPattern (mid : id) (p : PT.Pattern) : RT.fluidPattern =
    let r = pt2ocamlPattern mid

    match p with
    | PT.PVariable (id, str) -> RT.FPVariable(mid, id, str)
    | PT.PConstructor (id, name, pats) ->
        RT.FPConstructor(mid, id, name, List.map r pats)
    | PT.PInteger (id, i) -> RT.FPInteger(mid, id, i)
    | PT.PCharacter (id, c) -> failwith "Character patterns not supported"
    | PT.PBool (id, b) -> RT.FPBool(mid, id, b)
    | PT.PString (id, s) -> RT.FPString(mid, id, s)
    | PT.PFloat (id, w, f) -> RT.FPFloat(mid, id, w, f)
    | PT.PNull (id) -> RT.FPNull(mid, id)
    | PT.PBlank (id) -> RT.FPBlank(mid, id)

  let rec pt2ocamlSter (p : PT.SendToRail) : RT.sendToRail =
    match p with
    | PT.Rail -> RT.Rail
    | PT.NoRail -> RT.Rail

  let rec pt2ocamlExpr (p : PT.Expr) : RT.fluidExpr =
    let r = pt2ocamlExpr

    match p with
    | PT.EBlank id -> RT.EBlank id
    | PT.EInteger (id, num) -> RT.EInteger(id, num)
    | PT.ECharacter (id, num) -> failwith "Characters not supported"
    | PT.EString (id, str) -> RT.EString(id, str)
    | PT.EFloat (id, whole, fraction) -> RT.EFloat(id, whole, fraction)
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


  let pt2ocamlSpec (p : PT.Handler.Spec) : RT.HandlerT.spec =
    let types : RT.HandlerT.spec_types =
      { input = OT.Blank(gid ()); output = OT.Blank(gid ()) }

    match p with
    | PT.Handler.HTTP (route, method, ids) ->
        { ``module`` = pt2bo ids.moduleID "HTTP"
          name = pt2bo ids.nameID route
          modifier = pt2bo ids.modifierID method
          types = types }
    | PT.Handler.Worker (name, ids) ->
        { ``module`` = pt2bo ids.moduleID "WORKER"
          name = pt2bo ids.nameID name
          modifier = pt2bo ids.modifierID "_"
          types = types }
    | PT.Handler.Cron (name, interval, ids) ->
        { ``module`` = pt2bo ids.moduleID "HTTP"
          name = pt2bo ids.nameID name
          modifier = pt2bo ids.modifierID interval
          types = types }
    | PT.Handler.REPL (name, ids) ->
        { ``module`` = pt2bo ids.moduleID "REPL"
          name = pt2bo ids.nameID name
          modifier = pt2bo ids.modifierID "_"
          types = types }
    | PT.Handler.OldWorker (workerName, name, ids) ->
        { ``module`` = pt2bo ids.moduleID workerName
          name = pt2bo ids.nameID name
          modifier = pt2bo ids.modifierID "_"
          types = types }

  let pt2ocamlHandler (p : PT.Handler.T) : RT.HandlerT.handler<RT.fluidExpr> =
    { tlid = p.tlid; ast = pt2ocamlExpr p.ast; spec = pt2ocamlSpec p.spec }


// ----------------
// Binary conversions
// ----------------
let toplevelOfCachedBinary ((data, pos) : (byte array * string option))
                           : PT.Toplevel =
  Binary.handlerBin2Json (data, data.Length)
  |> Json.AutoSerialize.deserialize<OCamlTypes.RuntimeT.HandlerT.handler<OCamlTypes.RuntimeT.fluidExpr>>
  |> Yojson.ocamlHandler2PT
  |> PT.TLHandler

let toplevelToCachedBinary (toplevel : PT.Toplevel) : byte array =
  match toplevel with
  | PT.TLHandler h ->
      let json = h |> Yojson.pt2ocamlHandler |> Json.AutoSerialize.serialize
      let mutable destPtr = System.IntPtr()
      let length = Binary.handlerJson2Bin (json, &destPtr)
      let mutable (bytes : byte array) = Array.zeroCreate length
      Marshal.Copy(destPtr, bytes, 0, length)
      bytes

  | _ -> failwith $"toplevel not supported yet {toplevel}"
