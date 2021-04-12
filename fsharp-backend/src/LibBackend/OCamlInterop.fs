module LibBackend.OCamlInterop

// Interoperation functions with OCaml.

// Programs are stored using an OCaml-only serialization format, so we have to
// call OCaml code to fetch it and save it.  We send binary code which we get
// from the DB, convert it to OCaml types, then json convert it to get it back
// into F#. At that point we convert it to these types, and potentially convert
// it to the runtime types to run it.

// We also use these types to convert to the types the API uses, which are
// typically direct deserializations of these types.

open FSharpPlus

open System.Threading.Tasks
open FSharp.Control.Tasks

open Prelude
open Tablecloth

module PT = ProgramTypes
module RT = LibExecution.RuntimeTypes

let digest () = "0e91e490041f06fae012f850231eb6ab"

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
      | DPassword of byte array // We dont use this path for testing, see DvalRepr.Tests
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
  // types. Prelude.Json.OCamlCompatible generates JSON in the same format as
  // OCaml's Yojson so we can use these types directly to communicate with the
  // client (which also uses these types) and the legacyserver
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
    let any = PT.TVariable "a"

    match o with
    | OT.TAny -> any
    | OT.TInt -> PT.TInt
    | OT.TFloat -> PT.TFloat
    | OT.TBool -> PT.TBool
    | OT.TNull -> PT.TNull
    | OT.TDeprecated1 -> any
    | OT.TStr -> PT.TStr
    | OT.TList -> PT.TList any
    | OT.TObj -> PT.TDict any
    | OT.TIncomplete -> PT.TIncomplete
    | OT.TError -> PT.TError
    | OT.TBlock -> PT.TFn([ any ], any)
    | OT.TResp -> PT.THttpResponse any
    | OT.TDB -> PT.TDB any
    | OT.TDeprecated6 -> any
    | OT.TDate -> PT.TDate
    | OT.TDeprecated2 -> any
    | OT.TDeprecated3 -> any
    | OT.TDeprecated4 string -> any
    | OT.TDeprecated5 string -> any
    | OT.TDbList tipe -> PT.TDbList(ocamlTipe2PT tipe)
    | OT.TPassword -> PT.TPassword
    | OT.TUuid -> PT.TUuid
    | OT.TOption -> PT.TOption any
    | OT.TErrorRail -> PT.TErrorRail
    | OT.TCharacter -> PT.TChar
    | OT.TResult -> PT.TResult(any, any)
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

  let ocamlParameter2PT (o : ORT.ufn_param) : PT.UserFunction.Parameter =
    { name = o.name |> bo2String
      nameID = o.name |> bo2ID
      typ = o.tipe |> bo2Option |> Option.map ocamlTipe2PT
      typeID = o.tipe |> bo2ID
      description = o.description }

  let ocamlUserFunction2PT (o : ORT.user_fn<ORT.fluidExpr>) : PT.UserFunction.T =
    { tlid = o.tlid
      name = o.metadata.name |> bo2String
      nameID = o.metadata.name |> bo2ID
      parameters = o.metadata.parameters |> List.map ocamlParameter2PT
      returnType =
        o.metadata.return_type
        |> bo2Option
        |> Option.map ocamlTipe2PT
        |> Option.defaultValue (PT.TVariable "a")
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


  let ocamlToplevel2PT
    (list : List<ORT.toplevel>)
    : List<PT.Handler.T> * List<PT.DB.T> =
    list
    |> List.fold
         ([], [])
         (fun (hs, dbs) tl ->
           match tl.data with
           | ORT.Handler h -> (hs @ [ ocamlHandler2PT tl.pos h ], dbs)
           | ORT.DB db -> (hs, dbs @ [ ocamlDB2PT tl.pos db ]))



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
        ORT.EFnCall(
          id,
          name.ToString() |> String.replace "_v0" "",
          List.map r args,
          pt2ocamlSter ster
        )
    | PT.EBinOp (id, name, arg1, arg2, ster) ->
        ORT.EBinOp(
          id,
          name.ToString() |> String.replace "_v0" "",
          r arg1,
          r arg2,
          pt2ocamlSter ster
        )
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
    | PT.TVariable _ -> OT.TAny
    | PT.TInt -> OT.TInt
    | PT.TFloat -> OT.TFloat
    | PT.TFn _ -> OT.TBlock
    | PT.TBool -> OT.TBool
    | PT.TNull -> OT.TNull
    | PT.TStr -> OT.TStr
    | PT.TList _ -> OT.TList
    | PT.TRecord _ -> OT.TObj
    | PT.TIncomplete -> OT.TIncomplete
    | PT.TError -> OT.TError
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

  let pt2ocamlParameter (p : PT.UserFunction.Parameter) : ORT.ufn_param =
    { name = string2bo p.nameID p.name
      tipe = option2bo p.typeID (Option.map pt2ocamlTipe p.typ)
      description = p.description
      optional = false
      block_args = [] // FSTODO
    }

  let pt2ocamlUserFunction (p : PT.UserFunction.T) : ORT.user_fn<ORT.fluidExpr> =
    { tlid = p.tlid
      metadata =
        { name = string2bo p.nameID p.name
          parameters = List.map pt2ocamlParameter p.parameters
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

               tls @ [ ocamlTL ], ufns, uts
           | PT.TLDB db ->
               let ocamlDB = pt2ocamlDB db

               let ocamlTL : ORT.toplevel =
                 { tlid = db.tlid; pos = db.pos; data = ORT.DB ocamlDB }

               tls @ [ ocamlTL ], ufns, uts
           | PT.TLFunction f -> (tls, pt2ocamlUserFunction f :: ufns, uts)
           | PT.TLType t -> (tls, ufns, pt2ocamlUserType t :: uts))

  let ocamlPackageManagerParameter2PT
    (o : OT.PackageManager.parameter)
    : PT.Package.Parameter =
    { name = o.name; description = o.description; typ = ocamlTipe2PT o.tipe }

  let pt2ocamlPackageManagerParameter
    (p : PT.Package.Parameter)
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
  let pt2ocamlPackageManagerFn (p : PT.Package.Fn) : OT.PackageManager.fn =
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
    | RT.DPassword (Password bytes) -> ORT.DPassword bytes
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
    | ORT.DPassword bytes -> RT.DPassword(Password bytes)
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
// Getting values from OCaml
// ----------------
// FSTODO: this is not the right way I think
let client = new System.Net.Http.HttpClient()

let legacyReq
  (endpoint : string)
  (data : byte array)
  : Task<System.Net.Http.HttpContent> =
  task {
    let url = $"http://localhost:5000/{endpoint}"

    use message =
      new System.Net.Http.HttpRequestMessage(System.Net.Http.HttpMethod.Post, url)

    message.Content <- new System.Net.Http.ByteArrayContent(data)

    let! response = client.SendAsync(message)

    if response.StatusCode <> System.Net.HttpStatusCode.OK then
      let! content = response.Content.ReadAsStringAsync()
      failwith $"not a 200 response to {endpoint}: {response.StatusCode}, {content}"
    else
      ()

    return response.Content
  }

let legacyStringReq (endpoint : string) (data : byte array) : Task<string> =
  task {
    let! content = legacyReq endpoint data
    return! content.ReadAsStringAsync()
  }

let legacyBytesReq (endpoint : string) (data : byte array) : Task<byte array> =
  task {
    let! content = legacyReq endpoint data
    return! content.ReadAsByteArrayAsync()
  }

let serialize (v : 'a) : byte array = v |> Json.OCamlCompatible.serialize |> toBytes

let stringToBytesReq (endpoint : string) (str : string) : Task<byte array> =
  str |> toBytes |> legacyBytesReq endpoint

let bytesToStringReq (endpoint : string) (data : byte array) : Task<string> =
  data |> legacyStringReq endpoint

let stringToStringReq (endpoint : string) (str : string) : Task<string> =
  str |> toBytes |> legacyStringReq endpoint

let stringToDvalReq (endpoint : string) (str : string) : Task<RT.Dval> =
  str
  |> toBytes
  |> legacyStringReq endpoint
  |> Task.map Json.OCamlCompatible.deserialize<OCamlTypes.RuntimeT.dval>
  |> Task.map Convert.ocamlDval2rt

let dvalToStringReq (endpoint : string) (dv : RT.Dval) : Task<string> =
  dv |> Convert.rt2ocamlDval |> serialize |> legacyStringReq endpoint

let dvalListToStringReq (endpoint : string) (l : List<RT.Dval>) : Task<string> =
  l |> List.map Convert.rt2ocamlDval |> serialize |> legacyStringReq endpoint


// Binary deserialization functions

let oplistOfBinary (data : byte array) : Task<PT.Oplist> =
  data
  |> bytesToStringReq "bs/oplist_bin2json"
  |> Task.map
       Json.OCamlCompatible.deserialize<OCamlTypes.oplist<OCamlTypes.RuntimeT.fluidExpr>>
  |> Task.map Convert.ocamlOplist2PT

let oplistToBinary (oplist : PT.Oplist) : Task<byte array> =
  oplist
  |> Convert.pt2ocamlOplist
  |> Json.OCamlCompatible.serialize
  |> stringToBytesReq "bs/oplist_json2bin"

let exprTLIDPairOfCachedBinary (data : byte array) : Task<PT.Expr * tlid> =
  data
  |> bytesToStringReq "bs/expr_tlid_pair_bin2json"
  |> Task.map
       Json.OCamlCompatible.deserialize<OCamlTypes.RuntimeT.fluidExpr * OCamlTypes.tlid>
  |> Task.map Convert.ocamlexprTLIDPair2PT

let exprTLIDPairToCachedBinary ((expr, tlid) : (PT.Expr * tlid)) : Task<byte array> =
  (expr, tlid)
  |> Convert.pt2ocamlexprTLIDPair
  |> Json.OCamlCompatible.serialize
  |> stringToBytesReq "bs/expr_tlid_pair_json2bin"

let handlerBin2Json (data : byte array) (pos : pos) : Task<PT.Handler.T> =
  data
  |> bytesToStringReq "bs/handler_bin2json"
  |> Task.map
       Json.OCamlCompatible.deserialize<OCamlTypes.RuntimeT.HandlerT.handler<OCamlTypes.RuntimeT.fluidExpr>>
  |> Task.map (Convert.ocamlHandler2PT pos)

let dbBin2Json (data : byte array) (pos : pos) : Task<PT.DB.T> =
  data
  |> bytesToStringReq "bs/db_bin2json"
  |> Task.map
       Json.OCamlCompatible.deserialize<OCamlTypes.RuntimeT.DbT.db<OCamlTypes.RuntimeT.fluidExpr>>
  |> Task.map (Convert.ocamlDB2PT pos)

let userFnBin2Json (data : byte array) : Task<PT.UserFunction.T> =
  data
  |> bytesToStringReq "bs/user_fn_bin2json"
  |> Task.map
       Json.OCamlCompatible.deserialize<OCamlTypes.RuntimeT.user_fn<OCamlTypes.RuntimeT.fluidExpr>>
  |> Task.map Convert.ocamlUserFunction2PT

let userTypeBin2Json (data : byte array) : Task<PT.UserType.T> =
  data
  |> bytesToStringReq "bs/user_tipe_bin2json"
  |> Task.map Json.OCamlCompatible.deserialize<OCamlTypes.RuntimeT.user_tipe>
  |> Task.map Convert.ocamlUserType2PT


let toplevelOfCachedBinary
  ((data, pos) : (byte array * string option))
  : Task<PT.Toplevel> =
  let pos =
    pos
    |> Option.map Json.OCamlCompatible.deserialize<pos>
    |> Option.unwrap { x = 0; y = 0 }

  task {
    try
      return! handlerBin2Json data pos |> Task.map PT.TLHandler
    with e1 ->
      try
        return! dbBin2Json data pos |> Task.map PT.TLDB
      with e2 ->
        try
          return! userFnBin2Json data |> Task.map PT.TLFunction
        with e3 ->
          try
            return! userTypeBin2Json data |> Task.map PT.TLType
          with e4 ->
            failwith
              $"could not parse binary toplevel {e1}\n\n{e2}\n\n{e3}\n\n{e4}\n\n"

            let (ids : PT.Handler.ids) =
              { moduleID = id 0; nameID = id 0; modifierID = id 0 }

            return
              PT.TLHandler
                { tlid = id 0
                  pos = pos
                  ast = PT.EBlank(id 0)
                  spec = PT.Handler.REPL("somename", ids) }
  }


let handlerJson2Bin (h : PT.Handler.T) : Task<byte array> =
  h
  |> Convert.pt2ocamlHandler
  |> Json.OCamlCompatible.serialize
  |> stringToBytesReq "bs/handler_json2bin"

let dbJson2Bin (db : PT.DB.T) : Task<byte array> =
  db
  |> Convert.pt2ocamlDB
  |> Json.OCamlCompatible.serialize
  |> stringToBytesReq "bs/db_json2bin"


let userFnJson2Bin (userFn : PT.UserFunction.T) : Task<byte array> =
  userFn
  |> Convert.pt2ocamlUserFunction
  |> Json.OCamlCompatible.serialize
  |> stringToBytesReq "bs/user_fn_json2bin"


let userTypeJson2Bin (userType : PT.UserType.T) : Task<byte array> =
  userType
  |> Convert.pt2ocamlUserType
  |> Json.OCamlCompatible.serialize
  |> stringToBytesReq "bs/user_tipe_json2bin"

let toplevelToCachedBinary (toplevel : PT.Toplevel) : Task<byte array> =
  match toplevel with
  | PT.TLHandler h -> handlerJson2Bin h
  | PT.TLDB db -> dbJson2Bin db
  | PT.TLFunction f -> userFnJson2Bin f
  | PT.TLType t -> userTypeJson2Bin t


// ---------------------------
// These are only here for fuzzing. We should not be fetching dvals via the
// OCaml runtime, but always via HTTP or via the DB.
// ---------------------------
let ofInternalQueryableV0 (str : string) : Task<RT.Dval> =
  stringToDvalReq "fuzzing/of_internal_queryable_v0" str

let ofInternalQueryableV1 (str : string) : Task<RT.Dval> =
  stringToDvalReq "fuzzing/of_internal_queryable_v1" str

let ofInternalRoundtrippableV0 (str : string) : Task<RT.Dval> =
  stringToDvalReq "fuzzing/of_internal_roundtrippable_v0" str

let ofUnknownJsonV1 (str : string) : Task<RT.Dval> =
  stringToDvalReq "fuzzing/of_unknown_json_v1" str

let toDeveloperRepr (dv : RT.Dval) : Task<string> =
  dvalToStringReq "fuzzing/to_developer_repr_v0" dv

let toEnduserReadableTextV0 (dv : RT.Dval) : Task<string> =
  dvalToStringReq "fuzzing/to_enduser_readable_text_v0" dv

let toHashableRepr (dv : RT.Dval) : Task<string> =
  dvalToStringReq "fuzzing/to_hashable_repr" dv

let toInternalQueryableV0 (dv : RT.Dval) : Task<string> =
  dvalToStringReq "fuzzing/to_internal_queryable_v0" dv

let toInternalQueryableV1 (dv : RT.Dval) : Task<string> =
  dvalToStringReq "fuzzing/to_internal_queryable_v1" dv

let toInternalRoundtrippableV0 (dv : RT.Dval) : Task<string> =
  dvalToStringReq "fuzzing/to_internal_roundtrippable_v0" dv

let toPrettyMachineJsonV1 (dv : RT.Dval) : Task<string> =
  dvalToStringReq "fuzzing/to_pretty_machine_json_v1" dv

let toUrlString (dv : RT.Dval) : Task<string> =
  dvalToStringReq "fuzzing/to_url_string" dv

let hashV0 (l : List<RT.Dval>) : Task<string> =
  dvalListToStringReq "fuzzing/hash_v0" l

let hashV1 (l : List<RT.Dval>) : Task<string> =
  dvalListToStringReq "fuzzing/hash_v1" l

let execute
  (ownerID : UserID)
  (canvasID : CanvasID)
  (program : PT.Expr)
  (symtable : Map<string, RT.Dval>)
  (dbs : List<PT.DB.T>)
  (fns : List<PT.UserFunction.T>)
  : Task<RT.Dval> =
  let program = Convert.pt2ocamlExpr program

  let args =
    symtable |> Map.toList |> List.map (fun (k, dv) -> (k, Convert.rt2ocamlDval dv))

  let dbs = List.map Convert.pt2ocamlDB dbs
  let fns = List.map Convert.pt2ocamlUserFunction fns

  let str =
    Json.OCamlCompatible.serialize ((ownerID, canvasID, program, args, dbs, fns))

  stringToDvalReq "execute" str
