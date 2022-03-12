module LibExecution.OCamlTypes

// Interoperation types with OCaml.

// Programs are stored using an OCaml-only serialization format, so we have to
// call OCaml code to fetch it and save it.

// These types come directly from OCaml, and are used for automatic json
// serializers, which match the Yojson derived serializers on the OCaml side.

// We also use these types to convert to the types the API uses, which are
// typically direct deserializations of these types.

// fsharplint:disable FL0038


open Prelude
open VendoredTablecloth

module PT = LibExecution.ProgramTypes
module RT = RuntimeTypes
module PT2RT = LibExecution.ProgramTypesToRuntimeTypes

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
  | TUserType of string * int64
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
      { starting_version : int64
        version : int64
        state : db_migration_state
        rollforward : 'expr_type
        rollback : 'expr_type
        cols : col list }

    type 'expr_type db =
      { tlid : tlid
        name : string or_blank
        cols : col list
        version : int64
        old_migrations : 'expr_type db_migration list
        active_migration : 'expr_type db_migration option }

  module HandlerT =
    type dtdeprecated = int64 or_blank

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
      version : int64
      definition : user_tipe_definition }

  type tldata =
    | Handler of HandlerT.handler<fluidExpr>
    | DB of DbT.db<fluidExpr>

  type toplevel = { tlid : id; pos : pos; data : tldata }

  type toplevels = List<toplevel>

  type dval_map = Map<string, dval>

  and dhttp =
    | Redirect of string
    | Response of int64 * (string * string) list

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
    | DDate of RT.DDateTime.T
    | DPassword of Password // We dont use this path for testing, see DvalReprExternal.Tests
    | DUuid of System.Guid
    | DOption of optionT
    | DCharacter of string
    | DResult of resultT
    | DBytes of byte []

type secret = { secret_name : string; secret_value : string }

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
  module ORT = RuntimeT

  // ----------------
  // OCaml to ProgramTypes
  // ----------------
  let bo2Option (bo : 'a or_blank) : Option<'a> =
    match bo with
    | Partial (_, s) -> None
    | Filled (_, s) -> Some s
    | Blank (_) -> None

  let bo2ID (bo : 'a or_blank) : id =
    match bo with
    | Partial (id, _) -> id
    | Filled (id, _) -> id
    | Blank (id) -> id

  let bo2String (bo : string or_blank) : string =
    match bo with
    | Partial (_, s) -> s
    | Filled (_, s) -> s
    | Blank (_) -> ""

  let rec ocamlPattern2PT (o : ORT.fluidPattern) : PT.Pattern =
    let r = ocamlPattern2PT

    match o with
    | ORT.FPVariable (_, id, str) -> PT.PVariable(id, str)
    | ORT.FPConstructor (_, id, name, pats) ->
      PT.PConstructor(id, name, List.map r pats)
    | ORT.FPInteger (_, id, i) -> PT.PInteger(id, parseInt64 i)
    | ORT.FPBool (_, id, b) -> PT.PBool(id, b)
    | ORT.FPString fp -> PT.PString(fp.patternID, fp.str)
    | ORT.FPFloat (_, id, w, f) ->
      let sign, whole =
        if w[0] = '-' then (Negative, String.dropLeft 1 w) else Positive, w
      PT.PFloat(id, sign, whole, f)
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
    | ORT.EInteger (id, num) -> PT.EInteger(id, parseInt64 num)
    | ORT.EString (id, str) -> PT.EString(id, str)
    | ORT.EFloat (id, w, f) ->
      let sign, whole =
        if w.Length > 0 && w[0] = '-' then
          (Negative, String.dropLeft 1 w)
        else
          Positive, w
      PT.EFloat(id, sign, whole, f)
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
    | ORT.ERecord (id, pairs) -> PT.ERecord(id, List.map (Tuple2.mapSecond r) pairs)
    | ORT.EPipe (id, expr1 :: expr2 :: rest) ->
      PT.EPipe(id, r expr1, r expr2, List.map r rest)
    | ORT.EPipe (id, [ expr ]) -> r expr
    | ORT.EPipe (id, []) ->
      Exception.raiseInternal "Invalid pipe" [ "expr", o; "id", id ]
    | ORT.EConstructor (id, name, exprs) ->
      PT.EConstructor(id, name, List.map r exprs)
    | ORT.EMatch (id, mexpr, pairs) ->
      PT.EMatch(
        id,
        r mexpr,
        List.map ((Tuple2.mapFirst ocamlPattern2PT) << (Tuple2.mapSecond r)) pairs
      )
    | ORT.EPipeTarget id -> PT.EPipeTarget id
    | ORT.EFeatureFlag (id, name, cond, caseA, caseB) ->
      PT.EFeatureFlag(id, name, r cond, r caseA, r caseB)


  let ocamlexprTLIDPair2PT ((expr, tlid) : (ORT.fluidExpr * tlid)) : PT.Expr * tlid =
    (ocamlExpr2PT expr, tlid)

  let ocamlSpec2PT (o : ORT.HandlerT.spec) : PT.Handler.Spec =
    let ids : PT.Handler.ids =
      { moduleID = bo2ID o.``module``
        nameID = bo2ID o.name
        modifierID = bo2ID o.modifier }

    match o.``module``, bo2String o.name, bo2String o.modifier with
    | Filled (_, "HTTP"), route, method -> PT.Handler.HTTP(route, method, ids)
    | Filled (_, "WORKER"), name, _ -> PT.Handler.Worker(name, ids)
    | Filled (_, "CRON"), name, interval ->
      PT.Handler.Cron(name, PT.Handler.CronInterval.parse interval, ids)
    | Filled (_, "REPL"), name, _ -> PT.Handler.REPL(name, ids)
    | Filled (_, workerName), name, _ -> PT.Handler.OldWorker(workerName, name, ids)
    | Partial (_, _), name, modifier
    | Blank _, name, modifier -> PT.Handler.UnknownHandler(name, modifier, ids)

  let ocamlHandler2PT
    (pos : pos)
    (o : ORT.HandlerT.handler<ORT.fluidExpr>)
    : PT.Handler.T =
    { tlid = o.tlid
      ast = ocamlExpr2PT o.ast
      spec = ocamlSpec2PT o.spec
      pos = pos }


  let rec ocamlTipe2PT (o : tipe) : PT.DType =
    let any = PT.TVariable "a"

    match o with
    | TAny -> any
    | TInt -> PT.TInt
    | TFloat -> PT.TFloat
    | TBool -> PT.TBool
    | TNull -> PT.TNull
    | TDeprecated1 -> any
    | TStr -> PT.TStr
    | TList -> PT.TList any
    | TObj -> PT.TDict any
    | TIncomplete -> PT.TIncomplete
    | TError -> PT.TError
    | TBlock -> PT.TFn([ any ], any)
    | TResp -> PT.THttpResponse any
    | TDB -> PT.TDB any
    | TDeprecated6 -> any
    | TDate -> PT.TDate
    | TDeprecated2 -> any
    | TDeprecated3 -> any
    | TDeprecated4 string -> any
    | TDeprecated5 string -> any
    | TDbList tipe -> PT.TDbList(ocamlTipe2PT tipe)
    | TPassword -> PT.TPassword
    | TUuid -> PT.TUuid
    | TOption -> PT.TOption any
    | TErrorRail -> PT.TErrorRail
    | TCharacter -> PT.TChar
    | TResult -> PT.TResult(any, any)
    | TUserType (name, version) -> PT.TUserType(name, int version)
    | TBytes -> PT.TBytes

  let ocamlDBCol2PT ((name, tipe) : ORT.DbT.col) : PT.DB.Col =
    { nameID = bo2ID name
      name = bo2Option name
      typ = bo2Option tipe |> Option.map ocamlTipe2PT
      typeID = bo2ID tipe }

  let ocamlDB2PT (pos : pos) (o : ORT.DbT.db<ORT.fluidExpr>) : PT.DB.T =
    { tlid = o.tlid
      name = bo2String o.name
      nameID = bo2ID o.name
      pos = pos
      cols = List.map ocamlDBCol2PT o.cols
      version = int o.version }

  let ocamlUserType2PT (o : ORT.user_tipe) : PT.UserType.T =
    { tlid = o.tlid
      name = o.name |> bo2String
      nameID = bo2ID o.name
      version = int o.version
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

  let ocamlOp2PT (o : op<ORT.fluidExpr>) : PT.Op =
    match o with
    | SetHandler (tlid, pos, handler) ->
      PT.SetHandler(tlid, pos, ocamlHandler2PT pos handler)
    | CreateDB (tlid, pos, name) -> PT.CreateDB(tlid, pos, name)
    | AddDBCol (tlid, id1, id2) -> PT.AddDBCol(tlid, id1, id2)
    | SetDBColName (tlid, id, name) -> PT.SetDBColName(tlid, id, name)
    | SetDBColType (tlid, id, string) -> PT.SetDBColType(tlid, id, string)
    | DeleteTL tlid -> PT.DeleteTL tlid
    | MoveTL (tlid, pos) -> PT.MoveTL(tlid, pos)
    | SetFunction fn -> PT.SetFunction(ocamlUserFunction2PT fn)
    | ChangeDBColName (tlid, id, string) -> PT.ChangeDBColName(tlid, id, string)
    | ChangeDBColType (tlid, id, string) -> PT.ChangeDBColType(tlid, id, string)
    | UndoTL tlid -> PT.UndoTL tlid
    | RedoTL tlid -> PT.RedoTL tlid
    | DeprecatedInitDbm (tlid, id1, id2, id3, kind) ->
      PT.DeprecatedInitDBm(tlid, id1, id2, id3, PT.DeprecatedMigrationKind)
    | SetExpr (tlid, id, e) -> PT.SetExpr(tlid, id, ocamlExpr2PT e)
    | TLSavepoint tlid -> PT.TLSavepoint tlid
    | DeleteFunction tlid -> PT.DeleteFunction tlid
    | CreateDBMigration (tlid, id1, id2, rollingFns) ->
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
    | AddDBColToDBMigration (tlid, id1, id2) ->
      PT.AddDBColToDBMigration(tlid, id1, id2)
    | SetDBColNameInDBMigration (tlid, id, name) ->
      PT.SetDBColNameInDBMigration(tlid, id, name)
    | SetDBColTypeInDBMigration (tlid, id, tipe) ->
      PT.SetDBColTypeInDBMigration(tlid, id, tipe)
    | AbandonDBMigration tlid -> PT.AbandonDBMigration tlid
    | DeleteColInDBMigration (tlid, id) -> PT.DeleteColInDBMigration(tlid, id)
    | DeleteDBCol (tlid, id) -> PT.DeleteDBCol(tlid, id)
    | RenameDBname (tlid, string) -> PT.RenameDBname(tlid, string)
    | CreateDBWithBlankOr (tlid, pos, id, string) ->
      PT.CreateDBWithBlankOr(tlid, pos, id, string)
    | DeleteTLForever tlid -> PT.DeleteTLForever tlid
    | DeleteFunctionForever tlid -> PT.DeleteFunctionForever tlid
    | SetType tipe -> PT.SetType(ocamlUserType2PT tipe)
    | DeleteType tlid -> PT.DeleteType tlid
    | DeleteTypeForever tlid -> PT.DeleteTypeForever tlid


  let ocamlOplist2PT (list : oplist<ORT.fluidExpr>) : PT.Oplist =
    List.map ocamlOp2PT list

  let ocamlTLIDOplist2PT
    ((tlid, oplist) : tlid_oplist<ORT.fluidExpr>)
    : Prelude.tlid * PT.Oplist =
    (tlid, ocamlOplist2PT oplist)


  let ocamlToplevel2PT
    (list : List<ORT.toplevel>)
    : List<PT.Handler.T> * List<PT.DB.T> =
    list
    |> List.fold ([], []) (fun (hs, dbs) tl ->
      match tl.data with
      | ORT.Handler h -> (hs @ [ ocamlHandler2PT tl.pos h ], dbs)
      | ORT.DB db -> (hs, dbs @ [ ocamlDB2PT tl.pos db ]))

  module BSTypes =
    type tl =
      | Handler of ORT.HandlerT.handler<ORT.fluidExpr>
      | DB of ORT.DbT.db<ORT.fluidExpr>
      | UserType of ORT.user_tipe
      | UserFn of ORT.user_fn<ORT.fluidExpr>


  let ocamlBinarySerializationToplevel2PT
    (pos : pos)
    (tl : BSTypes.tl)
    : PT.Toplevel =
    match tl with
    | BSTypes.Handler h -> PT.TLHandler(ocamlHandler2PT pos h)
    | BSTypes.DB db -> PT.TLDB(ocamlDB2PT pos db)
    | BSTypes.UserType ut -> PT.TLType(ocamlUserType2PT ut)
    | BSTypes.UserFn uf -> PT.TLFunction(ocamlUserFunction2PT uf)



  // ----------------
// ProgramTypes to OCaml
// ----------------
  let string2bo (id : id) (str : string) : (string or_blank) =
    if str = "" then Blank id else Filled(id, str)

  let option2bo (id : id) (o : Option<'a>) : 'a or_blank =
    match o with
    | None -> Blank id
    | Some v -> Filled(id, v)

  let rec pt2ocamlPattern (mid : id) (p : PT.Pattern) : ORT.fluidPattern =
    let r = pt2ocamlPattern mid

    match p with
    | PT.PVariable (id, str) -> ORT.FPVariable(mid, id, str)
    | PT.PConstructor (id, name, pats) ->
      ORT.FPConstructor(mid, id, name, List.map r pats)
    | PT.PInteger (id, i) -> ORT.FPInteger(mid, id, string i)
    | PT.PCharacter (id, c) ->
      Exception.raiseInternal "Character patterns not supported" [ "id", id; "c", c ]
    | PT.PBool (id, b) -> ORT.FPBool(mid, id, b)
    | PT.PString (id, s) -> ORT.FPString { matchID = mid; patternID = id; str = s }
    | PT.PFloat (id, Positive, w, f) -> ORT.FPFloat(mid, id, string w, string f)
    | PT.PFloat (id, Negative, w, f) -> ORT.FPFloat(mid, id, $"-{w}", string f)
    | PT.PNull (id) -> ORT.FPNull(mid, id)
    | PT.PBlank (id) -> ORT.FPBlank(mid, id)

  let rec rt2ocamlPattern (mid : id) (p : RT.Pattern) : ORT.fluidPattern =
    let r = rt2ocamlPattern mid

    match p with
    | RT.PVariable (id, str) -> ORT.FPVariable(mid, id, str)
    | RT.PConstructor (id, name, pats) ->
      ORT.FPConstructor(mid, id, name, List.map r pats)
    | RT.PInteger (id, i) -> ORT.FPInteger(mid, id, string i)
    | RT.PCharacter (id, c) ->
      Exception.raiseInternal "Character patterns not supported" [ "id", id; "c", c ]
    | RT.PBool (id, b) -> ORT.FPBool(mid, id, b)
    | RT.PString (id, s) -> ORT.FPString { matchID = mid; patternID = id; str = s }
    | RT.PFloat (id, d) ->
      // CLEANUP: doesn't support -0.5
      let s, w, f = readFloat d
      let w = if s = Positive then string w else $"-{w}"
      ORT.FPFloat(mid, id, w, string f)
    | RT.PNull (id) -> ORT.FPNull(mid, id)
    | RT.PBlank (id) -> ORT.FPBlank(mid, id)



  let rec pt2ocamlSter (p : PT.SendToRail) : ORT.sendToRail =
    match p with
    | PT.Rail -> ORT.Rail
    | PT.NoRail -> ORT.NoRail

  let rec rt2ocamlSter (p : RT.SendToRail) : ORT.sendToRail =
    match p with
    | RT.Rail -> ORT.Rail
    | RT.NoRail -> ORT.NoRail



  let rec pt2ocamlExpr (p : PT.Expr) : ORT.fluidExpr =
    let r = pt2ocamlExpr

    match p with
    | PT.EBlank id -> ORT.EBlank id
    | PT.EInteger (id, num) -> ORT.EInteger(id, string num)
    | PT.ECharacter (id, c) ->
      Exception.raiseInternal "Characters not supported" [ "id", id; "c", c ]
    | PT.EString (id, str) -> ORT.EString(id, str)
    | PT.EFloat (id, Positive, w, f) -> ORT.EFloat(id, string w, string f)
    | PT.EFloat (id, Negative, w, f) -> ORT.EFloat(id, $"-{w}", string f)
    | PT.EBool (id, b) -> ORT.EBool(id, b)
    | PT.ENull id -> ORT.ENull id
    | PT.EVariable (id, var) -> ORT.EVariable(id, var)
    | PT.EFieldAccess (id, obj, fieldname) -> ORT.EFieldAccess(id, r obj, fieldname)
    | PT.EFnCall (id, name, args, ster) ->
      let name =
        if string name = "JSON::parse" || string name = "DB::add" then
          // Some things were named wrong in OCaml
          $"{name}_v0"
        else
          match name with
          | RT.FQFnName.Stdlib _
          | RT.FQFnName.User _ -> string name |> String.replace "_v0" ""
          // Keep the _v0 here
          | RT.FQFnName.Package _ -> string name

      ORT.EFnCall(id, name, List.map r args, pt2ocamlSter ster)
    | PT.EBinOp (id, name, arg1, arg2, ster) ->
      ORT.EBinOp(
        id,
        name |> string |> String.replace "_v0" "",
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
    | PT.ERecord (id, pairs) -> ORT.ERecord(id, List.map (Tuple2.mapSecond r) pairs)
    | PT.EPipe (id, expr1, expr2, rest) ->
      ORT.EPipe(id, r expr1 :: r expr2 :: List.map r rest)
    | PT.EConstructor (id, name, exprs) ->
      ORT.EConstructor(id, name, List.map r exprs)
    | PT.EMatch (id, mexpr, pairs) ->
      ORT.EMatch(
        id,
        r mexpr,
        List.map
          ((Tuple2.mapFirst (pt2ocamlPattern id)) << (Tuple2.mapSecond r))
          pairs
      )
    | PT.EPipeTarget id -> ORT.EPipeTarget id
    | PT.EFeatureFlag (id, name, cond, caseA, caseB) ->
      ORT.EFeatureFlag(id, name, r cond, r caseA, r caseB)

  let rec rt2ocamlExpr (e : RT.Expr) : ORT.fluidExpr =
    let r = rt2ocamlExpr

    match e with
    | RT.EBlank id -> ORT.EBlank id
    | RT.EInteger (id, num) -> ORT.EInteger(id, string num)
    | RT.ECharacter (id, c) ->
      Exception.raiseInternal "Characters not supported" [ "id", id; "c", c ]
    | RT.EString (id, str) -> ORT.EString(id, str)
    | RT.EFloat (id, d) ->
      let (sign, whole, fraction) = readFloat d
      let whole = if sign = Positive then string whole else $"-{whole}"
      ORT.EFloat(id, whole, string fraction)
    | RT.EBool (id, b) -> ORT.EBool(id, b)
    | RT.ENull id -> ORT.ENull id
    | RT.EVariable (id, var) -> ORT.EVariable(id, var)
    | RT.EFieldAccess (id, obj, fieldname) -> ORT.EFieldAccess(id, r obj, fieldname)
    | RT.ELambda (id, vars, body) -> ORT.ELambda(id, vars, r body)
    | RT.ELet (id, lhs, rhs, body) -> ORT.ELet(id, lhs, r rhs, r body)
    | RT.EIf (id, cond, thenExpr, elseExpr) ->
      ORT.EIf(id, r cond, r thenExpr, r elseExpr)
    | RT.EPartial (id, oldExpr) -> ORT.EPartial(id, "partial", r oldExpr)
    | RT.EList (id, exprs) -> ORT.EList(id, List.map r exprs)
    | RT.ERecord (id, pairs) -> ORT.ERecord(id, List.map (Tuple2.mapSecond r) pairs)
    | RT.EConstructor (id, name, exprs) ->
      ORT.EConstructor(id, name, List.map r exprs)
    | RT.EMatch (id, mexpr, pairs) ->
      ORT.EMatch(
        id,
        r mexpr,
        List.map
          ((Tuple2.mapFirst (rt2ocamlPattern id)) << (Tuple2.mapSecond r))
          pairs
      )
    | RT.EFeatureFlag (id, cond, caseA, caseB) ->
      ORT.EFeatureFlag(id, "flag", r cond, r caseA, r caseB)
    | RT.EApply (id, RT.EFQFnValue (_, name), args, RT.NotInPipe, rail) ->
      let name =
        if string name = "JSON::parse" || string name = "DB::add" then
          // Some things were named wrong in OCaml
          $"{name}_v0"
        else
          string name |> String.replace "_v0" ""


      ORT.EFnCall(id, name, List.map r args, rt2ocamlSter rail)
    | expr ->
      Exception.raiseInternal "TODO: add more cases to rt2ocamlExpr" [ "expr", expr ]



  let pt2ocamlexprTLIDPair ((expr, tlid) : (PT.Expr * tlid)) : ORT.fluidExpr * tlid =
    (pt2ocamlExpr expr, tlid)


  let pt2ocamlSpec (p : PT.Handler.Spec) : ORT.HandlerT.spec =
    let types : ORT.HandlerT.spec_types =
      { input = Blank(gid ()); output = Blank(gid ()) }

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
        modifier =
          interval
          |> Option.map string
          |> Option.defaultValue ""
          |> string2bo ids.modifierID
        types = types }
    | PT.Handler.REPL (name, ids) ->
      { ``module`` = string2bo ids.moduleID "REPL"
        name = string2bo ids.nameID name
        modifier = string2bo ids.modifierID "_"
        types = types }
    | PT.Handler.OldWorker (workerName, name, ids) ->
      { ``module`` = string2bo ids.moduleID workerName
        name = string2bo ids.nameID name
        modifier = Blank ids.modifierID
        types = types }
    | PT.Handler.UnknownHandler (name, modifier, ids) ->
      { ``module`` = Blank ids.moduleID
        name = string2bo ids.nameID name
        modifier = string2bo ids.modifierID modifier
        types = types }




  let pt2ocamlHandler (p : PT.Handler.T) : ORT.HandlerT.handler<ORT.fluidExpr> =
    { tlid = p.tlid; ast = pt2ocamlExpr p.ast; spec = pt2ocamlSpec p.spec }

  let rec pt2ocamlTipe (p : PT.DType) : tipe =
    match p with
    | PT.TVariable _ -> TAny
    | PT.TInt -> TInt
    | PT.TFloat -> TFloat
    | PT.TFn _ -> TBlock
    | PT.TBool -> TBool
    | PT.TNull -> TNull
    | PT.TStr -> TStr
    | PT.TList _ -> TList
    | PT.TRecord _ -> TObj
    | PT.TIncomplete -> TIncomplete
    | PT.TError -> TError
    | PT.THttpResponse _ -> TResp
    | PT.TDB _ -> TDB
    | PT.TDate -> TDate
    | PT.TDict _ -> TObj
    | PT.TDbList tipe -> TDbList(pt2ocamlTipe tipe)
    | PT.TPassword -> TPassword
    | PT.TUuid -> TUuid
    | PT.TOption _ -> TOption
    | PT.TErrorRail -> TErrorRail
    | PT.TChar -> TCharacter
    | PT.TResult _ -> TResult
    | PT.TUserType (name, version) -> TUserType(name, int64 version)
    | PT.TBytes -> TBytes

  let pt2ocamlDBCol (p : PT.DB.Col) : ORT.DbT.col =
    (option2bo p.nameID p.name, option2bo p.typeID (Option.map pt2ocamlTipe p.typ))

  let pt2ocamlDB (p : PT.DB.T) : ORT.DbT.db<ORT.fluidExpr> =
    { tlid = p.tlid
      name = string2bo p.nameID p.name
      cols = List.map pt2ocamlDBCol p.cols
      version = int64 p.version
      old_migrations = []
      active_migration = None }


  let pt2ocamlUserType (p : PT.UserType.T) : ORT.user_tipe =
    { tlid = p.tlid
      name = p.name |> string2bo p.nameID
      version = int64 p.version
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
      // is only ever empty list and it's no longer used in F# backend
      block_args = [] }

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

  let pt2ocamlOp (p : PT.Op) : op<ORT.fluidExpr> =
    match p with
    | PT.SetHandler (tlid, pos, handler) ->
      SetHandler(tlid, pos, pt2ocamlHandler handler)
    | PT.CreateDB (tlid, pos, name) -> CreateDB(tlid, pos, name)
    | PT.AddDBCol (tlid, id1, id2) -> AddDBCol(tlid, id1, id2)
    | PT.SetDBColName (tlid, id, name) -> SetDBColName(tlid, id, name)
    | PT.SetDBColType (tlid, id, string) -> SetDBColType(tlid, id, string)
    | PT.DeleteTL tlid -> DeleteTL tlid
    | PT.MoveTL (tlid, pos) -> MoveTL(tlid, pos)
    | PT.SetFunction fn -> SetFunction(pt2ocamlUserFunction fn)
    | PT.ChangeDBColName (tlid, id, string) -> ChangeDBColName(tlid, id, string)
    | PT.ChangeDBColType (tlid, id, string) -> ChangeDBColType(tlid, id, string)
    | PT.UndoTL tlid -> UndoTL tlid
    | PT.RedoTL tlid -> RedoTL tlid
    | PT.SetExpr (tlid, id, e) -> SetExpr(tlid, id, pt2ocamlExpr e)
    | PT.TLSavepoint tlid -> TLSavepoint tlid
    | PT.DeleteFunction tlid -> DeleteFunction tlid
    | PT.CreateDBMigration (tlid, id1, id2, rollingFns) ->
      CreateDBMigration(
        tlid,
        id1,
        id2,
        List.map
          (fun (s1, id1, s2, id2) -> (string2bo id1 s1, string2bo id2 s2))
          rollingFns
      )
    | PT.AddDBColToDBMigration (tlid, id1, id2) ->
      AddDBColToDBMigration(tlid, id1, id2)
    | PT.SetDBColNameInDBMigration (tlid, id, name) ->
      SetDBColNameInDBMigration(tlid, id, name)
    | PT.SetDBColTypeInDBMigration (tlid, id, tipe) ->
      SetDBColTypeInDBMigration(tlid, id, tipe)
    | PT.AbandonDBMigration tlid -> AbandonDBMigration tlid
    | PT.DeleteColInDBMigration (tlid, id) -> DeleteColInDBMigration(tlid, id)
    | PT.DeprecatedInitDBm (tlid, id1, id2, id3, kind) ->
      DeprecatedInitDbm(tlid, id1, id2, id3, RuntimeT.DbT.DeprecatedMigrationKind)
    | PT.DeleteDBCol (tlid, id) -> DeleteDBCol(tlid, id)
    | PT.RenameDBname (tlid, string) -> RenameDBname(tlid, string)
    | PT.CreateDBWithBlankOr (tlid, pos, id, string) ->
      CreateDBWithBlankOr(tlid, pos, id, string)
    | PT.DeleteTLForever tlid -> DeleteTLForever tlid
    | PT.DeleteFunctionForever tlid -> DeleteFunctionForever tlid
    | PT.SetType tipe -> SetType(pt2ocamlUserType tipe)
    | PT.DeleteType tlid -> DeleteType tlid
    | PT.DeleteTypeForever tlid -> DeleteTypeForever tlid


  let pt2ocamlOplist (list : PT.Oplist) : oplist<ORT.fluidExpr> =
    List.map pt2ocamlOp list

  let pt2ocamlToplevels
    (toplevels : Map<tlid, PT.Toplevel>)
    : ORT.toplevels * ORT.user_fn<ORT.fluidExpr> list * ORT.user_tipe list =
    toplevels
    |> Map.values
    |> List.ofSeq
    |> List.fold ([], [], []) (fun (tls, ufns, uts) tl ->
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
    (o : PackageManager.parameter)
    : PT.Package.Parameter =
    { name = o.name; description = o.description; typ = ocamlTipe2PT o.tipe }

  let pt2ocamlPackageManagerParameter
    (p : PT.Package.Parameter)
    : PackageManager.parameter =
    { name = p.name; description = p.description; tipe = pt2ocamlTipe p.typ }


  // let ocamlPackageManagerFn2PT (o : PackageManager.fn) : PT.PackageManager.Fn =
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
  let pt2ocamlPackageManagerFn (p : PT.Package.Fn) : PackageManager.fn =
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
    | RT.DFnVal (RT.FnName _) -> ORT.DNull // ignore these intermediate values
    | RT.DFnVal (RT.Lambda args) ->
      ORT.DBlock
        { ``params`` = args.parameters
          symtable = Map.map c args.symtable
          body = rt2ocamlExpr args.body }
    | RT.DIncomplete RT.SourceNone -> ORT.DIncomplete ORT.SourceNone
    | RT.DIncomplete (RT.SourceID (tlid, id)) ->
      ORT.DIncomplete(ORT.SourceId(tlid, id))
    | RT.DError (RT.SourceNone, msg) -> ORT.DError(ORT.SourceNone, msg)
    | RT.DError (RT.SourceID (tlid, id), msg) ->
      ORT.DError(ORT.SourceId(tlid, id), msg)
    | RT.DDate d -> ORT.DDate d
    | RT.DDB name -> ORT.DDB name
    | RT.DUuid uuid -> ORT.DUuid uuid
    | RT.DPassword pw -> ORT.DPassword pw
    | RT.DHttpResponse (RT.Redirect url) -> ORT.DResp(ORT.Redirect url, c RT.DNull)
    | RT.DHttpResponse (RT.Response (code, headers, hdv)) ->
      ORT.DResp(ORT.Response(int64 code, headers), c hdv)
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
    | ORT.DInt i -> RT.DInt i
    | ORT.DBool true -> RT.DBool true
    | ORT.DBool false -> RT.DBool false
    | ORT.DFloat f -> RT.DFloat f
    | ORT.DNull -> RT.DNull
    | ORT.DBlock (args) ->
      RT.DFnVal(
        RT.Lambda
          { parameters = args.``params``
            symtable = Map.map c args.symtable
            body = args.body |> ocamlExpr2PT |> PT2RT.Expr.toRT }
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
    | ORT.DPassword pw -> RT.DPassword pw
    | ORT.DResp (ORT.Redirect url, _) -> RT.DHttpResponse(RT.Redirect url)
    | ORT.DResp (ORT.Response (code, headers), hdv) ->
      RT.DHttpResponse(RT.Response(code, headers, c hdv))
    | ORT.DList l -> RT.DList(List.map c l)
    | ORT.DObj o -> RT.DObj(Map.map c o)
    | ORT.DOption ORT.OptNothing -> RT.DOption None
    | ORT.DOption (ORT.OptJust dv) -> RT.DOption(Some(c dv))
    | ORT.DResult (ORT.ResOk dv) -> RT.DResult(Ok(c dv))
    | ORT.DResult (ORT.ResError dv) -> RT.DResult(Error(c dv))
    | ORT.DErrorRail dv -> RT.DErrorRail(c dv)
    | ORT.DBytes bytes -> RT.DBytes bytes
