open Prelude
open Json.Encode

(* Dark *)

(* XXX(JULIAN): All of this should be cleaned up and moved somewhere nice! *)
type jsArrayBuffer = {byteLength : int} [@@bs.deriving abstract]

type jsUint8Array [@@bs.deriving abstract]

external createUint8Array : int -> jsUint8Array = "Uint8Array" [@@bs.new]

external setUint8ArrayIdx : jsUint8Array -> int -> int -> unit = ""
  [@@bs.set_index]

let dark_arrayBuffer_to_b64url =
  [%raw
    {|
  function (arraybuffer) {
    // TODO(JULIAN): Actually import https://github.com/niklasvh/base64-arraybuffer/blob/master/lib/base64-arraybuffer.js as a lib and use encode here
    var chars = "ABCDEFGHIJKLMNOPQRSTUVWXYZabcdefghijklmnopqrstuvwxyz0123456789-_";

    // Use a lookup table to find the index.
    var lookup = new Uint8Array(256);
    for (var i = 0; i < chars.length; i++) {
      lookup[chars.charCodeAt(i)] = i;
    }

      var bytes = new Uint8Array(arraybuffer),
      i, len = bytes.length, base64 = "";

      for (i = 0; i < len; i+=3) {
        base64 += chars[bytes[i] >> 2];
        base64 += chars[((bytes[i] & 3) << 4) | (bytes[i + 1] >> 4)];
        base64 += chars[((bytes[i + 1] & 15) << 2) | (bytes[i + 2] >> 6)];
        base64 += chars[bytes[i + 2] & 63];
      }

      if ((len % 3) === 2) {
        base64 = base64.substring(0, base64.length - 1) + "=";
      } else if (len % 3 === 1) {
        base64 = base64.substring(0, base64.length - 2) + "==";
      }

      return base64;
  }
|}]


let _bytes_to_uint8Array (input : Bytes.t) : jsUint8Array =
  let len = Bytes.length input in
  let buf = createUint8Array len in
  for i = 0 to len - 1 do
    setUint8ArrayIdx buf i (int_of_char (Bytes.get input i))
  done ;
  buf


let base64url_bytes (input : Bytes.t) : string =
  input |> _bytes_to_uint8Array |> dark_arrayBuffer_to_b64url


(* Don't attempt to encode these as integers, because we're not capable
 * of expressing all existing ids as ints because bucklescript is strict
 * about int == 32 bit. As far as we're concerned, ids are strings and
 * we know nothing about their parseability as ints *)
let id (Types.ID id) = string id

let tlid (Types.TLID tlid) = string tlid

let pos (p : Types.pos) = object_ [("x", int p.x); ("y", int p.y)]

let vPos (vp : Types.vPos) = object_ [("vx", int vp.vx); ("vy", int vp.vy)]

let blankOr (encoder : 'a -> Js.Json.t) (v : 'a Types.blankOr) =
  match v with
  | F (i, s) ->
      variant "Filled" [id i; encoder s]
  | Blank i ->
      variant "Blank" [id i]


let rec dval (dv : Types.dval) : Js.Json.t =
  let ev = variant in
  let dhttp h =
    match h with
    | Redirect s ->
        ev "Redirect" [string s]
    | Response (code, headers) ->
        ev "Response" [int code; list (tuple2 string string) headers]
  in
  match dv with
  | DInt i ->
      ev "DInt" [int i]
  | DFloat f ->
      ev "DFloat" [Json.Encode.float f]
  | DBool b ->
      ev "DBool" [bool b]
  | DNull ->
      ev "DNull" []
  | DStr s ->
      ev "DStr" [string s]
  | DList l ->
      ev "DList" [array dval l]
  | DObj o ->
      o
      |> StrDict.map ~f:dval
      |> StrDict.toList
      |> Js.Dict.fromList
      |> jsonDict
      |> fun x -> [x] |> ev "DObj"
  | DBlock {body; params; symtable} ->
      let dblock_args =
        object_
          [ ("symtable", tcStrDict dval symtable)
          ; ("params", list (pair id string) params)
          ; ("body", body |> OldExpr.fromFluidExpr |> expr) ]
      in
      ev "DBlock" [dblock_args]
  | DIncomplete SourceNone ->
      ev "DIncomplete" [ev "SourceNone" []]
  | DIncomplete (SourceId i) ->
      ev "DIncomplete" [ev "SourceId" [id i]]
  (* user-ish types *)
  | DCharacter c ->
      ev "DCharacter" [string c]
  | DError (SourceNone, msg) ->
      ev "DError" [pair (ev "SourceNone") string ([], msg)]
  | DError (SourceId i, msg) ->
      ev "DError" [pair (ev "SourceId") string ([id i], msg)]
  | DResp (h, hdv) ->
      ev "DResp" [tuple2 dhttp dval (h, hdv)]
  | DDB name ->
      ev "DDB" [string name]
  | DDate date ->
      ev "DDate" [string date]
  | DPassword hashed ->
      ev "DPassword" [string hashed]
  | DUuid uuid ->
      ev "DUuid" [string uuid]
  | DOption opt ->
      ev
        "DOption"
        [ ( match opt with
          | OptNothing ->
              ev "OptNothing" []
          | OptJust dv ->
              ev "OptJust" [dval dv] ) ]
  | DErrorRail dv ->
      ev "DErrorRail" [dval dv]
  | DResult res ->
      ev
        "DResult"
        [ ( match res with
          | ResOk dv ->
              ev "ResOk" [dval dv]
          | ResError dv ->
              ev "ResError" [dval dv] ) ]
  | DBytes bin ->
      ev "DBytes" [string (bin |> base64url_bytes)]


and blankOrData (pd : Types.blankOrData) : Js.Json.t =
  let ev = variant in
  match pd with
  | PEventName name ->
      ev "PEventName" [blankOr string name]
  | PEventModifier modifier ->
      ev "PEventModifier" [blankOr string modifier]
  | PEventSpace space ->
      ev "PEventSpace" [blankOr string space]
  | PDBName name ->
      ev "PDBName" [blankOr string name]
  | PDBColName colname ->
      ev "PDBColName" [blankOr string colname]
  | PDBColType coltype ->
      ev "PDBColType" [blankOr string coltype]
  | PFnName msg ->
      ev "PFnName" [blankOr string msg]
  | PParamName msg ->
      ev "PParamName" [blankOr string msg]
  | PParamTipe msg ->
      ev "PParamTipe" [blankOr tipe msg]
  | PTypeName n ->
      ev "PTypeName" [blankOr string n]
  | PTypeFieldName n ->
      ev "PTypeFieldName" [blankOr string n]
  | PTypeFieldTipe t ->
      ev "PTypeFieldTipe" [blankOr tipe t]
  | PGroupName g ->
      ev "PGroupName" [blankOr string g]


and tlidOf (op : Types.op) : Types.tlid =
  match op with
  | SetHandler (tlid, _, _) ->
      tlid
  | CreateDB (tlid, _, _) ->
      tlid
  | AddDBCol (tlid, _, _) ->
      tlid
  | SetDBColName (tlid, _, _) ->
      tlid
  | ChangeDBColName (tlid, _, _) ->
      tlid
  | SetDBColType (tlid, _, _) ->
      tlid
  | ChangeDBColType (tlid, _, _) ->
      tlid
  | DeprecatedInitDbm (tlid, _, _, _, _) ->
      tlid
  | TLSavepoint tlid ->
      tlid
  | UndoTL tlid ->
      tlid
  | RedoTL tlid ->
      tlid
  | DeleteTL tlid ->
      tlid
  | MoveTL (tlid, _) ->
      tlid
  | SetFunction f ->
      f.ufTLID
  | DeleteFunction tlid ->
      tlid
  | SetExpr (tlid, _, _) ->
      tlid
  | CreateDBMigration (tlid, _, _, _) ->
      tlid
  | AddDBColToDBMigration (tlid, _, _) ->
      tlid
  | SetDBColNameInDBMigration (tlid, _, _) ->
      tlid
  | SetDBColTypeInDBMigration (tlid, _, _) ->
      tlid
  | AbandonDBMigration tlid ->
      tlid
  | DeleteColInDBMigration (tlid, _) ->
      tlid
  | DeleteDBCol (tlid, _) ->
      tlid
  | RenameDBname (tlid, _) ->
      tlid
  | CreateDBWithBlankOr (tlid, _, _, _) ->
      tlid
  | DeleteFunctionForever tlid ->
      tlid
  | DeleteTLForever tlid ->
      tlid
  | SetType ut ->
      ut.utTLID
  | DeleteType tlid ->
      tlid
  | DeleteTypeForever tlid ->
      tlid


and ops (ops : Types.op list) : Js.Json.t =
  list
    op
    ( match ops with
    | [UndoTL _] ->
        ops
    | [RedoTL _] ->
        ops
    | [] ->
        ops
    | _ ->
        let savepoints =
          List.map ~f:(fun op -> Types.TLSavepoint (tlidOf op)) ops
        in
        savepoints @ ops )


and spec (spec : Types.handlerSpec) : Js.Json.t =
  object_
    [ ("name", blankOr string spec.name)
    ; ("module", blankOr string spec.space)
    ; ("modifier", blankOr string spec.modifier)
    ; ( "types"
      , object_
          [ ("input", blankOr int (BlankOr.new_ ()))
          ; ("output", blankOr int (BlankOr.new_ ())) ] ) ]


and handler (h : Types.handler) : Js.Json.t =
  object_
    [ ("tlid", tlid h.hTLID)
    ; ("spec", spec h.spec)
    ; ("ast", h.ast |> OldExpr.fromFluidExpr |> expr) ]


and dbMigrationKind (k : Types.dbMigrationKind) : Js.Json.t =
  let ev = variant in
  match k with DeprecatedMigrationKind -> ev "DeprecatedMigrationKind" []


and colList (cols : Types.dbColumn list) : Js.Json.t =
  list (pair (blankOr string) (blankOr string)) cols


and dbMigrationState (s : Types.dbMigrationState) : Js.Json.t =
  let ev = variant in
  match s with
  | DBMigrationAbandoned ->
      ev "DBMigrationAbandoned" []
  | DBMigrationInitialized ->
      ev "DBMigrationInitialized" []


and dbMigration (dbm : Types.dbMigration) : Js.Json.t =
  object_
    [ ("starting_version", int dbm.startingVersion)
    ; ("version", int dbm.version)
    ; ("state", dbMigrationState dbm.state)
    ; ("cols", colList dbm.cols)
    ; ("rollforward", dbm.rollforward |> OldExpr.fromFluidExpr |> expr)
    ; ("rollback", dbm.rollback |> OldExpr.fromFluidExpr |> expr) ]


and db (db : Types.db) : Js.Json.t =
  object_
    [ ("tlid", tlid db.dbTLID)
    ; ("name", blankOr string db.dbName)
    ; ("cols", colList db.cols)
    ; ("version", int db.version)
    ; ("old_migrations", list dbMigration db.oldMigrations)
    ; ( "active_migration"
      , Option.map ~f:dbMigration db.activeMigration
        |> Option.withDefault ~default:null ) ]


and op (call : Types.op) : Js.Json.t =
  let ev = variant in
  match call with
  | SetHandler (t, p, h) ->
      ev "SetHandler" [tlid t; pos p; handler h]
  | CreateDB (t, p, name) ->
      ev "CreateDB" [tlid t; pos p; string name]
  | AddDBCol (t, cn, ct) ->
      ev "AddDBCol" [tlid t; id cn; id ct]
  | SetDBColName (t, i, name) ->
      ev "SetDBColName" [tlid t; id i; string name]
  | ChangeDBColName (t, i, name) ->
      ev "ChangeDBColName" [tlid t; id i; string name]
  | SetDBColType (t, i, tipe) ->
      ev "SetDBColType" [tlid t; id i; string tipe]
  | ChangeDBColType (t, i, name) ->
      ev "ChangeDBColType" [tlid t; id i; string name]
  | DeleteDBCol (t, i) ->
      ev "DeleteDBCol" [tlid t; id i]
  | DeprecatedInitDbm (t, i, rbid, rfid, kind) ->
      ev
        "DeprecatedInitDbm"
        [tlid t; id i; id rbid; id rfid; dbMigrationKind kind]
  | CreateDBMigration (t, rbid, rfid, cols) ->
      ev "CreateDBMigration" [tlid t; id rbid; id rfid; colList cols]
  | AddDBColToDBMigration (t, colnameid, coltypeid) ->
      ev "AddDBColToDBMigration" [tlid t; id colnameid; id coltypeid]
  | SetDBColNameInDBMigration (t, i, name) ->
      ev "SetDBColNameInDBMigration" [tlid t; id i; string name]
  | SetDBColTypeInDBMigration (t, i, tipe) ->
      ev "SetDBColTypeInDBMigration" [tlid t; id i; string tipe]
  | AbandonDBMigration t ->
      ev "AbandonDBMigration" [tlid t]
  | DeleteColInDBMigration (t, i) ->
      ev "DeleteColInDBMigration" [tlid t; id i]
  | TLSavepoint t ->
      ev "TLSavepoint" [tlid t]
  | UndoTL t ->
      ev "UndoTL" [tlid t]
  | RedoTL t ->
      ev "RedoTL" [tlid t]
  | DeleteTL t ->
      ev "DeleteTL" [tlid t]
  | MoveTL (t, p) ->
      ev "MoveTL" [tlid t; pos p]
  | SetFunction uf ->
      ev "SetFunction" [userFunction uf]
  | DeleteFunction t ->
      ev "DeleteFunction" [tlid t]
  | SetExpr (t, i, e) ->
      ev "SetExpr" [tlid t; id i; e |> OldExpr.fromFluidExpr |> expr]
  | RenameDBname (t, name) ->
      ev "RenameDBname" [tlid t; string name]
  | CreateDBWithBlankOr (t, p, i, name) ->
      ev "CreateDBWithBlankOr" [tlid t; pos p; id i; string name]
  | DeleteFunctionForever t ->
      ev "DeleteFunctionForever" [tlid t]
  | DeleteTLForever t ->
      ev "DeleteTLForever" [tlid t]
  | SetType t ->
      ev "SetType" [userTipe t]
  | DeleteType t ->
      ev "DeleteType" [tlid t]
  | DeleteTypeForever t ->
      ev "DeleteTypeForever" [tlid t]


and addOpAPIParams (params : Types.addOpAPIParams) : Js.Json.t =
  object_
    [ ("ops", ops params.ops)
    ; ("opCtr", int params.opCtr)
    ; ("clientOpCtrId", string params.clientOpCtrId) ]


and executeFunctionAPIParams (params : Types.executeFunctionAPIParams) :
    Js.Json.t =
  object_
    [ ("tlid", tlid params.efpTLID)
    ; ("trace_id", string params.efpTraceID)
    ; ("caller_id", id params.efpCallerID)
    ; ("args", list dval params.efpArgs)
    ; ("fnname", string params.efpFnName) ]


and packageFnParameter (pfp : Types.packageFnParameter) : Js.Json.t =
  object_
    [ ("name", string pfp.name)
    ; ("tipe", tipe pfp.tipe)
    ; ("description", string pfp.description) ]


and packageFn (pf : Types.packageFn) : Js.Json.t =
  object_
    [ ("user", string pf.user)
    ; ("package", string pf.package)
    ; ("module", string pf.module_)
    ; ("fnname", string pf.fnname)
    ; ("version", int pf.version)
    ; ("body", fluidExpr pf.body)
    ; ("parameters", list packageFnParameter pf.parameters)
    ; ("return_type", tipe pf.return_type)
    ; ("description", string pf.description)
    ; ("author", string pf.author)
    ; ("deprecated", bool pf.deprecated)
    ; ("tlid", tlid pf.pfTLID) ]


and uploadFnAPIParams (params : Types.uploadFnAPIParams) : Js.Json.t =
  object_ [("fn", userFunction params.uplFn)]


and triggerHandlerAPIParams (params : Types.triggerHandlerAPIParams) : Js.Json.t
    =
  object_
    [ ("tlid", tlid params.thTLID)
    ; ("trace_id", string params.thTraceID)
    ; ("input", list (tuple2 string dval) (StrDict.toList params.thInput)) ]


and sendPresenceParams (params : Types.sendPresenceParams) : Js.Json.t =
  object_
    [ ("canvasName", string params.canvasName)
    ; ("browserId", string params.browserId)
    ; ("tlid", nullable tlid params.tlid)
    ; ("timestamp", Json.Encode.float params.timestamp) ]


and sendInviteParams (params : Types.sendInviteParams) : Js.Json.t =
  object_
    [ ("email", string params.email)
    ; ("inviterUsername", string params.inviterUsername)
    ; ("inviterName", string params.inviterName) ]


and getTraceDataAPIParams (params : Types.getTraceDataAPIParams) : Js.Json.t =
  object_
    [("tlid", tlid params.gtdrpTlid); ("trace_id", traceID params.gtdrpTraceID)]


and dbStatsAPIParams (params : Types.dbStatsAPIParams) : Js.Json.t =
  object_ [("tlids", list tlid params.dbStatsTlids)]


and workerStatsAPIParams (params : Types.workerStatsAPIParams) : Js.Json.t =
  object_ [("tlid", tlid params.workerStatsTlid)]


and updateWorkerScheduleAPIParams (params : Types.updateWorkerScheduleAPIParams)
    : Js.Json.t =
  object_
    [("name", string params.workerName); ("schedule", string params.schedule)]


and performHandlerAnalysisParams (params : Types.performHandlerAnalysisParams) :
    Js.Json.t =
  object_
    [ ("handler", handler params.handler)
    ; ("trace_id", traceID params.traceID)
    ; ("trace_data", traceData params.traceData)
    ; ("dbs", list db params.dbs)
    ; ("user_fns", list userFunction params.userFns)
    ; ("user_tipes", list userTipe params.userTipes) ]


and performFunctionAnalysisParams (params : Types.performFunctionAnalysisParams)
    : Js.Json.t =
  object_
    [ ("func", userFunction params.func)
    ; ("trace_id", traceID params.traceID)
    ; ("trace_data", traceData params.traceData)
    ; ("dbs", list db params.dbs)
    ; ("user_fns", list userFunction params.userFns)
    ; ("user_tipes", list userTipe params.userTipes) ]


and userFunction (uf : Types.userFunction) : Js.Json.t =
  object_
    [ ("tlid", tlid uf.ufTLID)
    ; ("metadata", userFunctionMetadata uf.ufMetadata)
    ; ("ast", uf.ufAST |> OldExpr.fromFluidExpr |> expr) ]


and userFunctionMetadata (f : Types.userFunctionMetadata) : Js.Json.t =
  object_
    [ ("name", blankOr string f.ufmName)
    ; ("parameters", list userFunctionParameter f.ufmParameters)
    ; ("description", string f.ufmDescription)
    ; ("return_type", blankOr tipe f.ufmReturnTipe)
    ; ("infix", bool f.ufmInfix) ]


and userTipe (ut : Types.userTipe) : Js.Json.t =
  object_
    [ ("tlid", tlid ut.utTLID)
    ; ("name", blankOr string ut.utName)
    ; ("version", int ut.utVersion)
    ; ("definition", userTipeDefinition ut.utDefinition) ]


and userTipeDefinition (utd : Types.userTipeDefinition) : Js.Json.t =
  let ev = variant in
  match utd with
  | UTRecord fields ->
      ev "UTRecord" [(list userRecordField) fields]


and userRecordField (urf : Types.userRecordField) : Js.Json.t =
  object_
    [("name", blankOr string urf.urfName); ("tipe", blankOr tipe urf.urfTipe)]


and tipe (t : Types.tipe) : Js.Json.t =
  let ev = variant in
  match t with
  | TInt ->
      ev "TInt" []
  | TStr ->
      ev "TStr" []
  | TCharacter ->
      ev "TCharacter" []
  | TBool ->
      ev "TBool" []
  | TFloat ->
      ev "TFloat" []
  | TObj ->
      ev "TObj" []
  | TList ->
      ev "TList" []
  | TAny ->
      ev "TAny" []
  | TNull ->
      ev "TNull" []
  | TBlock ->
      ev "TBlock" []
  | TIncomplete ->
      ev "TIncomplete" []
  | TError ->
      ev "TError" []
  | TResp ->
      ev "TResp" []
  | TDB ->
      ev "TDB" []
  | TDate ->
      ev "TDate" []
  | TDbList a ->
      ev "TDbList" [tipe a]
  | TPassword ->
      ev "TPassword" []
  | TUuid ->
      ev "TUuid" []
  | TOption ->
      ev "TOption" []
  | TErrorRail ->
      ev "TErrorRail" []
  | TResult ->
      ev "TResult" []
  | TUserType (name, version) ->
      ev "TUserType" [string name; int version]
  | TBytes ->
      ev "TBytes" []


and userFunctionParameter (p : Types.userFunctionParameter) : Js.Json.t =
  object_
    [ ("name", blankOr string p.ufpName)
    ; ("tipe", blankOr tipe p.ufpTipe)
    ; ("block_args", list string p.ufpBlock_args)
    ; ("optional", bool p.ufpOptional)
    ; ("description", string p.ufpDescription) ]


and expr (expr : OldExpr.expr) : Js.Json.t = blankOr nExpr expr

and nExpr (nexpr : OldExpr.nExpr) : Js.Json.t =
  let e = expr in
  let ev = variant in
  match nexpr with
  | FnCall (F (_, n), exprs, r) ->
      let op = if r = Rail then "FnCallSendToRail" else "FnCall" in
      ev op [string n; list e exprs]
  | FnCall (Blank _, exprs, r) ->
      let op = if r = Rail then "FnCallSendToRail" else "FnCall" in
      let encoded = ev op [string "unknown"; list e exprs] in
      recover "fnCall hack used" ~debug:nexpr encoded
  | Let (lhs, rhs, body) ->
      ev "Let" [blankOr string lhs; e rhs; e body]
  | Lambda (vars, body) ->
      ev "Lambda" [list (blankOr string) vars; e body]
  | FieldAccess (obj, field) ->
      ev "FieldAccess" [e obj; blankOr string field]
  | If (cond, then_, else_) ->
      ev "If" [e cond; e then_; e else_]
  | Variable v ->
      ev "Variable" [string v]
  | Value v ->
      ev "Value" [string v]
  | Thread exprs ->
      ev "Thread" [list e exprs]
  | ObjectLiteral pairs ->
      ev "ObjectLiteral" [list (pair (blankOr string) expr) pairs]
  | ListLiteral elems ->
      ev "ListLiteral" [list e elems]
  | FeatureFlag (msg, cond, a, b) ->
      ev "FeatureFlag" [blankOr string msg; e cond; e a; e b]
  | Match (matchExpr, cases) ->
      ev "Match" [e matchExpr; list (pair pattern expr) cases]
  | Constructor (name, args) ->
      ev "Constructor" [blankOr string name; list e args]
  | FluidPartial (name, oldExpr) ->
      ev "FluidPartial" [string name; e oldExpr]
  | FluidRightPartial (name, oldExpr) ->
      ev "FluidRightPartial" [string name; e oldExpr]


and pattern (p : OldExpr.pattern) : Js.Json.t = blankOr nPattern p

and nPattern (npat : OldExpr.nPattern) : Js.Json.t =
  let ev = variant in
  match npat with
  | PVariable a ->
      ev "PVariable" [string a]
  | PLiteral a ->
      ev "PLiteral" [string a]
  | PConstructor (a, b) ->
      ev "PConstructor" [string a; list pattern b]


and sendToRail (sendToRail : FluidExpression.sendToRail) : Js.Json.t =
  let ev = variant in
  match sendToRail with Rail -> ev "Rail" [] | NoRail -> ev "NoRail" []


and fluidPattern (pattern : FluidPattern.t) : Js.Json.t =
  let fp = fluidPattern in
  let ev = variant in
  match pattern with
  (* Warning: A bunch of stuff here seems to expect that the
    second element of the tuples are match id but they are actually
    pattern ids. *)
  | FPVariable (id', mid, name) ->
      ev "FPVariable" [id id'; id mid; string name]
  | FPConstructor (id', mid, name, patterns) ->
      ev "FPConstructor" [id id'; id mid; string name; list fp patterns]
  | FPInteger (id', mid, v) ->
      ev "FPInteger" [id id'; id mid; string v]
  | FPBool (id', mid, v) ->
      ev "FPBool" [id id'; id mid; bool v]
  | FPFloat (id', mid, whole, fraction) ->
      ev "FPFloat" [id id'; id mid; string whole; string fraction]
  | FPString {matchID; patternID; str = v} ->
      ev "FPString" [id matchID; id patternID; string v]
  | FPNull (id', mid) ->
      ev "FPNull" [id id'; id mid]
  | FPBlank (id', mid) ->
      ev "FPBlank" [id id'; id mid]


and fluidExpr (expr : FluidExpression.t) : Js.Json.t =
  let fe = fluidExpr in
  let ev = variant in
  match expr with
  | ELet (id', lhs, rhs, body) ->
      ev "ELet" [id id'; string lhs; fe rhs; fe body]
  | EIf (id', cond, ifbody, elsebody) ->
      ev "EIf" [id id'; fe cond; fe ifbody; fe elsebody]
  | EFnCall (id', name, exprs, r) ->
      ev "EFnCall" [id id'; string name; list fe exprs; sendToRail r]
  | EBinOp (id', name, left, right, r) ->
      ev "EBinOp" [id id'; string name; fe left; fe right; sendToRail r]
  | ELambda (id', vars, body) ->
      ev "ELambda" [id id'; list (pair id string) vars; fe body]
  | EPipe (id', exprs) ->
      ev "EPipe" [id id'; list fe exprs]
  | EFieldAccess (id', obj, field) ->
      ev "EFieldAccess" [id id'; fe obj; string field]
  | EString (id', v) ->
      ev "EString" [id id'; string v]
  | EInteger (id', v) ->
      ev "EInteger" [id id'; string v]
  | EBool (id', v) ->
      ev "EBool" [id id'; bool v]
  | EFloat (id', whole, fraction) ->
      ev "EFloat" [id id'; string whole; string fraction]
  | ENull id' ->
      ev "ENull" [id id']
  | EBlank id' ->
      ev "EBlank" [id id']
  | EVariable (id', name) ->
      ev "EVariable" [id id'; string name]
  | EList (id', exprs) ->
      ev "EList" [id id'; list fe exprs]
  | ERecord (id', pairs) ->
      ev "ERecord" [id id'; list (pair string fe) pairs]
  | EFeatureFlag (id', name, cond, a, b) ->
      ev "EFeatureFlag" [id id'; string name; fe cond; fe a; fe b]
  | EMatch (id', matchExpr, cases) ->
      ev "EMatch" [id id'; fe matchExpr; list (pair fluidPattern fe) cases]
  | EConstructor (id', name, args) ->
      ev "EConstructor" [id id'; string name; list fe args]
  | EPartial (id', str, oldExpr) ->
      ev "EPartial" [id id'; string str; fe oldExpr]
  | ERightPartial (id', str, oldExpr) ->
      ev "ERightPartial" [id id'; string str; fe oldExpr]
  | EPipeTarget id' ->
      ev "EPipeTarget" [id id']


and cursorState (cs : Types.cursorState) : Js.Json.t =
  let ev = variant in
  match cs with
  | Selecting (tlid_, mId) ->
      ev "Selecting" [tlid tlid_; nullable id mId]
  | Entering (Creating maybePos) ->
      (* Hack to avoid changing the decoder: encode none as (0,0) *)
      let pos_ = Option.withDefault ~default:Defaults.origin maybePos in
      ev "Entering" [ev "Creating" [pos pos_]]
  | Entering (Filling (tlid_, id_)) ->
      ev "Entering" [ev "Filling" [tlid tlid_; id id_]]
  | DraggingTL (tlid_, vpos_, hasMoved, cursor) ->
      ev "DraggingTL" [tlid tlid_; vPos vpos_; bool hasMoved; cursorState cursor]
  | PanningCanvas {viewportStart; viewportCurr; prevCursorState} ->
      (* TODO: There's a danger of mismatching the decoder order here because we're using an inline record.
       * An order-independent encoding would alleviate this. *)
      ev
        "PanningCanvas"
        [vPos viewportStart; vPos viewportCurr; cursorState prevCursorState]
  | Deselected ->
      ev "Deselected" []
  | FluidEntering tlid_ ->
      ev "FluidEntering" [tlid tlid_]


and functionResult (fr : Types.functionResult) : Js.Json.t =
  list
    identity
    [ string fr.fnName
    ; id fr.callerID
    ; string fr.argHash
    ; int fr.argHashVersion
    ; dval fr.value ]


and traceID = string

and traceData (t : Types.traceData) : Js.Json.t =
  object_
    [ ("input", list (tuple2 string dval) (StrDict.toList t.input))
    ; ("timestamp", string t.timestamp)
    ; ("function_results", list functionResult t.functionResults) ]


and trace (t : Types.trace) : Js.Json.t =
  let data v =
    Result.map traceData v
    |> Result.toOption
    |> Option.withDefault ~default:null
  in
  pair traceID data t


and handlerState (s : Types.handlerState) : Js.Json.t =
  let ev = variant in
  match s with
  | HandlerExpanded ->
      ev "HandlerExpanded" []
  | HandlerPrepCollapse ->
      ev "HandlerPrepCollapse" []
  | HandlerCollapsing ->
      ev "HandlerCollapsing" []
  | HandlerCollapsed ->
      ev "HandlerCollapsed" []
  | HandlerExpanding ->
      ev "HandlerExpanding" []


let handlerProp (p : Types.handlerProp) : Js.Json.t =
  object_
    [ ("handlerLock", bool p.handlerLock)
    ; ("handlerState", handlerState p.handlerState) ]


let editorSettings (es : Types.editorSettings) : Js.Json.t =
  object_
    [ ("runTimers", bool es.runTimers)
    ; ("showFluidDebugger", bool es.showFluidDebugger) ]


let savedUserSettings (se : Types.savedUserSettings) : Js.Json.t =
  object_ [("showUserWelcomeModal", bool se.showUserWelcomeModal)]


let savedSettings (se : Types.savedSettings) : Js.Json.t =
  object_
    [ ("editorSettings", editorSettings se.editorSettings)
    ; ("cursorState", cursorState se.cursorState)
    ; ("routingTableOpenDetails", tcStrSet se.routingTableOpenDetails)
    ; ("tlTraceIDs", tcStrDict traceID se.tlTraceIDs)
    ; ("featureFlags", tcStrDict bool se.featureFlags)
    ; ("handlerProps", tcStrDict handlerProp se.handlerProps)
    ; ("canvasPos", pos se.canvasPos)
    ; ( "lastReload"
      , nullable string (Option.map ~f:Js.Date.toString se.lastReload) )
    ; ("sidebarOpen", bool se.sidebarOpen)
    ; ("showTopbar1", bool se.showTopbar) ]


let fof (fof : Types.fourOhFour) : Js.Json.t =
  object_
    [ ("space", string fof.space)
    ; ("path", string fof.path)
    ; ("modifier", string fof.modifier) ]


let httpError (e : string Tea.Http.error) : Js.Json.t =
  let module Http = Tea.Http in
  let responseBody (r : Http.responseBody) =
    match r with
    | NoResponse ->
        object_ [("noResponse", null)]
    | StringResponse str ->
        string str
    | ArrayBufferResponse () ->
        object_ [("arrayBufferResponse", null)]
    | BlobResponse () ->
        object_ [("blobResponse", null)]
    | DocumentResponse _ ->
        object_ [("documentResponse", string "<documentResponse>")]
    | JsonResponse json ->
        json
    | TextResponse text ->
        string text
    | RawResponse (str, ()) ->
        object_ [("rawResponse", string str)]
  in
  let response (r : Http.response) =
    let module StringMap = Map.Make (Caml.String) in
    object_
      [ ("url", string r.url)
      ; ( "status"
        , object_
            [("code", int r.status.code); ("message", string r.status.message)]
        )
      ; ( "headers"
        , r.headers
          |> StringMap.bindings
          |> List.map ~f:(fun (k, v) -> (k, string v))
          |> object_ )
      ; ("body", responseBody r.body) ]
  in
  match e with
  | Http.BadUrl url ->
      object_ [("type", string "BadUrl"); ("url", string url)]
  | Http.Timeout ->
      object_ [("type", string "Timeout")]
  | Http.NetworkError ->
      object_ [("type", string "NetworkError")]
  | Http.BadStatus r ->
      object_ [("type", string "BadStatus"); ("response", response r)]
  | Http.BadPayload (msg, r) ->
      object_
        [ ("type", string "BadPayload")
        ; ("message", string msg)
        ; ("response", response r) ]
  | Http.Aborted ->
      object_ [("type", string "Aborted")]
