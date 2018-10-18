open Tea
open! Porting
module RT = Runtime
open Json_encode_extended

let id (Types.ID id) =
  int id

let tlid (Types.TLID tlid) =
  int tlid

let pos (p: Types.pos) =
  object_
    [ ("x", int p.x)
    ; ("y", int p.y)
    ]

let vPos (vp: Types.vPos) =
  object_
    [ ("vx", int vp.vx)
    ; ("vy", int vp.vy)
    ]

let blankOr (encoder: 'a -> Js.Json.t)(v: 'a Types.blankOr) =
  match v with
  | F (ID id, s) -> variant "Filled" [int id; encoder s]
  | Blank (ID id) -> variant "Blank" [int id]

let rec pointerData (pd : Types.pointerData) : Js.Json.t =
  let ev = variant in
  match pd with
  | PVarBind var -> ev "PVarBind" [blankOr string var]
  | PEventName name -> ev "PEventName" [blankOr string name]
  | PEventModifier modifier -> ev "PEventModifier" [blankOr string modifier]
  | PEventSpace space -> ev "PEventSpace" [blankOr string space]
  | PExpr e -> ev "PExpr" [expr e]
  | PField field -> ev "PField" [blankOr string field]
  | PKey key -> ev "PKey" [blankOr string key]
  | PDBColName colname -> ev "PDBColName" [blankOr string colname]
  | PDBColType coltype -> ev "PDBColType" [blankOr string coltype]
  | PFFMsg msg -> ev "PFFMsg" [blankOr string msg]
  | PFnName msg -> ev "PFnName" [blankOr string msg]
  | PParamName msg -> ev "PParamName" [blankOr string msg]
  | PParamTipe msg -> ev "PParamTipe" [blankOr tipe msg]

(* let ops (ops : op list) : Js.Json.t = *)
(*   ops *)
(*   |> (fun ops_ -> *)
(*        match ops_ with *)
(*        | [UndoTL _] -> ops_ *)
(*        | [RedoTL _] -> ops_ *)
(*        | [] -> ops_ *)
(*        | _ -> *)
(*            let savepoints = *)
(*              ops_ |> List.map tlidOf |> List.map (fun x -> TLSavepoint x) *)
(*            in *)
(*            savepoints ^ ops_ ) *)
(*   |> List.map encodeOp |> list *)
(*  *)
(* let spec (spec : handlerSpec) : Js.Json.t = *)
(*   object_ *)
(*     [ ("name", blankOr string spec.name) *)
(*     ; ("module", blankOr string spec.module_) *)
(*     ; ("modifier", blankOr string spec.modifier) *)
(*     ; ( "types" *)
(*       , object_ *)
(*           [ ("input", blankOr int (Blank.new_ ())) *)
(*           ; ("output", blankOr int (Blank.new_ ())) ] ) ] *)
(*  *)
(* let handler (h : handler) : Js.Json.t = *)
(*   object_ *)
(*     [ ("tlid", encodeTLID h.tlid) *)
(*     ; ("spec", encodeSpec h.spec) *)
(*     ; ("ast", encodeExpr h.ast) ] *)
(*  *)
(* let dBMigrationKind (k : dBMigrationKind) : Js.Json.t = *)
(*   let ev = variant in *)
(*   match k with DeprecatedMigrationKind -> ev "DeprecatedMigrationKind" [] *)
(*  *)
(* let colList (cols : dBColumn list) : Js.Json.t = *)
(*   let col = *)
(*     encodePair (blankOr string) (blankOr string) *)
(*   in *)
(*   list (List.map encodeCol cols) *)
(*  *)
(* let dBMigrationState (s : dBMigrationState) : Js.Json.t = *)
(*   let ev = variant in *)
(*   match s with *)
(*   | DBMigrationAbandoned -> ev "DBMigrationAbandoned" [] *)
(*   | DBMigrationInitialized -> ev "DBMigrationInitialized" [] *)
(*  *)
(* let dBMigration (dbm : dBMigration) : Js.Json.t = *)
(*   object_ *)
(*     [ ("starting_version", int dbm.startingVersion) *)
(*     ; ("version", int dbm.version) *)
(*     ; ("state", encodeDBMigrationState dbm.state) *)
(*     ; ("cols", encodeColList dbm.cols) *)
(*     ; ("rollforward", encodeExpr dbm.rollforward) *)
(*     ; ("rollback", encodeExpr dbm.rollback) ] *)
(*  *)
(* let dB (db : dB) : Js.Json.t = *)
(*   object_ *)
(*     [ ("tlid", encodeTLID db.dbTLID) *)
(*     ; ("name", string db.dbName) *)
(*     ; ("cols", encodeColList db.cols) *)
(*     ; ("version", int db.version) *)
(*     ; ("old_migrations", list (List.map encodeDBMigration db.oldMigrations)) *)
(*     ; ( "active_migration" *)
(*       , Option.map encodeDBMigration db.activeMigration *)
(*         |> Option.withDefault null ) ] *)
(*  *)
(* let op (call : op) : Js.Json.t = *)
(*   let ev = variant in *)
(*   match call with *)
(*   | SetHandler (id, pos, h) -> *)
(*       ev "SetHandler" [encodeTLID id; encodePos pos; encodeHandler h] *)
(*   | CreateDB (id, pos, name) -> *)
(*       ev "CreateDB" [encodeTLID id; encodePos pos; string name] *)
(*   | AddDBCol (tlid, colnameid, coltypeid) -> *)
(*       ev "AddDBCol" [encodeTLID tlid; encodeID colnameid; encodeID coltypeid] *)
(*   | SetDBColName (tlid, id, name) -> *)
(*       ev "SetDBColName" [encodeTLID tlid; encodeID id; string name] *)
(*   | ChangeDBColName (tlid, id, name) -> *)
(*       ev "ChangeDBColName" [encodeTLID tlid; encodeID id; string name] *)
(*   | SetDBColType (tlid, id, tipe) -> *)
(*       ev "SetDBColType" [encodeTLID tlid; encodeID id; string tipe] *)
(*   | ChangeDBColType (tlid, id, name) -> *)
(*       ev "ChangeDBColType" [encodeTLID tlid; encodeID id; string name] *)
(*   | DeprecatedInitDbm (tlid, id, rbid, rfid, kind) -> *)
(*       ev "DeprecatedInitDbm" *)
(*         [ encodeTLID tlid *)
(*         ; encodeID id *)
(*         ; encodeID rbid *)
(*         ; encodeID rfid *)
(*         ; encodeDBMigrationKind kind ] *)
(*   | CreateDBMigration (tlid, rbid, rfid, cols) -> *)
(*       ev "CreateDBMigration" *)
(*         [encodeTLID tlid; encodeID rbid; encodeID rfid; encodeColList cols] *)
(*   | AddDBColToDBMigration (tlid, colnameid, coltypeid) -> *)
(*       ev "AddDBColToDBMigration" *)
(*         [encodeTLID tlid; encodeID colnameid; encodeID coltypeid] *)
(*   | SetDBColNameInDBMigration (tlid, id, name) -> *)
(*       ev "SetDBColNameInDBMigration" *)
(*         [encodeTLID tlid; encodeID id; string name] *)
(*   | SetDBColTypeInDBMigration (tlid, id, tipe) -> *)
(*       ev "SetDBColTypeInDBMigration" *)
(*         [encodeTLID tlid; encodeID id; string tipe] *)
(*   | AbandonDBMigration tlid -> ev "AbandonDBMigration" [encodeTLID tlid] *)
(*   | DeleteColInDBMigration (tlid, id) -> *)
(*       ev "DeleteColInDBMigration" [encodeTLID tlid; encodeID id] *)
(*   | TLSavepoint tlid -> ev "TLSavepoint" [encodeTLID tlid] *)
(*   | UndoTL tlid -> ev "UndoTL" [encodeTLID tlid] *)
(*   | RedoTL tlid -> ev "RedoTL" [encodeTLID tlid] *)
(*   | DeleteTL tlid -> ev "DeleteTL" [encodeTLID tlid] *)
(*   | MoveTL (tlid, pos) -> ev "MoveTL" [encodeTLID tlid; encodePos pos] *)
(*   | SetFunction uf -> ev "SetFunction" [encodeUserFunction uf] *)
(*   | DeleteFunction tlid -> ev "DeleteFunction" [encodeTLID tlid] *)
(*   | SetExpr (tlid, id, e) -> *)
(*       ev "SetExpr" [encodeTLID tlid; encodeID id; encodeExpr e] *)
(*  *)
(* let rPCParams (params : rpcParams) : Js.Json.t = *)
(*   object_ [("ops", encodeOps params.ops)] *)
(*  *)
(* let executeFunctionRPCParams (params : executeFunctionRPCParams) : *)
(*     Js.Json.t = *)
(*   object_ *)
(*     [ ("tlid", encodeTLID params.efpTLID) *)
(*     ; ("trace_id", string params.efpTraceID) *)
(*     ; ("caller_id", encodeID params.efpCallerID) *)
(*     ; ("args", encodeList encodeDval params.efpArgs) *)
(*     ; ("fnname", string params.efpFnName) ] *)
(*  *)
(* let analysisParams (params : analysisParams) : Js.Json.t = *)
(*   encodeList encodeTLID params *)
(*  *)
(* let userFunction (uf : userFunction) : Js.Json.t = *)
(*   object_ *)
(*     [ ("tlid", encodeTLID uf.ufTLID) *)
(*     ; ("metadata", encodeUserFunctionMetadata uf.ufMetadata) *)
(*     ; ("ast", encodeExpr uf.ufAST) ] *)
(*  *)
(* let userFunctionMetadata (f : userFunctionMetadata) : Js.Json.t = *)
(*   object_ *)
(*     [ ("name", blankOr string f.ufmName) *)
(*     ; ( "parameters" *)
(*       , list (List.map encodeUserFunctionParameter f.ufmParameters) ) *)
(*     ; ("description", string f.ufmDescription) *)
(*     ; ("return_type", blankOr tipe f.ufmReturnTipe) *)
(*     ; ("infix", bool f.ufmInfix) ] *)
(*  *)
and tipe (t : Types.tipe) : Js.Json.t =
  let ev = variant in
  match t with
  | TInt -> ev "TInt" []
  | TStr -> ev "TStr" []
  | TChar -> ev "TChar" []
  | TBool -> ev "TBool" []
  | TFloat -> ev "TFloat" []
  | TObj -> ev "TObj" []
  | TList -> ev "TList" []
  | TAny -> ev "TAny" []
  | TNull -> ev "TNull" []
  | TBlock -> ev "TBlock" []
  | TIncomplete -> ev "TIncomplete" []
  | TError -> ev "TError" []
  | TResp -> ev "TResp" []
  | TDB -> ev "TDB" []
  | TID -> ev "TID" []
  | TDate -> ev "TDate" []
  | TTitle -> ev "TTitle" []
  | TUrl -> ev "TUrl" []
  | TBelongsTo s -> ev "TBelongsTo" [string s]
  | THasMany s -> ev "THasMany" [string s]
  | TDbList a -> ev "TDbList" [tipe a]
  | TPassword -> ev "TPassword" []
  | TUuid -> ev "TUuid" []
  | TOption -> ev "TOption" []
  | TErrorRail -> ev "TErrorRail" []

(* let userFunctionParameter (p : userFunctionParameter) : Js.Json.t = *)
(*   object_ *)
(*     [ ("name", blankOr string p.ufpName) *)
(*     ; ("tipe", blankOr tipe p.ufpTipe) *)
(*     ; ("block_args", list (List.map string p.ufpBlock_args)) *)
(*     ; ("optional", bool p.ufpOptional) *)
(*     ; ("description", string p.ufpDescription) ] *)
(*  *)
and expr (expr : Types.expr) : Js.Json.t =
  blankOr nExpr expr

and nExpr (nexpr : Types.nExpr) : Js.Json.t =
  let e = expr in
  let ev = variant in
  match nexpr with
  | FnCall (n, exprs, r) ->
      if r = Rail then
        ev "FnCallSendToRail" [string n; list e exprs]
      else ev "FnCall" [string n; list e exprs]
  | Let (lhs, rhs, body) ->
      ev "Let" [blankOr string lhs; e rhs; e body]
  | Lambda (vars, body) ->
      ev "Lambda" [list (blankOr string) vars; e body]
  | FieldAccess (obj, field) ->
      ev "FieldAccess" [e obj; blankOr string field]
  | If (cond, then_, else_) -> ev "If" [e cond; e then_; e else_]
  | Variable v -> ev "Variable" [string v]
  | Value v -> ev "Value" [string v]
  | Thread exprs -> ev "Thread" [list e exprs]
  | ObjectLiteral pairs ->
      let encoder = pair (blankOr string) e in
      ev "ObjectLiteral" [list encoder pairs]
  | ListLiteral elems -> ev "ListLiteral" [list e elems]
  | FeatureFlag (msg, cond, a, b) ->
      ev "FeatureFlag" [blankOr string msg; e cond; e a; e b]

and cursorState (cs : Types.cursorState) : Js.Json.t =
  let ev = variant in
  match cs with
  | Selecting (tlid_, mId) ->
      ev "Selecting" [tlid tlid_; nullable id mId]
  | SelectingCommand (tlid_, mId) ->
      ev "SelectingCommand" [tlid tlid_; id mId]
  | Entering (Creating pos_) -> ev "Entering" [ev "Creating" [pos pos_]]
  | Entering (Filling (tlid_, id_)) ->
      ev "Entering" [ev "Filling" [tlid tlid_; id id_]]
  | Dragging (tlid_, vpos_, hasMoved, cursor) ->
      ev "Dragging"
        [ tlid tlid_
        ; vPos vpos_
        ; bool hasMoved
        ; cursorState cursor ]
  | Deselected -> ev "Deselected" []

let serializableEditor (se : Types.serializableEditor) : Js.Json.t =
  object_
    [ ("clipboard", nullable pointerData se.clipboard)
    ; ("timersEnabled", bool se.timersEnabled)
    ; ("cursorState", cursorState se.cursorState)
    ; ("lockedHandlers", list tlid se.lockedHandlers)
    ]

(* let 404 (fof : fourOhFour) : Js.Json.t = *)
(*   object_ *)
(*     [ ("space", string fof.space) *)
(*     ; ("path", string fof.path) *)
(*     ; ("modifier", string fof.modifier) ] *)
(*  *)
(* let inputValueDict (dict : inputValueDict) : Js.Json.t = *)
(*   dict |> Dict.toList |> encodeList (encodePair string encodeDval) *)
(*  *)
(* let trace (t : trace) : Js.Json.t = *)
(*   object_ *)
(*     [ ( "input" *)
(*       , JSONUtils.encodeList *)
(*           (encodePair string encodeDval) *)
(*           (Dict.toList t.input) ) *)
(*     ; ( "function_results" *)
(*       , JSONUtils.encodeList encodeFunctionResult t.functionResults ) *)
(*     ; ("id", string t.traceID) ] *)
(*  *)
(* let functionResult (fr : functionResult) : Js.Json.t = *)
(*   list *)
(*     [ string fr.fnName *)
(*     ; encodeID fr.callerID *)
(*     ; string fr.argHash *)
(*     ; encodeDval fr.value ] *)
(*  *)
(* let dval (dv : dval) : Js.Json.t = *)
(*   let ev = variant in *)
(*   let dhttp h = *)
(*     match h with *)
(*     | Redirect s -> ev "Redirect" [string s] *)
(*     | Response (code, headers) -> *)
(*         ev "Response" *)
(*           [int code; encodeList (encodePair string string) headers] *)
(*   in *)
(*   match dv with *)
(*   | DInt i -> ev "DInt" [int i] *)
(*   | DFloat f -> ev "DFloat" [float f] *)
(*   | DBool b -> ev "DBool" [bool b] *)
(*   | DNull -> ev "DNull" [] *)
(*   | DStr s -> ev "DStr" [string s] *)
(*   | DList l -> ev "DList" [encodeList encodeDval l] *)
(*   | DObj o -> ev "DObj" [JSEE.dict identity encodeDval o] *)
(*   | DBlock -> ev "DBlock" [] *)
(*   | DIncomplete -> ev "DIncomplete" [] *)
(*   | DChar c -> ev "DChar" [string (String.fromList [c])] *)
(*   | DError msg -> ev "DError" [string msg] *)
(*   | DResp (h, hdv) -> *)
(*       ev "DResp" [JSONUtils.encodePair encodeDhttp encodeDval (h, hdv)] *)
(*   | DDB name -> ev "DDB" [string name] *)
(*   | DID id -> ev "DID" [string id] *)
(*   | DUrl url -> ev "DUrl" [string url] *)
(*   | DTitle title -> ev "DTitle" [string title] *)
(*   | DDate date -> ev "DDate" [string date] *)
(*   | DPassword hashed -> ev "DPassword" [string (Base64.encode hashed)] *)
(*   | DUuid uuid -> ev "DUuid" [string uuid] *)
(*   | DOption opt -> *)
(*       ev "DOption" *)
(*         [ ( match opt with *)
(*           | OptNothing -> ev "OptNothing" [] *)
(*           | OptJust dv -> ev "OptJust" [encodeDval dv] ) ] *)
(*   | DErrorRail dv -> ev "DErrorRail" [encodeDval dv] *)
