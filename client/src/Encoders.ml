open Tc
open Json_encode_extended

(* Dark *)
module RT = Runtime

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
  | Partial (i, str) ->
      variant "Partial" [id i; string str]
  | Blank i ->
      variant "Blank" [id i]


let rec dval (dv : Types.dval) : Js.Json.t =
  let open Types in
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
      ev "DFloat" [Json_encode_extended.float f]
  | DBool b ->
      ev "DBool" [bool b]
  | DNull ->
      ev "DNull" []
  | DStr s ->
      ev "DStr" [string s]
  | DList l ->
      ev "DList" [list dval l]
  | DObj o ->
      o
      |> StrDict.map ~f:dval
      |> StrDict.toList
      |> Js.Dict.fromList
      |> dict
      |> fun x -> [x] |> ev "DObj"
  (* opaque types *)
  | DBlock ->
      ev "DBlock" [null]
  | DIncomplete ->
      ev "DIncomplete" []
  (* user-ish types *)
  | DCharacter c ->
      ev "DCharacter" [string c]
  | DError msg ->
      ev "DError" [string msg]
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
      ev "DBytes" [string (bin |> Bytes.to_string |> Webapi.Base64.atob)]


let rec pointerData (pd : Types.pointerData) : Js.Json.t =
  let ev = variant in
  match pd with
  | PVarBind var ->
      ev "PVarBind" [blankOr string var]
  | PEventName name ->
      ev "PEventName" [blankOr string name]
  | PEventModifier modifier ->
      ev "PEventModifier" [blankOr string modifier]
  | PEventSpace space ->
      ev "PEventSpace" [blankOr string space]
  | PExpr e ->
      ev "PExpr" [expr e]
  | PField field ->
      ev "PField" [blankOr string field]
  | PKey key ->
      ev "PKey" [blankOr string key]
  | PDBName name ->
      ev "PDBName" [blankOr string name]
  | PDBColName colname ->
      ev "PDBColName" [blankOr string colname]
  | PDBColType coltype ->
      ev "PDBColType" [blankOr string coltype]
  | PFFMsg msg ->
      ev "PFFMsg" [blankOr string msg]
  | PFnName msg ->
      ev "PFnName" [blankOr string msg]
  | PParamName msg ->
      ev "PParamName" [blankOr string msg]
  | PParamTipe msg ->
      ev "PParamTipe" [blankOr tipe msg]
  | PPattern p ->
      ev "PPattern" [pattern p]
  | PConstructorName n ->
      ev "PConstructorName" [blankOr string n]
  | PFnCallName n ->
      ev "PFnCallName" [blankOr string n]
  | PTypeName n ->
      ev "PTypeName" [blankOr string n]
  | PTypeFieldName n ->
      ev "PTypeFieldName" [blankOr string n]
  | PTypeFieldTipe t ->
      ev "PTypeFieldTipe" [blankOr tipe t]


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
    ; ("module", blankOr string spec.module_)
    ; ("modifier", blankOr string spec.modifier)
    ; ( "types"
      , object_
          [ ("input", blankOr int (Blank.new_ ()))
          ; ("output", blankOr int (Blank.new_ ())) ] ) ]


and handler (h : Types.handler) : Js.Json.t =
  object_ [("tlid", tlid h.tlid); ("spec", spec h.spec); ("ast", expr h.ast)]


and dbMigrationKind (k : Types.dBMigrationKind) : Js.Json.t =
  let ev = variant in
  match k with DeprecatedMigrationKind -> ev "DeprecatedMigrationKind" []


and colList (cols : Types.dBColumn list) : Js.Json.t =
  list (pair (blankOr string) (blankOr string)) cols


and dbMigrationState (s : Types.dBMigrationState) : Js.Json.t =
  let ev = variant in
  match s with
  | DBMigrationAbandoned ->
      ev "DBMigrationAbandoned" []
  | DBMigrationInitialized ->
      ev "DBMigrationInitialized" []


and dbMigration (dbm : Types.dBMigration) : Js.Json.t =
  object_
    [ ("starting_version", int dbm.startingVersion)
    ; ("version", int dbm.version)
    ; ("state", dbMigrationState dbm.state)
    ; ("cols", colList dbm.cols)
    ; ("rollforward", expr dbm.rollforward)
    ; ("rollback", expr dbm.rollback) ]


and db (db : Types.dB) : Js.Json.t =
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
      ev "SetExpr" [tlid t; id i; expr e]
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


and addOpRPCParams (params : Types.addOpRPCParams) : Js.Json.t =
  object_ [("ops", ops params.ops)]


and executeFunctionRPCParams (params : Types.executeFunctionRPCParams) :
    Js.Json.t =
  object_
    [ ("tlid", tlid params.efpTLID)
    ; ("trace_id", string params.efpTraceID)
    ; ("caller_id", id params.efpCallerID)
    ; ("args", list dval params.efpArgs)
    ; ("fnname", string params.efpFnName) ]


and triggerCronRPCParams (params : Types.triggerCronRPCParams) : Js.Json.t =
  object_ [("tlid", tlid params.tcpTLID)]


and sendPresenceParams (params : Types.sendPresenceParams) : Js.Json.t =
  object_
    [ ("browserId", string params.browserId)
    ; ("tlid", nullable string params.tlid)
    ; ("timestamp", Json_encode_extended.float params.timestamp) ]


and getTraceDataRPCParams (params : Types.getTraceDataRPCParams) : Js.Json.t =
  object_
    [("tlid", tlid params.gtdrpTlid); ("trace_id", traceID params.gtdrpTraceID)]


and dbStatsRPCParams (params : Types.dbStatsRPCParams) : Js.Json.t =
  object_ [("tlids", list tlid params.dbStatsTlids)]


and performHandlerAnalysisParams (params : Types.performHandlerAnalysisParams)
    : Js.Json.t =
  object_
    [ ("handler", handler params.handler)
    ; ("trace_id", traceID params.traceID)
    ; ("trace_data", traceData params.traceData)
    ; ("dbs", list db params.dbs)
    ; ("user_fns", list userFunction params.userFns)
    ; ("user_tipes", list userTipe params.userTipes) ]


and performFunctionAnalysisParams
    (params : Types.performFunctionAnalysisParams) : Js.Json.t =
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
    ; ("ast", expr uf.ufAST) ]


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
  match utd with UTRecord fields ->
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
  | TBelongsTo s ->
      ev "TBelongsTo" [string s]
  | THasMany s ->
      ev "THasMany" [string s]
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
  | TDeprecated1 | TDeprecated2 | TDeprecated3 | TDeprecated4 ->
      raise (Js.Exn.raiseError "Deprecated type")


and userFunctionParameter (p : Types.userFunctionParameter) : Js.Json.t =
  object_
    [ ("name", blankOr string p.ufpName)
    ; ("tipe", blankOr tipe p.ufpTipe)
    ; ("block_args", list string p.ufpBlock_args)
    ; ("optional", bool p.ufpOptional)
    ; ("description", string p.ufpDescription) ]


and expr (expr : Types.expr) : Js.Json.t = blankOr nExpr expr

and nExpr (nexpr : Types.nExpr) : Js.Json.t =
  let e = expr in
  let ev = variant in
  match nexpr with
  | FnCall (F (_, n), exprs, r) ->
      if r = Rail
      then ev "FnCallSendToRail" [string n; list e exprs]
      else ev "FnCall" [string n; list e exprs]
  | FnCall (Partial _, _, _) | FnCall (Blank _, _, _) ->
      Debug.crash "fnCall hack used"
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


and pattern (p : Types.pattern) : Js.Json.t = blankOr nPattern p

and nPattern (npat : Types.nPattern) : Js.Json.t =
  let ev = variant in
  match npat with
  | PVariable a ->
      ev "PVariable" [string a]
  | PLiteral a ->
      ev "PLiteral" [string a]
  | PConstructor (a, b) ->
      ev "PConstructor" [string a; list pattern b]


and cursorState (cs : Types.cursorState) : Js.Json.t =
  let ev = variant in
  match cs with
  | Selecting (tlid_, mId) ->
      ev "Selecting" [tlid tlid_; nullable id mId]
  | SelectingCommand (tlid_, mId) ->
      ev "SelectingCommand" [tlid tlid_; id mId]
  | Entering (Creating pos_) ->
      ev "Entering" [ev "Creating" [pos pos_]]
  | Entering (Filling (tlid_, id_)) ->
      ev "Entering" [ev "Filling" [tlid tlid_; id id_]]
  | Dragging (tlid_, vpos_, hasMoved, cursor) ->
      ev "Dragging" [tlid tlid_; vPos vpos_; bool hasMoved; cursorState cursor]
  | Deselected ->
      ev "Deselected" []


and functionResult (fr : Types.functionResult) : Js.Json.t =
  list
    identity
    [string fr.fnName; id fr.callerID; string fr.argHash; dval fr.value]


and traceID = string

and traceData (t : Types.traceData) : Js.Json.t =
  object_
    [ ("input", list (tuple2 string dval) (StrDict.toList t.input))
    ; ("timestamp", string t.timestamp)
    ; ("function_results", list functionResult t.functionResults) ]


and trace (t : Types.trace) : Js.Json.t =
  let data v = Option.map ~f:traceData v |> Option.withDefault ~default:null in
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


let serializableEditor (se : Types.serializableEditor) : Js.Json.t =
  object_
    [ ("timersEnabled", bool se.timersEnabled)
    ; ("cursorState", cursorState se.cursorState)
    ; ("routingTableOpenDetails", tcStrSet se.routingTableOpenDetails)
    ; ("tlCursors", tcStrDict traceID se.tlCursors)
    ; ("featureFlags", tcStrDict bool se.featureFlags)
    ; ("handlerProps", tcStrDict handlerProp se.handlerProps)
    ; ("canvasPos", pos se.canvasPos) ]


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
