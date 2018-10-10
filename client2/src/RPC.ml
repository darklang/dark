open Tea
open! Porting
module DE = Dict.Extra
open JSON
module JSD = Json.Decode
module JSDP = Json.Decode.Pipeline
module JSE = Json.Encode
module JSEE = Json.Encode.Extra
module RT = Runtime
open Types

let rpc_ (m : model) (url : string)
    (callback : (rpcParams -> (string Http.error, rpcResult) result) -> msg)
    (params : rpcParams) : msg Cmd.t =
  let payload = encodeRPCParams params in
  let json = Http.jsonBody payload in
  let request = Http.post url json decodeRPC in
  Http.send (callback params) request

let postString (url : string) : string Http.request =
  Http.request
    { method_= "POST"
    ; headers= []
    ; url
    ; body= Http.emptyBody
    ; expect= Http.expectString
    ; timeout= None
    ; withCredentials= false }

let rpc (m : model) (canvasName : string) (focus : focus) (params : rpcParams)
    : msg Cmd.t =
  rpc_ m
    (String.concat ["/api/"; Http.encodeUri canvasName; "/rpc"])
    (RPCCallback focus) params

let executeFunctionRPC (canvasName : string)
    (params : executeFunctionRPCParams) : msg Cmd.t =
  let url =
    String.concat ["/api/"; Http.encodeUri canvasName; "/execute_function"]
  in
  let payload = encodeExecuteFunctionRPCParams params in
  let json = Http.jsonBody payload in
  let request = Http.post url json decodeExecuteFunctionRPC in
  Http.send (ExecuteFunctionRPCCallback params) request

let getAnalysisRPC (canvasName : string) (params : analysisParams) : msg Cmd.t
    =
  let url =
    String.concat ["/api/"; Http.encodeUri canvasName; "/get_analysis"]
  in
  let payload = encodeAnalysisParams params in
  let json = Http.jsonBody payload in
  let request = Http.post url json decodeGetAnalysisRPC in
  Http.send GetAnalysisRPCCallback request

let delete404RPC (canvasName : string) (param : delete404Param) : msg Cmd.t =
  let url =
    String.concat ["/api/"; Http.encodeUri canvasName; "/delete_404"]
  in
  let payload = encode404 param in
  let json = Http.jsonBody payload in
  let request = Http.post url json (JSD.list decode404) in
  Http.send GetDelete404RPCCallback request

let initialLoadRPC (canvasName : string) (focus : focus) : msg Cmd.t =
  let url =
    String.concat ["/api/"; Http.encodeUri canvasName; "/initial_load"]
  in
  let request = Http.post url Http.emptyBody decodeInitialLoadRPC in
  Http.send (InitialLoadRPCCallback (focus, NoChange)) request

let saveTestRPC (canvasName : string) : msg Cmd.t =
  let url = String.concat ["/api/"; Http.encodeUri canvasName; "/save_test"] in
  let request = postString url in
  Http.send SaveTestRPCCallback request

let emptyParams : rpcParams = {ops= []}

let opsParams (ops : op list) : rpcParams = {ops}

let integrationRPC (m : model) (canvasName : string) (name : string) :
    msg Cmd.t =
  let url =
    String.concat ["/api/"; Http.encodeUri canvasName; "/initial_load"]
  in
  let request = Http.post url Http.emptyBody decodeInitialLoadRPC in
  Http.send
    (InitialLoadRPCCallback (FocusNothing, TriggerIntegrationTest name))
    request

let decodePointerData : pointerData JSD.decoder =
  let dv1 = decodeVariant1 in
  decodeVariants
    [ ("PVarBind", dv1 PVarBind (decodeBlankOr JSD.string))
    ; ("PEventName", dv1 PEventName (decodeBlankOr JSD.string))
    ; ("PEventModifier", dv1 PEventModifier (decodeBlankOr JSD.string))
    ; ("PEventSpace", dv1 PEventSpace (decodeBlankOr JSD.string))
    ; ("PExpr", dv1 PExpr decodeExpr)
    ; ("PField", dv1 PField (decodeBlankOr JSD.string))
    ; ("PDBColName", dv1 PDBColName (decodeBlankOr JSD.string))
    ; ("PDBColType", dv1 PDBColType (decodeBlankOr JSD.string))
    ; ("PDarkType", dv1 PDarkType decodeDarkType)
    ; ("PDarkTypeField", dv1 PDarkTypeField (decodeBlankOr JSD.string))
    ; ("PFFMsg", dv1 PFFMsg (decodeBlankOr JSD.string))
    ; ("PFnName", dv1 PFnName (decodeBlankOr JSD.string))
    ; ("PParamName", dv1 PParamName (decodeBlankOr JSD.string))
    ; ("PParamTipe", dv1 PParamTipe (decodeBlankOr decodeTipe)) ]

let encodePointerData (pd : pointerData) : JSE.value =
  let ev = encodeVariant in
  match pd with
  | PVarBind var -> ev "PVarBind" [encodeBlankOr JSE.string var]
  | PEventName name -> ev "PEventName" [encodeBlankOr JSE.string name]
  | PEventModifier modifier ->
      ev "PEventModifier" [encodeBlankOr JSE.string modifier]
  | PEventSpace space -> ev "PEventSpace" [encodeBlankOr JSE.string space]
  | PExpr expr -> ev "PExpr" [encodeExpr expr]
  | PField field -> ev "PField" [encodeBlankOr JSE.string field]
  | PKey key -> ev "PKey" [encodeBlankOr JSE.string key]
  | PDBColName colname -> ev "PDBColName" [encodeBlankOr JSE.string colname]
  | PDBColType coltype -> ev "PDBColType" [encodeBlankOr JSE.string coltype]
  | PDarkType darktype -> ev "PDarkType" [encodeDarkType darktype]
  | PDarkTypeField darktypefield ->
      ev "PDarkTypeField" [encodeBlankOr JSE.string darktypefield]
  | PFFMsg msg -> ev "PFFMsg" [encodeBlankOr JSE.string msg]
  | PFnName msg -> ev "PFnName" [encodeBlankOr JSE.string msg]
  | PParamName msg -> ev "PParamName" [encodeBlankOr JSE.string msg]
  | PParamTipe msg -> ev "PParamTipe" [encodeBlankOr encodeTipe msg]

let tlidOf (op : op) : tlid =
  match op with
  | SetHandler (tlid, _, _) -> tlid
  | CreateDB (tlid, _, _) -> tlid
  | AddDBCol (tlid, _, _) -> tlid
  | SetDBColName (tlid, _, _) -> tlid
  | ChangeDBColName (tlid, _, _) -> tlid
  | SetDBColType (tlid, _, _) -> tlid
  | ChangeDBColType (tlid, _, _) -> tlid
  | DeprecatedInitDbm (tlid, _, _, _, _) -> tlid
  | TLSavepoint tlid -> tlid
  | UndoTL tlid -> tlid
  | RedoTL tlid -> tlid
  | DeleteTL tlid -> tlid
  | MoveTL (tlid, _) -> tlid
  | SetFunction f -> f.tlid
  | DeleteFunction tlid -> tlid
  | SetExpr (tlid, _, _) -> tlid
  | CreateDBMigration (tlid, _, _, _) -> tlid
  | AddDBColToDBMigration (tlid, _, _) -> tlid
  | SetDBColNameInDBMigration (tlid, _, _) -> tlid
  | SetDBColTypeInDBMigration (tlid, _, _) -> tlid
  | AbandonDBMigration tlid -> tlid
  | DeleteColInDBMigration (tlid, _) -> tlid

let encodeOps (ops : op list) : JSE.value =
  ops
  |> (fun ops_ ->
       match ops_ with
       | [UndoTL _] -> ops_
       | [RedoTL _] -> ops_
       | [] -> ops_
       | _ ->
           let savepoints = ops_ |> List.map tlidOf |> List.map TLSavepoint in
           savepoints ^ ops_ )
  |> List.map encodeOp |> JSE.list

let encodeSpec (spec : handlerSpec) : JSE.value =
  JSE.object_
    [ ("name", encodeBlankOr JSE.string spec.name)
    ; ("module", encodeBlankOr JSE.string spec.module_)
    ; ("modifier", encodeBlankOr JSE.string spec.modifier)
    ; ("types", encodeSpecTypes spec.types) ]

let encodeHandler (h : handler) : JSE.value =
  JSE.object_
    [ ("tlid", encodeTLID h.tlid)
    ; ("spec", encodeSpec h.spec)
    ; ("ast", encodeExpr h.ast) ]

let encodeDBMigrationKind (k : dBMigrationKind) : JSE.value =
  let ev = encodeVariant in
  match k with DeprecatedMigrationKind -> ev "DeprecatedMigrationKind" []

let encodeColList (cols : dBColumn list) : JSE.value =
  let encodeCol =
    encodePair (encodeBlankOr JSE.string) (encodeBlankOr JSE.string)
  in
  JSE.list (List.map encodeCol cols)

let encodeDBMigrationState (s : dBMigrationState) : JSE.value =
  let ev = encodeVariant in
  match s with
  | DBMigrationAbandoned -> ev "DBMigrationAbandoned" []
  | DBMigrationInitialized -> ev "DBMigrationInitialized" []

let encodeDBMigration (dbm : dBMigration) : JSE.value =
  JSE.object_
    [ ("starting_version", JSE.int dbm.startingVersion)
    ; ("version", JSE.int dbm.version)
    ; ("state", encodeDBMigrationState dbm.state)
    ; ("cols", encodeColList dbm.cols)
    ; ("rollforward", encodeExpr dbm.rollforward)
    ; ("rollback", encodeExpr dbm.rollback) ]

let encodeDB (db : dB) : JSE.value =
  JSE.object_
    [ ("tlid", encodeTLID db.tlid)
    ; ("name", JSE.string db.name)
    ; ("cols", encodeColList db.cols)
    ; ("version", JSE.int db.version)
    ; ("old_migrations", JSE.list (List.map encodeDBMigration db.oldMigrations))
    ; ( "active_migration"
      , Option.map encodeDBMigration db.activeMigration
        |> Option.withDefault JSE.null ) ]

let encodeOp (call : op) : JSE.value =
  let ev = encodeVariant in
  match call with
  | SetHandler (id, pos, h) ->
      ev "SetHandler" [encodeTLID id; encodePos pos; encodeHandler h]
  | CreateDB (id, pos, name) ->
      ev "CreateDB" [encodeTLID id; encodePos pos; JSE.string name]
  | AddDBCol (tlid, colnameid, coltypeid) ->
      ev "AddDBCol" [encodeTLID tlid; encodeID colnameid; encodeID coltypeid]
  | SetDBColName (tlid, id, name) ->
      ev "SetDBColName" [encodeTLID tlid; encodeID id; JSE.string name]
  | ChangeDBColName (tlid, id, name) ->
      ev "ChangeDBColName" [encodeTLID tlid; encodeID id; JSE.string name]
  | SetDBColType (tlid, id, tipe) ->
      ev "SetDBColType" [encodeTLID tlid; encodeID id; JSE.string tipe]
  | ChangeDBColType (tlid, id, name) ->
      ev "ChangeDBColType" [encodeTLID tlid; encodeID id; JSE.string name]
  | DeprecatedInitDbm (tlid, id, rbid, rfid, kind) ->
      ev "DeprecatedInitDbm"
        [ encodeTLID tlid
        ; encodeID id
        ; encodeID rbid
        ; encodeID rfid
        ; encodeDBMigrationKind kind ]
  | CreateDBMigration (tlid, rbid, rfid, cols) ->
      ev "CreateDBMigration"
        [encodeTLID tlid; encodeID rbid; encodeID rfid; encodeColList cols]
  | AddDBColToDBMigration (tlid, colnameid, coltypeid) ->
      ev "AddDBColToDBMigration"
        [encodeTLID tlid; encodeID colnameid; encodeID coltypeid]
  | SetDBColNameInDBMigration (tlid, id, name) ->
      ev "SetDBColNameInDBMigration"
        [encodeTLID tlid; encodeID id; JSE.string name]
  | SetDBColTypeInDBMigration (tlid, id, tipe) ->
      ev "SetDBColTypeInDBMigration"
        [encodeTLID tlid; encodeID id; JSE.string tipe]
  | AbandonDBMigration tlid -> ev "AbandonDBMigration" [encodeTLID tlid]
  | DeleteColInDBMigration (tlid, id) ->
      ev "DeleteColInDBMigration" [encodeTLID tlid; encodeID id]
  | TLSavepoint tlid -> ev "TLSavepoint" [encodeTLID tlid]
  | UndoTL tlid -> ev "UndoTL" [encodeTLID tlid]
  | RedoTL tlid -> ev "RedoTL" [encodeTLID tlid]
  | DeleteTL tlid -> ev "DeleteTL" [encodeTLID tlid]
  | MoveTL (tlid, pos) -> ev "MoveTL" [encodeTLID tlid; encodePos pos]
  | SetFunction uf -> ev "SetFunction" [encodeUserFunction uf]
  | DeleteFunction tlid -> ev "DeleteFunction" [encodeTLID tlid]
  | SetExpr (tlid, id, e) ->
      ev "SetExpr" [encodeTLID tlid; encodeID id; encodeExpr e]

let encodeRPCParams (params : rpcParams) : JSE.value =
  JSE.object_ [("ops", encodeOps params.ops)]

let encodeExecuteFunctionRPCParams (params : executeFunctionRPCParams) :
    JSE.value =
  JSE.object_
    [ ("tlid", encodeTLID params.tlid)
    ; ("trace_id", JSE.string params.traceID)
    ; ("caller_id", encodeID params.callerID)
    ; ("args", encodeList encodeDval params.args)
    ; ("fnname", JSE.string params.fnName) ]

let encodeAnalysisParams (params : analysisParams) : JSE.value =
  encodeList encodeTLID params

let encodeUserFunction (uf : userFunction) : JSE.value =
  JSE.object_
    [ ("tlid", encodeTLID uf.tlid)
    ; ("metadata", encodeUserFunctionMetadata uf.metadata)
    ; ("ast", encodeExpr uf.ast) ]

let encodeUserFunctionMetadata (f : userFunctionMetadata) : JSE.value =
  JSE.object_
    [ ("name", encodeBlankOr JSE.string f.name)
    ; ( "parameters"
      , JSE.list (List.map encodeUserFunctionParameter f.parameters) )
    ; ("description", JSE.string f.description)
    ; ("return_type", encodeBlankOr encodeTipe f.returnTipe)
    ; ("infix", JSE.bool f.infix) ]

let encodeTipe (t : tipe) : JSE.value =
  let ev = encodeVariant in
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
  | TBelongsTo s -> ev "TBelongsTo" [JSE.string s]
  | THasMany s -> ev "THasMany" [JSE.string s]
  | TDbList a -> ev "TDbList" [encodeTipe a]
  | TPassword -> ev "TPassword" []
  | TUuid -> ev "TUuid" []
  | TOption -> ev "TOption" []
  | TErrorRail -> ev "TErrorRail" []

let encodeUserFunctionParameter (p : userFunctionParameter) : JSE.value =
  JSE.object_
    [ ("name", encodeBlankOr JSE.string p.name)
    ; ("tipe", encodeBlankOr encodeTipe p.tipe)
    ; ("block_args", JSE.list (List.map JSE.string p.block_args))
    ; ("optional", JSE.bool p.optional)
    ; ("description", JSE.string p.description) ]

let encodeExpr (expr : expr) : JSE.value = encodeBlankOr encodeNExpr expr

let encodeNExpr (expr : nExpr) : JSE.value =
  let e = encodeExpr in
  let ev = encodeVariant in
  match expr with
  | FnCall (n, exprs, r) ->
      if r = Rail then
        ev "FnCallSendToRail" [JSE.string n; JSE.list (List.map e exprs)]
      else ev "FnCall" [JSE.string n; JSE.list (List.map e exprs)]
  | Let (lhs, rhs, body) ->
      ev "Let" [encodeBlankOr JSE.string lhs; e rhs; e body]
  | Lambda (vars, body) ->
      ev "Lambda" [List.map (encodeBlankOr JSE.string) vars |> JSE.list; e body]
  | FieldAccess (obj, field) ->
      ev "FieldAccess" [e obj; encodeBlankOr JSE.string field]
  | If (cond, then_, else_) -> ev "If" [e cond; e then_; e else_]
  | Variable v -> ev "Variable" [JSE.string v]
  | Value v -> ev "Value" [JSE.string v]
  | Thread exprs -> ev "Thread" [JSE.list (List.map e exprs)]
  | ObjectLiteral pairs ->
      let encoder = JSON.encodePair (encodeBlankOr JSE.string) e in
      ev "ObjectLiteral" [List.map encoder pairs |> JSE.list]
  | ListLiteral elems -> ev "ListLiteral" [JSE.list (List.map e elems)]
  | FeatureFlag (msg, cond, a, b) ->
      ev "FeatureFlag" [encodeBlankOr JSE.string msg; e cond; e a; e b]

let encodeSpecTypes (st : specTypes) : JSE.value =
  JSE.object_
    [("input", encodeDarkType st.input); ("output", encodeDarkType st.output)]

let encodeDarkType (dt : darkType) : JSE.value =
  encodeBlankOr encodeNDarkType dt

let encodeNDarkType (t : nDarkType) : JSE.value =
  let ev = encodeVariant in
  match t with
  | DTEmpty -> ev "Empty" []
  | DTAny -> ev "Any" []
  | DTString -> ev "String" []
  | DTInt -> ev "Int" []
  | DTObj ts ->
      ev "Obj"
        [ JSE.list
            (List.map (encodePair (encodeBlankOr JSE.string) encodeDarkType) ts)
        ]

let encodeCursorState (cs : cursorState) : JSE.value =
  let ev = encodeVariant in
  match cs with
  | Selecting (tlid, mId) ->
      ev "Selecting" [encodeTLID tlid; JSEE.maybe encodeID mId]
  | SelectingCommand (tlid, mId) ->
      ev "SelectingCommand" [encodeTLID tlid; encodeID mId]
  | Entering (Creating pos) -> ev "Entering" [ev "Creating" [encodePos pos]]
  | Entering (Filling (tlid, id)) ->
      ev "Entering" [ev "Filling" [encodeTLID tlid; encodeID id]]
  | Dragging (tlid, vpos, hasMoved, cursor) ->
      ev "Dragging"
        [ encodeTLID tlid
        ; encodeVPos vpos
        ; JSE.bool hasMoved
        ; encodeCursorState cursor ]
  | Deselected -> ev "Deselected" []

let encodeSerializableEditor (se : serializableEditor) : JSE.value =
  JSE.object_
    [ ("clipboard", JSEE.maybe encodePointerData se.clipboard)
    ; ("timersEnabled", JSE.bool se.timersEnabled)
    ; ("cursorState", encodeCursorState se.cursorState)
    ; ( "lockedHandlers"
      , JSE.list (List.map (fun id -> encodeTLID id) se.lockedHandlers) ) ]

let decodeSerializableEditor : serializableEditor JSD.decoder =
  JSDP.decode SerializableEditor
  |> JSDP.optional "clipboard" (JSD.maybe decodePointerData) None
  |> JSDP.optional "timersEnabled" JSD.bool true
  |> JSDP.optional "cursorState" decodeCursorState Deselected
  |> JSDP.optional "lockedHandlers" (JSD.list decodeTLID) []

let decodeCursorState : cursorState JSD.decoder =
  let dv4 = decodeVariant4 in
  let dv3 = decodeVariant3 in
  let dv2 = decodeVariant2 in
  let dv1 = decodeVariant1 in
  let dv0 = decodeVariant0 in
  let dcs = JSD.lazy_ (fun _ -> decodeCursorState) in
  let decodeEntering =
    decodeVariants
      [ ("Creating", dv1 Creating decodePos)
      ; ("Filling", dv2 Filling decodeTLID decodeID) ]
  in
  decodeVariants
    [ ("Selecting", dv2 Selecting decodeTLID (JSD.maybe decodeID))
    ; ("Entering", dv1 Entering decodeEntering)
    ; ("Dragging", dv4 Dragging decodeTLID decodeVPos JSD.bool dcs)
    ; ("Deselected", dv0 Deselected)
    ; ("SelectingCommand", dv2 SelectingCommand decodeTLID decodeID) ]

let decodeNDarkType : nDarkType JSD.decoder =
  let dv4 = decodeVariant4 in
  let dv3 = decodeVariant3 in
  let dv2 = decodeVariant2 in
  let dv1 = decodeVariant1 in
  let dv0 = decodeVariant0 in
  decodeVariants
    [ ("Empty", dv0 DTEmpty)
    ; ("Any", dv0 DTAny)
    ; ("String", dv0 DTString)
    ; ("Int", dv0 DTInt)
    ; ( "Obj"
      , dv1 DTObj
          (JSD.list
             (decodePair (decodeBlankOr JSD.string)
                (JSD.lazy_ (fun _ -> decodeDarkType)))) ) ]

let decodeDarkType : darkType JSD.decoder = decodeBlankOr decodeNDarkType

let decodeExpr : expr JSD.decoder =
  let dn = JSD.lazy_ (fun _ -> decodeNExpr) in
  decodeVariants
    [ ("Filled", decodeVariant2 F decodeID dn)
    ; ("Blank", decodeVariant1 Blank decodeID) ]

let decodeNExpr : nExpr JSD.decoder =
  let de = JSD.lazy_ (fun _ -> decodeExpr) in
  let did = decodeID in
  let dv4 = decodeVariant4 in
  let dv3 = decodeVariant3 in
  let dv2 = decodeVariant2 in
  let dv1 = decodeVariant1 in
  decodeVariants
    [ ("Let", dv3 Let (decodeBlankOr JSD.string) de de)
    ; ("Value", dv1 Value JSD.string)
    ; ("If", dv3 If de de de)
    ; ( "FnCall"
      , dv2 (fun a b -> FnCall (a, b, NoRail)) JSD.string (JSD.list de) )
    ; ( "FnCallSendToRail"
      , dv2 (fun a b -> FnCall (a, b, Rail)) JSD.string (JSD.list de) )
    ; ("Lambda", dv2 Lambda (JSD.list (decodeBlankOr JSD.string)) de)
    ; ("Variable", dv1 Variable JSD.string)
    ; ("Thread", dv1 Thread (JSD.list de))
    ; ("FieldAccess", dv2 FieldAccess de (decodeBlankOr JSD.string))
    ; ("ListLiteral", dv1 ListLiteral (JSD.list de))
    ; ( "ObjectLiteral"
      , dv1 ObjectLiteral (JSD.list (decodePair (decodeBlankOr JSD.string) de))
      )
    ; ("FeatureFlag", dv4 FeatureFlag (decodeBlankOr JSD.string) de de de) ]

let decodeAnalysisResults : analysisResults JSD.decoder =
  let toAResult liveValues availableVarnames =
    { liveValues= DE.mapKeys (Util.toIntWithDefault 0) liveValues
    ; availableVarnames= DE.mapKeys (Util.toIntWithDefault 0) availableVarnames
    }
  in
  JSDP.decode toAResult
  |> JSDP.required "live_values" (JSD.dict decodeDval)
  |> JSDP.required "available_varnames" (JSD.dict (JSD.list JSD.string))

let decodeAnalysisEnvelope : (traceID * analysisResults) JSD.decoder =
  JSON.decodePair JSD.string decodeAnalysisResults

let decodeHandlerSpec : handlerSpec JSD.decoder =
  let toHS module_ name modifier input output =
    {module_; name; modifier; types= {input; output}}
  in
  JSDP.decode toHS
  |> JSDP.required "module" (decodeBlankOr JSD.string)
  |> JSDP.required "name" (decodeBlankOr JSD.string)
  |> JSDP.required "modifier" (decodeBlankOr JSD.string)
  |> JSDP.requiredAt ["types"; "input"] decodeDarkType
  |> JSDP.requiredAt ["types"; "output"] decodeDarkType

let decodeHandler : handler JSD.decoder =
  let toHandler ast spec tlid = {ast; spec; tlid} in
  JSDP.decode toHandler
  |> JSDP.required "ast" decodeExpr
  |> JSDP.required "spec" decodeHandlerSpec
  |> JSDP.required "tlid" decodeTLID

let decodeTipeString : string JSD.decoder = JSD.map RT.tipe2str decodeTipe

let decodeDBColList : dBColumn list JSD.decoder =
  JSD.list
    (decodePair (decodeBlankOr JSD.string) (decodeBlankOr decodeTipeString))

let decodeDBMigrationState : dBMigrationState JSD.decoder =
  let dv0 = decodeVariant0 in
  decodeVariants
    [ ("DBMigrationAbandoned", dv0 DBMigrationAbandoned)
    ; ("DBMigrationInitialized", dv0 DBMigrationInitialized) ]

let decodeDBMigration : dBMigration JSD.decoder =
  let toDBM sv v s cols rollf rollb =
    { startingVersion= sv
    ; version= v
    ; state= s
    ; cols
    ; rollforward= rollf
    ; rollback= rollb }
  in
  JSDP.decode toDBM
  |> JSDP.required "starting_version" JSD.int
  |> JSDP.required "version" JSD.int
  |> JSDP.required "state" decodeDBMigrationState
  |> JSDP.required "cols" decodeDBColList
  |> JSDP.required "rollforward" decodeExpr
  |> JSDP.required "rollback" decodeExpr

let decodeDB : dB JSD.decoder =
  let toDB tlid name cols version old active =
    { tlid= TLID tlid
    ; name
    ; cols
    ; version
    ; oldMigrations= old
    ; activeMigration= active }
  in
  JSDP.decode toDB
  |> JSDP.required "tlid" JSD.int
  |> JSDP.required "name" JSD.string
  |> JSDP.required "cols" decodeDBColList
  |> JSDP.required "version" JSD.int
  |> JSDP.required "old_migrations" (JSD.list decodeDBMigration)
  |> JSDP.required "active_migration" (JSD.maybe decodeDBMigration)

let decodeToplevel : toplevel JSD.decoder =
  let toToplevel id x y data = {id; pos= {x; y}; data} in
  let variant =
    decodeVariants
      [ ("Handler", decodeVariant1 TLHandler decodeHandler)
      ; ("DB", decodeVariant1 TLDB decodeDB) ]
  in
  JSDP.decode toToplevel
  |> JSDP.required "tlid" decodeTLID
  |> JSDP.requiredAt ["pos"; "x"] JSD.int
  |> JSDP.requiredAt ["pos"; "y"] JSD.int
  |> JSDP.required "data" variant

let decodeTipe : tipe JSD.decoder =
  let dv0 = decodeVariant0 in
  let dv1 = decodeVariant1 in
  decodeVariants
    [ ("TInt", dv0 TInt)
    ; ("TStr", dv0 TStr)
    ; ("TChar", dv0 TChar)
    ; ("TBool", dv0 TBool)
    ; ("TFloat", dv0 TFloat)
    ; ("TObj", dv0 TObj)
    ; ("TList", dv0 TList)
    ; ("TAny", dv0 TAny)
    ; ("TNull", dv0 TNull)
    ; ("TBlock", dv0 TBlock)
    ; ("TIncomplete", dv0 TIncomplete)
    ; ("TError", dv0 TError)
    ; ("TResp", dv0 TResp)
    ; ("TDB", dv0 TDB)
    ; ("TID", dv0 TID)
    ; ("TDate", dv0 TDate)
    ; ("TTitle", dv0 TTitle)
    ; ("TUrl", dv0 TUrl)
    ; ("TUuid", dv0 TUuid)
    ; ("TBelongsTo", dv1 TBelongsTo JSD.string)
    ; ("THasMany", dv1 THasMany JSD.string)
    ; ("TDbList", dv1 TDbList (JSD.lazy_ (fun _ -> decodeTipe)))
    ; ("TPassword", dv0 TPassword)
    ; ("TOption", dv0 TOption)
    ; ("TErrorRail", dv0 TErrorRail) ]

let decodeUserFunctionParameter : userFunctionParameter JSD.decoder =
  let toParam name tipe args option desc =
    {name; tipe; block_args= args; optional= option; description= desc}
  in
  JSDP.decode toParam
  |> JSDP.required "name" (decodeBlankOr JSD.string)
  |> JSDP.required "tipe" (decodeBlankOr decodeTipe)
  |> JSDP.required "block_args" (JSD.list JSD.string)
  |> JSDP.required "optional" JSD.bool
  |> JSDP.required "description" JSD.string

let decodeUserFunctionMetadata : userFunctionMetadata JSD.decoder =
  let toFn name params desc returnTipe infix =
    {name; parameters= params; description= desc; returnTipe; infix}
  in
  JSDP.decode toFn
  |> JSDP.required "name" (decodeBlankOr JSD.string)
  |> JSDP.required "parameters" (JSD.list decodeUserFunctionParameter)
  |> JSDP.required "description" JSD.string
  |> JSDP.required "return_type" (decodeBlankOr decodeTipe)
  |> JSDP.required "infix" JSD.bool

let decodeUserFunction : userFunction JSD.decoder =
  let toUserFn id meta ast = {tlid= id; metadata= meta; ast} in
  JSDP.decode toUserFn
  |> JSDP.required "tlid" decodeTLID
  |> JSDP.required "metadata" decodeUserFunctionMetadata
  |> JSDP.required "ast" decodeExpr

let decode404 : fourOhFour JSD.decoder =
  JSD.map3 FourOhFour (JSD.index 0 JSD.string) (JSD.index 1 JSD.string)
    (JSD.index 2 JSD.string)

let encode404 (fof : fourOhFour) : JSE.value =
  JSE.object_
    [ ("space", JSE.string fof.space)
    ; ("path", JSE.string fof.path)
    ; ("modifier", JSE.string fof.modifier) ]

let encodeInputValueDict (dict : inputValueDict) : JSE.value =
  dict |> Dict.toList |> encodeList (encodePair JSE.string encodeDval)

let decodeInputValueDict : inputValueDict JSD.decoder =
  JSD.map Dict.fromList (JSD.list (decodePair JSD.string decodeDval))

let decodeFunctionResult : functionResult JSD.decoder =
  let toFunctionResult (fnName, id, hash, value) =
    {fnName; callerID= id; argHash= hash; value}
  in
  JSD.map toFunctionResult
    (JSON.decodeQuadriple JSD.string decodeID JSD.string decodeDval)

let decodeTraces : traces JSD.decoder =
  JSD.map Dict.fromList (JSD.list (decodePair JSD.int (JSD.list decodeTrace)))

let decodeTrace : trace JSD.decoder =
  let toTrace id input functionResults = {id; input; functionResults} in
  JSDP.decode toTrace
  |> JSDP.required "id" JSD.string
  |> JSDP.required "input" decodeInputValueDict
  |> JSDP.required "function_results" (JSD.list decodeFunctionResult)

let encodeTrace (t : trace) : JSE.value =
  JSE.object_
    [ ( "input"
      , JSON.encodeList
          (encodePair JSE.string encodeDval)
          (Dict.toList t.input) )
    ; ( "function_results"
      , JSON.encodeList encodeFunctionResult t.functionResults )
    ; ("id", JSE.string t.id) ]

let encodeFunctionResult (fr : functionResult) : JSE.value =
  JSE.list
    [ JSE.string fr.fnName
    ; encodeID fr.callerID
    ; JSE.string fr.argHash
    ; encodeDval fr.value ]

let decodeExecuteFunctionTarget : (tlid * id) JSD.decoder =
  JSD.map2 Tuple2.create (JSD.index 0 decodeTLID) (JSD.index 1 decodeID)

let decodeRPC : rpcResult JSD.decoder =
  JSDP.decode Tuple6.create
  |> JSDP.required "toplevels" (JSD.list decodeToplevel)
  |> JSDP.required "deleted_toplevels" (JSD.list decodeToplevel)
  |> JSDP.required "new_traces" decodeTraces
  |> JSDP.required "global_varnames" (JSD.list JSD.string)
  |> JSDP.required "user_functions" (JSD.list decodeUserFunction)
  |> JSDP.required "unlocked_dbs" (JSD.list decodeTLID)

let decodeGetAnalysisRPC : getAnalysisResult JSD.decoder =
  JSDP.decode Tuple4.create
  |> JSDP.required "traces" decodeTraces
  |> JSDP.required "global_varnames" (JSD.list JSD.string)
  |> JSDP.required "404s" (JSD.list decode404)
  |> JSDP.required "unlocked_dbs" (JSD.list decodeTLID)

let decodeInitialLoadRPC : initialLoadResult JSD.decoder = decodeRPC

let decodeExecuteFunctionRPC : executeFunctionRPCResult JSD.decoder =
  JSDP.decode Tuple2.create
  |> JSDP.required "result" decodeDval
  |> JSDP.required "hash" JSD.string

let isLiteralString (s : string) : bool =
  match parseDvalLiteral s with None -> false | Some dv -> RT.isLiteral dv

let typeOfLiteralString (s : string) : tipe =
  match parseDvalLiteral s with None -> TIncomplete | Some dv -> RT.typeOf dv

let parseDvalLiteral (str : string) : dval option =
  let firstChar = String.uncons str |> Option.map Tuple.first in
  if String.toLower str = "nothing" then Some (DOption OptNothing)
  else
    match String.toList str with
    | ['\''; c; '\''] -> Some (DChar c)
    | '"' :: rest ->
        if List.last rest = Some '"' then
          List.init rest |> Option.withDefault [] |> String.fromList
          |> (fun x -> DStr x)
          |> fun x -> Some x
        else None
    | _ -> JSD.decodeString parseBasicDval str |> Result.toOption

let parseBasicDval : dval JSD.decoder =
  let dd = JSD.lazy_ (fun _ -> decodeDval) in
  JSD.oneOf
    [ JSD.map DInt JSD.int
    ; JSD.map DFloat JSD.float
    ; JSD.map DBool JSD.bool
    ; JSD.null DNull
    ; JSD.map DStr JSD.string
    ; JSD.map DList (JSD.list dd) ]

let decodeDval : dval JSD.decoder =
  let dv0 = decodeVariant0 in
  let dv1 = decodeVariant1 in
  let dv2 = decodeVariant2 in
  let dd = JSD.lazy_ (fun _ -> decodeDval) in
  let decodeOptionT =
    decodeVariants [("OptJust", dv1 OptJust dd); ("OptNothing", dv0 OptNothing)]
  in
  let decodeDhttp =
    decodeVariants
      [ ("Redirect", dv1 Redirect JSD.string)
      ; ( "Response"
        , dv2 Response JSD.int
            (JSD.list (JSON.decodePair JSD.string JSD.string)) ) ]
  in
  decodeVariants
    [ ("DInt", dv1 DInt JSD.int)
    ; ("DFloat", dv1 DFloat JSD.float)
    ; ("DBool", dv1 DBool JSD.bool)
    ; ("DNull", dv0 DNull)
    ; ("DStr", dv1 DStr JSD.string)
    ; ("DList", dv1 DList (JSD.list dd))
    ; ("DObj", dv1 DObj (JSD.dict dd))
    ; ("DIncomplete", dv0 DIncomplete)
    ; ("DError", dv1 DError JSD.string)
    ; ("DBlock", dv0 DBlock)
    ; ("DErrorRail", dv1 DErrorRail dd)
    ; ( "DResp"
      , dv1 (fun (h, dv) -> DResp (h, dv)) (JSON.decodePair decodeDhttp dd) )
    ; ("DDB", dv1 DDB JSD.string)
    ; ("DID", dv1 DID JSD.string)
    ; ("DDate", dv1 DDate JSD.string)
    ; ("DTitle", dv1 DTitle JSD.string)
    ; ("DUrl", dv1 DUrl JSD.string)
    ; ( "DPassword"
      , dv1
          ( Base64.decode
          >> Result.withDefault "<Internal error in base64 decoding>"
          >> DPassword )
          JSD.string )
    ; ("DUuid", dv1 DUuid JSD.string)
    ; ("DOption", dv1 DOption decodeOptionT) ]

let encodeDval (dv : dval) : JSE.value =
  let ev = encodeVariant in
  let encodeDhttp h =
    match h with
    | Redirect s -> ev "Redirect" [JSE.string s]
    | Response (code, headers) ->
        ev "Response"
          [JSE.int code; encodeList (encodePair JSE.string JSE.string) headers]
  in
  match dv with
  | DInt i -> ev "DInt" [JSE.int i]
  | DFloat f -> ev "DFloat" [JSE.float f]
  | DBool b -> ev "DBool" [JSE.bool b]
  | DNull -> ev "DNull" []
  | DStr s -> ev "DStr" [JSE.string s]
  | DList l -> ev "DList" [encodeList encodeDval l]
  | DObj o -> ev "DObj" [JSEE.dict identity encodeDval o]
  | DBlock -> ev "DBlock" []
  | DIncomplete -> ev "DIncomplete" []
  | DChar c -> ev "DChar" [JSE.string (String.fromList [c])]
  | DError msg -> ev "DError" [JSE.string msg]
  | DResp (h, hdv) ->
      ev "DResp" [JSON.encodePair encodeDhttp encodeDval (h, hdv)]
  | DDB name -> ev "DDB" [JSE.string name]
  | DID id -> ev "DID" [JSE.string id]
  | DUrl url -> ev "DUrl" [JSE.string url]
  | DTitle title -> ev "DTitle" [JSE.string title]
  | DDate date -> ev "DDate" [JSE.string date]
  | DPassword hashed -> ev "DPassword" [JSE.string (Base64.encode hashed)]
  | DUuid uuid -> ev "DUuid" [JSE.string uuid]
  | DOption opt ->
      ev "DOption"
        [ ( match opt with
          | OptNothing -> ev "OptNothing" []
          | OptJust dv -> ev "OptJust" [encodeDval dv] ) ]
  | DErrorRail dv -> ev "DErrorRail" [encodeDval dv]
