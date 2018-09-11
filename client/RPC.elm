module RPC exposing (..)

-- builtin
import Http
import Json.Encode as JSE
import Json.Decode as JSD
import Json.Decode.Pipeline as JSDP
import Dict.Extra as DE
import Json.Encode.Extra as JSEE
import Dict

-- lib

-- dark
import Types exposing (..)
import Util
import JSON exposing (..)
import Runtime as RT
import List.Extra as LE
-- import Blank as B
-- import Prelude exposing (..)

rpc_ : Model -> String ->
 (RPCParams -> Result Http.Error RPCResult -> Msg) ->
 RPCParams -> Cmd Msg
rpc_ m url callback params =
  let payload = encodeRPCParams params
      json = Http.jsonBody payload
      request = Http.post url json decodeRPC
  in Http.send (callback params) request

postString : String -> Http.Request String
postString url =
  Http.request
    { method = "POST"
    , headers = []
    , url = url
    , body = Http.emptyBody
    , expect = Http.expectString
    , timeout = Nothing
    , withCredentials = False
    }

rpc : Model -> Focus -> RPCParams -> Cmd Msg
rpc m focus params =
  rpc_ m "/admin/api/rpc" (RPCCallback focus) params

executeFunctionRPC : ExecuteFunctionRPCParams -> Cmd Msg
executeFunctionRPC params =
  let url = "/admin/api/execute_function"
      payload = encodeExecuteFunctionRPCParams params
      json = Http.jsonBody payload
      request = Http.post url json decodeExecuteFunctionRPC
  in Http.send (ExecuteFunctionRPCCallback params) request

getAnalysisRPC : AnalysisParams -> Cmd Msg
getAnalysisRPC params =
  let url = "/admin/api/get_analysis"
      payload = encodeAnalysisParams params
      json = Http.jsonBody payload
      request = Http.post url json decodeGetAnalysisRPC
  in Http.send GetAnalysisRPCCallback request

initialLoadRPC : Focus -> Cmd Msg
initialLoadRPC focus =
  let url = "/admin/api/initial_load"
      request = Http.post url Http.emptyBody decodeInitialLoadRPC
  in Http.send (InitialLoadRPCCallback focus NoChange) request

saveTestRPC : Cmd Msg
saveTestRPC =
  let url = "/admin/api/save_test"
      request = postString url
  in Http.send SaveTestRPCCallback request

emptyParams : RPCParams
emptyParams =
  { ops = [] }

opsParams : List Op -> RPCParams
opsParams ops =
  { ops = ops }

integrationRPC : Model -> String -> Cmd Msg
integrationRPC m name =
  let url = "/admin/api/initial_load"
      request = Http.post url Http.emptyBody decodeInitialLoadRPC
  in Http.send
      (InitialLoadRPCCallback FocusNothing (TriggerIntegrationTest name))
      request


decodePointerData : JSD.Decoder PointerData
decodePointerData =
  let dv1 = decodeVariant1 in
  decodeVariants
    [ ("PVarBind", dv1 PVarBind (decodeBlankOr JSD.string))
    , ("PEventName", dv1 PEventName (decodeBlankOr JSD.string))
    , ("PEventModifier", dv1 PEventModifier (decodeBlankOr JSD.string))
    , ("PEventSpace", dv1 PEventSpace (decodeBlankOr JSD.string))
    , ("PExpr", dv1 PExpr decodeExpr)
    , ("PField", dv1 PField (decodeBlankOr JSD.string))
    , ("PDBColName", dv1 PDBColName (decodeBlankOr JSD.string))
    , ("PDBColType", dv1 PDBColType (decodeBlankOr JSD.string))
    , ("PDarkType", dv1 PDarkType (decodeDarkType))
    , ("PDarkTypeField", dv1 PDarkTypeField (decodeBlankOr JSD.string))
    , ("PFFMsg", dv1 PFFMsg (decodeBlankOr JSD.string))
    , ("PFnName", dv1 PFnName (decodeBlankOr JSD.string))
    , ("PParamName", dv1 PParamName (decodeBlankOr JSD.string))
    , ("PParamTipe", dv1 PParamTipe (decodeBlankOr decodeTipe))
    ]

encodePointerData : PointerData -> JSE.Value
encodePointerData pd =
  let ev = encodeVariant in
  case pd of
    PVarBind var ->
      ev "PVarBind" [encodeBlankOr JSE.string var]
    PEventName name ->
      ev "PEventName" [encodeBlankOr JSE.string name]
    PEventModifier modifier ->
      ev "PEventModifier" [encodeBlankOr JSE.string modifier]
    PEventSpace space ->
      ev "PEventSpace" [encodeBlankOr JSE.string space]
    PExpr expr ->
      ev "PExpr" [encodeExpr expr]
    PField field ->
      ev "PField" [encodeBlankOr JSE.string field]
    PKey key ->
      ev "PKey" [encodeBlankOr JSE.string key]
    PDBColName colname ->
      ev "PDBColName" [encodeBlankOr JSE.string colname]
    PDBColType coltype ->
      ev "PDBColType" [encodeBlankOr JSE.string coltype]
    PDarkType darktype ->
      ev "PDarkType" [encodeDarkType darktype]
    PDarkTypeField darktypefield ->
      ev "PDarkTypeField" [encodeBlankOr JSE.string darktypefield]
    PFFMsg msg ->
      ev "PFFMsg" [encodeBlankOr JSE.string msg]
    PFnName msg ->
      ev "PFnName" [encodeBlankOr JSE.string msg]
    PParamName msg ->
      ev "PParamName" [encodeBlankOr JSE.string msg]
    PParamTipe msg ->
      ev "PParamTipe" [encodeBlankOr encodeTipe msg]

tlidOf : Op -> TLID
tlidOf op =
  case op of
    SetHandler tlid _ _ -> tlid
    CreateDB tlid _ _ -> tlid
    AddDBCol tlid _ _ -> tlid
    SetDBColName tlid _ _ -> tlid
    ChangeDBColName tlid _ _ -> tlid
    SetDBColType tlid _ _ -> tlid
    ChangeDBColType tlid _ _ -> tlid
    InitDBMigration tlid _ _ _ _ -> tlid
    TLSavepoint tlid -> tlid
    UndoTL tlid -> tlid
    RedoTL tlid -> tlid
    DeleteTL tlid -> tlid
    MoveTL tlid _ -> tlid
    SetFunction f -> f.tlid
    DeleteFunction tlid -> tlid
    SetExpr tlid _ _ -> tlid

encodeOps : List Op -> JSE.Value
encodeOps ops =
  ops
  |> (\ops ->
        case ops of
          [UndoTL _] -> ops
          [RedoTL _] -> ops
          [] -> ops
          _ ->
            let savepoints = ops
                             |> List.map tlidOf
                             |> List.map TLSavepoint
            in savepoints ++ ops)
  |> List.map encodeOp
  |> JSE.list

encodeSpec : HandlerSpec -> JSE.Value
encodeSpec spec =
  JSE.object
    [ ("name", encodeBlankOr JSE.string spec.name)
    , ("module", encodeBlankOr JSE.string spec.module_)
    , ("modifier", encodeBlankOr JSE.string spec.modifier)
    , ("types", encodeSpecTypes spec.types)
    ]

encodeHandler : Handler -> JSE.Value
encodeHandler h =
  JSE.object [ ("tlid", encodeTLID h.tlid)
             , ("spec", encodeSpec h.spec)
             , ("ast", encodeExpr h.ast) ]

encodeDBMigrationKind : DBMigrationKind -> JSE.Value
encodeDBMigrationKind k =
  let ev = encodeVariant in
  case k of
    ChangeColType -> ev "ChangeColType" []

encodeDBMigration : DBMigration -> JSE.Value
encodeDBMigration dbm =
  JSE.object [ ("starting_version", JSE.int dbm.startingVersion)
             , ("kind", encodeDBMigrationKind dbm.kind)
             , ("rollforward", encodeExpr dbm.rollforward)
             , ("rollback", encodeExpr dbm.rollback)
             , ("target", encodeID dbm.target)
             ]

encodeDB : DB -> JSE.Value
encodeDB db =
  let encodeCol =
        encodePair (encodeBlankOr JSE.string) (encodeBlankOr JSE.string)
  in
  JSE.object [ ("tlid", encodeTLID db.tlid)
             , ("name", JSE.string db.name)
             , ("cols", JSE.list (List.map encodeCol db.cols))
             , ("version", JSE.int db.version)
             , ("old_migrations", JSE.list (List.map encodeDBMigration db.oldMigrations))
             , ("active_migration", Maybe.map encodeDBMigration db.activeMigration
                                    |> Maybe.withDefault JSE.null)
             ]

encodeOp : Op -> JSE.Value
encodeOp call =
  let ev = encodeVariant in
    case call of
      SetHandler id pos h ->
        ev "SetHandler" [encodeTLID id, encodePos pos, encodeHandler h]

      CreateDB id pos name ->
        ev "CreateDB" [encodeTLID id, encodePos pos, JSE.string name]

      AddDBCol tlid colnameid coltypeid ->
        ev "AddDBCol" [encodeTLID tlid, encodeID colnameid, encodeID coltypeid]

      SetDBColName tlid id name ->
        ev "SetDBColName" [encodeTLID tlid, encodeID id, JSE.string name]

      ChangeDBColName tlid id name ->
        ev "ChangeDBColName" [encodeTLID tlid, encodeID id, JSE.string name]

      SetDBColType tlid id tipe ->
        ev "SetDBColType" [encodeTLID tlid, encodeID id, JSE.string tipe]

      ChangeDBColType tlid id name ->
        ev "ChangeDBColType" [encodeTLID tlid, encodeID id, JSE.string name]

      InitDBMigration tlid id rbid rfid kind ->
        ev "InitDBMigration"
          [ encodeTLID tlid
          , encodeID id
          , encodeID rbid
          , encodeID rfid
          , encodeDBMigrationKind kind]

      TLSavepoint tlid ->
        ev "TLSavepoint" [encodeTLID tlid]
      UndoTL tlid -> ev "UndoTL" [encodeTLID tlid]
      RedoTL tlid -> ev "RedoTL" [encodeTLID tlid]
      DeleteTL tlid -> ev "DeleteTL" [encodeTLID tlid]
      MoveTL tlid pos -> ev "MoveTL" [encodeTLID tlid, encodePos pos]
      SetFunction uf -> ev "SetFunction" [encodeUserFunction uf]
      DeleteFunction tlid -> ev "DeleteFunction" [encodeTLID tlid]
      SetExpr tlid id e ->
        ev "SetExpr" [encodeTLID tlid, encodeID id, encodeExpr e]

encodeRPCParams : RPCParams -> JSE.Value
encodeRPCParams params =
  JSE.object
    [ ("ops", encodeOps params.ops) ]

encodeExecuteFunctionRPCParams : ExecuteFunctionRPCParams -> JSE.Value
encodeExecuteFunctionRPCParams params =
  JSE.object
    [ ("tlid", encodeTLID params.tlid)
    , ("trace_id", JSE.string params.traceID)
    , ("caller_id", encodeID params.callerID)
    , ("args", encodeList encodeDval params.args)
    , ("fnname", JSE.string params.fnName)
    ]

encodeAnalysisParams : AnalysisParams -> JSE.Value
encodeAnalysisParams params =
  encodeList encodeTLID params

encodeUserFunction : UserFunction -> JSE.Value
encodeUserFunction uf =
  JSE.object
    [("tlid", encodeTLID uf.tlid)
    ,("metadata", encodeUserFunctionMetadata uf.metadata)
    ,("ast", encodeExpr uf.ast)
    ]


encodeUserFunctionMetadata : UserFunctionMetadata -> JSE.Value
encodeUserFunctionMetadata f =
  JSE.object
  [("name", encodeBlankOr JSE.string f.name)
  ,("parameters", JSE.list (List.map encodeUserFunctionParameter f.parameters))
  ,("description", JSE.string f.description)
  ,("return_type", encodeBlankOr encodeTipe f.returnTipe)
  ,("infix", JSE.bool f.infix)
  ]

encodeTipe : Tipe -> JSE.Value
encodeTipe t =
  let ev = encodeVariant
  in
      case t of
        TInt -> ev "TInt" []
        TStr -> ev "TStr" []
        TChar -> ev "TChar" []
        TBool -> ev "TBool" []
        TFloat -> ev "TFloat" []
        TObj -> ev "TObj" []
        TList -> ev "TList" []
        TAny -> ev "TAny" []
        TNull -> ev "TNull" []
        TBlock -> ev "TBlock" []
        TIncomplete -> ev "TIncomplete" []
        TError -> ev "TError" []
        TResp -> ev "TResp" []
        TDB -> ev "TDB" []
        TID -> ev "TID" []
        TDate -> ev "TDate" []
        TTitle -> ev "TTitle" []
        TUrl -> ev "TUrl" []
        TBelongsTo s -> ev "TBelongsTo" [JSE.string s]
        THasMany s -> ev "THasMany" [JSE.string s]
        TDbList a -> ev "TDbList" [encodeTipe a]
        TPassword -> ev "TPassword" []
        TUuid -> ev "TUuid" []
        TOption -> ev "TOption" []
        TErrorRail -> ev "TErrorRail" []

encodeUserFunctionParameter : UserFunctionParameter -> JSE.Value
encodeUserFunctionParameter p =
  JSE.object
  [("name", encodeBlankOr JSE.string p.name)
  ,("tipe", encodeBlankOr encodeTipe p.tipe)
  ,("block_args", JSE.list (List.map JSE.string p.block_args))
  ,("optional", JSE.bool p.optional)
  ,("description", JSE.string p.description)
  ]


encodeExpr : Expr -> JSE.Value
encodeExpr expr =
  encodeBlankOr encodeNExpr expr

encodeNExpr : NExpr -> JSE.Value
encodeNExpr expr =
  let e = encodeExpr
      ev = encodeVariant in
  case expr of
    FnCall n exprs r ->
      if r == Rail
      then
        ev "FnCallSendToRail" [ JSE.string n
                              , JSE.list (List.map e exprs)]
      else
        ev "FnCall" [ JSE.string n
                    , JSE.list (List.map e exprs)]

    Let lhs rhs body ->
      ev "Let" [ encodeBlankOr JSE.string lhs
               , e rhs
               , e body]

    Lambda vars body ->
      ev "Lambda" [ List.map (encodeBlankOr JSE.string) vars |> JSE.list
                  , e body]

    FieldAccess obj field ->
      ev "FieldAccess" [e obj, encodeBlankOr JSE.string field]

    If cond then_ else_ -> ev "If" [e cond, e then_, e else_]
    Variable v -> ev "Variable" [ JSE.string v]
    Value v -> ev "Value" [ JSE.string v]
    Thread exprs -> ev "Thread" [JSE.list (List.map e exprs)]
    ObjectLiteral pairs ->
      let encoder = JSON.encodePair (encodeBlankOr JSE.string) e in
      ev "ObjectLiteral" [(List.map encoder pairs) |> JSE.list ]
    ListLiteral elems ->
      ev "ListLiteral" [JSE.list (List.map e elems)]
    FeatureFlag msg cond a b ->
      ev "FeatureFlag" [encodeBlankOr JSE.string msg, e cond, e a, e b]




  -- about the lazy decodeExpr
  -- TODO: check if elm 0.19 is saner than this
  -- elm 0.18 gives "Cannot read property `tag` of undefined` which
  -- leads to https://github.com/elm-lang/elm-compiler/issues/1562
  -- and https://github.com/elm-lang/elm-compiler/issues/1591
  -- which gets potentially fixed by
  -- https://github.com/elm-lang/elm-compiler/commit/e2a51574d3c4f1142139611cb359d0e68bb9541a

encodeSpecTypes : SpecTypes -> JSE.Value
encodeSpecTypes st =
  JSE.object
    [ ("input", encodeDarkType st.input)
    , ("output", encodeDarkType st.output)
    ]

encodeDarkType : DarkType -> JSE.Value
encodeDarkType dt =
  encodeBlankOr encodeNDarkType dt

encodeNDarkType : NDarkType -> JSE.Value
encodeNDarkType t =
  let ev = encodeVariant in
  case t of
    DTEmpty -> ev "Empty" []
    DTAny -> ev "Any" []
    DTString -> ev "String" []
    DTInt -> ev "Int" []
    DTObj ts ->
      ev "Obj"
        [(JSE.list
          (List.map
            (encodePair
              (encodeBlankOr JSE.string)
              encodeDarkType)
            ts))]

encodeCursorState : CursorState -> JSE.Value
encodeCursorState cs =
  let ev = encodeVariant in
  case cs of
    Selecting tlid mId ->
      ev "Selecting" [encodeTLID tlid, JSEE.maybe encodeID mId]
    SelectingCommand tlid mId ->
      ev "SelectingCommand" [encodeTLID tlid, encodeID mId]
    Entering (Creating pos) ->
      ev "Entering" [ev "Creating" [encodePos pos]]
    Entering (Filling tlid id) ->
      ev "Entering" [ev "Filling" [encodeTLID tlid, encodeID id]]
    Dragging tlid vpos hasMoved cursor ->
      ev "Dragging" [ encodeTLID tlid
                    , encodeVPos vpos
                    , JSE.bool hasMoved
                    , encodeCursorState cursor
                    ]
    Deselected ->
      ev "Deselected" []



encodeSerializableEditor : SerializableEditor -> JSE.Value
encodeSerializableEditor se =
  JSE.object
    [ ("clipboard", JSEE.maybe encodePointerData se.clipboard)
    , ("timersEnabled", JSE.bool se.timersEnabled)
    , ("cursorState", encodeCursorState se.cursorState)
    , ("lockedHandlers",  JSE.list (List.map (\id -> encodeTLID id) se.lockedHandlers))
    ]


decodeSerializableEditor : JSD.Decoder SerializableEditor
decodeSerializableEditor =
  -- always make these optional so that we don't crash the page when we
  -- change the structure
  JSDP.decode SerializableEditor
  |> JSDP.optional "clipboard" (JSD.maybe decodePointerData) Nothing
  |> JSDP.optional "timersEnabled" JSD.bool True
  |> JSDP.optional "cursorState" decodeCursorState Deselected
  |> JSDP.optional "lockedHandlers" (JSD.list decodeTLID) []



decodeCursorState : JSD.Decoder CursorState
decodeCursorState =
  let dv4 = decodeVariant4
      dv3 = decodeVariant3
      dv2 = decodeVariant2
      dv1 = decodeVariant1
      dv0 = decodeVariant0
      dcs = JSD.lazy (\_ -> decodeCursorState)
      decodeEntering =
        decodeVariants
          [ ("Creating", dv1 Creating decodePos)
          , ("Filling", dv2 Filling decodeTLID decodeID)
          ]
  in
  decodeVariants
    [ ("Selecting", dv2 Selecting decodeTLID (JSD.maybe decodeID))
    , ("Entering", dv1 Entering decodeEntering)
    , ("Dragging", dv4 Dragging decodeTLID decodeVPos JSD.bool dcs)
    , ("Deselected", dv0 Deselected)
    , ("SelectingCommand", dv2 SelectingCommand decodeTLID decodeID)
    ]


decodeNDarkType : JSD.Decoder NDarkType
decodeNDarkType =
  let dv4 = decodeVariant4
      dv3 = decodeVariant3
      dv2 = decodeVariant2
      dv1 = decodeVariant1
      dv0 = decodeVariant0 in
  decodeVariants
    [ ("Empty", dv0 DTEmpty)
    , ("Any", dv0 DTAny)
    , ("String", dv0 DTString)
    , ("Int", dv0 DTInt)
    , ("Obj", dv1 DTObj
               (JSD.list
                 (decodePair
                   (decodeBlankOr JSD.string)
                   (JSD.lazy (\_ -> decodeDarkType)))))
    ]

decodeDarkType : JSD.Decoder DarkType
decodeDarkType = decodeBlankOr decodeNDarkType

decodeExpr : JSD.Decoder Expr
decodeExpr =
  let dn = JSD.lazy (\_ -> decodeNExpr) in
  decodeVariants
  [ ("Filled", decodeVariant2 F decodeID dn)
  , ("Blank", decodeVariant1 Blank decodeID)
  ]



decodeNExpr : JSD.Decoder NExpr
decodeNExpr =
  let de = (JSD.lazy (\_ -> decodeExpr))
      did = decodeID
      dv4 = decodeVariant4
      dv3 = decodeVariant3
      dv2 = decodeVariant2
      dv1 = decodeVariant1 in
  decodeVariants
    -- In order to ignore the server for now, we tweak from one format
    -- to the other.
    [ ("Let", dv3 Let (decodeBlankOr JSD.string) de de)
    , ("Value", dv1 Value JSD.string)
    , ("If", dv3 If de de de)
    , ("FnCall", dv2 (\a b -> FnCall a b NoRail) JSD.string (JSD.list de))
    , ("FnCallSendToRail", dv2 (\a b -> FnCall a b Rail) JSD.string (JSD.list de))
    , ("Lambda", dv2 Lambda (JSD.list (decodeBlankOr JSD.string)) de)
    , ("Variable", dv1 Variable JSD.string)
    , ("Thread", dv1 Thread (JSD.list de))
    , ("FieldAccess", dv2 FieldAccess de (decodeBlankOr JSD.string))
    , ("ListLiteral", dv1 ListLiteral (JSD.list de))
    , ("ObjectLiteral", dv1 ObjectLiteral
                            (JSD.list (decodePair
                                         (decodeBlankOr JSD.string)
                                         de)))
    , ("FeatureFlag", dv4 FeatureFlag (decodeBlankOr JSD.string) de de de)
    ]


decodeAnalysisResults : JSD.Decoder AnalysisResults
decodeAnalysisResults =
  let toAResult liveValues availableVarnames =
        { liveValues = (DE.mapKeys (Util.toIntWithDefault 0) liveValues)
        , availableVarnames = (DE.mapKeys (Util.toIntWithDefault 0) availableVarnames)
        }
  in
  JSDP.decode toAResult
  |> JSDP.required "live_values" (JSD.dict decodeDval)
  |> JSDP.required "available_varnames" (JSD.dict (JSD.list JSD.string))


decodeAnalysisEnvelope : JSD.Decoder (TraceID, AnalysisResults)
decodeAnalysisEnvelope =
  JSON.decodePair JSD.string decodeAnalysisResults

decodeHandlerSpec : JSD.Decoder HandlerSpec
decodeHandlerSpec =
  let toHS module_ name modifier input output =
        { module_ = module_
        , name = name
        , modifier = modifier
        , types = { input = input
                  , output = output
                  }
        }
  in
  JSDP.decode toHS
  |> JSDP.required "module" (decodeBlankOr JSD.string)
  |> JSDP.required "name" (decodeBlankOr JSD.string)
  |> JSDP.required "modifier" (decodeBlankOr JSD.string)
  |> JSDP.requiredAt ["types", "input"] decodeDarkType
  |> JSDP.requiredAt ["types", "output"] decodeDarkType

decodeHandler : JSD.Decoder Handler
decodeHandler =
  let toHandler ast spec tlid = {ast = ast, spec = spec, tlid = tlid } in
  JSDP.decode toHandler
  |> JSDP.required "ast" decodeExpr
  |> JSDP.required "spec" decodeHandlerSpec
  |> JSDP.required "tlid" decodeTLID

decodeTipeString : JSD.Decoder String
decodeTipeString =
  JSD.map RT.tipe2str decodeTipe

decodeDBMigrationKind : JSD.Decoder DBMigrationKind
decodeDBMigrationKind =
  decodeVariant0 ChangeColType


decodeDBMigration : JSD.Decoder DBMigration
decodeDBMigration =
  let toDBM v kind rollf rollb target =
        { startingVersion = v
        , kind = kind
        , rollforward = rollf
        , rollback = rollb
        , target = target
        }
  in
  JSDP.decode toDBM
  |> JSDP.required "starting_version" JSD.int
  |> JSDP.required "kind" decodeDBMigrationKind
  |> JSDP.required "rollforward" decodeExpr
  |> JSDP.required "rollback" decodeExpr
  |> JSDP.required "target" decodeID

decodeDB : JSD.Decoder DB
decodeDB =
  let toDB tlid name cols version old active =
      { tlid = TLID tlid
      , name = name
      , cols = cols
      , version = version
      , oldMigrations = old
      , activeMigration = active
      }
  in
  JSDP.decode toDB
  |> JSDP.required "tlid" JSD.int
  |> JSDP.required "name" JSD.string
  |> JSDP.required "cols" (JSD.list
                            (decodePair
                              (decodeBlankOr JSD.string)
                              (decodeBlankOr decodeTipeString)))
  |> JSDP.required "version" JSD.int
  |> JSDP.required "old_migrations" (JSD.list decodeDBMigration)
  |> JSDP.required "active_migration" (JSD.maybe decodeDBMigration)


decodeToplevel : JSD.Decoder Toplevel
decodeToplevel =
  let toToplevel id x y data =
        { id = id
        , pos = { x=x, y=y }
        , data = data }
      variant = decodeVariants
                  [ ("Handler", decodeVariant1 TLHandler decodeHandler)
                  , ("DB", decodeVariant1 TLDB decodeDB) ]

  in
  JSDP.decode toToplevel
  |> JSDP.required "tlid" decodeTLID
  |> JSDP.requiredAt ["pos", "x"] JSD.int
  |> JSDP.requiredAt ["pos", "y"] JSD.int
  |> JSDP.required "data" variant

decodeTipe : JSD.Decoder Tipe
decodeTipe =
  let dv0 = decodeVariant0
      dv1 = decodeVariant1
  in
      decodeVariants
      [("TInt", dv0 TInt)
      ,("TStr", dv0 TStr)
      ,("TChar", dv0 TChar)
      ,("TBool", dv0 TBool)
      ,("TFloat", dv0 TFloat)
      ,("TObj", dv0 TObj)
      ,("TList", dv0 TList)
      ,("TAny", dv0 TAny)
      ,("TNull", dv0 TNull)
      ,("TBlock", dv0 TBlock)
      ,("TIncomplete", dv0 TIncomplete)
      ,("TError", dv0 TError)
      ,("TResp", dv0 TResp)
      ,("TDB", dv0 TDB)
      ,("TID", dv0 TID)
      ,("TDate", dv0 TDate)
      ,("TTitle", dv0 TTitle)
      ,("TUrl", dv0 TUrl)
      ,("TUuid", dv0 TUuid)
      ,("TBelongsTo", dv1 TBelongsTo JSD.string)
      ,("THasMany", dv1 THasMany JSD.string)
      ,("TDbList", dv1 TDbList (JSD.lazy (\_ -> decodeTipe)))
      ,("TPassword", dv0 TPassword)
      ,("TOption", dv0 TOption)
      ]

decodeUserFunctionParameter : JSD.Decoder UserFunctionParameter
decodeUserFunctionParameter =
  let toParam name tipe args option desc =
        { name = name
        , tipe = tipe
        , block_args = args
        , optional = option
        , description = desc
        }
  in
      JSDP.decode toParam
      |> JSDP.required "name" (decodeBlankOr JSD.string)
      |> JSDP.required "tipe" (decodeBlankOr decodeTipe)
      |> JSDP.required "block_args" (JSD.list JSD.string)
      |> JSDP.required "optional" JSD.bool
      |> JSDP.required "description" JSD.string

decodeUserFunctionMetadata : JSD.Decoder UserFunctionMetadata
decodeUserFunctionMetadata =
  let toFn name params desc returnTipe infix =
        { name = name
        , parameters = params
        , description = desc
        , returnTipe = returnTipe
        , infix = infix
        }
  in
      JSDP.decode toFn
      |> JSDP.required "name" (decodeBlankOr JSD.string)
      |> JSDP.required "parameters" (JSD.list decodeUserFunctionParameter)
      |> JSDP.required "description" JSD.string
      |> JSDP.required "return_type" (decodeBlankOr decodeTipe)
      |> JSDP.required "infix" JSD.bool

decodeUserFunction : JSD.Decoder UserFunction
decodeUserFunction =
  let toUserFn id meta ast =
      { tlid = id
      , metadata = meta
      , ast = ast
      }
  in
      JSDP.decode toUserFn
      |> JSDP.required "tlid" decodeTLID
      |> JSDP.required "metadata" decodeUserFunctionMetadata
      |> JSDP.required "ast" decodeExpr

decode404 : JSD.Decoder FourOhFour
decode404 =
  JSD.map3 FourOhFour
    (JSD.index 0 JSD.string)
    (JSD.index 1 JSD.string)
    (JSD.index 2 JSD.string)

encodeInputValueDict : InputValueDict -> JSE.Value
encodeInputValueDict dict =
  dict
  |> Dict.toList
  |> encodeList (encodePair JSE.string encodeDval)

decodeInputValueDict : JSD.Decoder InputValueDict
decodeInputValueDict =
  JSD.map Dict.fromList
    (JSD.list (decodePair JSD.string decodeDval))

decodeFunctionResult : JSD.Decoder FunctionResult
decodeFunctionResult =
  let toFunctionResult fnName id hash value =
        { fnName = fnName
        , callerID = id
        , argHash = hash
        , value = value
        }
  in
  JSD.map4 toFunctionResult
      JSD.index 0 JSD.string
      JSD.index 1 decodeID
      JSD.index 2 JSD.string
      JSD.index 3 decodeDval

decodeTraces : JSD.Decoder Traces
decodeTraces =
  JSD.map Dict.fromList
    (JSD.list (decodePair JSD.int (JSD.list decodeTrace)))

decodeTrace : JSD.Decoder Trace
decodeTrace =
  let toTrace id input functionResults =
    { id = id, input = input, functionResults = functionResults }
  in
  JSDP.decode toTrace
  |> JSDP.required "id" JSD.string
  |> JSDP.required "input" decodeInputValueDict
  |> JSDP.required "function_results" (JSD.list decodeFunctionResult)

encodeTrace : Trace -> JSE.Value
encodeTrace t =
  JSE.object [ ( "input"
               , JSON.encodeList
                   (encodePair JSE.string encodeDval)
                   (Dict.toList t.input))
             , ( "function_results"
               , JSON.encodeList encodeFunctionResult t.functionResults)
             , ( "id", JSE.string t.id)
            ]

encodeFunctionResult : FunctionResult -> JSE.Value
encodeFunctionResult fr =
  JSE.list [ JSE.string fr.fnName
           , encodeID fr.callerID
           , JSE.string fr.argHash
           , encodeDval fr.value
           ]

decodeExecuteFunctionTarget : JSD.Decoder (TLID, ID)
decodeExecuteFunctionTarget =
  JSD.map2 (,)
    (JSD.index 0 decodeTLID)
    (JSD.index 1 decodeID)

decodeRPC : JSD.Decoder RPCResult
decodeRPC =
  JSDP.decode RPCResult
  |> JSDP.required "toplevels" (JSD.list decodeToplevel)
  |> JSDP.required "deleted_toplevels" (JSD.list decodeToplevel)
  |> JSDP.required "new_traces" decodeTraces
  |> JSDP.required "global_varnames" (JSD.list JSD.string)
  |> JSDP.required "user_functions" (JSD.list decodeUserFunction)
  |> JSDP.required "unlocked_dbs" (JSD.list decodeTLID)

decodeGetAnalysisRPC : JSD.Decoder GetAnalysisResult
decodeGetAnalysisRPC =
  JSDP.decode GetAnalysisResult
  |> JSDP.required "traces" decodeTraces
  |> JSDP.required "global_varnames" (JSD.list JSD.string)
  |> JSDP.required "404s" (JSD.list decode404)
  |> JSDP.required "unlocked_dbs" (JSD.list decodeTLID)

decodeInitialLoadRPC : JSD.Decoder InitialLoadResult
decodeInitialLoadRPC = decodeRPC

decodeExecuteFunctionRPC : JSD.Decoder ExecuteFunctionRPCResult
decodeExecuteFunctionRPC =
  JSDP.decode (,)
  |> JSDP.required "result" decodeDval
  |> JSDP.required "hash" JSD.string

--------------------------
-- Dval (some here because of cyclic dependencies)
-------------------------



isLiteralString : String -> Bool
isLiteralString s =
  case parseDvalLiteral s of
    Nothing -> False
    Just dv -> RT.isLiteral dv

typeOfLiteralString : String -> Tipe
typeOfLiteralString s =
  case parseDvalLiteral s of
    Nothing -> TIncomplete
    Just dv -> RT.typeOf dv




encodeDval : Dval -> JSE.Value
encodeDval dv =
  let tipe = dv |> RT.typeOf |> RT.tipe2str |> String.toLower |> JSE.string
      ev = encodeVariant
      encodeDhttp h =
        case h of
          Redirect s -> ev "Redirect" [JSE.string  s]
          Response resp  -> ev "Response"
            [encodePair
              JSE.int
              (encodeList (encodePair JSE.string JSE.string))
              resp]

      wrapUserType value =
        JSE.object [ ("type", tipe)
                   , ("value", value)]

      wrapUserStr value = wrapUserType (JSE.string value)
  in
  case dv of
    DInt i -> JSE.int i
    DFloat f -> JSE.float f
    DBool b -> JSE.bool b
    DNull -> JSE.null
    DStr s -> JSE.string s
    DList l -> encodeList encodeDval l
    DObj o -> JSEE.dict identity encodeDval o

    -- opaque types
    DBlock -> wrapUserType JSE.null
    DIncomplete -> wrapUserType JSE.null

    -- user-ish types
    DChar c -> wrapUserStr (String.fromList [c])
    DError msg -> wrapUserStr msg

    DResp (h, hdv) ->
      wrapUserType (JSE.list [ encodeDhttp h, encodeDval hdv])

    DDB name -> wrapUserStr name
    DID id -> wrapUserStr id
    DUrl url -> wrapUserStr url
    DTitle title -> wrapUserStr title
    DDate date -> wrapUserStr date
    DPassword hashed -> wrapUserStr hashed
    DUuid uuid -> wrapUserStr uuid
    DOption opt ->
      case opt of
        Nothing -> wrapUserType JSE.null
        Just dv -> wrapUserType (encodeDval dv)
    DErrorRail _ -> wrapUserType JSE.null


processObject : Dict.Dict String Dval -> Dval
processObject dict =
  case (Dict.get "type" dict, Dict.get "value" dict) of
    (Just (DStr tipe), Just DNull) ->
      case tipe of
        "incomplete" -> DIncomplete
        _ -> DObj dict

    (Just (DStr tipe), Just (DStr value)) ->
      case tipe of
        "date" -> DDate value
        "id" -> DID value
        "title" -> DTitle value
        "url" -> DUrl value
        "error" -> DError value
        "char" ->
          case String.uncons value of
            Just (c, "") -> DChar c
            _ -> DObj dict
        "password" -> DPassword value
        "db" -> DDB value
        "block" -> DBlock
        "uuid" -> DUuid value
        "datastore" -> DDB value
        _ -> DObj dict
    _ -> DObj dict

-- Ported directly from Dval.parse in the backend
parseDvalLiteral : String -> Maybe Dval
parseDvalLiteral str =
  let firstChar = String.uncons str
                  |> Maybe.map Tuple.first
  in
  if String.toLower str == "nothing"
  then Just (DOption Nothing)
  else
    case String.toList str of
      ['\'', c, '\'' ] -> Just (DChar c)
      '"' :: rest ->
        if LE.last rest == Just '"'
        then LE.init rest
             |> Maybe.withDefault []
             |> String.fromList
             |> DStr
             |> Just
        else Nothing
      _ ->
        JSD.decodeString decodeDval str
        |> Result.toMaybe

decodeDval : JSD.Decoder Dval
decodeDval =
  let dd = JSD.lazy (\_ -> decodeDval) in
  JSD.oneOf
  [ JSD.map DInt JSD.int
  , JSD.map DFloat JSD.float
  , JSD.map DBool JSD.bool
  , JSD.null DNull
  , JSD.map DStr JSD.string
  , JSD.map DList (JSD.list dd)
  , JSD.map processObject (JSD.dict dd)
  ]
