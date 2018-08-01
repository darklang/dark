module RPC exposing (..)

-- builtin
import Http
import Json.Encode as JSE
import Json.Decode as JSD
import Json.Decode.Pipeline as JSDP
import Dict.Extra as DE
import Json.Encode.Extra as JSEE

-- lib

-- dark
import Types exposing (..)
import Runtime as RT
import Util
import JSON exposing (..)
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
  in Http.send ExecuteFunctionRPCCallback request

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

encodeOp : Op -> JSE.Value
encodeOp call =
  let ev = encodeVariant
  in
    case call of
      SetHandler id pos h ->
        let hs = JSE.object
                   [ ("name", encodeBlankOr JSE.string h.spec.name)
                   , ("module", encodeBlankOr JSE.string h.spec.module_)
                   , ("modifier", encodeBlankOr JSE.string h.spec.modifier)
                   , ("types", encodeSpecTypes h.spec.types)
                   ]
            handler = JSE.object [ ("tlid", encodeTLID id)
                                 , ("spec", hs)
                                 , ("ast", encodeExpr h.ast) ] in
        ev "SetHandler" [encodeTLID id, encodePos pos, handler]

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
      SetExpr tlid id e ->
        ev "SetExpr" [encodeTLID tlid, encodeID id, encodeExpr e]

encodeRPCParams : RPCParams -> JSE.Value
encodeRPCParams params =
  JSE.object
    [ ("ops", encodeOps params.ops) ]

encodeExecuteFunctionRPCParams : ExecuteFunctionRPCParams -> JSE.Value
encodeExecuteFunctionRPCParams params =
  let fns = [params.function] in
  JSE.object
    [ ("executable_fns"
      , JSE.list (List.map (encodeTriple encodeTLID encodeID JSE.int) fns)
      )
    ]

encodeAnalysisParams : AnalysisParams -> JSE.Value
encodeAnalysisParams params =
  JSE.list (List.map encodeTLID params)

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
    FnCall n exprs ->
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

encodeDBMigrationKind : DBMigrationKind -> JSE.Value
encodeDBMigrationKind k =
  let ev = encodeVariant in
  case k of
    ChangeColType -> ev "ChangeColType" []

encodeSerializableEditor : SerializableEditor -> JSE.Value
encodeSerializableEditor se =
  JSE.object
    [ ("clipboard", JSEE.maybe encodePointerData se.clipboard)
    , ("timersEnabled", JSE.bool se.timersEnabled)
    , ("cursorState", encodeCursorState se.cursorState)
    ]


decodeSerializableEditor : JSD.Decoder SerializableEditor
decodeSerializableEditor =
  -- always make these optional so that we don't crash the page when we
  -- change the structure
  JSDP.decode SerializableEditor
  |> JSDP.optional "clipboard" (JSD.maybe decodePointerData) Nothing
  |> JSDP.optional "timersEnabled" JSD.bool True
  |> JSDP.optional "cursorState" decodeCursorState Deselected



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
    , ("FnCall", dv2 FnCall JSD.string (JSD.list de))
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

decodeLiveValue : JSD.Decoder LiveValue
decodeLiveValue =
  let toLiveValue value tipe json exc =
      { value = value
      , tipe = RT.str2tipe tipe
      , json = json
      , exc = exc}
  in
  JSDP.decode toLiveValue
    |> JSDP.required "value" JSD.string
    |> JSDP.required "type" JSD.string
    |> JSDP.required "json" JSD.string
    |> JSDP.optional "exc" (JSD.maybe JSON.decodeException) Nothing

decodeAResult : JSD.Decoder AResult
decodeAResult =
  let toAResult astValue liveValues availableVarnames inputValues =
        { astValue = astValue
        , liveValues = (DE.mapKeys (Util.toIntWithDefault 0) liveValues)
        , availableVarnames = (DE.mapKeys (Util.toIntWithDefault 0) availableVarnames)
        , inputValues = inputValues
        }
  in
      JSDP.decode toAResult
      |> JSDP.required "ast_value" decodeLiveValue
      |> JSDP.required "live_values" (JSD.dict decodeLiveValue)
      |> JSDP.required "available_varnames" (JSD.dict (JSD.list JSD.string))
      |> JSDP.required "input_values" (JSD.dict decodeLiveValue)


decodeTLAResult : JSD.Decoder TLAResult
decodeTLAResult =
  let toTLAResult tlid results =
        { id = TLID tlid
        , results = results
        }
  in
      JSDP.decode toTLAResult
      |> JSDP.required "id" JSD.int
      |> JSDP.required "results" (JSD.list decodeAResult)


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
  let toHandler ast spec = {ast = ast, spec = spec } in
  JSDP.decode toHandler
  |> JSDP.required "ast" decodeExpr
  |> JSDP.required "spec" decodeHandlerSpec

decodeTipeString : JSD.Decoder String
decodeTipeString =
  decodeTipe
  |> JSD.map (RT.tipe2str)

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
  let toDB name cols version old active =
      { name = name
      , cols = cols
      , version = version
      , oldMigrations = old
      , activeMigration = active
      }
  in
  JSDP.decode toDB
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
      ,("TBelongsTo", dv1 TBelongsTo JSD.string)
      ,("THasMany", dv1 THasMany JSD.string)
      ,("TDbList", dv1 TDbList (JSD.lazy (\_ -> decodeTipe)))
      ,("TPassword", dv0 TPassword)
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
  JSD.map4 (,,,)
    (JSD.index 0 JSD.string)
    (JSD.index 1 JSD.string)
    (JSD.index 2 JSD.string)
    (JSD.index 3 (JSD.list JSD.value))

decodeExecuteFunctionTarget : JSD.Decoder (TLID, ID)
decodeExecuteFunctionTarget =
  JSD.map2 (,)
    (JSD.index 0 decodeTLID)
    (JSD.index 1 decodeID)

decodeRPC : JSD.Decoder RPCResult
decodeRPC =
  JSDP.decode (,,,,)
  |> JSDP.required "toplevels" (JSD.list decodeToplevel)
  |> JSDP.required "new_analyses" (JSD.list decodeTLAResult)
  |> JSDP.required "global_varnames" (JSD.list JSD.string)
  |> JSDP.required "user_functions" (JSD.list decodeUserFunction)
  |> JSDP.required "unlocked_dbs" (JSD.list decodeTLID)

decodeGetAnalysisRPC : JSD.Decoder GetAnalysisResult
decodeGetAnalysisRPC =
  JSDP.decode (,,,)
  |> JSDP.required "analyses" (JSD.list decodeTLAResult)
  |> JSDP.required "global_varnames" (JSD.list JSD.string)
  |> JSDP.required "404s" (JSD.list decode404)
  |> JSDP.required "unlocked_dbs" (JSD.list decodeTLID)

decodeInitialLoadRPC : JSD.Decoder InitialLoadResult
decodeInitialLoadRPC =
  JSDP.decode (,,,,)
  |> JSDP.required "toplevels" (JSD.list decodeToplevel)
  |> JSDP.required "new_analyses" (JSD.list decodeTLAResult)
  |> JSDP.required "global_varnames" (JSD.list JSD.string)
  |> JSDP.required "user_functions" (JSD.list decodeUserFunction)
  |> JSDP.required "unlocked_dbs" (JSD.list decodeTLID)



decodeExecuteFunctionRPC : JSD.Decoder ExecuteFunctionRPCResult
decodeExecuteFunctionRPC =
  JSDP.decode (,)
  |> JSDP.required "targets" (JSD.list decodeExecuteFunctionTarget)
  |> JSDP.required "new_analyses" (JSD.list decodeTLAResult)


