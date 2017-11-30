module RPC exposing (..)

-- builtin
import Http
import Json.Encode as JSE
import Json.Decode as JSD
import Json.Decode.Pipeline as JSDP
import Dict exposing (Dict)
import Dict.Extra as DE

-- lib

-- dark
import Types exposing (..)
import Runtime as RT
import Util
import JSON exposing (..)

rpc : Model -> Focus -> List RPC -> Cmd Msg
rpc m focus calls =
  rpc_ m "/admin/api/rpc" (RPCCallBack focus) calls

rpc_ : Model -> String ->
 (List RPC -> Result Http.Error RPCResult -> Msg) ->
 List RPC -> Cmd Msg
rpc_ m url callback calls =
  let payload = encodeRPCs m calls
      json = Http.jsonBody payload
      request = Http.post url json decodeRPC
  in Http.send (callback calls) request

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

saveTest : Cmd Msg
saveTest =
  let url = "/admin/api/save_test"
      request = postString url
  in Http.send SaveTestCallBack request


encodeRPCs : Model -> List RPC -> JSE.Value
encodeRPCs m calls =
  calls
  |> List.filter ((/=) NoOp)
  |> (\cs -> if cs == [Undo] || cs == [Redo] || cs == []
             then cs
             else Savepoint :: cs)
  |> List.map (encodeRPC m)
  |> JSE.list

encodeRPC : Model -> RPC -> JSE.Value
encodeRPC m call =
  let encodePos {x,y} =
        JSE.object [ ("x", JSE.int x)
                   , ("y", JSE.int y)]
      ev = encodeVariant
  in
    case call of
      SetHandler id pos h ->
        let hs = JSE.object
                   [ ("name", encodeHoleOr h.spec.name JSE.string)
                   , ("module", encodeHoleOr h.spec.module_ JSE.string)
                   , ("modifier", encodeHoleOr h.spec.modifier JSE.string)]
            handler = JSE.object [ ("tlid", encodeTLID id)
                                 , ("spec", hs)
                                 , ("ast", encodeAST h.ast) ] in
        ev "SetHandler" [encodeTLID id, encodePos pos, handler]

      CreateDB id pos name ->
        ev "CreateDB" [encodeTLID id, encodePos pos, JSE.string name]

      AddDBRow tlid rownameid rowtypeid ->
        ev "AddDBRow" [encodeTLID tlid, encodeID rownameid, encodeID rowtypeid]

      SetDBRowName tlid id name ->
        ev "SetDBRowName" [encodeTLID tlid, encodeID id, JSE.string name]

      SetDBRowType tlid id tipe ->
        ev "SetDBRowType" [encodeTLID tlid, encodeID id, JSE.string tipe]

      NoOp -> ev "NoOp" []
      DeleteAll -> ev "DeleteAll" []
      Savepoint -> ev "Savepoint" []
      Undo -> ev "Undo" []
      Redo -> ev "Redo" []
      DeleteTL id -> ev "DeleteTL" [encodeTLID id]
      MoveTL id pos -> ev "MoveTL" [encodeTLID id, encodePos pos]

encodeAST : Expr -> JSE.Value
encodeAST expr =
  let e = encodeAST
      eid = encodeID
      ev = encodeVariant in
  case expr of
    FnCall id n exprs ->
      ev "FnCall" [ eid id
                  , JSE.string n
                  , JSE.list (List.map e exprs)]

    Let id binds body ->
      ev "Let" [ eid id
               , (List.map (\(v, bexpr) ->
                                 JSE.list [ encodeHoleOr v JSE.string
                                          , e bexpr])
                    binds)
                  |> JSE.list
               , e body]

    Lambda id vars body ->
      ev "Lambda" [ eid id
                  , List.map JSE.string vars |> JSE.list
                  , e body]

    If id cond then_ else_ -> ev "If" [eid id, e cond, e then_, e else_]
    Variable id v -> ev "Variable" [ eid id , JSE.string v]
    Value id v -> ev "Value" [ eid id , JSE.string v]
    Hole id -> ev "Hole" [eid id]
    Thread id exprs -> ev "Thread" [eid id, JSE.list (List.map e exprs)]



  -- about the lazy decodeExpr
  -- TODO: check if elm 0.19 is saner than this
  -- elm 0.18 gives "Cannot read property `tag` of undefined` which
  -- leads to https://github.com/elm-lang/elm-compiler/issues/1562
  -- and https://github.com/elm-lang/elm-compiler/issues/1591
  -- which gets potentially fixed by
  -- https://github.com/elm-lang/elm-compiler/commit/e2a51574d3c4f1142139611cb359d0e68bb9541a


decodeExpr : JSD.Decoder Expr
decodeExpr =
  let de = (JSD.lazy (\_ -> decodeExpr))
      did = decodeID
      dv4 = decodeVariant4
      dv3 = decodeVariant3
      dv2 = decodeVariant2
      dv1 = decodeVariant1
      vb = decodePair (decodeHoleOr JSD.string) de in
  decodeVariants
    [ ("Let", dv3 Let did (JSD.list vb) de)
    , ("Hole", dv1 Hole did)
    , ("Value", dv2 Value did JSD.string)
    , ("If", dv4 If did de de de)
    , ("FnCall", dv3 FnCall did JSD.string (JSD.list de))
    , ("Lambda", dv3 Lambda did (JSD.list JSD.string) de)
    , ("Variable", dv2 Variable did JSD.string)
    , ("Thread", dv2 Thread did (JSD.list de))
    ]

decodeAST : JSD.Decoder AST
decodeAST = decodeExpr

decodeLiveValue : JSD.Decoder LiveValue
decodeLiveValue =
  let toLiveValue value tipe json exc =
      { value = value
      , tipe = RT.str2tipe tipe
      , json = json
      , exc = exc}
      toExc : String -> String -> String -> String -> String ->
              String -> String -> String -> Dict String String ->
              List String -> Maybe Exception
      toExc short long tipe actual actualType result resultType expected info workarounds =
        Just { short=short
             , long=long
             , tipe=tipe
             , actual=actual
             , actualType=actualType
             , result=result
             , resultType=resultType
             , expected=expected
             , info=info
             , workarounds=workarounds }
  in
  JSDP.decode toLiveValue
    |> JSDP.required "value" JSD.string
    |> JSDP.required "type" JSD.string
    |> JSDP.required "json" JSD.string
    |> JSDP.optional "exc"
         (JSDP.decode toExc
            |> JSDP.required "short" JSD.string
            |> JSDP.required "long" JSD.string
            |> JSDP.required "tipe" JSD.string
            |> JSDP.required "actual" JSD.string
            |> JSDP.required "actual_tipe" JSD.string
            |> JSDP.required "result" JSD.string
            |> JSDP.required "result_tipe" JSD.string
            |> JSDP.required "expected" JSD.string
            |> JSDP.required "info" (JSD.dict JSD.string)
            |> JSDP.required "workarounds" (JSD.list JSD.string))
            Nothing

decodeTLAResult : JSD.Decoder TLAResult
decodeTLAResult =
  let toTLAResult tlid astValue liveValues availableVarnames =
        { id = TLID tlid
        , astValue = astValue
        , liveValues = (DE.mapKeys (Util.toIntWithDefault 0) liveValues)
        , availableVarnames = (DE.mapKeys (Util.toIntWithDefault 0) availableVarnames)
        } in
  JSDP.decode toTLAResult
  |> JSDP.required "id" JSD.int
  |> JSDP.required "ast_value" decodeLiveValue
  |> JSDP.required "live_values" (JSD.dict decodeLiveValue)
  |> JSDP.required "available_varnames" (JSD.dict (JSD.list JSD.string))


decodeHandlerSpec : JSD.Decoder HandlerSpec
decodeHandlerSpec =
  let toHS module_ name modifier =
        { name = name
        , module_ = module_
        , modifier = modifier}
  in
  JSDP.decode toHS
  |> JSDP.required "module" (decodeHoleOr JSD.string)
  |> JSDP.required "name" (decodeHoleOr JSD.string)
  |> JSDP.required "modifier" (decodeHoleOr JSD.string)

decodeHandler : JSD.Decoder Handler
decodeHandler =
  let toHandler ast spec = {ast = ast, spec = spec } in
  JSDP.decode toHandler
  |> JSDP.required "ast" decodeAST
  |> JSDP.required "spec" decodeHandlerSpec

decodeDB : JSD.Decoder DB
decodeDB =
  let toDB name rows = {name = name, rows = rows} in
  JSDP.decode toDB
  |> JSDP.required "name" JSD.string
  |> JSDP.required "rows" (JSD.list
                            (decodePair
                              (decodeHoleOr JSD.string)
                              (decodeHoleOr JSD.string)))


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

decodeRPC : JSD.Decoder RPCResult
decodeRPC =
  JSDP.decode (,)
  |> JSDP.required "toplevels" (JSD.list decodeToplevel)
  |> JSDP.required "analyses" (JSD.list decodeTLAResult)


