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
  let encodePos pos = case pos of
                        {x,y} -> ("pos", JSE.object [ ("x", JSE.int x)
                                                    , ("y", JSE.int y)])
      encodeID (TLID id) = ("id", JSE.int id)
      (cmd, args) =
    case call of
      SetAST id pos expr ->
        ("set_ast", JSE.object [ encodeID id
                               , encodePos pos
                               , ("ast", encodeAST expr)])

      NoOp ->
        ("noop", JSE.object [])

      DeleteAll ->
        ("delete_all", JSE.object [])

      Savepoint ->
        ("savepoint", JSE.object [])

      Undo ->
        ("undo", JSE.object [])

      Redo ->
        ("redo", JSE.object [])

      DeleteAST id ->
        ("delete_ast", JSE.object [encodeID id])

      MoveAST id pos ->
        ("move_ast", JSE.object [ encodeID id
                                , encodePos pos])

  in JSE.object [ (cmd, args) ]

encodeAST : Expr -> JSE.Value
encodeAST expr =
  let e = encodeAST
      eid (ID id) = ("id", JSE.int id) in
  case expr of
    If id cond then_ else_ ->
      JSE.object [("if"
                  , JSE.object [ eid id
                               , ("cond", e cond)
                               , ("then", e then_)
                               , ("else", e else_)])]

    FnCall id n exprs ->
      JSE.object [( "fncall"
                  , JSE.object [ eid id
                               , ("name", JSE.string n)
                               , ("arguments", JSE.list (List.map e exprs)) ])]

    Variable id v ->
      JSE.object [("variable"
                  , JSE.object [ eid id
                               , ("name", JSE.string v)])]

    Let id binds body ->
      JSE.object [("let"
                  , JSE.object [ eid id
                               , ("bindings"
                                 , List.map (\(v, bexpr) ->
                                     JSE.object [ (case v of
                                                     Named s -> ("name", JSE.string s)
                                                     BindHole (ID id) -> ("hole", JSE.object [("id", JSE.int id)]))
                                                , ("expr", e bexpr)]) binds |> JSE.list)
                               , ("body", e body)])]

    Lambda id vars body ->
      JSE.object [("lambda"
                  , JSE.object [ eid id
                               , ("varnames", List.map JSE.string vars |> JSE.list)
                               , ("body", e body)])]

    Value id v ->
      JSE.object [("value"
                  , JSE.object [ eid id
                               , ("valuestr", JSE.string v)])]

    Hole id ->
      JSE.object [("hole", JSE.object [eid id])]

decodeID : JSD.Decoder ID
decodeID = JSD.map ID JSD.int

decodeIf : JSD.Decoder Expr
decodeIf =
  let de = (JSD.lazy (\_ -> decodeExpr)) in
  JSDP.decode If
  |> JSDP.requiredAt ["if", "id"] (JSD.lazy (\_ -> decodeID))
  |> JSDP.requiredAt ["if", "cond"] de
  |> JSDP.requiredAt ["if", "then"] de
  |> JSDP.requiredAt ["if", "else"] de

decodeFnCall : JSD.Decoder Expr
decodeFnCall =
  let de = (JSD.lazy (\_ -> decodeExpr)) in
  JSDP.decode FnCall
  |> JSDP.requiredAt ["fncall", "id"] (JSD.lazy (\_ -> decodeID))
  |> JSDP.requiredAt ["fncall", "name"] JSD.string
  |> JSDP.requiredAt ["fncall", "arguments"] (JSD.list de)

decodeVariable : JSD.Decoder Expr
decodeVariable =
  JSDP.decode Variable
  |> JSDP.requiredAt ["variable", "id"] (JSD.lazy (\_ -> decodeID))
  |> JSDP.requiredAt ["variable", "name"] JSD.string

decodeLet : JSD.Decoder Expr
decodeLet =
  let de = (JSD.lazy (\_ -> decodeExpr))
      db id e = (BindHole (ID id), e)
      dn n e = (Named n, e)
      dbindhole = JSDP.decode db
                  |> JSDP.requiredAt ["hole", "id"] JSD.int
                  |> JSDP.required "expr" de
      dnamed = JSDP.decode dn
               |> JSDP.required "name" JSD.string
               |> JSDP.required "expr" de
      vb = JSD.oneOf [dbindhole, dnamed]
  in
  JSDP.decode Let
  |> JSDP.requiredAt ["let", "id"] (JSD.lazy (\_ -> decodeID))
  |> JSDP.requiredAt ["let", "bindings"] (JSD.list vb)
  |> JSDP.requiredAt ["let", "body"] de

decodeLambda : JSD.Decoder Expr
decodeLambda =
  let de = (JSD.lazy (\_ -> decodeExpr)) in
  JSDP.decode Lambda
  |> JSDP.requiredAt ["lambda", "id"] (JSD.lazy (\_ -> decodeID))
  |> JSDP.requiredAt ["lambda", "varnames"] (JSD.list JSD.string)
  |> JSDP.requiredAt ["lambda", "body"] de

decodeValue : JSD.Decoder Expr
decodeValue =
  JSDP.decode Value
  |> JSDP.requiredAt ["value", "id"] (JSD.lazy (\_ -> decodeID))
  |> JSDP.requiredAt ["value", "valuestr"] JSD.string

decodeHole : JSD.Decoder Expr
decodeHole =
  JSDP.decode Hole
  |> JSDP.requiredAt ["hole", "id"] (JSD.lazy (\_ -> decodeID))

decodeExpr : JSD.Decoder Expr
decodeExpr =
  JSD.oneOf [ JSD.lazy (\_ -> decodeIf)
            -- TODO: check if elm 0.19 is saner than this
            -- elm 0.18 gives "Cannot read property `tag` of undefined` which
            -- leads to https://github.com/elm-lang/elm-compiler/issues/1562
            -- and https://github.com/elm-lang/elm-compiler/issues/1591
            -- which gets potentially fixed by
            -- https://github.com/elm-lang/elm-compiler/commit/e2a51574d3c4f1142139611cb359d0e68bb9541a
            , JSD.lazy (\_ -> decodeFnCall)
            , JSD.lazy (\_ -> decodeVariable)
            , JSD.lazy (\_ -> decodeLet)
            , JSD.lazy (\_ -> decodeLambda)
            , JSD.lazy (\_ -> decodeValue)
            , JSD.lazy (\_ -> decodeHole)
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


decodeToplevel : JSD.Decoder Toplevel
decodeToplevel =
  let toToplevel id x y ast =  { id = TLID id, pos = { x=x, y=y }, ast = ast }
  in
  JSDP.decode toToplevel
  |> JSDP.required "id" JSD.int
  |> JSDP.requiredAt ["pos", "x"] JSD.int
  |> JSDP.requiredAt ["pos", "y"] JSD.int
  |> JSDP.required "ast" decodeAST

decodeRPC : JSD.Decoder RPCResult
decodeRPC =
  JSDP.decode (,)
  |> JSDP.required "toplevels" (JSD.list decodeToplevel)
  |> JSDP.required "analyses" (JSD.list decodeTLAResult)


