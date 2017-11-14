module RPC exposing (..)

-- builtin
import Http
import Json.Encode as JSE
import Json.Decode as JSD
import Json.Decode.Pipeline as JSDP

-- lib

-- dark
import Types exposing (..)

rpc : Model -> Focus -> List RPC -> Cmd Msg
rpc m focus calls =
  rpc_ m "/admin/api/rpc" (RPCCallBack focus) calls

rpc_ : Model -> String -> (List RPC -> Result Http.Error (List Toplevel) -> Msg) -> List RPC -> Cmd Msg
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

      DeleteAST (TLID id) ->
        ("delete_ast", JSE.object [("id", JSE.int id)])

  in JSE.object [ (cmd, args) ]

encodeAST : Expr -> JSE.Value
encodeAST expr =
  let e = encodeAST
      eid (HID id) = ("id", JSE.int id) in
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
                               , ("varname", JSE.string v)])]

    Let id binds body ->
      JSE.object [("let"
                  , JSE.object [ eid id
                               , ("bindings"
                                 , List.map (\(v, bexpr) ->
                                     JSE.object [ ("name", JSE.string v)
                                                , ("expr", e bexpr)]) binds |> JSE.list)
                               , ("body", e body)])]

    Lambda id vars body ->
      JSE.object [("lambda"
                  , JSE.object [ eid id
                               , ("vars", List.map JSE.string vars |> JSE.list)
                               , ("body", e body)])]

    Value id v ->
      JSE.object [("value"
                  , JSE.object [ eid id
                               , ("valuestr", JSE.string v)])]

    Hole id ->
      JSE.object [("hole", JSE.object [eid id])]

decodeHID : JSD.Decoder HID
decodeHID = JSD.map HID JSD.int

decodeIf : JSD.Decoder Expr
decodeIf =
  let de = (JSD.lazy (\_ -> decodeExpr)) in
  JSDP.decode If
  |> JSDP.requiredAt ["if", "id"] (JSD.lazy (\_ -> decodeHID))
  |> JSDP.requiredAt ["if", "cond"] de
  |> JSDP.requiredAt ["if", "then"] de
  |> JSDP.requiredAt ["if", "else"] de

decodeFnCall : JSD.Decoder Expr
decodeFnCall =
  let de = (JSD.lazy (\_ -> decodeExpr)) in
  JSDP.decode FnCall
  |> JSDP.requiredAt ["fncall", "id"] (JSD.lazy (\_ -> decodeHID))
  |> JSDP.requiredAt ["fncall", "name"] JSD.string
  |> JSDP.requiredAt ["fncall", "arguments"] (JSD.list de)

decodeVariable : JSD.Decoder Expr
decodeVariable =
  JSDP.decode Variable
  |> JSDP.requiredAt ["variable", "id"] (JSD.lazy (\_ -> decodeHID))
  |> JSDP.requiredAt ["variable", "name"] JSD.string

decodeLet : JSD.Decoder Expr
decodeLet =
  let de = (JSD.lazy (\_ -> decodeExpr))
      be = JSDP.decode (,)
           |> JSDP.required "name" JSD.string
           |> JSDP.required "expr" de
  in
  JSDP.decode Let
  |> JSDP.requiredAt ["let", "id"] (JSD.lazy (\_ -> decodeHID))
  |> JSDP.requiredAt ["let", "bindings"] (JSD.list be)
  |> JSDP.requiredAt ["let", "body"] de

decodeLambda : JSD.Decoder Expr
decodeLambda =
  let de = (JSD.lazy (\_ -> decodeExpr)) in
  JSDP.decode Lambda
  |> JSDP.requiredAt ["lambda", "id"] (JSD.lazy (\_ -> decodeHID))
  |> JSDP.requiredAt ["lambda", "varnames"] (JSD.list JSD.string)
  |> JSDP.requiredAt ["lambda", "body"] de

decodeValue : JSD.Decoder Expr
decodeValue =
  JSDP.decode Value
  |> JSDP.requiredAt ["value", "id"] (JSD.lazy (\_ -> decodeHID))
  |> JSDP.requiredAt ["value", "valuestr"] JSD.string

decodeHole : JSD.Decoder Expr
decodeHole =
  JSDP.decode Hole
  |> JSDP.requiredAt ["hole", "id"] (JSD.lazy (\_ -> decodeHID))

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

decodeToplevel : JSD.Decoder Toplevel
decodeToplevel =
  let toToplevel id x y ast =  { id = TLID id, pos = { x=x, y=y }, ast = ast }
  in
  JSDP.decode toToplevel
  |> JSDP.required "id" JSD.int
  |> JSDP.requiredAt ["pos", "x"] JSD.int
  |> JSDP.requiredAt ["pos", "y"] JSD.int
  |> JSDP.required "ast" decodeAST

decodeRPC : JSD.Decoder (List Toplevel)
decodeRPC =
  JSDP.decode identity
  |> JSDP.required "toplevels" (JSD.list decodeToplevel)


