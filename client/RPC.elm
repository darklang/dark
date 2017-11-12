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
  in JSE.object [ (cmd, args) ]

encodeAST : Expr -> JSE.Value
encodeAST expr =
  let e = encodeAST in
  case expr of
    If cond then_ else_ ->
      JSE.object [("if", JSE.object [("cond", e cond), ("then", e then_), ("else", e else_)])]
    Value v -> JSE.object [("value", JSE.string v)]
    Hole (HID id) -> JSE.object [("hole", JSE.object [("id", JSE.int id)])]
    _ -> JSE.object [("type", JSE.string "todo")]

decodeIf : JSD.Decoder Expr
decodeIf =
  let de = (JSD.lazy (\_ -> decodeExpr)) in
  JSDP.decode If
  |> JSDP.requiredAt ["if", "cond"] de
  |> JSDP.requiredAt ["if", "then"] de
  |> JSDP.requiredAt ["if", "else"] de

decodeValue : JSD.Decoder Expr
decodeValue =
  JSDP.decode Value
  |> JSDP.required "value" JSD.string

decodeHole : JSD.Decoder Expr
decodeHole =
  let toHole i = Hole (HID i) in
  JSDP.decode toHole
  |> JSDP.requiredAt ["hole", "id"] JSD.int

decodeExpr : JSD.Decoder Expr
decodeExpr =
  JSD.oneOf [decodeIf, decodeValue, decodeHole]

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


