module RPC exposing (..)

-- builtin
import Http
import Json.Encode as JSE
import Json.Decode as JSD

-- lib

-- dark
import Types exposing (..)

rpc : Model -> Focus -> List RPC -> Cmd Msg
rpc m focus calls =
  rpc_ m "/admin/api/rpc" (RPCCallBack focus) calls

rpc_ : Model -> String -> (List RPC -> Result Http.Error Int -> Msg) -> List RPC -> Cmd Msg
rpc_ m url callback calls =
  let payload = encodeRPCs m calls
      json = Http.jsonBody payload
      request = Http.post url json JSD.int
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
      encodeID (ID id) = ("id", JSE.int id)
      (cmd, args) =
    case call of
      SetAST id pos expr ->
        ("set_ast", JSE.object [ encodeID id
                               , encodePos pos
                               , ("expr", encodeAST expr)])

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
      JSE.object [("type", JSE.string "if"), ("cond", e cond), ("then", e then_), ("else", e else_)]
    _ -> JSE.object [("type", JSE.string "todo")]


decodeCanvas : JSD.Decoder Int
decodeCanvas = JSD.int

