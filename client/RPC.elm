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
    _ -> JSE.object [("type", JSE.string "todo")]

decodeExpr : JSD.Decoder Expr
decodeExpr =
  -- let toExpr =
  -- in
  JSDP.decode toExpr
  |> JSDP.optional


decodeTopLevel : JSD.Decoder TopLevel
decodeTopLevel =
  let toTopLevel id x y expr =  { id = ID id, pos = { x=x, y=y }, expr = expr }
  in
  JSDP.decode toTopLevel
  |> JSDP.required "id" JSD.int
  |> JSDP.requiredAt ["pos", "x"] JSD.int
  |> JSDP.requiredAt ["pos", "y"] JSD.int
  |> JSDP.required "ast" decodeExpr

decodeRPC : JSD.Decoder Int
decodeRPC = JSD.int

-- decodeRPCNode : JSD.Decoder RPCNode
-- -  in JSDP.decode toRPCNode
-- -    |> JSDP.required "name" JSD.string
-- -    |> JSDP.required "id" JSD.int
-- -    |> JSDP.optional "fields" (JSD.list
-- -                                 (JSD.map2 (,)
-- -                                    (JSD.index 0 JSD.string)
-- -                                    (JSD.index 1 JSD.string))) []
-- -    |> JSDP.required "arguments"
-- -         (JSD.list
-- -           (JSD.map2 (,)
-- -            (JSD.index 0
-- -              (JSDP.decode toParameter
-- -                |> JSDP.required "name" JSD.string
-- -                |> JSDP.required "tipe" JSD.string
-- -                |> JSDP.required "block_args" (JSD.list JSD.string)
-- -                |> JSDP.required "optional" JSD.bool
-- -                |> JSDP.required "description" JSD.string))
-- -            (JSD.index 1
-- -              (JSD.map toArgument (JSD.list JSD.value)))))
-- -    |> JSDP.requiredAt ["live", "value"] JSD.string
-- -    |> JSDP.requiredAt ["live", "type"] JSD.string
-- -    |> JSDP.requiredAt ["live", "json"] JSD.string
-- -    |> JSDP.optionalAt ["live", "exc"]
-- -         (JSDP.decode toExc
-- -            |> JSDP.required "short" JSD.string
-- -            |> JSDP.required "long" JSD.string
-- -            |> JSDP.required "tipe" JSD.string
-- -            |> JSDP.required "actual" JSD.string
-- -            |> JSDP.required "actual_tipe" JSD.string
-- -            |> JSDP.required "result" JSD.string
-- -            |> JSDP.required "result_tipe" JSD.string
-- -            |> JSDP.required "expected" JSD.string
-- -            |> JSDP.required "info" (JSD.dict JSD.string)
-- -            |> JSDP.required "workarounds" (JSD.list JSD.string))
-- -            Nothing
-- -    |> JSDP.optional "block_id" JSD.int Defaults.unsetInt
-- -    |> JSDP.required "arg_ids" (JSD.list JSD.int)
-- -    |> JSDP.required "type" JSD.string
-- -    |> JSDP.required "pos" (JSD.index 0 JSD.string)
-- -    |> JSDP.required "pos" (JSD.maybe (JSD.index 1 (JSD.field "x" JSD.int)))
-- -    |> JSDP.required "pos" (JSD.maybe (JSD.index 1 (JSD.field "y" JSD.int)))
-- -    |> JSDP.required "cursor" JSD.int



