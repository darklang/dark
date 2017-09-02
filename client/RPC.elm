module RPC exposing (rpc)

-- builtin
import Dict exposing (Dict)
import Http
import Json.Encode as JSE
import Json.Decode as JSD

-- lib
import Json.Decode.Pipeline as JSDP
import Tuple3

-- dark
import Types exposing (..)
import Util exposing (deMaybe)


rpc : Model -> List RPC -> Cmd Msg
rpc m calls =
  let payload = encodeRPCs m calls
      json = Http.jsonBody payload
      request = Http.post "/admin/api/rpc" json decodeGraph
  in Http.send (RPCCallBack calls) request

encodeRPCs : Model -> List RPC -> JSE.Value
encodeRPCs m calls =
  JSE.list (List.map (encodeRPC m) calls)

encodeImplicitEdges : List ImplicitEdge -> JSE.Value
encodeImplicitEdges edges =
  edges
    |> List.map
       (\e -> case e of
                ReceivingEdge (ID id) ->
                  JSE.object [("receiving_edge",
                                 JSE.object [("source", JSE.int id)])]

                ParamEdge (ID id) p ->
                  JSE.object [("param_edge",
                                 JSE.object [("target", JSE.int id),
                                             ("param", JSE.string p)])]

                Constant value p ->
                  JSE.object [("constant",
                                 JSE.object [("value", JSE.string value),
                                             ("param", JSE.string p)])])
    |> JSE.list

encodeRPC : Model -> RPC -> JSE.Value
encodeRPC m call =
  let jse_pos {x,y} = ("pos", JSE.object [("x", JSE.int x),
                                            ("y", JSE.int y)])
      jse_id (ID id) = ("id", JSE.int id)
      (cmd, args) =
    case call of
      LoadInitialGraph ->
        ("load_initial_graph", JSE.object [])

      AddDatastore name pos ->
        ("add_datastore"
        , JSE.object [ ("name", JSE.string name)
                     , jse_pos pos
                     ])

      AddDatastoreField id name tipe ->
        ("add_datastore_field",
           JSE.object [ jse_id id
                      , ("name", JSE.string name)
                      , ("tipe", JSE.string tipe)])

      AddFunctionCall name pos edges ->
        ("add_function_call",
           JSE.object [ ("name", JSE.string name)
                      , jse_pos pos
                      , ("edges", encodeImplicitEdges edges)])

      AddAnon pos ->
        ("add_anon", JSE.object [jse_pos pos])


      AddValue str pos edges ->
        ("add_value",
           JSE.object [ ("value", JSE.string str)
                      , jse_pos pos
                      , ("edges", encodeImplicitEdges edges)])

      SetConstant value id param ->
        ("set_constant",
           JSE.object [ ("value", JSE.string value)
                      , ("target", JSE.int (deID id))
                      , ("param", JSE.string param)])

      UpdateNodePosition id pos ->
        ("update_node_position",
           JSE.object [ jse_id id
                      , jse_pos pos])

      SetEdge (ID src) (ID target, param) ->
        ("set_edge",
           JSE.object [ ("source", JSE.int src)
                      , ("target", JSE.int target)
                      , ("param", JSE.string param)
                      ])

      DeleteNode id ->
        ("delete_node", JSE.object [ jse_id id ])

      ClearArgs id ->
        ("clear_args", JSE.object [ jse_id id ])

      RemoveLastField id ->
        ("remove_last_field", JSE.object [ jse_id id ])

  in JSE.object [ (cmd, args) ]

decodeNode : JSD.Decoder Node
decodeNode =
  let toParameter: Name -> TypeName -> Bool -> String -> Parameter
      toParameter name tipe optional description = { name = name
                                                   , tipe = tipe
                                                   , optional = optional
                                                   , description = description}
      toArg: List JSD.Value -> Argument
      toArg strs = case strs of
                     [name, val] -> case JSD.decodeValue JSD.string name of
                                      Result.Ok "AConst" ->
                                        if (toString val) == "\"<incomplete>\"" then NoArg else Const (toString val)
                                      Result.Ok "AEdge" ->
                                        val |> JSD.decodeValue JSD.int |> Result.withDefault (-1) |> ID |> Edge
                                      Result.Ok op ->
                                        Debug.crash <| "Unexpected: " ++ op
                                      Result.Err e ->
                                        Debug.crash <| "Invalid: " ++ e
                     _ -> Debug.crash "impossible"


      toNode : Name -> Int -> List(FieldName,TypeName) ->
               List Parameter -> List (List JSD.Value) ->
               Dict String String -> Int -> List Int ->
               String -> Int -> Int ->
               Node
      toNode name id fields parameters arguments liveDict returnID argIDs tipe x y =
        let liveValue = Dict.get "value" liveDict
            liveTipe = Dict.get "type" liveDict
            liveJson = Dict.get "json" liveDict in
          { name = name
          , id = ID id
          , fields = fields
          , parameters = parameters
          , arguments = List.map toArg arguments
          , liveValue = (liveValue, liveTipe, liveJson)
                        |> Tuple3.mapAll deMaybe
          , returnID = if returnID == -42 then Nothing else Just <| ID returnID
          , argIDs = List.map ID argIDs
          , tipe = case tipe of
                     "datastore" -> Datastore
                     "function" -> FunctionCall
                     "definition" -> FunctionDef
                     "value" -> Value
                     "page" -> Page
                     "arg" -> Arg
                     "return" -> Return
                     _ -> Debug.crash "shouldnt happen"
          , pos = {x=x, y=y}
          }


  in JSDP.decode toNode
    |> JSDP.required "name" JSD.string
    |> JSDP.required "id" JSD.int
    |> JSDP.optional "fields" (JSD.list
                                 (JSD.map2 (,)
                                    (JSD.index 0 JSD.string)
                                    (JSD.index 1 JSD.string))) []
    |> JSDP.required "parameters" (JSD.list
                                     (JSDP.decode toParameter
                                     |> JSDP.required "name" JSD.string
                                     |> JSDP.required "tipe" JSD.string
                                     |> JSDP.required "optional" JSD.bool
                                     |> JSDP.required "description" JSD.string))
    |> JSDP.required "arguments" (JSD.list (JSD.list JSD.value))
    |> JSDP.required "live" (JSD.dict JSD.string)
    |> JSDP.optional "return_id" JSD.int -42
    |> JSDP.required "arg_ids" (JSD.list JSD.int)
    |> JSDP.required "type" JSD.string
    |> JSDP.required "x" JSD.int
    |> JSDP.required "y" JSD.int


decodeGraph : JSD.Decoder (NodeDict, Maybe ID)
decodeGraph =
  let toGraph : List Node -> Maybe Int -> (NodeDict, Maybe ID)
      toGraph nodes idint =
        let nodedict = List.foldl
                    (\v d -> Dict.insert (v.id |> deID) v d)
                    Dict.empty
                    nodes
        in (nodedict, Maybe.map ID idint)
  in JSDP.decode toGraph
    |> JSDP.required "nodes" (JSD.list decodeNode)
    |> JSDP.required "last_node" (JSD.nullable JSD.int)
