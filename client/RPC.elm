module RPC exposing (rpc)

import Dict exposing (Dict)
import Http
import Json.Encode as JSE
import Json.Decode as JSD
import Json.Decode.Pipeline as JSDP

import Types exposing (..)
import Graph as G

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

      UpdateNodePosition id pos ->
        ("update_node_position",
           JSE.object [ jse_id id
                      , jse_pos pos])

      AddEdge (ID src) (ID target, param) ->
        ("add_edge",
           JSE.object [ ("source", JSE.int src)
                      , ("target", JSE.int target)
                      , ("param", JSE.string param)
                      ])

      DeleteNode id ->
        ("delete_node", JSE.object [ jse_id id ])

      ClearEdges id ->
        ("clear_edges", JSE.object [ jse_id id ])

      RemoveLastField id ->
        ("remove_last_field", JSE.object [ jse_id id ])

  in JSE.object [ (cmd, args) ]

decodeNode : JSD.Decoder Node
decodeNode =
  let toNode : Name -> Int -> List(FieldName,TypeName) -> List ParamName -> String -> Int -> Int -> Node
      toNode name id fields parameters tipe x y =
          { name = name
          , id = ID id
          , fields = fields
          , parameters = parameters
          , tipe = case tipe of
                     "datastore" -> Datastore
                     "function" -> FunctionCall
                     "definition" -> FunctionDef
                     "value" -> Value
                     "page" -> Page
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
    |> JSDP.optional "parameters" (JSD.list JSD.string) []
    |> JSDP.required "type" JSD.string
    |> JSDP.required "x" JSD.int
    |> JSDP.required "y" JSD.int

decodeEdge : JSD.Decoder Edge
decodeEdge =
  let toEdge : Int -> Int -> ParamName -> Edge
      toEdge source target param =
        { source = ID source
        , target = ID target
        , param = param
        }
  in JSDP.decode toEdge
    |> JSDP.required "source" JSD.int
    |> JSDP.required "target" JSD.int
    |> JSDP.required "param" JSD.string


decodeGraph : JSD.Decoder (NodeDict, List Edge, Maybe ID)
decodeGraph =
  let toGraph : List Node -> List Edge -> Maybe Int -> (NodeDict, List Edge, Maybe ID)
      toGraph nodes edges idint =
        let nodedict = List.foldl
                    (\v d -> Dict.insert (v.id |> deID) v d)
                    Dict.empty
                    nodes
        in (nodedict, edges, Maybe.map ID idint)
  in JSDP.decode toGraph
    |> JSDP.required "nodes" (JSD.list decodeNode)
    |> JSDP.required "edges" (JSD.list decodeEdge)
    |> JSDP.required "just_added" (JSD.nullable JSD.int)
