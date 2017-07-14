module RPC exposing (rpc)

import Dict exposing (Dict)
import Http
import Json.Encode as JSE
import Json.Decode as JSD
import Json.Decode.Pipeline as JSDP

import Types exposing (..)


rpc : Model -> RPC -> Cmd Msg
rpc m call =
  let payload = encodeRPC m call
      json = Http.jsonBody payload
      request = Http.post "/admin/api/rpc" json decodeGraph
  in Http.send RPCCallBack request

encodeRPC : Model -> RPC -> JSE.Value
encodeRPC m call =
  let (cmd, args) =
    case call of
      LoadInitialGraph -> ("load_initial_graph", JSE.object [])
      AddDatastore name {x,y} -> ("add_datastore"
                                 , JSE.object [ ("name", JSE.string name)
                                              , ("x", JSE.int x)
                                              , ("y", JSE.int y)])
      AddDatastoreField (ID id) name tipe -> ("add_datastore_field",
                                                JSE.object [ ("id", JSE.string id)
                                                           , ("name", JSE.string name)
                                                           , ("tipe", JSE.string tipe)])
      AddFunctionCall name {x,y} -> ("add_function_call",
                                       JSE.object [ ("name", JSE.string name)
                                                  , ("x", JSE.int x)
                                                  , ("y", JSE.int y)])
      AddValue str {x,y} -> ("add_value",
                               JSE.object [ ("value", JSE.string str)
                                          , ("x", JSE.int x)
                                          , ("y", JSE.int y)])
                -- TODO: get passed the node
      UpdateNodePosition (ID id) ->
        case Dict.get id m.nodes of
          Nothing -> Debug.crash "should never happen"
          Just node -> ("update_node_position",
                          JSE.object [ ("id", JSE.string id)
                                     , ("x" , JSE.int node.pos.x)
                                     , ("y" , JSE.int node.pos.y)])
      AddEdge (ID src) (ID target, param) -> ("add_edge",
                                                JSE.object [ ("src", JSE.string src)
                                                           , ("target", JSE.string target)
                                                           , ("param", JSE.string param)
                                                           ])
      DeleteNode (ID id) -> ("delete_node",
                               JSE.object [ ("id", JSE.string id) ])
      ClearEdges (ID id) -> ("clear_edges",
                               JSE.object [ ("id", JSE.string id) ])
      RemoveLastField (ID id) -> ("remove_last_field",
                                    JSE.object [ ("id", JSE.string id) ])

  in JSE.object [ ("command", JSE.string cmd)
                , ("args", args) ]

decodeNode : JSD.Decoder Node
decodeNode =
  let toNode : Name -> String -> List(FieldName,TypeName) -> List ParamName -> String -> Int -> Int -> Node
      toNode name id fields parameters tipe x y =
          { name = name
          , id = ID id
          , fields = fields
          , parameters = parameters
          , tipe = case tipe of
                     "datastore" -> Datastore
                     "function" -> Function
                     "value" -> Value
                     "page" -> Page
                     _ -> Debug.crash "shouldnt happen"
          , pos={x=x, y=y}
          }
  in JSDP.decode toNode
    |> JSDP.required "name" JSD.string
    |> JSDP.required "id" JSD.string
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
  let toEdge : String -> String -> ParamName -> Edge
      toEdge source target param =
        { source = ID source
        , target = ID target
        , targetParam = param
        }
  in JSDP.decode toEdge
    |> JSDP.required "source" JSD.string
    |> JSDP.required "target" JSD.string
    |> JSDP.required "param" JSD.string

decodeGraph : JSD.Decoder (NodeDict, List Edge, Cursor)
decodeGraph =
  let toGraph : NodeDict -> List Edge -> String -> (NodeDict, List Edge, Cursor)
      toGraph nodes edges cursor = (nodes, edges, case cursor of
                                                    "" -> Nothing
                                                    str -> Just (ID str))
  in JSDP.decode toGraph
    |> JSDP.required "nodes" (JSD.dict decodeNode)
    |> JSDP.required "edges" (JSD.list decodeEdge)
    |> JSDP.optional "cursor" JSD.string ""
