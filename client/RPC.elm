module RPC exposing (rpc)

import Dict exposing (Dict)
import Http
import Json.Encode as JSE
import Json.Decode as JSD
import Json.Decode.Pipeline as JSDP

import Types exposing (..)

invalidID = -45

rpc : Model -> List RPC -> Cmd Msg
rpc m calls =
  let payload = encodeRPCs m calls
      json = Http.jsonBody payload
      request = Http.post "/admin/api/rpc" json decodeGraph
  in Http.send (RPCCallBack calls) request

encodeRPCs : Model -> List RPC -> JSE.Value
encodeRPCs m calls =
  JSE.list (List.map (encodeRPC m) calls)

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
                                                JSE.object [ ("id", JSE.int id)
                                                           , ("name", JSE.string name)
                                                           , ("tipe", JSE.string tipe)])

      AddFunctionCall name {x,y} edges -> ("add_function_call",
                                             JSE.object [ ("name", JSE.string name)
                                                        , ("x", JSE.int x)
                                                        , ("y", JSE.int y)
                                                        , ("edges", JSE.list
                                                             (List.map (\(ID i) -> JSE.int i) edges))])
      AddAnon {x,y} -> ("add_anon",
                              JSE.object [ ("x", JSE.int x)
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
                          JSE.object [ ("id", JSE.int id)
                                     , ("x" , JSE.int node.pos.x)
                                     , ("y" , JSE.int node.pos.y)])
      AddEdge (ID src) (ID target, param) -> ("add_edge",
                                                JSE.object [ ("src", JSE.int src)
                                                           , ("target", JSE.int target)
                                                           , ("param", JSE.string param)
                                                           ])
      DeleteNode (ID id) -> ("delete_node",
                               JSE.object [ ("id", JSE.int id) ])
      ClearEdges (ID id) -> ("clear_edges",
                               JSE.object [ ("id", JSE.int id) ])
      RemoveLastField (ID id) -> ("remove_last_field",
                                    JSE.object [ ("id", JSE.int id) ])

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
        , targetParam = param
        }
  in JSDP.decode toEdge
    |> JSDP.required "source" JSD.int
    |> JSDP.required "target" JSD.int
    |> JSDP.required "param" JSD.string

decodeGraph : JSD.Decoder (NodeDict, List Edge)
decodeGraph =
  let toGraph : List Node -> List Edge -> (NodeDict, List Edge)
      toGraph nodes edges =
        let nodedict = List.foldl
                    (\v d -> Dict.insert (v.id |> deID) v d)
                    Dict.empty
                    nodes
        in (nodedict, edges)
  in JSDP.decode toGraph
    |> JSDP.required "nodes" (JSD.list decodeNode)
    |> JSDP.required "edges" (JSD.list decodeEdge)
