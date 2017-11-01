module RPC exposing (..)

-- builtin
import Dict exposing (Dict)
import Http
import Json.Encode as JSE
import Json.Decode as JSD

-- lib
import Json.Decode.Pipeline as JSDP
import Dict.Extra as DE

-- dark
import Types exposing (..)
import Defaults
import Runtime as RT
import Util exposing (deMaybe)

type alias RPCNode = { argIDs : List Int
                     , arguments : List ( Parameter, Argument )
                     , blockID : Int
                     , cursor : Int
                     , fields : List ( FieldName, String )
                     , id : Int
                     , liveExc : Maybe Exception
                     , liveJson : String
                     , liveTipe : String
                     , liveValue : String
                     , name : Name
                     , posType : String
                     , posX : Maybe Int
                     , posY : Maybe Int
                     , tipe : String
                     }

type alias FullNode = { name : Name
                      , id : ID
                      , pos : MPos
                      , tipe : NodeType
                      , liveValue : LiveValue
                      , cursor: Cursor
                      -- for functions
                      , arguments : List (Parameter, Argument)
                      -- for blocks
                      , isBlockParent : Bool
                      , blockID : Maybe ID
                      , argIDs : List ID
                      , deleteWith : List ID
                      }


toFullNode : RPCNode -> FullNode
toFullNode rn = { name = rn.name
                , id = ID rn.id
                , arguments = rn.arguments
                , liveValue = { value = rn.liveValue
                              , tipe = rn.liveTipe |> RT.str2tipe
                              , json = rn.liveJson
                              , exc = rn.liveExc
                              }
                , argIDs = List.map ID rn.argIDs
                , blockID = if rn.blockID == Defaults.unsetInt then Nothing else Just <| ID rn.blockID
                , tipe = case rn.tipe of
                          "datastore" -> Datastore
                          "function" -> FunctionCall
                          "value" -> Value
                          "page" -> Page
                          "arg" -> Arg
                          "definition" -> Block
                          _ -> Debug.crash "shouldnt happen"
                , pos = case (rn.posType, rn.posX, rn.posY) of
                          ("Root", Just x, Just y) -> Root {x=x, y=y}
                          ("Dependent", Nothing, Nothing) -> Dependent Nothing
                          ("Free", Nothing, Nothing) -> Free Nothing
                          ("NoPos", Nothing, Nothing) -> NoPos Nothing
                          _ -> Debug.crash "Bad Pos in RPC"
                , cursor = rn.cursor
                , deleteWith = []
                , isBlockParent = False
                }

toNode : FullNode -> Node
toNode fn = { name = fn.name
            , id = fn.id
            , arguments = fn.arguments
            , liveValue = fn.liveValue
            , tipe = fn.tipe
            , pos = fn.pos
            , cursor = fn.cursor
            , face = ""
            -- todo kill
            , argIDs = []
            , visible = True
            , isBlockParent = fn.isBlockParent
            , deleteWith = fn.deleteWith
            }


fixupBlockNodes : Dict Int FullNode -> Dict Int FullNode
fixupBlockNodes nodes =
  let findChildOf id =
        nodes
        |> Dict.values
        |> List.filter
             (\n ->
               List.any
                 (\(p,a) ->
                   case a of
                     Edge eid False -> id == eid
                     _ -> False)
                 n.arguments)
        |> Util.hdExn
        |> .id

      stdParent = { name = "parent"
                  , tipe = TAny
                  , block_args = []
                  , optional = False
                  , description = ""
                  }
      convertArg _ n =
        let arguments =
              if n.tipe == Arg
              then [(stdParent, Edge (n.blockID |> deMaybe |> findChildOf) True)]
              else n.arguments
        in { n | arguments = arguments }
      convertFn _ n =
        let (blocks, others) =
              List.partition (\(p,a) -> p.tipe == TBlock) n.arguments
            blockIDs =
              List.map (\(p,a) ->
                case a of
                  Edge id _ -> let block = Dict.get (deID id) nodes |> Util.deMaybe
                               in block.id :: block.argIDs
                  _ -> Debug.crash "should all be blocks")
                blocks
        in { n | arguments = others
               , deleteWith = List.concat blockIDs
               , isBlockParent = blocks |> List.isEmpty |> not
        }
  in nodes
      |> Dict.map convertArg
      |> Dict.map convertFn


phantomRpc : Model -> EntryCursor -> List RPC -> Cmd Msg
phantomRpc m cursor calls =
  rpc_ m "/admin/api/phantom" (PhantomCallBack cursor) calls

rpc : Model -> Focus -> List RPC -> Cmd Msg
rpc m focus calls =
  rpc_ m "/admin/api/rpc" (RPCCallBack focus) calls

rpc_ : Model -> String -> (List RPC -> Result Http.Error NodeDict -> Msg) -> List RPC -> Cmd Msg
rpc_ m url callback calls =
  let payload = encodeRPCs m calls
      json = Http.jsonBody payload
      request = Http.post url json decodeGraph
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
  let jsePos pos = case pos of
                     Root {x,y} -> ("pos", JSE.list [ JSE.string "Root"
                                                    , JSE.object [ ("x", JSE.int x)
                                                                 , ("y", JSE.int y)]])
                     Dependent _ -> ("pos", JSE.list [ JSE.string "Dependent" ])
                     Free _ -> ("pos", JSE.list [ JSE.string "Free" ])
                     NoPos _ -> ("pos", JSE.list [ JSE.string "NoPos" ])
      jseId (ID id) = ("id", JSE.int id)
      (cmd, args) =
    case call of
      AddDatastore id name pos ->
        ("add_datastore"
        , JSE.object [ jseId id
                     , jsePos pos
                     , ("name", JSE.string name)
                     ])

      AddDatastoreField id name tipe ->
        ("add_datastore_field",
           JSE.object [ jseId id
                      , ("name", JSE.string name)
                      , ("tipe", JSE.string (tipe |> RT.tipe2str))])

      AddFunctionCall id name pos ->
        ("add_function_call",
           JSE.object [ jseId id, jsePos pos, ("name", JSE.string name)])

      AddBlock id pos args argnames ->
        ("add_block", JSE.object [ jseId id
                                , jsePos pos
                                , ("args", JSE.list (List.map (JSE.int << deID) args))
                                , ("argnames", JSE.list (List.map (JSE.string) argnames))])

      AddValue id str pos ->
        ("add_value",
           JSE.object [ jseId id, jsePos pos, ("value", JSE.string str)] )

      SetConstant value (ID target, param) ->
        ("set_constant",
           JSE.object [ ("value", JSE.string value)
                      , ("target", JSE.int target)
                      , ("param", JSE.string param)])

      SetEdge (ID src) (ID target, param) ->
        ("set_edge", JSE.object
           [ ("source", JSE.int src)
           , ("target", JSE.int target)
           , ("param", JSE.string param)])

      UpdateNodeCursor id cursor ->
        ("update_node_cursor",
          JSE.object [ jseId id
                     , ("cursor", JSE.int cursor)])

      UpdateNodePosition id pos ->
        ("update_node_position",
          JSE.object [ jseId id
                     , jsePos pos])

      DeleteNode id ->
        ("delete_node", JSE.object [ jseId id ])

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

decodeRPCNode : JSD.Decoder RPCNode
decodeRPCNode =
  let toParameter: Name -> String -> List String -> Bool -> String -> Parameter
      toParameter name tipe block_args optional description =
        { name = name
        , tipe = tipe |> RT.str2tipe
        , block_args = block_args
        , optional = optional
        , description = description}
      toArgument: List JSD.Value -> Argument
      toArgument strs =
        case strs of
          [name, val] ->
            case JSD.decodeValue JSD.string name of
              Result.Ok "AConst" ->
                case JSD.decodeValue JSD.string val of
                  Result.Ok stringifiedVal ->
                    if stringifiedVal == "<incomplete>"
                    then NoArg
                    else Const stringifiedVal
                  Result.Err exc ->
                    Const (toString exc)
              Result.Ok "AEdge" ->
                val
                |> JSD.decodeValue JSD.int
                |> Result.withDefault (-1)
                |> ID
                |> \id -> Edge id False
              Result.Ok op ->
                Debug.crash <| "Unexpected: " ++ op
              Result.Err e ->
                Debug.crash <| "Invalid: " ++ e
          _ ->
            Debug.crash "Bad argument in RPC"
      toRPCNode : Name -> Int -> List (FieldName, String) ->
               List (Parameter, Argument) -> String ->
               String -> String -> Maybe Exception -> Int -> List Int ->
               String -> String -> Maybe Int -> Maybe Int -> Int -> RPCNode
      toRPCNode name id fields arguments liveValue liveTipe liveJson liveExc blockID argIDs tipe posType x y cursor =
          { name      = name
          , id        = id
          , fields    = fields
          , arguments = arguments
          , liveValue = liveValue
          , liveTipe  = liveTipe
          , liveJson  = liveJson
          , liveExc   = liveExc
          , blockID   = blockID
          , argIDs    = argIDs
          , tipe      = tipe
          , posType   = posType
          , posX      = x
          , posY      = y
          , cursor    = cursor
          }
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


  in JSDP.decode toRPCNode
    |> JSDP.required "name" JSD.string
    |> JSDP.required "id" JSD.int
    |> JSDP.optional "fields" (JSD.list
                                 (JSD.map2 (,)
                                    (JSD.index 0 JSD.string)
                                    (JSD.index 1 JSD.string))) []
    |> JSDP.required "arguments"
         (JSD.list
           (JSD.map2 (,)
            (JSD.index 0
              (JSDP.decode toParameter
                |> JSDP.required "name" JSD.string
                |> JSDP.required "tipe" JSD.string
                |> JSDP.required "block_args" (JSD.list JSD.string)
                |> JSDP.required "optional" JSD.bool
                |> JSDP.required "description" JSD.string))
            (JSD.index 1
              (JSD.map toArgument (JSD.list JSD.value)))))
    |> JSDP.requiredAt ["live", "value"] JSD.string
    |> JSDP.requiredAt ["live", "type"] JSD.string
    |> JSDP.requiredAt ["live", "json"] JSD.string
    |> JSDP.optionalAt ["live", "exc"]
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
    |> JSDP.optional "block_id" JSD.int Defaults.unsetInt
    |> JSDP.required "arg_ids" (JSD.list JSD.int)
    |> JSDP.required "type" JSD.string
    |> JSDP.required "pos" (JSD.index 0 JSD.string)
    |> JSDP.required "pos" (JSD.maybe (JSD.index 1 (JSD.field "x" JSD.int)))
    |> JSDP.required "pos" (JSD.maybe (JSD.index 1 (JSD.field "y" JSD.int)))
    |> JSDP.required "cursor" JSD.int


decodeGraph : JSD.Decoder NodeDict
decodeGraph =
  let toGraph : List RPCNode -> NodeDict
      toGraph rpcNodes =
        let nodes = List.map toFullNode rpcNodes
            nodeDict = DE.fromListBy (.id >> deID) nodes
            nodeDict2 = fixupBlockNodes nodeDict
            nodeDict3 = Dict.filter (\_ n -> n.tipe /= Block) nodeDict2
            nodeDict4 = Dict.map (\_ n -> toNode n) nodeDict3
        in nodeDict4
  in JSDP.decode toGraph
    |> JSDP.required "nodes" (JSD.list decodeRPCNode)
