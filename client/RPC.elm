module RPC exposing (rpc, phantomRpc)

-- builtin
import Dict exposing (Dict)
import Http
import Json.Encode as JSE
import Json.Decode as JSD

-- lib
import Json.Decode.Pipeline as JSDP

-- dark
import Types exposing (..)
import Defaults
import Runtime as RT


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


encodeRPCs : Model -> List RPC -> JSE.Value
encodeRPCs m calls =
  calls
  |> List.filter ((/=) NoOp)
  |> (\cs -> if cs == [Undo] || cs == [Redo]
             then cs
             else SavePoint :: cs)
  |> List.map (encodeRPC m)
  |> JSE.list


encodeRPC : Model -> RPC -> JSE.Value
encodeRPC m call =
  let jsePos pos = case pos of
                     Just {x,y} -> ("pos", JSE.object [ ("x", JSE.int x)
                                                      , ("y", JSE.int y)])
                     Nothing -> ("pos", JSE.null)
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

      AddAnon id pos args argnames ->
        ("add_anon", JSE.object [ jseId id
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

      DeleteNode id ->
        ("delete_node", JSE.object [ jseId id ])

      ClearArgs id ->
        ("clear_args", JSE.object [ jseId id ])

      NoOp ->
        ("noop", JSE.object [])

      DeleteAll ->
        ("delete_all", JSE.object [])

      SavePoint ->
        ("savepoint", JSE.object [])

      Undo ->
        ("undo", JSE.object [])

      Redo ->
        ("redo", JSE.object [])
  in JSE.object [ (cmd, args) ]

decodeNode : JSD.Decoder Node
decodeNode =
  let toParameter: Name -> String -> List String -> Bool -> String -> Parameter
      toParameter name tipe anon_args optional description =
        { name = name
        , tipe = tipe |> RT.str2tipe
        , anon_args = anon_args
        , optional = optional
        , description = description}
      toArg: List JSD.Value -> Argument
      toArg strs = case strs of
                     [name, val] -> case JSD.decodeValue JSD.string name of
                                      Result.Ok "AConst" ->
                                        if (toString val) == "\"<incomplete>\""
                                        then NoArg
                                        else Const (toString val)
                                      Result.Ok "AEdge" ->
                                        val
                                        |> JSD.decodeValue JSD.int
                                        |> Result.withDefault (-1)
                                        |> ID
                                        |> Edge
                                      Result.Ok op ->
                                        Debug.crash <| "Unexpected: " ++ op
                                      Result.Err e ->
                                        Debug.crash <| "Invalid: " ++ e
                     _ -> Debug.crash "impossible"


      toNode : Name -> Int -> List (FieldName, String) ->
               List Parameter -> List (List JSD.Value) -> String ->
               String -> String -> Maybe Exception -> Int -> List Int ->
               String -> Int -> Int -> Int -> Node
      toNode name id fields parameters arguments liveValue liveTipe liveJson liveExc anonID argIDs tipe x y cursor =
          { name = name
          , id = ID id
          , fields = List.map (\(f,tipe) -> (f, RT.str2tipe tipe)) fields
          , parameters = parameters
          , arguments = List.map toArg arguments
          , liveValue = { value = liveValue
                        , tipe = liveTipe |> RT.str2tipe
                        , json = liveJson
                        , exc = liveExc}
          , anonID = if anonID == Defaults.unsetPosition then Nothing else Just <| ID anonID
          , argIDs = List.map ID argIDs
          , tipe = case tipe of
                     "datastore" -> Datastore
                     "function" -> FunctionCall
                     "definition" -> FunctionDef
                     "value" -> Value
                     "page" -> Page
                     "arg" -> Arg
                     _ -> Debug.crash "shouldnt happen"
          , pos = {x=x, y=y}
          , cursor = cursor
          , visible = tipe /= "definition"
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


  in JSDP.decode toNode
    |> JSDP.required "name" JSD.string
    |> JSDP.required "id" JSD.int
    |> JSDP.optional "fields" (JSD.list
                                 (JSD.map2 (,)
                                    (JSD.index 0 JSD.string)
                                    (JSD.index 1 JSD.string))) []
    |> JSDP.required "parameters"
         (JSD.list
            (JSDP.decode toParameter
               |> JSDP.required "name" JSD.string
               |> JSDP.required "tipe" JSD.string
               |> JSDP.required "anon_args" (JSD.list JSD.string)
               |> JSDP.required "optional" JSD.bool
               |> JSDP.required "description" JSD.string))
    |> JSDP.required "arguments" (JSD.list (JSD.list JSD.value))
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
    |> JSDP.optional "anon_id" JSD.int Defaults.unsetPosition
    |> JSDP.required "arg_ids" (JSD.list JSD.int)
    |> JSDP.required "type" JSD.string
    |> JSDP.optionalAt ["pos", "x"] JSD.int Defaults.unsetPosition
    |> JSDP.optionalAt ["pos", "y"] JSD.int Defaults.unsetPosition
    |> JSDP.required "cursor" JSD.int


decodeGraph : JSD.Decoder (NodeDict)
decodeGraph =
  let toGraph : List Node -> (NodeDict)
      toGraph nodes =
        let nodedict = List.foldl
                    (\v d -> Dict.insert (v.id |> deID) v d)
                    Dict.empty
                    nodes
        in (nodedict)
  in JSDP.decode toGraph
    |> JSDP.required "nodes" (JSD.list decodeNode)
