port module Main exposing (..)

-- builtins
import Html
import Html.Attributes as Attrs
import Html.Events as Events
import Result
import Char
import Dict exposing (Dict)
import Json.Encode as JSE
import Json.Decode as JSD
import Json.Decode.Pipeline as JSDP
import Array

-- lib
import Keyboard
import Mouse
import Dom
import Task
import Http
import Svg
import Svg.Attributes as SA
import Svg.Events as SEvents


-- mine
import Native.Window
import Native.Timestamp


-- TOP-LEVEL
main : Program Never Model Msg
main = Html.program
       { init = init
       , view = view
       , update = update
       , subscriptions = subscriptions}

consts = { nodeHeight = round 31
         , backspaceKeycode = 8
         , escapeKeycode = 27
         , inputID = "darkInput"
         }


-- MODEL
type alias Model = { nodes : NodeDict
                   , edges : List Edge
                   , cursor : Cursor
                   , inputValue : String
                   , focused : Bool
                   , state : State
                   , tempFieldName : FieldName
                   , errors : List String
                   , lastPos : Pos
                   , drag : Drag
                   }

type alias Node = { name : Name
                  , id : ID
                  , pos : Pos
                  , isDatastore : Bool
                  -- for DSes
                  , fields : List (FieldName, TypeName)
                  -- for functions
                  , parameters : List ParamName
                  }

type alias Edge = { source : ID
                  , target : ID
                  , targetParam : ParamName
                  }

type alias Name = String
type alias FieldName = String
type alias ParamName = String
type alias TypeName = String

type ID = ID String
type alias Pos = Mouse.Position
type alias Offset = {x: Int, y: Int, offsetCheck: Int}
type alias CanvasPos = {x: Int, y: Int, canvasPosCheck : Int}

type alias NodeDict = Dict Name Node
type alias Cursor = Maybe ID
type Drag = NoDrag
          | DragNode ID Offset -- offset between the click and the node pos
          | DragSlot ID ParamName Mouse.Position -- starting point of edge
init : ( Model, Cmd Msg )
init = let m = { nodes = Dict.empty
               , edges = []
               , cursor = Nothing
               , state = ADD_FUNCTION
               , errors = ["."]
               , inputValue = ""
               , focused = False
               , tempFieldName = ""
               , lastPos = {x=-1, y=-1}
               , drag = NoDrag
               }
       in (m, rpc m <| LoadInitialGraph)



-- RPC
type RPC
    = LoadInitialGraph
    | AddDatastore Name Pos
    | AddDatastoreField ID FieldName TypeName
    | AddFunctionCall Name Pos
    | AddValue String Pos
    | UpdateNodePosition ID -- no pos cause it's in the node
    | AddEdge ID (ID, ParamName)
    | DeleteNode ID
    | ClearEdges ID
    | RemoveLastField ID

rpc : Model -> RPC -> Cmd Msg
rpc model call =
    let payload = encodeRPC model call
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
                AddDatastoreField (ID id) name type_ -> ("add_datastore_field",
                                                             JSE.object [ ("id", JSE.string id)
                                                                        , ("name", JSE.string name)
                                                                        , ("type", JSE.string type_)])
                AddFunctionCall name {x,y} -> ("add_function_call",
                                                 JSE.object [ ("name", JSE.string name)
                                                            , ("x", JSE.int x)
                                                            , ("y", JSE.int y)])
                AddValue str {x,y} -> ("add_value",
                                           JSE.object [ ("value", JSE.string str)
                                                      , ("x", JSE.int x)
                                                      , ("y", JSE.int y)])
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
  let toNode : Name -> String -> List(FieldName,TypeName) -> List ParamName -> Bool -> Int -> Int -> Node
      toNode name id fields parameters isDatastore x y =
          { name = name
          , id = ID id
          , fields = fields
          , parameters = parameters
          , isDatastore = isDatastore
          , pos={x=x, y=y}
          }
  in JSDP.decode toNode
      |> JSDP.required "name" JSD.string
      |> JSDP.required "id" JSD.string
      |> JSDP.optional "fields" (JSD.keyValuePairs JSD.string) []
      |> JSDP.optional "parameters" (JSD.list JSD.string) []
      |> JSDP.optional "is_datastore" JSD.bool False
      |> JSDP.required "x" JSD.int
      |> JSDP.required "y" JSD.int

decodeEdge : JSD.Decoder Edge
decodeEdge =
    let toEdge : String -> String -> ParamName -> Edge
        toEdge source target paramname =
            { source = ID source
            , target = ID target
            , targetParam = paramname
            }
    in JSDP.decode toEdge
        |> JSDP.required "source" JSD.string
        |> JSDP.required "target" JSD.string
        |> JSDP.required "paramname" JSD.string

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



-- UPDATE
type Msg
    = ClearCursor Mouse.Position
    | NodeClick Node
    | RecordClick Mouse.Position
    | DragNodeStart Node Mouse.Position
    | DragNodeMove ID Offset Mouse.Position
    | DragNodeEnd ID Mouse.Position
    | DragSlotStart Node ParamName Mouse.Position
    | DragSlotMove ID ParamName Mouse.Position Mouse.Position
    | DragSlotEnd Node
    | DragSlotStop Mouse.Position
    | InputMsg String
    | SubmitMsg
    | KeyPress Keyboard.KeyCode
    | CheckEscape Keyboard.KeyCode
    | FocusResult (Result Dom.Error ())
    | RPCCallBack (Result Http.Error (NodeDict, List Edge, Cursor))

type State
    = ADD_FUNCTION
    | ADD_DS
    | ADD_DS_FIELD_NAME
    | ADD_DS_FIELD_TYPE
    | ADD_VALUE

-- simple updates for char codes
forCharCode m char =
    let _ = Debug.log "char" char in
    case Char.fromCode char of
        'F' -> ({ m | state = ADD_FUNCTION}, Cmd.none, NoFocus)
        'V' -> ({ m | state = ADD_VALUE}, Cmd.none, NoFocus)
        'D' -> ({ m | state = ADD_DS}, Cmd.none, NoFocus)
        _ -> let _ = Debug.log "nothing" (Char.fromCode char)
             in (m, Cmd.none, NoFocus)

update : Msg -> Model -> (Model, Cmd Msg)
update msg m =
    let (m2, cmd2, focus) = update_ msg m
        m3 = case focus of
                 Focus -> { m2 | inputValue = ""
                          , focused = True}
                 NoFocus -> m2
                 DropFocus -> { m2 | focused = False }
        cmd3 = case focus of
                   Focus -> Cmd.batch [cmd2, focusInput]
                   NoFocus -> cmd2
                   DropFocus -> Cmd.batch [cmd2, unfocusInput]
    in (m3, cmd3)

type Focus = Focus | NoFocus | DropFocus


update_ : Msg -> Model -> (Model, Cmd Msg, Focus)
update_ msg m =
    case (m.state, msg, m.cursor) of
        (_, CheckEscape code, _) ->
            if code == 27 -- escape
            then ({ m | cursor = Nothing }, Cmd.none, DropFocus)
            else (m, Cmd.none, NoFocus)

        (_, KeyPress code, Just id) ->
            case code of
                8 -> (m, rpc m <| DeleteNode id, NoFocus) -- backspace
                _ -> case Char.fromCode code of
                         'C' -> (m, rpc m <| ClearEdges id, NoFocus)
                         'L' -> (m, rpc m <| RemoveLastField id, NoFocus)
                         'A' -> ({m | state = ADD_DS_FIELD_NAME}, Cmd.none, Focus)
                         _ -> forCharCode m code
        (_, KeyPress code, _) ->
            forCharCode m code
            -- TODO: ESCAPE - unfocus

        (_, NodeClick node, _) ->
          ({ m | state = ADD_FUNCTION
               , cursor = Just node.id
           }, Cmd.none, DropFocus)

        (_, RecordClick pos, _) ->
          ({ m | lastPos = pos
           }, Cmd.none, Focus)

        (_, ClearCursor mpos, _) ->
          ({ m | cursor = Nothing
           }, Cmd.none, Focus)

        (_, DragNodeStart node mpos, _) ->
          if m.drag == NoDrag -- If we're dragging a slot don't change it
            then ({ m | drag = DragNode node.id (findOffset node.pos mpos)}, Cmd.none, NoFocus)
            else (m, Cmd.none, NoFocus)

        (_, DragNodeMove id offset currentPos, _) ->
          ({ m | nodes = updateDragPosition currentPos offset id m.nodes
               , lastPos = currentPos -- debugging
           }, Cmd.none, NoFocus)

        (_, DragNodeEnd id _, _) ->
          ({ m | drag = NoDrag
           }, rpc m <| UpdateNodePosition id, NoFocus)

        (_, DragSlotStart node param mpos, _) ->
          ({ m | cursor = Just node.id
               , drag = DragSlot node.id param mpos}, Cmd.none, NoFocus)

        (_, DragSlotMove id param mStartPos mpos, _) ->
            ({ m | lastPos = mpos
                 -- TODO: may not be necessary
                 , drag = DragSlot id param mStartPos
             }, Cmd.none, NoFocus)

        (_, DragSlotEnd node, _) ->
          case m.drag of
            DragSlot id param starting ->
              ({ m | drag = NoDrag}
              , rpc m <| AddEdge node.id (id, param), NoFocus)
            _ -> (m, Cmd.none, NoFocus)

        (_, DragSlotStop _, _) ->
          ({ m | drag = NoDrag}, Cmd.none, NoFocus)

        (ADD_FUNCTION, SubmitMsg, _) ->
            ({ m | state = ADD_FUNCTION
             }, rpc m <| AddFunctionCall m.inputValue m.lastPos, DropFocus)
        (ADD_VALUE, SubmitMsg, _) ->
            ({ m | state = ADD_VALUE
             }, rpc m <| AddValue m.inputValue m.lastPos, DropFocus)

        (ADD_DS, SubmitMsg, _) ->
            ({ m | state = ADD_DS_FIELD_NAME
             }, rpc m <| AddDatastore m.inputValue m.lastPos, DropFocus)

        (ADD_DS_FIELD_NAME, SubmitMsg, _) ->
            if m.inputValue == ""
            then -- the DS has all its fields
                ({ m | state = ADD_FUNCTION
                     , inputValue = ""
                 }, Cmd.none, NoFocus)
            else  -- save the field name, we'll submit it later the type
                ({ m | state = ADD_DS_FIELD_TYPE
                     , tempFieldName = m.inputValue
                 }, Cmd.none, Focus)

        (ADD_DS_FIELD_TYPE, SubmitMsg, Just id) ->
            ({ m | state = ADD_DS_FIELD_NAME
             }, rpc m <| AddDatastoreField id m.tempFieldName m.inputValue, Focus)

        (_, RPCCallBack (Ok (nodes, edges, cursor)), _) ->
            -- if the new cursor is blank, keep the old cursor if it's valid
            let oldCursor = Maybe.map (\(ID id) -> Dict.get id nodes) m.cursor
                newCursor = case cursor of
                                Nothing -> m.cursor
                                _ -> cursor
                newFocus = if m.state == ADD_DS_FIELD_NAME then Focus else DropFocus
            in ({ m | nodes = nodes
                    , edges = edges
                    , cursor = newCursor
                }, Cmd.none, newFocus )

        (_, RPCCallBack (Err (Http.BadStatus error)), _) ->
            ({ m | errors = addError ("Bad RPC call: " ++ toString(error.body)) m
                 , state = ADD_FUNCTION
             }, Cmd.none, NoFocus)

        (_, FocusResult (Ok ()), _) ->
            -- Yay, you focused a field! Ignore.
            (m, Cmd.none, NoFocus)

        (_, InputMsg target, _) ->
            -- Syncs the form with the model. The actual submit is in SubmitMsg
            ({ m | inputValue = target
             }, Cmd.none, NoFocus)

        t -> -- All other cases
            ({ m | errors = addError ("Nothing for " ++ (toString t)) m }, Cmd.none, NoFocus)




-- SUBSCRIPTIONS
subscriptions : Model -> Sub Msg
subscriptions m =
    let dragSubs = case m.drag of
                       DragNode id offset -> [ Mouse.moves (DragNodeMove id offset)
                                             , Mouse.ups (DragNodeEnd id)]
                       DragSlot id param start ->
                         [ Mouse.moves (DragSlotMove id param start)
                         , Mouse.ups DragSlotStop]
                       NoDrag -> [ Mouse.downs ClearCursor ]
        -- dont trigger commands if we're typing
        keySubs = if m.focused
                  then []
                  else [ Keyboard.downs KeyPress]
        standardSubs = [ Keyboard.downs CheckEscape
                       , Mouse.downs RecordClick]
    in Sub.batch
        (List.concat [standardSubs, keySubs, dragSubs])







-- VIEW
view : Model -> Html.Html Msg
view model =
  let  (w, h) = windowSize ()
  in
    Html.div
      [Attrs.id "grid"]
      [ viewInput model.inputValue
      , viewState model.state
      -- , viewClick model.lastPos
      , viewErrors model.errors
      , (Svg.svg
           [ SA.width (toString w) , SA.height (toString h)]
           (viewCanvas model))
      ]

viewInput value = Html.form [
                   Events.onSubmit (SubmitMsg)
                  ] [
                   Html.input [ Attrs.id consts.inputID
                              , Events.onInput InputMsg
                              , Attrs.value value
                              ] []
                  ]


viewState state = Html.text ("state: " ++ toString state)
viewErrors errors = Html.span [] <| (Html.text " -----> errors: ") :: (List.map Html.text errors)

viewCanvas : Model -> List (Svg.Svg Msg)
viewCanvas m =
    let  (w, h) = windowSize ()
         allNodes = List.map (viewNode m) (Dict.values m.nodes)
         edges = List.map (viewEdge m) m.edges
         mDragEdge = viewDragEdge m.drag m.lastPos
         dragEdge = case mDragEdge of
                      Just de -> [de]
                      Nothing -> []
    in svgArrowHead :: (allNodes ++ dragEdge ++ edges)

placeHtml : Pos -> Html.Html Msg -> Svg.Svg Msg
placeHtml pos html =
  Svg.foreignObject
    [ SA.x (toString pos.x)
    , SA.y (toString pos.y)
    ]
    [ html ]

nodeWidth : Node -> Bool -> Int
nodeWidth node selected =
  let l1 = [String.length node.name]
      l2 = if selected
           then
             List.map
               (\p -> (String.length p) + 2)
               node.parameters
           else []
      l3 = List.map
           (\(n,t) -> String.length n + String.length t + 3)
           node.fields
      charWidth = List.foldl max 0 (l1 ++ l2 ++ l3)
      width = charWidth * 11
  in ((width // 20) + 1) * 20

nodeHeight : Node -> Bool -> Int
nodeHeight node selected =
  if node.isDatastore
  then consts.nodeHeight * ( 1 + (List.length node.fields))
  else consts.nodeHeight
    -- Don't move edges when the node opens
    -- if selected
    -- then consts.nodeHeight * ( 1 + (List.length node.parameters))
    -- else consts.nodeHeight

nodeSize node selected =
  (nodeWidth node selected, nodeHeight node selected)

viewNode : Model -> Node -> Html.Html Msg
viewNode m n =
  let selected = case m.cursor of
                       Just id -> id == n.id
                       _ -> False
      class = if n.isDatastore then "datastore" else "function"
      -- [ Attrs.class ("block round-med function " ++ width)
      events =
        [ Events.onClick (NodeClick n)
        , Events.on "mousedown" (decodeClickLocation (DragNodeStart n))
        , Events.onMouseUp (DragSlotEnd n)
        ]

      width = "width" ++ (toString (nodeWidth n selected))
      selectedCl = if selected then ["selected"] else []
      classes = String.join " " (["node", class, width] ++ selectedCl)
      attrs = [Attrs.class classes ] ++ events
      heading = if n.isDatastore
                then Html.h3 [Attrs.class "name"] [ Html.text n.name ]
                else Html.span [Attrs.class "name"] [ Html.text n.name ]

      includeList = n.isDatastore ||
                    (selected && (List.length n.parameters > 0))
      field (name, tipe) = [ Html.text (name ++ " : " ++ tipe)
                           , Html.br [] []]
      slotHandler name = (decodeClickLocation (DragSlotStart n name))
      param name = Html.span
                   [ Attrs.class "parameter"
                   , Events.on "mousedown" (slotHandler name)
                   ]
                   [Html.text name]
      params = [Html.span
                  [Attrs.class "list"]
                  (List.map param n.parameters)]
      list = if includeList
             then
               [Html.span
                 [ Attrs.class "list"]
                 (List.concat
                    (List.map field n.fields)
                    ++ (List.map param n.parameters))]
             else []
  in
    placeHtml
      n.pos <|
      Html.span
        attrs
        (heading :: list)


dragEdgeStyle =
  [ SA.strokeWidth "2px"
  , SA.stroke "red"
  ]

edgeStyle =
  [ SA.strokeWidth "2.25px"
  , SA.stroke "#777"
  , SA.markerEnd "url(#triangle)"
  ]

svgLine : Pos -> Pos -> List (Svg.Attribute Msg) -> Svg.Svg Msg
svgLine p1 p2 attrs =
  Svg.line
    ([ SA.x1 (toString p1.x)
     , SA.y1 (toString p1.y)
     , SA.x2 (toString p2.x)
     , SA.y2 (toString p2.y)
     ] ++ attrs)
    []

viewDragEdge : Drag -> Pos -> Maybe (Svg.Svg Msg)
viewDragEdge drag currentPos =
  case drag of
    DragNode _ _ -> Nothing
    NoDrag -> Nothing
    DragSlot id param mStartPos ->
      Just <|
        svgLine mStartPos
                currentPos
                dragEdgeStyle

deID (ID x) = x
viewEdge : Model -> Edge -> Svg.Svg Msg
viewEdge m {source, target, targetParam} =
    let mSourceN = Dict.get (deID source) m.nodes
        mTargetN = Dict.get (deID target) m.nodes
        (sourceN, targetN) = case (mSourceN, mTargetN) of
                             (Just s, Just t) -> (s, t)
                             _ -> Debug.crash "Can't happen"
        targetPos = targetN.pos
        (sourceW, sourceH) =
          nodeSize sourceN (m.cursor == Just sourceN.id)
        (targetW, targetH) =
          nodeSize targetN (m.cursor == Just targetN.id)

    in svgLine
      (offset sourceN.pos (sourceW // 2) (sourceH // 2))
      (if sourceN.pos.x > targetPos.x
       then offset targetPos (targetW) (targetH // 2) -- right edge
       else offset targetPos 0 (targetH // 2) --left edge
      )
      edgeStyle

-- UTIL
timestamp : () -> Int
timestamp a = Native.Timestamp.timestamp a

windowSize : () -> (Int, Int)
windowSize a = let size = Native.Window.size a
               in (size.width, size.height)

focusInput = Dom.focus consts.inputID |> Task.attempt FocusResult
unfocusInput = Dom.blur consts.inputID |> Task.attempt FocusResult

addError error model =
    let time = timestamp ()
               in
    List.take 1 ((error ++ " (" ++ toString time ++ ") ") :: model.errors)


dlMap : (b -> c) -> Dict comparable b -> List c
dlMap fn d = List.map fn (Dict.values d)

updateDragPosition : Pos -> Offset -> ID -> NodeDict -> NodeDict
updateDragPosition pos off (ID id) nodes =
  Dict.update id (Maybe.map (\n -> {n | pos = offset pos off.x off.y})) nodes


decodeClickLocation : (Mouse.Position -> a) -> JSD.Decoder a
decodeClickLocation fn =
  let toA : Int -> Int -> a
      toA px py = fn {x=px, y=py}
  in JSDP.decode toA
      |> JSDP.required "pageX" JSD.int
      |> JSDP.required "pageY" JSD.int


findOffset : Pos -> Mouse.Position -> Offset
findOffset pos mpos =
 {x=pos.x - mpos.x, y= pos.y - mpos.y, offsetCheck=1}

offset p x y = { p | x = p.x + x
                   , y = p.y + y }

svgArrowHead =
  Svg.marker [ SA.id "triangle"
             , SA.viewBox "0 0 10 10"
             , SA.refX "4"
             , SA.refY "5"
             , SA.markerUnits "strokeWidth"
             , SA.markerWidth "7"
             , SA.markerHeight "7"
             , SA.orient "auto"
             , SA.fill "#777"
             ]
    [Svg.path [SA.d "M 0 0 L 5 5 L 0 10 z"] []]
