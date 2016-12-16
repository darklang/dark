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
import Collage
import Element
import Text
import Color


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

consts = { spacer = round 5
         , lineHeight = round 18
         , paramWidth = round 50
         , dotRadius = round 5
         , dotWidth = round 10
         , dotContainer = round 20
         , toolbarOffset = round 77
         , letterWidth = round 7
         , inputID = "darkInput"
         }


-- MODEL
type alias Model = { nodes : NodeDict
                   , edges : List Edge
                   , cursor : Cursor
                   , inputValue : String
                   , state : State
                   , tempFieldName : FieldName
                   , errors : List String
                   , lastPos : Pos
                   , drag : Drag
                   }

type alias Node = { name : Name
                  , id : ID
                  , pos : Pos
                  , is_datastore : Bool
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
type alias Pos = {x: Int, y: Int}
type alias NodeDict = Dict Name Node
type alias Cursor = Maybe ID
type Drag = NoDrag
          | DragNode ID
          | DragSlot ID ParamName Pos -- starting point of edge

type NodeSlot = NSNode Node
              | NSSlot Node ParamName
              | NSNone

init : ( Model, Cmd Msg )
init = let m = { nodes = Dict.empty
               , edges = []
               , cursor = Nothing
               , state = ADDING_FUNCTION
               , errors = [".", "."]
               , inputValue = ""
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
    | UpdateNodePosition ID -- no pos cause it's in the node
    | AddEdge ID (ID, ParamName)
    | DeleteNode ID

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

    in JSE.object [ ("command", JSE.string cmd)
                  , ("args", args) ]

decodeNode : JSD.Decoder Node
decodeNode =
  let toNode : Name -> String -> List(FieldName,TypeName) -> List ParamName -> Bool -> Int -> Int -> Node
      toNode name id fields parameters is_datastore x y =
          { name = name
          , id = ID id
          , fields = fields
          , parameters = parameters
          , is_datastore = is_datastore
          , pos = {x=x, y=y}
          }
  in JSDP.decode toNode
      |> JSDP.required "name" JSD.string
      |> JSDP.required "id" JSD.string
      |> JSDP.optional "fields" (JSD.keyValuePairs JSD.string) []
      |> JSDP.optional "parameters" (JSD.list JSD.string) []
      |> JSDP.optional "is_datastore" JSD.bool False
      |> JSDP.required "x" JSD.int
      |> JSDP.required "y" JSD.int
      -- |> JSDP.resolve

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
    = MouseDown Mouse.Position
    | DragStart Mouse.Position
    | DragNodeMove ID Mouse.Position
    | DragNodeEnd ID Mouse.Position
    | DragSlotMove ID ParamName Mouse.Position Mouse.Position
    | DragSlotEnd ID ParamName Mouse.Position Mouse.Position
    | InputMsg String
    | SubmitMsg
    | KeyPress Keyboard.KeyCode
    | FocusResult (Result Dom.Error ())
    | RPCCallBack (Result Http.Error (NodeDict, List Edge, Cursor))

type State
    = ADDING_FUNCTION
    | ADDING_DS_NAME
    | ADDING_DS_FIELD_NAME
    | ADDING_DS_FIELD_TYPE

update : Msg -> Model -> (Model, Cmd Msg)
update msg m =
    case (m.state, msg, m.cursor) of
        (_, KeyPress code, Just id) ->
            case code of
                -- backspace: delete node
                8 -> (m, rpc m <| DeleteNode id)
                -- TODO
                _ -> let _ = Debug.log "code" code
                     in (m, Cmd.none)
            -- clear edges - C
            -- remove last field - L
        (_, KeyPress code, _) ->
            let _ = Debug.log "code (no cursor)" code
            in (m, Cmd.none)
            -- ESCAPE - unfocus
            -- change to "function adding mode" - F
            -- change to "adding value" - V
            -- change to "ds adding mode" - D
            -- change to "input adding mode" - I
            -- change to "output adding mode" - O
        (_, MouseDown pos, _) ->
            -- if the mouse is within a node, select the node. Else create a new one.
            case findNode m pos of
                Nothing -> ({ m | cursor = Nothing
                                , inputValue = ""
                                , lastPos = pos
                            }, focusInput)
                Just node -> ({ m | state = if node.is_datastore then ADDING_DS_FIELD_NAME else ADDING_FUNCTION
                                  , inputValue = ""
                                  , lastPos = pos
                                  , cursor = Just node.id
                              }, focusInput)
        (_, DragStart pos, _) ->
            case findNodeOrSlot m pos of
                NSNone -> (m, Cmd.none)
                NSSlot node param -> ({ m | drag = DragSlot node.id param pos}, Cmd.none)
                NSNode node -> ({ m | drag = DragNode node.id}, Cmd.none)
        (_, DragNodeMove id pos, _) ->
            ({ m | nodes = updateDragPosition pos id m.nodes
             }, Cmd.none)
        (_, DragNodeEnd id _, _) ->
            -- to avoid moving when we just want to select, don't set to mouseUp position
            ({ m | drag = NoDrag
             }, rpc m <| UpdateNodePosition id)
        (_, DragSlotMove id param starting pos, _) ->
            ({ m | lastPos = pos
                 , drag = DragSlot id param starting
             }, Cmd.none)
        (_, DragSlotEnd id param starting pos, _) ->
            -- to avoid moving when we just want to select, don't set to mouseUp position
            let event = case findNode m pos of
                            Just node -> rpc m <| AddEdge node.id (id, param)
                            Nothing -> Cmd.none
            in ({ m | drag = NoDrag}, event)

        (ADDING_FUNCTION, SubmitMsg, _) ->
            if String.toLower(m.inputValue) == "ds"
            then ({ m | state = ADDING_DS_NAME
                      , inputValue = ""
                  }, focusInput)
            else ({ m | state = ADDING_FUNCTION
                      , inputValue = ""
                  }, Cmd.batch [focusInput, rpc m <| AddFunctionCall m.inputValue m.lastPos])
        (ADDING_DS_NAME, SubmitMsg, _) ->
            ({ m | state = ADDING_DS_FIELD_NAME
                 , inputValue = ""
             }, Cmd.batch [focusInput, rpc m <| AddDatastore m.inputValue m.lastPos])
        (ADDING_DS_FIELD_NAME, SubmitMsg, _) ->
            if m.inputValue == ""
            then -- the DS has all its fields
                ({ m | state = ADDING_FUNCTION
                     , inputValue = ""
                 }, Cmd.none)
            else  -- save the field name, we'll submit it later the type
                ({ m | state = ADDING_DS_FIELD_TYPE
                     , inputValue = ""
                     , tempFieldName = m.inputValue
                 }, focusInput)
        (ADDING_DS_FIELD_TYPE, SubmitMsg, Just id) ->
            ({ m | state = ADDING_DS_FIELD_NAME
                 , inputValue = ""
             }, Cmd.batch [focusInput, rpc m <| AddDatastoreField id m.tempFieldName m.inputValue])

        (_, RPCCallBack (Ok (nodes, edges, cursor)), _) ->
            -- if the new cursor is blank, keep the old cursor if it's valid
            let oldCursor = Maybe.map (\(ID id) -> Dict.get id nodes) m.cursor
                newCursor = case cursor of
                                Nothing -> m.cursor
                                _ -> cursor
            in ({ m | nodes = nodes
                    , edges = edges
                    , cursor = newCursor
                }, Cmd.none)
        (_, RPCCallBack (Err (Http.BadStatus error)), _) ->
            ({ m | errors = addError ("Bad RPC call: " ++ toString(error.status.message)) m
                 , state = ADDING_FUNCTION
             }, Cmd.none)

        (_, FocusResult (Ok ()), _) ->
            -- Yay, you focused a field! Ignore.
            ( m, Cmd.none )
        (_, InputMsg target, _) ->
            -- Syncs the form with the model. The actual submit is in SubmitMsg
            ({ m | inputValue = target
             }, Cmd.none)
        t -> -- All other cases
            ({ m | errors = addError ("Nothing for " ++ (toString t)) m }, Cmd.none )

        -- KeyMsg key -> -- Keyboard input
        --     ({ model | errors = addError "Not supported yet" model}, Cmd.none)




-- SUBSCRIPTIONS
subscriptions : Model -> Sub Msg
subscriptions m =
    let dragSubs = case m.drag of
                       DragNode id -> [ Mouse.moves (DragNodeMove id)
                                      , Mouse.ups (DragNodeEnd id)]
                       DragSlot id param start -> [ Mouse.moves (DragSlotMove id param start)
                                                  , Mouse.ups (DragSlotEnd id param start)]
                       NoDrag -> []
        standardSubs = [ Mouse.downs MouseDown
                       , Mouse.downs DragStart
                       , Keyboard.downs KeyPress]
    in Sub.batch
        (List.concat [standardSubs, dragSubs])







-- VIEW
view : Model -> Html.Html Msg
view model =
    Html.div [] [ viewInput model.inputValue
                , viewState model.state
                , viewErrors model.errors
                , viewCanvas model
                ]

viewInput value = Html.div [] [
                   Html.form [
                        Events.onSubmit (SubmitMsg)
                       ] [
                        Html.input [ Attrs.id consts.inputID
                                   , Events.onInput InputMsg
                                   , Attrs.value value
                                   ] []
                       ]
                  ]

viewState state = Html.div [] [ Html.text ("state: " ++ toString state) ]
viewErrors errors = Html.div [] (List.map str2div errors)

viewCanvas : Model -> Html.Html msg
viewCanvas m =
    let (w, h) = windowSize ()
        allNodes = viewAllNodes m m.nodes
        edges = viewAllEdges m m.edges
        click = viewClick m.lastPos
        mDragEdge = viewDragEdge m.drag m.lastPos
        dragEdge = case mDragEdge of
                       Just de -> [de]
                       Nothing -> []
    in Element.toHtml
        (Collage.collage w h
             (dragEdge ++ edges ++ (click :: allNodes)))



viewClick : Pos -> Collage.Form
viewClick pos = Collage.circle 10
                |> Collage.filled Color.lightCharcoal
                |> Collage.move (p2c pos)

viewAllEdges : Model -> List Edge -> List Collage.Form
viewAllEdges model edges = List.map (viewEdge model) edges
deID (ID x) = x
viewEdge : Model -> Edge -> Collage.Form
viewEdge m {source, target, targetParam} =
    let mSourceN = Dict.get (deID source) m.nodes
        mTargetN = Dict.get (deID target) m.nodes
        (sourceN, targetN) = case (mSourceN, mTargetN) of
                             (Just s, Just t) -> (s, t)
                             _ -> Debug.crash "Can't happen"
        sourcePos = sourceN.pos
        targetPos = dotPos targetN targetParam
        segment = Collage.segment (p2c sourcePos) (p2c targetPos)
        trace = Collage.traced Collage.defaultLine segment
    in trace

viewDragEdge : Drag -> Pos -> Maybe Collage.Form
viewDragEdge drag pos =
    case drag of
        DragNode _ -> Nothing
        NoDrag -> Nothing
        DragSlot id param startingPos ->
            let segment = Collage.segment (p2c startingPos) (p2c pos)
                trace = Collage.traced Collage.defaultLine segment
            in Just trace


viewAllNodes : Model -> NodeDict -> List Collage.Form
viewAllNodes model nodes = dlMap (viewNode model) nodes

viewNode : Model -> Node -> Collage.Form
viewNode model node =
    let
        color = nodeColor model node
        name = Element.centered (node.name |> Text.fromString |> Text.bold)
        fields = viewFields node.fields
        parameters = viewParameters node.parameters
        entire = Element.flow Element.down [ name
                                           , Element.spacer consts.spacer consts.spacer
                                           , parameters
                                           , fields]
        (w, h) = Element.sizeOf entire
        box = Collage.rect (toFloat w) (toFloat h)
                     |> Collage.filled color
        group = Collage.group [ box
                              , Collage.toForm entire]
    in Collage.move (p2c node.pos) group

nodeColor : Model -> Node -> Color.Color
nodeColor m node = if (DragNode node.id) == m.drag
                   then Color.lightRed
                   else if (Just node.id) == m.cursor
                        then Color.lightGreen
                        else Color.lightGrey


viewFields fields =
    Element.flow Element.down (List.map viewField fields)

viewField (name, type_) =
    (Element.flow
        Element.right
         [ Element.container
               consts.paramWidth consts.lineHeight
               Element.midLeft
                   (Element.leftAligned (Text.fromString name))
         , Element.container
               consts.paramWidth consts.lineHeight
               Element.midRight
                   (Element.rightAligned (Text.fromString type_))])

viewParameters parameters =
    Element.flow Element.down (List.map viewParameter parameters)

viewDot =
    Collage.collage
        consts.dotWidth
        consts.dotContainer
        [Collage.filled Color.red
             (Collage.circle (toFloat consts.dotRadius))]

viewParameter name =
    Element.flow
        Element.right
            [ viewDot
            , Element.container consts.paramWidth consts.lineHeight
                Element.midLeft (Element.leftAligned (Text.fromString name))]




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
    List.take 2 ((error ++ "-" ++ toString time) :: model.errors)

str2div str = Html.div [] [Html.text str]


p2c : Pos  -> (Float, Float)
p2c pos = let (w, h) = windowSize ()
          in ((toFloat pos.x) - ((toFloat w) / 2),
              ((toFloat h) / 2) - (toFloat pos.y) + (toFloat consts.toolbarOffset))

withinNode : Node -> Mouse.Position -> Bool
withinNode node pos =
    let height = nodeHeight node
        width = nodeWidth node
    in node.pos.x >= pos.x - (width // 2)
    && node.pos.x <= pos.x + (width // 2)
    && node.pos.y >= pos.y - (height // 2)
    && node.pos.y <= pos.y + (height // 2)

nodeWidth node =
    if node.is_datastore
    then 2 * consts.paramWidth
    else max consts.paramWidth (consts.letterWidth * String.length(node.name))

nodeHeight node =
    consts.spacer + consts.lineHeight * (1 + List.length node.parameters + List.length node.fields)

-- If the click is on a slot, return the slot. Else return the node.
slotOrNode : Node -> Pos -> NodeSlot
slotOrNode node pos =
    -- we clicked on a slot if we're on the left edge, below the spacer.
    let leftEdge = node.pos.x - (nodeWidth node // 2)
    in if pos.x > (leftEdge + consts.dotWidth)
       then NSNode node
       -- ok it's along the left. Now find its slot
       else let index = (pos.y - node.pos.y - consts.spacer) // consts.lineHeight
                asArray = Array.fromList node.parameters
                mParam = Array.get index asArray
                param = case mParam of
                            Just p -> p
                            Nothing -> Debug.crash "Can't happen"
            in if index < 0
               then NSNode node
               else NSSlot node param

dotPos : Node -> ParamName -> Pos
dotPos node paramName =
    let leftEdge = node.pos.x - (nodeWidth node // 2)
        (index, param) = List.foldl
                         (\p (i, p2) -> if p == paramName
                                        then (i, p)
                                        else (i+1, p2))
                         (0, "")
                         node.parameters
    in { x = leftEdge
       , y = node.pos.y + consts.spacer + consts.lineHeight * index}


findNode : Model -> Mouse.Position -> Maybe Node
findNode m pos =
    let nodes = Dict.values m.nodes
        candidates = List.filter (\n -> withinNode n pos) nodes
        distances = List.map
                    (\n -> (n, abs (pos.x - n.pos.x) + abs (pos.y - n.pos.y)))
                    candidates
        sorted = List.sortBy Tuple.second distances
        winner = List.head sorted
    in Maybe.map Tuple.first winner

findNodeOrSlot : Model -> Mouse.Position -> NodeSlot
findNodeOrSlot m pos = case findNode m pos of
                           Just node -> slotOrNode node pos
                           Nothing -> NSNone


dlMap : (b -> c) -> Dict comparable b -> List c
dlMap fn d = List.map fn (Dict.values d)

updateDragPosition : Pos -> ID -> NodeDict -> NodeDict
updateDragPosition pos (ID id) nodes =
    Dict.update id (Maybe.map (\n -> {n | pos = pos})) nodes
