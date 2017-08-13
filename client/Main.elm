port module Main exposing (..)

-- builtins
import Maybe

-- lib
import Json.Decode as JSD
import Http
import Html
import Keyboard
import Mouse
import Keyboard.Event
import Keyboard.Key as Key

-- dark
import RPC exposing (rpc)
import Types exposing (..)
import Util exposing (deMaybe)
import View
import Defaults
import Graph as G
import Canvas
import Entry
import Autocomplete
import Selection
import Window.Events exposing (onWindow)


-----------------------
-- TOP-LEVEL
-----------------------
main : Program Flags Model Msg
main = Html.programWithFlags
       { init = init
       , view = View.view
       , update = update
       , subscriptions = subscriptions}


-----------------------
-- MODEL
-----------------------
init : Flags -> ( Model, Cmd Msg )
init {state, complete} =
  let editor = case state of
            Just e -> e
            Nothing -> Defaults.defaultEditor
      m = Defaults.defaultModel editor
      m2 = { m | complete = Autocomplete.init complete }
  in
    (m2, rpc m <| [LoadInitialGraph])


-----------------------
-- ports, save Editor state in LocalStorage
-----------------------
port setStorage : Editor -> Cmd a

-----------------------
-- updates
-----------------------

update : Msg -> Model -> (Model, Cmd Msg)
update msg m =
  let mods = (update_ msg m)
      (newm, newc) = updateMod mods (m, Cmd.none)
  in
    ({ newm | lastMsg = msg
            , lastMod = newm}
     , Cmd.batch [newc, m |> Defaults.model2editor |> setStorage])

-- applied from left to right
updateMod : Modification -> (Model, Cmd Msg) -> (Model, Cmd Msg)
updateMod mod (m, cmd) =
  let (newm, newcmd) =
    case mod of
      Error e -> { m | error = (e, Util.timestamp ())} ! []
      RPC call -> m ! [rpc m [call]]
      NoChange -> m ! []
      Select id -> { m | state = Selecting id} ! []
      Enter entry -> { m | state = Entering entry } ! [Entry.focusEntry]
      Drag d -> { m | state = Dragging d } ! []
      ModelMod mm -> mm m ! []
      Pick -> m ! []
      Deselect -> { m | state = Deselected } ! []
      Many mods -> List.foldl updateMod (m, Cmd.none) mods
      AutocompleteMod mod ->
        { m | complete = Autocomplete.update m.complete mod } ! []
  in
    (newm, Cmd.batch [cmd, newcmd])


update_ : Msg -> Model -> Modification
update_ msg m =
  case (msg, m.state) of

    (NodeClick node, _) ->
      Select node.id

    (RecordClick pos, _) ->
      -- TODO: what does this mean?
      -- When we click on a node, drag is set when RecordClick happens.
      -- So this avoids firing if we click outside a node
      Enter <| Creating pos

    ------------------------
    -- dragging nodes
    ------------------------
    (DragNodeStart node event, _) ->
      NoChange
      -- If we're already dragging a slot don't change the nodea
      -- TODO: reenable
      -- if m.drag == NoDrag && event.button == Defaults.leftButton
      -- then
      --   let offset = Canvas.findOffset node.pos event.pos in
      --   Drag <| DragNode node.id offset
      -- else NoChange

    (DragNodeMove id offset pos, _) ->
      -- While it's kinda nasty to update a node in place, the drawing
      -- code get's really complex if we don't do this.
      let update = Canvas.updateDragPosition pos offset id m.nodes in
      Many [ ModelMod (\m -> { m | nodes = update })
           -- TODO reenable and fix
           -- , Drag <| DragNode id pos
           ]

    (DragNodeEnd id _, _) ->
      let node = G.getNodeExn m id in
      Many [ Entry.enter m id
           , RPC <| UpdateNodePosition id node.pos]

    (DragSlotStart target param event, _) ->
      if event.button == Defaults.leftButton
      then Many [ Drag <| DragSlot target param event.pos]
      else NoChange

    (DragSlotMove mpos, _) ->
      -- TODO reenable
      NoChange
      -- ModelMod (\m -> { m | dragPos = mpos })

    (DragSlotEnd source, _) ->
      case m.state of
        Dragging (DragSlot target param starting) ->
          -- TODO: select a node now
          RPC <| AddEdge source.id (target.id, param)
        _ -> NoChange

    (DragSlotStop _, _) ->
      -- TODO: select a node
      NoChange

    ------------------------
    -- entry node
    ------------------------
    (EntrySubmitMsg, Entering cursor) ->
      Entry.submit m cursor

    (EntryKeyPress event, Entering _ ) ->
      Entry.updateKeyPress m event

    (GlobalKeyPress event, state) ->
      case (event.keyCode, state, m.complete.value) of
        -- Selecting
        (Key.Backspace, Selecting id, _) ->
          RPC <| DeleteNode id
        (Key.Up, Selecting id, _) ->
          Selection.selectNextNode m id (\n o -> n.y > o.y)
        (Key.Down, Selecting id, _) ->
          Selection.selectNextNode m id (\n o -> n.y < o.y)
        (Key.Left, Selecting id, _) ->
          Selection.selectNextNode m id (\n o -> n.x > o.x)
        (Key.Right, Selecting id, _) ->
          Selection.selectNextNode m id (\n o -> n.x < o.x)
        (Key.Enter, Selecting id, _) ->
          Entry.enter m id
        (Key.Escape, Selecting id, _) ->
          Deselect
        -- Entering
        (Key.Up, Entering _, _) ->
          AutocompleteMod SelectUp
        (Key.Down, Entering _, _) ->
          AutocompleteMod SelectDown
        (Key.Right, Entering _, _) ->
          let sp = Autocomplete.sharedPrefix m.complete.current in
          if sp == "" then NoChange
          else Many [ AutocompleteMod <| SetEntry sp ]
        (Key.Enter, Entering _, _) ->
          case Autocomplete.highlighted m.complete of
            Just s -> AutocompleteMod <| SetEntry s
            Nothing -> NoChange
        (Key.Escape, Entering _, _) ->
          case Selection.getCursorID m.state of
            Just id -> Select id
            Nothing -> Deselect
        (key, Entering _, val) ->
          AutocompleteMod <| SetEntry val
        (code, _, _)
          -> Selection.selectByLetter m code



    (EntryInputMsg target, _) ->
      Entry.updateValue target

    (RPCCallBack calls (Ok (nodes, edges, justAdded)), _) ->
      let m2 = { m | nodes = nodes
                   , edges = edges }
          reaction = case justAdded of
                       -- if we deleted a node, the cursor is probably
                       -- invalid
                       Nothing ->
                         if m.state
                           |> Selection.getCursorID
                           |> Maybe.andThen (G.getNode m2)
                           |> (==) Nothing
                         then Deselect
                         else NoChange

                       -- TODO if the just-added node has an outgoing
                       -- edge, which was just selected, choose it
                       -- instead.

                       -- if we added a node, select it
                       Just id ->
                         Entry.enter m2 id

      in
        Many [ ModelMod (\_ -> m2)
             , Error ""
             , reaction
             , AutocompleteMod Reset
             ]


    ------------------------
    -- plumbing
    ------------------------
    (RPCCallBack _ (Err (Http.BadStatus error)), _) ->
      Error <| "Bad RPC call: " ++ (toString error.body)

    (FocusResult _, _) ->
      -- Yay, you focused a field! Ignore.
      AutocompleteMod Reset

    t -> Error <| "Nothing for " ++ (toString t)


-----------------------
-- SUBSCRIPTIONS
-----------------------
subscriptions : Model -> Sub Msg
subscriptions m =
  let dragSubs =
        case m.state of
          -- we use IDs here because the node will change
          -- before they're triggered
          Dragging (DragNode id offset) ->
            [ Mouse.moves (DragNodeMove id offset)
            , Mouse.ups (DragNodeEnd id)]
          Dragging (DragSlot _ _ _) ->
            [ Mouse.moves DragSlotMove
            , Mouse.ups DragSlotStop]
          _ -> []
      keySubs =
        [onWindow "keydown"
           (JSD.map GlobalKeyPress Keyboard.Event.decodeKeyboardEvent)]

      standardSubs = [ Mouse.downs RecordClick ]
  in Sub.batch
    (List.concat [standardSubs, keySubs, dragSubs])

