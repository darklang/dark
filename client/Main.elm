port module Main exposing (..)

-- builtins
import Result
import Char
import Dict exposing (Dict)
import Maybe

-- lib
import Http
import Html
import Keyboard
import Mouse
import Maybe.Extra
import Keyboard.Event exposing (KeyboardEvent, decodeKeyboardEvent)


-- dark
import RPC exposing (rpc)
import Types exposing (..)
import Util exposing (deMaybe)
import View
import Defaults
import Repl
import Graph as G
import Canvas
import Keyboard.Event exposing (KeyboardEvent)
import Keyboard.Key as Key


-----------------------
-- TOP-LEVEL
-----------------------
main : Program (Maybe Editor) Model Msg
main = Html.programWithFlags
       { init = init
       , view = View.view
       , update = update
       , subscriptions = subscriptions}


-----------------------
-- MODEL
-----------------------
init : Maybe Editor -> ( Model, Cmd Msg )
init mEditor =
  let e = case mEditor of
                 Just e -> e
                 Nothing -> Defaults.defaultEditor
      m = Defaults.defaultModel e
  in
    (m, Cmd.batch [Canvas.focusEntry, rpc m <| [LoadInitialGraph]])


-----------------------
-- ports, save Editor state in LocalStorage
-----------------------
port setStorage : Editor -> Cmd msg

-----------------------
-- updates
-----------------------

update : Msg -> Model -> (Model, Cmd Msg)
update msg m =
  let (m2, cmd) = update_ msg m in
  (m2, Cmd.batch [ cmd
                 , m |> Defaults.model2editor |> setStorage
                 , Canvas.maybeFocusEntry m m2])

-- We use this because a) it has other modifiers and b) the timing with which it
-- fires means we get the right value in the entryValue, so we know when to
-- delete a node (if we rely on globalKeyPress, it deletes one character too
-- early, though this is maybe fixable.
updateEntryKeyPress : Model -> KeyboardEvent -> Cursor -> (Model, Cmd Msg)
updateEntryKeyPress m kb cursor =
   case (kb.keyCode, cursor, m.entryValue) of
     -- backspace through an empty node
     (Key.Backspace, Filling n _, "") ->
       (m, rpc m <| [DeleteNode n.id])

     (Key.Up, _, "") ->
       ({ m | cursor = Canvas.selectNextNode m (\n o -> n.y > o.y)
        } , Cmd.none)

     (Key.Down, _, "") ->
       ({ m | cursor = Canvas.selectNextNode m (\n o -> n.y < o.y)
        } , Cmd.none)

     (Key.Left, _, "") ->
       ({ m | cursor = Canvas.selectNextNode m (\n o -> n.x > o.x)
        } , Cmd.none)

     (Key.Right, _, "") ->
       ({ m | cursor = Canvas.selectNextNode m (\n o -> n.x < o.x)
        } , Cmd.none)

     (key, cursor, _) ->
       let _ = Debug.log "[Entry] Nothing to do" (key, cursor, m.entryValue) in
       (m, Cmd.none)

-- This fires when we're not in the input box
updateGlobalKeyPress : Model -> Keyboard.KeyCode -> Cursor -> (Model, Cmd Msg)
updateGlobalKeyPress m code cursor =
  if cursor == Deselected then
    let cursor = code
               |> Char.fromCode
               |> Char.toLower
               |> String.fromChar
               |> G.fromLetter m
               |> Maybe.map (Canvas.selectNode m) in
    case cursor of
      Nothing ->
        let _ = Debug.log "[global] No node named " (Char.fromCode code) in
        (m, Cmd.none)
      Just c -> ({ m | cursor = c }, Cmd.none)
  else
    let _ = Debug.log "[global] Nothing to do" (Char.fromCode code, code, cursor, m.entryValue) in
    (m, Cmd.none)

update_ : Msg -> Model -> (Model, Cmd Msg)
update_ msg m_ =
  let m = { m_ | lastMsg = msg } in
  case (msg, m.cursor) of

    (CheckEscape code, _) ->
      if code == Defaults.escapeKeycode
      then ({ m | cursor = Deselected }, Cmd.none)
      else (m, Cmd.none)

    (EntryKeyPress event, cursor) ->
      updateEntryKeyPress m event cursor

    (GlobalKeyPress code, cursor) ->
      updateGlobalKeyPress m code cursor

    (NodeClick node, _) ->
      ({ m | cursor = Canvas.selectNode m node}, Cmd.none)

    (RecordClick pos, _) ->
      -- When we click on a node, drag is set when RecordClick happens. So this
      -- avoids firing if we click outside a node
      if m.drag == NoDrag then
        ({ m | cursor = Creating pos }, Cmd.none)
      else
        (m, Cmd.none)

    ------------------------
    -- dragging nodes
    ------------------------
    (DragNodeStart node event, _) ->
      if m.drag == NoDrag -- If we're already dragging a slot don't change the node
      && event.button == Defaults.leftButton
      then ({ m | drag = DragNode node.id
                         (Canvas.findOffset node.pos event.pos)
                , cursor = Dragging node.id
            } , Cmd.none)
      else (m, Cmd.none)

    (DragNodeMove id offset pos, _) ->
      -- While it's kinda nasty to update a node in place, the drawing code
      -- get's really complex if we don't do this.
      ({ m | nodes = Canvas.updateDragPosition pos offset id m.nodes
           , dragPos = pos -- debugging
       }, Cmd.none)

    (DragNodeEnd id _, _) ->
      let node = G.getNodeExn m id in
      ({ m | drag = NoDrag
           , cursor = Canvas.selectNode m node
       }, rpc m <| [UpdateNodePosition id node.pos])

    (DragSlotStart target param event, _) ->
      if event.button == Defaults.leftButton
      then ({ m | cursor = Dragging target.id
                , drag = DragSlot target param event.pos}, Cmd.none)
      else (m, Cmd.none)

    (DragSlotMove mpos, _) ->
      ({ m | dragPos = mpos
       }, Cmd.none)

    (DragSlotEnd source, _) ->
      case m.drag of
        DragSlot target param starting ->
          ({ m | drag = NoDrag}
               , rpc m <| [AddEdge source.id (target.id, param)])
        _ -> (m, Cmd.none)

    (DragSlotStop _, _) ->
      ({ m | drag = NoDrag}, Cmd.none)

    ------------------------
    -- entry node
    ------------------------
    (EntrySubmitMsg, Filling node pos) ->
      let extra = case G.findHole m node of
                    ResultHole n -> ReceivingEdge n.id
                    ParamHole n p _ -> ParamEdge n.id p
      in
        case String.uncons m.entryValue of
          -- allow $var instead
          Just ('$', rest) ->
            case G.fromLetter m rest of
              Just source ->
                case extra of
                  ParamEdge tid p -> (m, rpc m <| [AddEdge source.id (tid, p)])
                  _ -> report m "There isn't parameter we're looking to fill here"
              Nothing -> report m ("There isn't a node named '" ++ rest ++ "' to connect to")

          _ -> (m, rpc m <| addNode m.entryValue pos [extra])


    (EntrySubmitMsg, Creating pos) ->
      (m, rpc m <| addNode m.entryValue pos [])



    (RPCCallBack calls (Ok (nodes, edges, justAdded)), _) ->
      let m2 = { m | nodes = nodes
                   , edges = edges
                   , error = ""}
          cursor = case justAdded of
                     -- if we deleted a node, the cursor is probably invalid
                     Nothing ->
                       if m.cursor
                         |> Canvas.getCursorID
                         |> Maybe.andThen (G.getNode m2)
                         |> (==) Nothing
                       then Deselected
                       else m.cursor

                     -- if we added a node, select it
                     Just id ->
                       let node = G.getNodeExn m2 id in
                       Canvas.selectNode m2 node
      in
        ({ m2 | cursor = cursor
              , entryValue = ""}, Cmd.none)


    ------------------------
    -- plumbing
    ------------------------
    (RPCCallBack _ (Err (Http.BadStatus error)), _) ->
      report m ("Bad RPC call: " ++ (toString error.body))

    (FocusResult _, _) ->
      -- Yay, you focused a field! Ignore.
      -- TODO: should these be separate events?
      ({m | replValue = ""
          , entryValue = ""
       }, Cmd.none)

    (EntryInputMsg target, _) ->
      -- Syncs the form with the model. The actual submit is in EntrySubmitMsg
      ({ m | entryValue = target
       }, Cmd.none)

    t -> -- All other cases
      report m ("Nothing for " ++ (toString t))



-----------------------
-- SUBSCRIPTIONS
-----------------------
subscriptions : Model -> Sub Msg
subscriptions m =
  let dragSubs = case m.drag of
                   -- we use IDs here because the node will change before they're triggered
                   DragNode id offset -> [ Mouse.moves (DragNodeMove id offset)
                                         , Mouse.ups (DragNodeEnd id)]
                   DragSlot _ _ _ ->
                     [ Mouse.moves DragSlotMove
                     , Mouse.ups DragSlotStop]
                   NoDrag -> [ ]
      keySubs = [ Keyboard.downs GlobalKeyPress]
      standardSubs = [ Keyboard.downs CheckEscape
                     , Mouse.downs RecordClick]
  in Sub.batch
    (List.concat [standardSubs, keySubs, dragSubs])


-----------------------
-- UTIL
-----------------------
report : Model -> String -> (Model, Cmd msg)
report m err =
  let time = Util.timestamp ()
  in ({ m | error = err ++ " (" ++ toString time ++ ") "
          }, Cmd.none)

addNode : Name -> Pos -> List ImplicitEdge -> List RPC
addNode name pos extras =
  let newIsValue = Util.rematch "^[\"\'1-9].*" name in
  if newIsValue then
    case extras of
      [(ReceivingEdge _)] ->
        [AddValue name pos []]
      _ ->
        [AddValue name pos extras]
  else
    if name == "" then
      []
    else
      [AddFunctionCall name pos extras]
