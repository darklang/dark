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


-- dark
import RPC exposing (rpc)
import Types exposing (..)
import Util exposing (deMaybe)
import View
import Defaults
import Repl
import Graph as G
import Canvas


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

updateKeyPress : Model -> Char.KeyCode -> Cursor -> (Model, Cmd Msg)
updateKeyPress m code cursor =
   let char = Char.fromCode code in
   case (char, code, cursor, m.entryValue) of
     -- backspace through an empty node
     (_, 8, Filling n _, "") ->
       (m, rpc m <| [DeleteNode n.id])

     (_, 38, _, "") -> -- up
       ({ m | cursor = Canvas.selectNextNode m (\n o -> n.y > o.y)
        } , Cmd.none)

     (_, 40, _, "") -> -- down
       ({ m | cursor = Canvas.selectNextNode m (\n o -> n.y < o.y)
        } , Cmd.none)

     (_, 37, _, "") -> -- left
       ({ m | cursor = Canvas.selectNextNode m (\n o -> n.x > o.x)
        } , Cmd.none)

     (_, 39, _, "") -> -- right
       ({ m | cursor = Canvas.selectNextNode m (\n o -> n.x < o.x)
        } , Cmd.none)

     (char, code, cursor, _) ->
       let _ = Debug.log "Nothing to do" (char, code, cursor) in
       (m, Cmd.none)


update_ : Msg -> Model -> (Model, Cmd Msg)
update_ msg m_ =
  let m = { m_ | lastMsg = msg } in
  case (msg, m.cursor) of

    (CheckEscape code, _) ->
      if code == Defaults.escapeKeycode
      then ({ m | cursor = Deselected }, Cmd.none)
      else (m, Cmd.none)

    (KeyPress code, cursor) ->
      updateKeyPress m code cursor

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
      then ({ m | drag = DragNode node
                         (Canvas.findOffset node.pos event.pos)
                , cursor = Dragging node
            } , Cmd.none)
      else (m, Cmd.none)

    (DragNodeMove node offset pos, _) ->
      -- TODO: this is pretty nasty. we can avoid this (and issuing an
      -- updatenodeposition when the node didnt move), by only updating the node
      -- being drawn if cursor == node, in which case we use dragPos for it's
      -- position instead of the node position.
      ({ m | nodes = Canvas.updateDragPosition pos offset node.id m.nodes
           , dragPos = pos -- debugging
       }, Cmd.none)

    (DragNodeEnd node _, _) ->
      ({ m | drag = NoDrag
           , cursor = Canvas.selectNode m node
       }, rpc m <| [UpdateNodePosition node.id node.pos])

    (DragSlotStart target param event, _) ->
      if event.button == Defaults.leftButton
      then ({ m | cursor = Dragging target
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
        (m, rpc m <| [addNode m.entryValue pos [extra]])

    (EntrySubmitMsg, Creating pos) ->
      (m, rpc m <| [addNode m.entryValue pos []])



    (RPCCallBack calls (Ok (nodes, edges, justAdded)), _) ->
      let m2 = { m | nodes = nodes
                   , edges = edges
                   , errors = []}
          cursor = case justAdded of
                     -- if we deleted a node, the cursor is probably invalid
                     Nothing ->
                       case m.cursor |> Canvas.getCursorID |> Maybe.andThen (G.getNode m2) of
                         Nothing -> Deselected
                         Just _ -> m.cursor

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
      ({ m | errors = addError ("Bad RPC call: " ++ toString(error.body)) m
       }, Cmd.none)

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
      ({ m | errors = addError ("Nothing for " ++ (toString t)) m }, Cmd.none)



-----------------------
-- SUBSCRIPTIONS
-----------------------
subscriptions : Model -> Sub Msg
subscriptions m =
  let dragSubs = case m.drag of
                   DragNode node offset -> [ Mouse.moves (DragNodeMove node offset)
                                         , Mouse.ups (DragNodeEnd node)]
                   DragSlot _ _ _ ->
                     [ Mouse.moves DragSlotMove
                     , Mouse.ups DragSlotStop]
                   NoDrag -> [ ]
      keySubs = [ Keyboard.downs KeyPress]
      standardSubs = [ Keyboard.downs CheckEscape
                     , Mouse.downs RecordClick]
  in Sub.batch
    (List.concat [standardSubs, keySubs, dragSubs])


-----------------------
-- UTIL
-----------------------
addError : String -> Model -> List String
addError error model =
  let time = Util.timestamp ()
  in
    List.take 1
      ((error ++ " (" ++ toString time ++ ") ") :: model.errors)

addNode : Name -> Pos -> List ImplicitEdge -> RPC
addNode name pos extras =
  let newIsValue = Util.rematch "^[\"\'1-9].*" name in
  if newIsValue then
    case extras of
      [(ReceivingEdge _)] ->
        AddValue name pos []
      _ ->
        AddValue name pos extras
  else
    AddFunctionCall name pos extras
