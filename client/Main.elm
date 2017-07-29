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



-- TOP-LEVEL
main : Program (Maybe Editor) Model Msg
main = Html.programWithFlags
       { init = init
       , view = View.view
       , update = update
       , subscriptions = subscriptions}

focusEntry = Canvas.focusEntry

-- MODEL
init : Maybe Editor -> ( Model, Cmd Msg )
init mEditor =
  let e = case mEditor of
                 Just e -> e
                 Nothing -> Defaults.defaultEditor
      m = Defaults.defaultModel e
  in
    (m, Cmd.batch [focusEntry, rpc m <| [LoadInitialGraph]])


-- ports, save Editor state in LocalStorage
port setStorage : Editor -> Cmd msg

model2editor : Model -> Editor
model2editor m = { cursor = Maybe.map deID m.cursor
                 , focused = m.focused
                 , entryPos = m.entryPos
                 , clickPos = m.clickPos
                 , replValue = m.replValue
                 , entryValue = m.entryValue
                 , tempFieldName = m.tempFieldName
                 }

update : Msg -> Model -> (Model, Cmd Msg)
update msg m =
  let (m2, cmd) = update_ msg m in
  (m2, Cmd.batch [cmd, m |> model2editor |> setStorage])


-- updates
update_ : Msg -> Model -> (Model, Cmd Msg)
update_ msg m =
  case (msg, m.cursor) of

    (CheckEscape code, _) ->
      if code == Defaults.escapeKeycode
      then ({ m | cursor = Nothing }, Cmd.none)
      else (m, Cmd.none)

    (NodeClick node, _) ->
      ({ m | cursor = Just node.id
       }, focusEntry)

    (RecordClick pos, _) ->
      ({ m | entryPos = pos
       }, focusEntry)

    (ClearCursor mpos, _) ->
      ({ m | cursor = Nothing
       }, focusEntry)

    ------------------------
    -- dragging nodes
    ------------------------
    (DragNodeStart node event, _) ->
      if m.drag == NoDrag -- If we're already dragging a slot don't change the node
      && event.button == Defaults.leftButton
      then ({ m | drag = DragNode node.id
                         (Canvas.findOffset node.pos event.pos)}
           , Cmd.none)
      else (m, Cmd.none)

    (DragNodeMove id offset pos, _) ->
      ({ m | nodes = Canvas.updateDragPosition pos offset id m.nodes
           , dragPos = pos -- debugging
       }, focusEntry)

    (DragNodeEnd id _, _) ->
      let node = G.getNode m id in
      ({ m | drag = NoDrag
       }, rpc m <| [UpdateNodePosition id node.pos])

    (DragSlotStart node param event, _) ->
      if event.button == Defaults.leftButton
      then ({ m | cursor = Just node.id
                , drag = DragSlot node.id param event.pos}, Cmd.none)
      else (m, Cmd.none)

    (DragSlotMove mpos, _) ->
      ({ m | dragPos = mpos
       }, Cmd.none)

    (DragSlotEnd node, _) ->
      case m.drag of
        DragSlot id param starting ->
          ({ m | drag = NoDrag}
               , rpc m <| [AddEdge node.id (id, param)])
        _ -> (m, Cmd.none)

    (DragSlotStop _, _) ->
      ({ m | drag = NoDrag}, focusEntry)

    ------------------------
    -- entry node
    ------------------------
    (EntrySubmitMsg, cursor) ->
      let newIsValue = Util.rematch "^[\"\'1-9].*" m.entryValue
          extras =
            case G.findHole m m.cursor of
              NoHole -> []
              ResultHole n ->
                if newIsValue then
                  -- this doesnt take params but probably intentional so just
                  -- allow it
                  []
                else
                  [ReceivingEdge n.id]
              ParamHole n p _ -> [ParamEdge n.id p]
      in
        if newIsValue then
          (m, rpc m <| [AddValue m.entryValue m.entryPos extras])
        else
          (m, rpc m <| [AddFunctionCall m.entryValue m.entryPos extras])


    (RPCCallBack calls (Ok (nodes, edges, justAdded)), _) ->
      let m2 = { m | nodes = nodes
                   , edges = edges
                   , errors = []}
          pos =
            case G.findHole m2 justAdded of
              NoHole -> m.entryPos
              ResultHole n -> {x=n.pos.x+100,y=n.pos.y+100}
              ParamHole n _ i -> {x=n.pos.x-100+(i*100), y=n.pos.y-100}
      in
       ({m2 | entryPos = pos
            , cursor = justAdded
        }, focusEntry)


    ------------------------
    -- plumbing
    ------------------------
    (RPCCallBack _ (Err (Http.BadStatus error)), _) ->
      ({ m | errors = addError ("Bad RPC call: " ++ toString(error.body)) m
       }, focusEntry)

    -- (ReplSubmitMsg, cursor) ->
    --   let (m2, rpcs) = Repl.parse m m.replValue cursor
    --       m3 = { m2 | replValue = "" } in
    --   case rpcs of
    --     [] -> (m3, Cmd.none)
    --     rpcs -> (m3, RPC.rpc m3 rpcs)


    (FocusResult (Ok ()), _) ->
      -- Yay, you focused a field! Ignore.
      -- TODO: should these be separate events?
      ({m | replValue = ""
          , entryValue = ""
          , focused = True}, Cmd.none)

    -- (ReplInputMsg target, _) ->
    --   -- Syncs the form with the model. The actual submit is in ReplSubmitMsg
    --   ({ m | replValue = target
    --    }, Cmd.none)

    (EntryInputMsg target, _) ->
      -- Syncs the form with the model. The actual submit is in EntrySubmitMsg
      ({ m | entryValue = target
       }, Cmd.none)

    t -> -- All other cases
      ({ m | errors = addError ("Nothing for " ++ (toString t)) m }, focusEntry)




-- SUBSCRIPTIONS
subscriptions : Model -> Sub Msg
subscriptions m =
  let dragSubs = case m.drag of
                   DragNode id offset -> [ Mouse.moves (DragNodeMove id offset)
                                         , Mouse.ups (DragNodeEnd id)]
                   DragSlot _ _ _ ->
                     [ Mouse.moves DragSlotMove
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


-- UTIL
addError : String -> Model -> List String
addError error model =
  let time = Util.timestamp ()
  in
    List.take 1
      ((error ++ " (" ++ toString time ++ ") ") :: model.errors)
