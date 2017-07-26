port module Main exposing (..)

-- builtins
import Result
import Char
import Dict exposing (Dict)
import Http
import Html

-- lib
import Keyboard
import Mouse
import Dom
import Task


-- mine
import RPC exposing (rpc)
import Types exposing (..)
import Util exposing (deMaybe)
import View
import Consts
import Repl



-- TOP-LEVEL
main : Program Never Model Msg
main = Html.program
       { init = init
       , view = View.view
       , update = update
       , subscriptions = subscriptions}

-- MODEL
init : ( Model, Cmd Msg )
init = let m = { nodes = Dict.empty
               , edges = []
               , cursor = Nothing
               , live = Nothing
               , errors = ["None"]
               , entryValue = ""
               , replValue = ""
               , focused = False
               , tempFieldName = ""
               , lastPos = Consts.initialPos
               , drag = NoDrag
               , lastMsg = NoMsg
               } in
       let load = rpc m <| [LoadInitialGraph]
       in (m, Cmd.batch [focusEntry, load])


update : Msg -> Model -> (Model, Cmd Msg)
update msg m =
  case (msg, m.cursor) of
    (CheckEscape code, _) ->
      if code == Consts.escapeKeycode
      then ({ m | cursor = Nothing
                , lastPos = Consts.initialPos}, Cmd.none)
      else (m, Cmd.none)

    (NodeClick node, _) ->
      ({ m | cursor = Just node.id
       }, focusRepl)

    (RecordClick pos, _) ->
      ({ m | lastPos = pos
       }, focusRepl)

    (ClearCursor mpos, _) ->
      ({ m | cursor = Nothing
       }, focusRepl)

    (DragNodeStart node event, _) ->
      if m.drag == NoDrag -- If we're already dragging a slot don't change the node
      && event.button == Consts.leftButton
      then ({ m | drag = DragNode node.id (findOffset node.pos event.pos)}, Cmd.none)
      else (m, Cmd.none)

    (DragNodeMove id offset currentPos, _) ->
      ({ m | nodes = updateDragPosition currentPos offset id m.nodes
           , lastPos = currentPos -- debugging
       }, focusRepl)

    (DragNodeEnd id _, _) ->
      ({ m | drag = NoDrag
       }, rpc m <| [UpdateNodePosition id])

    (DragSlotStart node param event, _) ->
      if event.button == Consts.leftButton
      then ({ m | cursor = Just node.id
                , drag = DragSlot node.id param event.pos}, Cmd.none)
      else (m, Cmd.none)

    (DragSlotMove mpos, _) ->
      ({ m | lastPos = mpos
       }, Cmd.none)

    (DragSlotEnd node, _) ->
      case m.drag of
        DragSlot id param starting ->
          ({ m | drag = NoDrag}
               , rpc m <| [AddEdge node.id (id, param)])
        _ -> (m, Cmd.none)

    (DragSlotStop _, _) ->
      ({ m | drag = NoDrag}, focusRepl)

    (EntrySubmitMsg, cursor) ->
      (m, rpc m [AddFunctionCall m.entryValue m.lastPos []])

    (ReplSubmitMsg, cursor) ->
      let (m2, rpcs) = Repl.parse m m.replValue cursor
          m3 = { m2 | replValue = "" } in
      case rpcs of
        [] -> (m3, Cmd.none)
        rpcs -> (m3, RPC.rpc m3 rpcs)

    (RPCCallBack (Ok (nodes, edges)), _) ->
      ({ m | nodes = nodes
           , edges = edges
           , errors = []
       }, focusRepl)

    (RPCCallBack (Err (Http.BadStatus error)), _) ->
      ({ m | errors = addError ("Bad RPC call: " ++ toString(error.body)) m
       }, focusRepl)

    (FocusResult (Ok ()), _) ->
      -- Yay, you focused a field! Ignore.
      -- TODO: should these be separate events?
      ({m | replValue = ""
          , entryValue = ""
          , focused = True}, Cmd.none)

    (ReplInputMsg target, _) ->
      -- Syncs the form with the model. The actual submit is in ReplSubmitMsg
      ({ m | replValue = target
       }, Cmd.none)

    (EntryInputMsg target, _) ->
      -- Syncs the form with the model. The actual submit is in EntrySubmitMsg
      ({ m | entryValue = target
       }, Cmd.none)

    t -> -- All other cases
      ({ m | errors = addError ("Nothing for " ++ (toString t)) m }, focusRepl)




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
focusEntry : Cmd Msg
focusEntry = Dom.focus Consts.entryID |> Task.attempt FocusResult

focusRepl : Cmd Msg
focusRepl = Cmd.none -- Dom.focus Consts.replID |> Task.attempt FocusResult

unfocusRepl : Cmd Msg
unfocusRepl = Dom.blur Consts.replID |> Task.attempt FocusResult

addError : String -> Model -> List String
addError error model =
  let time = Util.timestamp ()
  in
    List.take 1
      ((error ++ " (" ++ toString time ++ ") ") :: model.errors)

updateDragPosition : Pos -> Offset -> ID -> NodeDict -> NodeDict
updateDragPosition pos off (ID id) nodes =
  Dict.update id (Maybe.map (\n -> {n | pos = {x=pos.x+off.x, y=pos.y+off.y}})) nodes


findOffset : Pos -> Mouse.Position -> Offset
findOffset pos mpos =
 {x=pos.x - mpos.x, y= pos.y - mpos.y, offsetCheck=1}
