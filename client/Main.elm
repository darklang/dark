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
import Dom
import Task


-- mine
import RPC exposing (rpc)
import Types exposing (..)
import Util exposing (deMaybe)
import View
import Consts
import Repl
import Graph as G



-- TOP-LEVEL
main : Program (Maybe Editor) Model Msg
main = Html.programWithFlags
       { init = init
       , view = View.view
       , update = update
       , subscriptions = subscriptions}



-- MODEL
init : Maybe Editor -> ( Model, Cmd Msg )
init e =
  let editor = case e of
                 Nothing -> { entryPos = Consts.initialPos
                            , clickPos = {x=0, y=0}
                            , entryValue = ""
                            , replValue = ""
                            }
                 Just e -> e
      m = { nodes = Dict.empty
          , edges = []
          , cursor = Nothing
          , live = Nothing
          , errors = ["None"]
          , focused = False
          , tempFieldName = ""
          , dragPos = {x=0, y=0}
          , drag = NoDrag
          , lastMsg = NoMsg
          , prevNode = Nothing
          , entryPos = editor.entryPos
          , clickPos = editor.clickPos
          , entryValue = editor.entryValue
          , replValue = editor.replValue
          }
      load = rpc m <| [LoadInitialGraph]
  in
    (m, Cmd.batch [focusEntry, load])


-- ports
port setStorage : Editor -> Cmd msg

model2editor : Model -> Editor
model2editor m = { entryPos = m.entryPos
                 , clickPos = m.clickPos
                 , entryValue = m.entryValue
                 , replValue = m.replValue
                 }

update : Msg -> Model -> (Model, Cmd Msg)
update msg m =
  let (m2, cmd) = update_ msg m in
  (m2, Cmd.batch [cmd, m |> model2editor |> setStorage])

update_ : Msg -> Model -> (Model, Cmd Msg)
update_ msg m =
  case (msg, m.cursor) of

    (CheckEscape code, _) ->
      if code == Consts.escapeKeycode
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
      && event.button == Consts.leftButton
      then ({ m | drag = DragNode node.id (findOffset node.pos event.pos)}, Cmd.none)
      else (m, Cmd.none)

    (DragNodeMove id offset currentPos, _) ->
      ({ m | nodes = updateDragPosition currentPos offset id m.nodes
           , dragPos = currentPos -- debugging
       }, focusEntry)

    (DragNodeEnd id _, _) ->
      ({ m | drag = NoDrag
       }, rpc m <| [UpdateNodePosition id])

    (DragSlotStart node param event, _) ->
      if event.button == Consts.leftButton
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
      if (Util.rematch "^[\"\'1-9].*" m.entryValue) then
        (m, rpc m <| [AddValue m.entryValue m.entryPos])
      else
        (m, rpc m <| [AddFunctionCall m.entryValue m.entryPos []])


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
            , prevNode = justAdded
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

nextPosition : Pos -> Pos
nextPosition {x, y} =
  if x > 900 then
    {x=100, y=y+100}
  else
    {x=x+100, y=y}
