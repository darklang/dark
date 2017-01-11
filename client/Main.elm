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
import Util
import View
import Consts


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
               , state = ADD_FUNCTION
               , errors = ["."]
               , inputValue = ""
               , focused = False
               , tempFieldName = ""
               , lastPos = {x=-1, y=-1}
               , drag = NoDrag
               , lastMsg = NoMsg
               }
       in (m, rpc m <| LoadInitialGraph)


-- UPDATE
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
        m4 = { m2 | lastMsg = msg }
    in (m4, cmd3)

type Focus = Focus | NoFocus | DropFocus

updateKeyPress : Model -> Char.KeyCode -> Cursor -> (Model, Cmd Msg, Focus)
updateKeyPress m code cursor =
  let char = Char.fromCode code
  in case (char, code, cursor) of
       (_, 8, Just id) -> -- backspace
         (m, rpc m <| DeleteNode id, NoFocus)
       ('C', _, Just id) ->
         (m, rpc m <| ClearEdges id, NoFocus)
       ('L', _, Just id) ->
         (m, rpc m <| RemoveLastField id, NoFocus)
       ('A', _, _) ->
         ({m | state = ADD_DS_FIELD_NAME}, Cmd.none, Focus)
       ('F', _, _) ->
         ({ m | state = ADD_FUNCTION}, Cmd.none, NoFocus)
       ('V', _, _) ->
         ({ m | state = ADD_VALUE}, Cmd.none, NoFocus)
       ('D', _, _) ->
         ({ m | state = ADD_DS}, Cmd.none, NoFocus)
       _ ->
         let _ = Debug.log "nothing" char
         in (m, Cmd.none, NoFocus)


update_ : Msg -> Model -> (Model, Cmd Msg, Focus)
update_ msg m =
  case (m.state, msg, m.cursor) of
    (_, CheckEscape code, _) ->
      if code == Consts.escapeKeycode
      then ({ m | cursor = Nothing }, Cmd.none, DropFocus)
      else (m, Cmd.none, NoFocus)

    (_, KeyPress code, cursor) ->
      updateKeyPress m code cursor

    (_, NodeClick node, _) ->
      ({ m | state = ADD_FUNCTION
           , cursor = Just node.id
       }, Cmd.none, DropFocus)

    (_, RecordClick pos, _) ->
      ({ m | lastPos = pos
       }, Cmd.none, NoFocus)

    (_, ClearCursor mpos, _) ->
      ({ m | cursor = Nothing
       }, Cmd.none, NoFocus)

    (_, DragNodeStart node event, _) ->
      if m.drag == NoDrag -- If we're already dragging a slot don't change the node
      && event.button == Consts.leftButton
      then ({ m | drag = DragNode node.id (findOffset node.pos event.pos)}, Cmd.none, NoFocus)
      else (m, Cmd.none, NoFocus)

    (_, DragNodeMove id offset currentPos, _) ->
      ({ m | nodes = updateDragPosition currentPos offset id m.nodes
           , lastPos = currentPos -- debugging
       }, Cmd.none, NoFocus)

    (_, DragNodeEnd id _, _) ->
      ({ m | drag = NoDrag
       }, rpc m <| UpdateNodePosition id, NoFocus)

    (_, DragSlotStart node param event, _) ->
      if event.button == Consts.leftButton
      then ({ m | cursor = Just node.id
                , drag = DragSlot node.id param event.pos}, Cmd.none, NoFocus)
      else (m, Cmd.none, NoFocus)

    (_, DragSlotMove mpos, _) ->
      ({ m | lastPos = mpos
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
focusInput : Cmd Msg
focusInput = Dom.focus Consts.inputID |> Task.attempt FocusResult

unfocusInput : Cmd Msg
unfocusInput = Dom.blur Consts.inputID |> Task.attempt FocusResult

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
