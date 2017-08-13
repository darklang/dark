module Selection exposing (..)

-- builtins
import Char
import Task

-- lib
import Mouse
import List.Extra
import Keyboard
import Dom
import Keyboard.Event exposing (KeyboardEvent)
import Keyboard.Key as Key

-- dark
import Types exposing (..)
import Util exposing (deMaybe)
import Graph as G
import Defaults


updateKeyPress : Model -> KeyboardEvent -> State -> Modification
updateKeyPress m kb state =
  case (kb.keyCode, state) of
    (Key.Escape, _) ->
      Deselect

     -- backspace through an empty node
     -- (Key.Backspace, Filling n _ _, "") ->
     --   RPC <| DeleteNode n.id

     -- (Key.Up, _, "") ->
     --   Cursor <| Canvas.selectNextNode m (\n o -> n.y > o.y)

     -- (Key.Down, _, "") ->
     --   Cursor <| Canvas.selectNextNode m (\n o -> n.y < o.y)

     -- (Key.Left, _, "") ->
     --   Cursor <| Canvas.selectNextNode m (\n o -> n.x > o.x)

     -- (Key.Right, _, "") ->
     --   Cursor <| Canvas.selectNextNode m (\n o -> n.x < o.x)

    (_, _) ->
      case kb.keyCode
        |> Key.toChar
        |> Maybe.map Char.toLower
        |> Maybe.map String.fromChar
        |> Maybe.andThen (G.fromLetter m)
        |> Maybe.map (selectNode m)
      of
        Just cursor -> Enter cursor
        Nothing -> NoChange

------------------
-- cursor stuff
----------------

isSelected : Model -> Node -> Bool
isSelected m n =
  case m.state of
    Entering (Filling node _ _) -> n == node
    Selecting id -> n.id == id
    _ -> False

entryVisible : State -> Bool
entryVisible state =
  case state of
    Entering _ -> True
    _ -> False

focusEntry : Cmd Msg
focusEntry = Dom.focus Defaults.entryID |> Task.attempt FocusResult

getCursorID : State -> Maybe ID
getCursorID s =
  case s of
    Dragging (DragNode id _) -> Just id
    Entering (Filling node _ _) -> Just node.id
    _ -> Nothing

selectNextNode : Model -> (Pos -> Pos  -> Bool) -> State
selectNextNode m cond =
  -- if we're currently in a node, follow the direction. For now, pick
  -- the nearest node to it, that it's connected to, that's roughly in
  -- that direction.
  case m.state of
    Entering (Filling n _ _) ->
      let other =
          G.connectedNodes m n
            -- that are above us
            |> List.filter (\o -> cond n.pos o.pos)
            -- the nearest to us
            |> List.sortBy (\other -> G.distance other n)
            |> List.head
      in
        case other of
          Nothing -> m.state
          Just node -> Entering <| selectNode m node
    _ -> m.state




selectNode : Model -> Node -> EntryCursor
selectNode m selected =
  let hole = G.findHole m selected
      pos = case hole of
              ResultHole n -> {x=n.pos.x+100,y=n.pos.y+100}
              ParamHole n _ i -> {x=n.pos.x-100+(i*100), y=n.pos.y-100}
  in
    Filling selected hole pos
