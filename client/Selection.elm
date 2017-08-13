module Selection exposing (..)

-- builtins
import Char

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


updateKeyPress : Model -> KeyboardEvent -> State -> Modification
updateKeyPress m kb state =
  case (kb.keyCode, state) of
    (Key.Escape, Selecting id) ->
      Deselect

    (Key.Backspace, Selecting id) ->
      RPC <| DeleteNode id

    (Key.Up, Selecting id) ->
      selectNextNode m id (\n o -> n.y > o.y)

    (Key.Down, Selecting id) ->
      selectNextNode m id (\n o -> n.y < o.y)

    (Key.Left, Selecting id) ->
      selectNextNode m id (\n o -> n.x > o.x)

    (Key.Right, Selecting id) ->
      selectNextNode m id (\n o -> n.x < o.x)

    (_, _) ->
      kb.keyCode
        |> Key.toChar
        |> Maybe.map Char.toLower
        |> Maybe.map String.fromChar
        |> Maybe.andThen (G.fromLetter m)
        |> Maybe.map (\n -> Select n.id)
        |> Maybe.withDefault NoChange

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

getCursorID : State -> Maybe ID
getCursorID s =
  case s of
    Dragging (DragNode id _) -> Just id
    Entering (Filling node _ _) -> Just node.id
    _ -> Nothing

selectNextNode : Model -> ID -> (Pos -> Pos -> Bool) -> Modification
selectNextNode m id cond =
  -- if we're currently in a node, follow the direction. For now, pick
  -- the nearest node to it, that it's connected to, that's roughly in
  -- that direction.
  let n = G.getNodeExn m id
  in
    n
    |> G.connectedNodes m
    -- that are above us
    |> List.filter (\o -> cond n.pos o.pos)
    -- the nearest to us
    |> List.sortBy (\other -> G.distance other n)
    |> List.head
    |> Maybe.map (\n -> Select n.id)
    |> Maybe.withDefault NoChange


