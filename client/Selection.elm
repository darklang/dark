module Selection exposing (..)

-- builtins
import Char
import Task

-- lib
import Mouse
import List.Extra
import Keyboard
import Dom

-- dark
import Types exposing (..)
import Util exposing (deMaybe)
import Graph as G
import Defaults


updateKeyPress : Model -> Keyboard.KeyCode -> Cursor -> Modification
updateKeyPress m code cursor =
    -- (CheckEscape code, _) ->
    --   if code == Defaults.escapeKeycode
    --   then Cursor Deselected
    --   else NoChange

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


  if cursor == Deselected then
    case code
      |> Char.fromCode
      |> Char.toLower
      |> String.fromChar
      |> G.fromLetter m
      |> Maybe.map (selectNode m)
         of
           Just cursor -> Cursor cursor
           Nothing -> NoChange
  else
    NoChange

------------------
-- cursor stuff
----------------

isSelected : Model -> Node -> Bool
isSelected m n =
  case m.cursor of
    Filling node _ _ -> n == node
    _ -> False

entryVisible : Cursor -> Bool
entryVisible cursor =
  case cursor of
    Deselected -> False
    Dragging _ -> False
    _ -> True

focusEntry : Cmd Msg
focusEntry = Dom.focus Defaults.entryID |> Task.attempt FocusResult

maybeFocusEntry : Cursor -> Cursor -> Cmd Msg
maybeFocusEntry oldc c =
  if entryVisible c then
    focusEntry
  else
    Cmd.none


getCursorID : Cursor -> Maybe ID
getCursorID c =
  case c of
    Dragging id -> Just id
    Filling node _ _ -> Just node.id
    _ -> Nothing

selectNextNode : Model -> (Pos -> Pos  -> Bool) -> Cursor
selectNextNode m cond =
  -- if we're currently in a node, follow the direction. For now, pick
  -- the nearest node to it, that it's connected to, that's roughly in
  -- that direction.
  case m.cursor of
    Filling n _ _ ->
      let other =
          G.connectedNodes m n
            -- that are above us
            |> List.filter (\o -> cond n.pos o.pos)
            -- the nearest to us
            |> List.sortBy (\other -> G.distance other n)
            |> List.head
      in
        case other of
          Nothing -> m.cursor
          Just node -> selectNode m node
    _ -> m.cursor




selectNode : Model -> Node -> Cursor
selectNode m selected =
  let hole = G.findHole m selected
      pos = case hole of
              ResultHole n -> {x=n.pos.x+100,y=n.pos.y+100}
              ParamHole n _ i -> {x=n.pos.x-100+(i*100), y=n.pos.y-100}
  in
    Filling selected hole pos
