module Entry exposing (..)

-- builtins
import Char
import Keyboard

-- lib
import Keyboard.Event exposing (KeyboardEvent, decodeKeyboardEvent)
import Keyboard.Key as Key

-- dark
import RPC exposing (rpc)
import Types exposing (..)
import Util exposing (deMaybe)
import Canvas
import Graph as G


-- We use this because a) it has other modifiers and b) the timing with
-- which it fires means we get the right value in the entryValue, so we
-- know when to delete a node (if we rely on globalKeyPress, it deletes
-- one character too early, though this is maybe fixable.

updateEntryKeyPress : Model -> KeyboardEvent -> Cursor -> Modification
updateEntryKeyPress m kb cursor =
   case (kb.keyCode, cursor, m.entryValue) of
     -- backspace through an empty node
     (Key.Backspace, Filling n _, "") ->
       NewRPC <| DeleteNode n.id

     (Key.Up, _, "") ->
       NewCursor <| Canvas.selectNextNode m (\n o -> n.y > o.y)

     (Key.Down, _, "") ->
       NewCursor <| Canvas.selectNextNode m (\n o -> n.y < o.y)

     (Key.Left, _, "") ->
       NewCursor <| Canvas.selectNextNode m (\n o -> n.x > o.x)

     (Key.Right, _, "") ->
       NewCursor <| Canvas.selectNextNode m (\n o -> n.x < o.x)

     (key, cursor, _) ->
       let _ = Debug.log "[Entry] Nothing to do" (key, cursor, m.entryValue) in
       NoChange

-- This fires when we're not in the input box
updateGlobalKeyPress : Model -> Keyboard.KeyCode -> Cursor -> Modification
updateGlobalKeyPress m code cursor =
  if cursor == Deselected then
    case code
      |> Char.fromCode
      |> Char.toLower
      |> String.fromChar
      |> G.fromLetter m
      |> Maybe.map (Canvas.selectNode m)
         of
           Just cursor -> NewCursor cursor
           Nothing -> NoChange
  else
    NoChange


addNode : Name -> Pos -> List ImplicitEdge -> Modification
addNode name pos extras =
  if Util.rematch "^[\"\'[1-9{].*" name then
    case extras of
      [(ReceivingEdge _)] -> NewRPC <| AddValue name pos []
      _ -> NewRPC <| AddValue name pos extras
  else
    NewRPC <| AddFunctionCall name pos extras

findImplicitEdge : Model -> Node -> ImplicitEdge
findImplicitEdge m node = case G.findHole m node of
                     ResultHole n -> ReceivingEdge n.id
                     ParamHole n p _ -> ParamEdge n.id p

addVar : Model -> String -> Modification
addVar m name =
  case G.fromLetter m name of
    Just source ->
      case findImplicitEdge m source of
        ParamEdge tid p -> NewRPC <| AddEdge source.id (tid, p)
        _ -> Error "There isn't parameter we're looking to fill here"
    Nothing ->
        Error <| "There isn't a node named '" ++ name ++ "' to connect to"
