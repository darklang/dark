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
import Autocomplete
import Canvas
import Graph as G


-- We use this because a) it has other modifiers and b) the timing with
-- which it fires means we get the right value in the entryValue, so we
-- know when to delete a node (if we rely on globalKeyPress, it deletes
-- one character too early, though this is maybe fixable.

updateEntryKeyPress : Model -> KeyboardEvent -> Cursor -> Modification
updateEntryKeyPress m kb cursor =
  case (kb.keyCode, cursor, m.complete.value) of
     -- backspace through an empty node
     (Key.Backspace, Filling n _ _, "") ->
       RPC <| DeleteNode n.id

     (Key.Up, _, "") ->
       Cursor <| Canvas.selectNextNode m (\n o -> n.y > o.y)

     (Key.Down, _, "") ->
       Cursor <| Canvas.selectNextNode m (\n o -> n.y < o.y)

     (Key.Left, _, "") ->
       Cursor <| Canvas.selectNextNode m (\n o -> n.x > o.x)

     (Key.Right, _, "") ->
       Cursor <| Canvas.selectNextNode m (\n o -> n.x < o.x)

     (Key.Up, _, _) ->
       AutocompleteMod SelectUp

     (Key.Down, _, _) ->
       AutocompleteMod SelectDown

     (Key.Right, _, _) ->
       let sp = Autocomplete.sharedPrefix m.complete.current in
       if sp == "" then NoChange
       else Many [ AutocompleteMod <| SetEntry sp ]

     (Key.Enter, _, _) ->
       let sp = Autocomplete.sharedPrefix m.complete.current in
       if sp == "" then NoChange
       else Many [ AutocompleteMod <| SetEntry sp ]

     (key, cursor, val) ->
       AutocompleteMod <| SetEntry val

updateEntryValue : String -> Modification
updateEntryValue target =
  AutocompleteMod <| SetEntry target

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
           Just cursor -> Cursor cursor
           Nothing -> NoChange
  else
    NoChange

isValueRepr : String -> Bool
isValueRepr name = Util.rematch "^[\"\'[1-9{].*" name

addFunction : Name -> Pos -> List ImplicitEdge -> Modification
addFunction name pos extras =
  RPC <| AddFunctionCall name pos extras

addConstant : String -> ID -> ParamName -> Modification
addConstant name id param =
  RPC <| AddConstant name id param

addValue name pos extras =
  case extras of
    [(ReceivingEdge _)] -> RPC <| AddValue name pos []
    _ -> RPC <| AddValue name pos extras

addNode : Name -> Pos -> List ImplicitEdge -> Modification
addNode name pos extras =
  if isValueRepr name then
    addValue name pos extras
  else
    addFunction name pos extras

findImplicitEdge : Model -> Node -> ImplicitEdge
findImplicitEdge m node = case G.findHole m node of
                     ResultHole n -> ReceivingEdge n.id
                     ParamHole n p _ -> ParamEdge n.id p

addVar : Model -> String -> Modification
addVar m name =
  case G.fromLetter m name of
    Just source ->
      case findImplicitEdge m source of
        ParamEdge tid p -> RPC <| AddEdge source.id (tid, p)
        _ -> Error "There isn't parameter we're looking to fill here"
    Nothing ->
        Error <| "There isn't a node named '" ++ name ++ "' to connect to"
