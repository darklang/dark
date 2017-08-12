module Entry exposing (..)

-- builtins
import Char
import Keyboard

-- lib
import Keyboard.Event exposing (KeyboardEvent, decodeKeyboardEvent)
import Keyboard.Key as Key
import Dom

-- dark
import RPC exposing (rpc)
import Types exposing (..)
import Util exposing (deMaybe)
import Autocomplete
import Canvas
import Graph as G
import Selection



updateKeyPress : Model -> KeyboardEvent -> Modification
updateKeyPress m kb =
  case (kb.keyCode, m.complete.value) of
     (Key.Up, _) ->
       AutocompleteMod SelectUp

     (Key.Down, _) ->
       AutocompleteMod SelectDown

     (Key.Right, _) ->
       let sp = Autocomplete.sharedPrefix m.complete.current in
       if sp == "" then NoChange
       else Many [ AutocompleteMod <| SetEntry sp ]

     (Key.Enter, _) ->
       case Autocomplete.highlighted m.complete of
         Just s -> AutocompleteMod <| SetEntry s
         Nothing -> NoChange

     (Key.Escape, _) ->
       case Selection.getCursorID m.state of
         Just id -> Select id
         Nothing -> Deselect

     (key, val) ->
       AutocompleteMod <| SetEntry val


updateValue : String -> Modification
updateValue target =
  AutocompleteMod <| SetEntry target


---------------------
-- Calling the server
---------------------
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

---------------------
-- Dealing with events
---------------------
submit : Model -> EntryCursor -> Modification
submit m cursor =
  case cursor of
    Filling node hole pos ->
      submitFilling m node hole pos

    Creating pos ->
      addNode m.complete.value pos []


submitFilling : Model -> Node -> Hole -> Pos -> Modification
submitFilling m node hole pos =
  case String.uncons m.complete.value of

    Nothing -> NoChange

    -- var lookup
    Just ('$', rest) -> addVar m rest


    -- field access
    Just ('.', fieldname) ->
      let constant = Constant ("\"" ++ fieldname ++ "\"") "fieldname"
          implicit = findImplicitEdge m node
      in
        addNode "." pos [implicit, constant]


    -- functions or constants
    _ ->
      if isValueRepr m.complete.value then
        case hole of
          ParamHole n p i -> addConstant m.complete.value node.id p
          ResultHole _ -> addValue m.complete.value pos []
      else
        let implicit = findImplicitEdge m node in
        addNode m.complete.value pos [implicit]
