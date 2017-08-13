module Entry exposing (..)

-- builtins

-- lib
import Dom
import Task

-- dark
import Types exposing (..)
import Util exposing (deMaybe)
import Graph as G
import Defaults



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

addValue : String -> Pos -> List ImplicitEdge -> Modification
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
-- Where to put the entry
---------------------
findImplicitEdge : Model -> Node -> ImplicitEdge
findImplicitEdge m node = case G.findHole m node of
                     ResultHole n -> ReceivingEdge n.id
                     ParamHole n p _ -> ParamEdge n.id p

enterNode : Model -> Node -> EntryCursor
enterNode m selected =
  let hole = G.findHole m selected
      pos = case hole of
              ResultHole n -> {x=n.pos.x+100,y=n.pos.y+100}
              ParamHole n _ i -> {x=n.pos.x-100+(i*100), y=n.pos.y-100}
  in
    Filling selected hole pos

enter : Model -> ID -> Modification
enter m id =
  Enter <| enterNode m (G.getNodeExn m id)



---------------------
-- Dealing with events
---------------------

focusEntry : Cmd Msg
focusEntry = Dom.focus Defaults.entryID |> Task.attempt FocusResult


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
