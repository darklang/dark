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


---------------------
-- Layout and events
---------------------
findImplicitEdge : Model -> Node -> ImplicitEdge
findImplicitEdge m node = case G.findHole m node of
                     ResultHole n -> ReceivingEdge n.id
                     ParamHole n p _ -> ParamEdge n.id p.name

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
  let node = (G.getNodeExn m id)
      cursor = enterNode m node
  in
  Many [ Enter <| cursor
       , case cursor of
           Filling n (ResultHole _) _ ->
             AutocompleteMod <| FilterByLiveValue n.liveValue
           Filling n (ParamHole _ p _) _ ->
             AutocompleteMod <| FilterByParamType p.tipe
           Creating _ ->
             NoChange
       ]

updateValue : String -> Modification
updateValue target =
  AutocompleteMod <| Query target

createInitial : Modification
createInitial = Enter <| Creating Defaults.initialPos

createFindSpace : Model -> Modification
createFindSpace m = createInitial
---------------------
-- Focus
---------------------

focusEntry : Cmd Msg
focusEntry = Dom.focus Defaults.entryID |> Task.attempt FocusResult


---------------------
-- Submitting the entry form to the server
---------------------

isValueRepr : String -> Bool
isValueRepr name = String.toLower name == "null"
                   || Util.rematch "^[\"\'[1-9{].*" name

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

addVar : Model -> String -> Node -> ParamName -> Modification
addVar m sourceLetter target param =
  case G.fromLetter m sourceLetter of
    Just source ->
      RPC <| AddEdge source.id (target.id, param)
    Nothing ->
      Error <| "There isn't a node named '" ++ sourceLetter ++ "' to connect to"



submit : Model -> EntryCursor -> Modification
submit m cursor =
  let value = m.complete.value in
  case cursor of
    Creating pos ->
      addNode value pos []

    Filling target hole pos ->
      let implicit = findImplicitEdge m target in
      case hole of
        ParamHole _ param _ ->
          case String.uncons value of
            Nothing ->
              if param.optional
              then addConstant "null" target.id param.name
              else NoChange

            Just ('$', rest) ->
              addVar m rest target param.name

            _ ->
              if isValueRepr value
              then addConstant value target.id param.name
              else addNode value pos [implicit]

        ResultHole _ ->
          case String.uncons value of
            Nothing -> NoChange

            Just ('.', fieldname) ->
              let constant = Constant ("\"" ++ fieldname ++ "\"") "fieldname" in
              addNode "." pos [implicit, constant]

            _ ->
              if isValueRepr value
              then addValue value pos []
              else addNode value pos [implicit]
