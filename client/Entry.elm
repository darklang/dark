module Entry exposing (..)

-- builtins

-- lib
import Dom
import Task
import List.Extra as LE

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

holePos : Hole -> Pos
holePos hole =
  case hole of
    ResultHole n -> {x=n.pos.x+100, y=n.pos.y+100}
    ParamHole n _ i -> {x=n.pos.x-100+(i*100), y=n.pos.y-100}


reenter : Model -> ID -> Int -> Modification
reenter m id i =
  -- TODO: Allow the input to be edited
  let n = G.getNodeExn m id
      args = G.args n
  in
    case LE.getAt i args of
      Nothing -> NoChange
      Just (p, a) ->
        let enter = Enter <| Filling n (ParamHole n p i) in
        case a of
          Edge id -> Many [ enter
                          , AutocompleteMod (Query <| "$" ++ (G.toLetter m id))]
          NoArg -> enter
          Const c -> Many [ enter
                          , AutocompleteMod (Query c)]

enterNode : Model -> Node -> EntryCursor
enterNode m selected =
  Filling selected (G.findHole m selected)

enterNext : Model -> Node -> EntryCursor
enterNext m n =
  case G.findNextHole m n of
    Nothing -> Filling n (ResultHole n)
    Just hole -> case hole of
                   ResultHole n -> Filling n hole
                   ParamHole n _ _ -> Filling n hole


-- finds the next hole in this node
enter : Model -> ID -> Bool -> Modification
enter m id exact =
  let node = G.getNodeExn m id
      cursor = if exact
               then enterNode m node
               else enterNext m node
  in
  Many [ Enter <| cursor
       , case cursor of
           Filling n (ResultHole _) ->
             AutocompleteMod <| FilterByLiveValue n.liveValue
           Filling n (ParamHole _ p _) ->
             Many [ AutocompleteMod <| FilterByParamType p.tipe
                  , AutocompleteMod <| Open False ]
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
                   || String.toLower name == "true"
                   || String.toLower name == "false"
                   || Util.rematch "^[\"\'[1-9{].*" name
                   || String.startsWith "-" name && Util.rematch "[0-9].*" name


addFunction : Name -> Pos -> List ImplicitEdge -> Modification
addFunction name pos extras =
  RPC <| AddFunctionCall name pos extras

addConstant : String -> ID -> ParamName -> Modification
addConstant name id param =
  RPC <| SetConstant name id param

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
      RPC <| SetEdge source.id (target.id, param)
    Nothing ->
      Error <| "There isn't a node named '" ++ sourceLetter ++ "' to connect to"



submit : Model -> EntryCursor -> Modification
submit m cursor =
  let value = m.complete.value in
  case cursor of
    Creating pos ->
      addNode value pos []

    Filling target hole ->
      let implicit = findImplicitEdge m target
          pos = holePos hole in
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
              else if value == "New function"
                   then RPC <| AddAnon { x=pos.x, y=pos.y+100 }
                     -- plan for implementing anonfns in the UI
                     -- - make it look good
                     --   - draw the letter on the anon
                     --   - draw arg node as a target
                     --   - draw return node as a target
                     -- - connect node to anon when "new function" chosen
                     -- - add "new function" to autocomplete
                     -- - nextNode should be the first arg or the return node
                     -- - give arg a value from the anonfn's input
                     -- - give approapriate types to autocomplete
                     -- - allow selecting a return node based on expected types
                     -- - allow selecting fn values

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
