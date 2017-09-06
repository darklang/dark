module Entry exposing (..)

-- builtins

-- lib
import Defaults
import Dom
import Graph as G
import List.Extra as LE
import Task
import Types exposing (..)
import Util


nodeFromHole : Hole -> Node
nodeFromHole h = case h of
                   ResultHole n -> n
                   ParamHole n _ _ -> n

holePos : Hole -> Pos
holePos hole =
  case hole of
    ResultHole n -> {x=n.pos.x+100, y=n.pos.y+100}
    ParamHole n _ i -> {x=n.pos.x-100+(i*100), y=n.pos.y-100}

entryPos : EntryCursor -> Pos
entryPos c =
  case c of
    Creating p -> p -- this is a vpos
    Filling _ h -> holePos h

entryNodePos : EntryCursor -> Pos
entryNodePos c =
  case c of
    Creating p -> p -- todo this is a vpos
    Filling n h -> n.pos


toViewport : Model -> Pos -> VPos
toViewport m pos =
  let d = Defaults.defaultModel {} |> .center in
  { vx = d.x + pos.x - m.center.x, vy = d.y + pos.y - m.center.y}

toAbsolute : Model -> VPos -> Pos
toAbsolute m pos =
  let d = Defaults.defaultModel {} |> .center in
  { x = pos.vx + m.center.x - d.x, y = pos.vy + m.center.y - d.x}


---------------------
-- Layout and events
---------------------
findImplicitEdge : Model -> Node -> RPC
findImplicitEdge m node = case G.findHole m node of
                     ResultHole n -> SetEdgeImplicitTarget n.id
                     ParamHole n p _ -> SetEdgeImplicitSource (n.id, p.name)


---------------------
-- Nodes
---------------------
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
          Edge eid -> Many [ enter
                          , AutocompleteMod (Query <| "$" ++ (G.toLetter m eid))]
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
    Just hole -> Filling (nodeFromHole hole) hole

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
           Filling _ (ParamHole _ p _) ->
             Many [ AutocompleteMod <| FilterByParamType p.tipe
                  , AutocompleteMod <| Open False ]
           Creating _ ->
             NoChange
       ]

updateValue : String -> Modification
updateValue target =
  AutocompleteMod <| Query target

createFindSpace : Model -> Modification
createFindSpace m = Enter <| Creating (toAbsolute m Defaults.initialPos)
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


addFunction : Name -> Pos -> List RPC -> Modification
addFunction name pos extras =
  RPC <| (AddFunctionCall name pos) :: extras

addAnon : Pos -> List RPC -> Modification
addAnon pos extras =
  RPC <| (AddAnon pos) :: extras

addConstant : String -> ID -> ParamName -> Modification
addConstant name id param =
  RPC <| [SetConstant name (id, param)]

addValue : String -> Pos -> List RPC -> Modification
addValue name pos extras =
    -- [(ReceivingEdge _)] -> RPC <| AddValue name pos [] -- TODO: why was this here
  RPC <| (AddValue name pos) :: extras

addNode : Name -> Pos -> List RPC -> Modification
addNode name pos extras =
  if isValueRepr name
  then addValue name pos extras
  else addFunction name pos extras

addVarSource : Model -> String -> Node -> ParamName -> Modification
addVarSource m sourceLetter target param =
  case G.fromLetter m sourceLetter of
    Just source ->
      RPC <| [SetEdge source.id (target.id, param)]
    Nothing ->
      Error <| "There isn't a node named '" ++ sourceLetter ++ "' to connect to"

addVarTarget : Model -> Node -> String -> Modification
addVarTarget m source targetLetter =
  case G.fromLetter m targetLetter of
    Just target ->
      -- TODO: get the type of the target and pick the right hole for it
      -- Pick the first free argument
      let free = LE.find (\(_,a) -> a == NoArg) (G.args target) in
      case free of
        Nothing -> Error <| "There are no free arguments"
        Just (param, _) ->
          RPC <| [SetEdge source.id (target.id, param.name)]
    Nothing ->
      Error <| "There isn't a node named '" ++ targetLetter ++ "' to connect to"



submit : Model -> EntryCursor -> Modification
submit m cursor =
  let value = m.complete.value in
  case cursor of
    Creating pos ->
      addNode value pos []

    Filling node hole ->
      let implicit = findImplicitEdge m node
          pos = holePos hole in
      case hole of
        ParamHole _ param _ ->
          case String.uncons value of
            Nothing ->
              if param.optional
              then addConstant "null" node.id param.name
              else NoChange

            Just ('$', rest) ->
              addVarSource m rest node param.name

            _ ->
              if isValueRepr value
              then addConstant value node.id param.name
              else if value == "New function"
                   then addAnon pos [implicit]
                     -- plan for implementing anonfns in the UI
                     -- - if you try to use the result of an anon,
                     -- actually get the string foreach result
                     -- - add "new function" to autocomplete
                     -- - nextNode should be the first arg or the return node
                     -- - give arg a value from the anonfn's input
                     -- - add an edge from foreach to the arg
                     -- - allow autocomplete on arg
                     -- - allow selecting a return node based on expected types
                     -- - allow selecting matching fns in autocomplete for Functions

              else addNode value pos [implicit]

        ResultHole _ ->
          case String.uncons value of
            Nothing -> NoChange

            Just ('.', fieldname) ->
              -- TODO: this should be an opcode
              let constant = SetConstantImplicit
                             ("\"" ++ fieldname ++ "\"")
                             "fieldname" in
              addNode "." pos [implicit, constant]

            Just ('$', rest) ->
              addVarTarget m node rest

            _ ->
              if isValueRepr value
              then addValue value pos []
              else addNode value pos [implicit]
