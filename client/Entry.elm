module Entry exposing (..)

-- builtins
import Task

-- lib
import Dom
import List.Extra as LE

-- dark
import Defaults
import Graph as G
import Types exposing (..)
import Util
import Autocomplete


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
-- Nodes
---------------------
gen_id : () -> ID
gen_id _ = ID (Util.random ())

id_of : RPC -> ID
id_of rpc =
  case rpc of
    AddDatastore id _ _ -> id
    AddDatastoreField id _ _ -> id
    AddFunctionCall id _ _ -> id
    AddAnon id _ -> id
    AddValue id _ _ -> id
    _ -> Debug.crash <| "getting id from unsupported: " ++ (toString rpc)

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
  Filling selected (G.findHole selected)

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


addFunction : Model -> Name -> Pos -> RPC
addFunction m name pos =
  -- let fn = LE.find (\f -> f.name == name) m.complete.functions in
  -- case fn of
  --   Just fn ->
  --     let fn_args = List.filter (\p -> p.tipe == "Function") fn.parameters in
  --     let new_extra = List.map (\p -> [AddAnon pos, SetEdgeImplicit) fn_args


  --     RPC <| (AddFunctionCall name pos) :: extras
  --   Nothing -> Error <- "No function named " + name

  if name == "New function"
  then addAnon pos
  else AddFunctionCall (gen_id ()) name pos

addAnon : Pos -> RPC
addAnon pos =
  AddAnon (gen_id ()) pos

addValue : String -> Pos -> RPC
addValue name pos =
  AddValue (gen_id ()) name pos

addByName : Model -> Name -> Pos -> RPC
addByName m name pos =
  if isValueRepr name
  then addValue name pos
  else addFunction m name pos

nodeFromLetter : Model -> String -> Result String Node
nodeFromLetter m letter =
  case G.fromLetter m letter of
    Just node -> Ok node
    Nothing ->
      Err <| "There isn't a node named '" ++ letter ++ "' to connect to"

-- TODO: get the type of the target and pick the right hole for it
findEdge : Model -> ID -> ID -> Result String RPC
findEdge m source target =
  case G.findParam (G.getNodeExn m target) of
    Nothing -> Err <| "There are no free arguments"
    Just (_, (param, _)) ->
      Ok <| SetEdge source (target, param.name)

result2RPC : Result String RPC -> Modification
result2RPC r =
  case r of
    Err e -> Error e
    Ok rpc -> RPC [rpc]


submit : Model -> EntryCursor -> Modification
submit m cursor =
  let value = m.complete.value in
  case cursor of
    Creating pos ->
      RPC [addByName m value pos]

    Filling _ hole ->
      let pos = holePos hole in
      case hole of
        ParamHole target param _ ->
          case String.uncons value of
            Nothing ->
              if param.optional
              then RPC [SetConstant "" (target.id, param.name)]
              else NoChange

            Just ('$', letter) ->
              case nodeFromLetter m letter of
                Err e -> Error e
                Ok source -> RPC [SetEdge source.id (target.id, param.name)]

            _ ->
              if isValueRepr value
              then RPC [SetConstant value (target.id, param.name)]
              else
                let f = addFunction m value pos
                    edge = SetEdge (id_of f) (target.id, param.name)
                in RPC [f, edge]

        ResultHole source ->
          case String.uncons value of
            Nothing -> NoChange

            -- TODO: this should be an opcode
            Just ('.', fieldname) ->
              let f = addFunction m "." pos
                  value = SetEdge source.id (id_of f, "value")
                  field = SetConstant
                          ("\"" ++ fieldname ++ "\"")
                          (id_of f, "fieldname")
              in RPC [f, value, field]

            Just ('$', letter) ->
              let edgeR = letter
                       |> nodeFromLetter m
                       |> Result.andThen (\t -> findEdge m source.id t.id)
              in case edgeR of
                Err e -> Error e
                Ok edge -> RPC [edge]

            _ ->
              let f = addByName m value pos in
              case Autocomplete.findFunction (m.complete) value of
                Nothing -> Error <| "Function " ++ value ++ " does not exist"
                Just {parameters} ->
                  case parameters of
                    (p :: _) -> RPC [f, SetEdge source.id (id_of f, p.name)]
                    [] -> RPC [f]
