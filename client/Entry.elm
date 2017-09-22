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
import Viewport


nodeFromHole : Hole -> Node
nodeFromHole h = case h of
                   ResultHole n -> n
                   ParamHole n _ _ -> n

holePos : Model -> Hole -> Pos
holePos m hole =
  case hole of
    ResultHole n ->
      let pos = case G.getAnonNodeOf m n.id |> Maybe.andThen .returnID of
                  Just rid -> rid |> G.getNodeExn m |> .pos
                  Nothing -> n.pos
      in
        {x=pos.x, y=pos.y+40}
    ParamHole n _ i -> {x=n.pos.x-50+(i*50), y=n.pos.y-100}

entryPos : Model -> EntryCursor -> Pos
entryPos m c =
  case c of
    Creating p -> p -- this is a vpos
    Filling _ h -> holePos m h

entryNodePos : EntryCursor -> Pos
entryNodePos c =
  case c of
    Creating p -> p -- todo this is a vpos
    Filling n h -> n.pos


---------------------
-- Nodes
---------------------
-- Reenter an existing node to edit the existing inputs
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

-- Enter this exact node
enterExact : Model -> Node -> Modification
enterExact m selected =
  Filling selected (G.findHole selected)
  |> cursor2mod

-- Enter the next needed node, searching from here
enterNext : Model -> Node -> Modification
enterNext m n =
  cursor2mod <|
    case G.findNextHole m n of
      Nothing -> Filling n (ResultHole n)
      Just hole -> Filling (nodeFromHole hole) hole

cursor2mod : EntryCursor -> Modification
cursor2mod cursor =
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
createFindSpace m = Enter <| Creating (Viewport.toAbsolute m Defaults.initialPos)
---------------------
-- Focus
---------------------

focusEntry : Cmd Msg
focusEntry = Dom.focus Defaults.entryID |> Task.attempt FocusResult


---------------------
-- Submitting the entry form to the server
---------------------
gen_id : () -> ID
gen_id _ = ID (Util.random ())

isValueRepr : String -> Bool
isValueRepr name = String.toLower name == "null"
                   || String.toLower name == "true"
                   || String.toLower name == "false"
                   || Util.rematch "^[\"\'[01-9{].*" name
                   || String.startsWith "-" name && Util.rematch "[0-9].*" name

addAnonParam : Model -> ID -> Pos -> ParamName -> Int -> (List RPC, Maybe ID)
addAnonParam m id pos name arity =
  let sid = gen_id ()
      retid = gen_id ()
      argids = () |> List.repeat arity |> List.map gen_id
      anon = AddAnon sid pos retid argids
      edge = SetEdge sid (id, name)
  in
    ([anon, edge], List.head argids)


addFunction : Model -> ID -> Name -> Pos -> (List RPC, Maybe ID)
addFunction m id name pos =
  let fn = Autocomplete.findFunction m.complete name in
  case fn of
    -- not a real function, but had to thread an error here
    Nothing ->
      if String.toLower name == "new function"
      then ([AddAnon id pos (gen_id ()) [(gen_id ())]], Just id)
      else ([], Nothing)
    Just fn ->
      -- automatically add anonymous functions
      let fn_args = List.filter (\p -> p.tipe == "Function") fn.parameters
          anonpairs = List.map (\p -> addAnonParam m id pos p.name p.arity) fn_args
          anonarg = anonpairs |> List.head |> Maybe.andThen Tuple.second
          anons = anonpairs |> List.unzip |> Tuple.first
          cursor = if anonarg == Nothing then Just id else anonarg
      in
        (AddFunctionCall id name pos :: List.concat anons, cursor)

addByName : Model -> ID -> Name -> Pos -> (List RPC, Maybe ID)
addByName m id name pos =
  if isValueRepr name
  then ([AddValue id name pos], Just id)
  else addFunction m id name pos
    -- anon, function, or value


submit : Model -> EntryCursor -> String -> Modification
submit m cursor value =
  let id = gen_id () in
  case cursor of
    Creating pos ->
      RPC <| addByName m id value pos

    Filling n hole ->
      let pos = holePos m hole in
      case hole of
        ParamHole target param _ ->
          case String.uncons value of
            Nothing ->
              if param.optional
              then RPC ([SetConstant "null" (target.id, param.name)]
                      , Just target.id)
              else NoChange

            Just ('$', letter) ->
              case G.fromLetter m letter of
                Just source -> RPC ([SetEdge source.id (target.id, param.name)], Just target.id)
                Nothing -> Error <| "No node named '" ++ letter ++ "'"

            _ ->
              if isValueRepr value
              then RPC ([SetConstant value (target.id, param.name)], Just target.id)
              else let (f, focus) = addFunction m id value pos in
                  RPC (f ++ [SetEdge id (target.id, param.name)], focus)

        ResultHole source ->
          case String.uncons value of
            Nothing -> NoChange

            -- TODO: this should be an opcode
            Just ('.', fieldname) ->
              let (f, focus) = addFunction m id "." pos in
                  RPC (f
                       ++ [ SetEdge source.id (id, "value")
                          , SetConstant ("\"" ++ fieldname ++ "\"") (id, "fieldname")]
                      , focus)

            Just ('$', letter) ->
              case G.fromLetter m letter of
                Nothing ->
                  Error <| "No node named '" ++ letter ++ "'"
                Just target ->
                  case G.findParam target of
                    Nothing -> Error "There are no argument slots available"
                    Just (_, (param, _)) ->
                      RPC ([SetEdge source.id (target.id, param.name)], Just target.id)

            _ ->
              let (f, focus) = addByName m id value pos in
              case Autocomplete.findFunction (m.complete) value of
                Nothing ->
                  -- Not a normal function, just return
                  RPC (f, focus)
                Just fn ->
                  let (_, tipe, _) = n.liveValue in
                  case Autocomplete.findParamByType fn tipe of
                    Just p -> RPC ( f ++ [SetEdge source.id (id, p.name)]
                                   , focus)
                    Nothing -> RPC (f, focus)
