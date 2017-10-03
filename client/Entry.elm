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

holeCreatePos : Model -> Hole -> Pos
holeCreatePos m hole =
  case hole of
    ParamHole n _ i -> {x=n.pos.x-50+(i*50), y=n.pos.y-40}
    ResultHole n ->
      let connected = G.entireSubgraph m n
          lowest = connected
                   |> List.map (\n -> n.pos.y)
                   |> List.maximum
                   |> Maybe.withDefault n.pos.y
      in
      {x=n.pos.x, y=lowest+40}


entryNodePos : EntryCursor -> Pos
entryNodePos c =
  case c of
    Creating p -> p -- todo this is a vpos
    Filling n _ -> n.pos


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
        let enter = Enter True <| Filling n (ParamHole n p i) in
        case a of
          Edge eid -> Many [ enter
                          , AutocompleteMod (Query <| "$" ++ G.toLetter m eid)]
          NoArg -> enter
          Const c -> Many [ enter
                          , AutocompleteMod (Query c)]

-- Enter this exact node
enterExact : Model -> Node -> Modification
enterExact _ selected =
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
  Many [ Enter False cursor
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
  Many [ AutocompleteMod <| Query target, Phantom ]

createFindSpace : Model -> Modification
createFindSpace m = Enter False <| Creating (Viewport.toAbsolute m Defaults.initialPos)
---------------------
-- Focus
---------------------

focusEntry : Cmd Msg
focusEntry = Dom.focus Defaults.entryID |> Task.attempt FocusEntry


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
                   || String.startsWith "-" name && Util.rematch "-[0-9.].+" name

addAnonParam : Model -> ID -> MPos -> ParamName -> List String -> (List RPC, Focus)
addAnonParam _ id pos name anon_args =
  let sid = gen_id ()
      argids = List.map (\_ -> gen_id ()) anon_args
      anon = AddAnon sid pos argids anon_args
      edge = SetEdge sid (id, name)
  in
    ([anon, edge], FocusNext id)


addFunction : Model -> ID -> Name -> MPos -> (List RPC, Focus)
addFunction m id name pos =
  let fn = Autocomplete.findFunction m.complete name in
  case fn of
    -- not a real function, but hard to thread an error here, so let the
    -- server fail instead
    Nothing ->
      ([AddFunctionCall id name pos], FocusNothing)
    Just fn ->
      -- automatically add anonymous functions
      let fn_args = List.filter (\p -> p.tipe == "Function") fn.parameters
          anonpairs = List.map (\p -> addAnonParam m id pos p.name p.anon_args) fn_args
          anonarg = anonpairs |> List.head |> Maybe.map Tuple.second
          anons = anonpairs |> List.unzip |> Tuple.first
          focus = case anonarg of
            Just f -> f
            Nothing -> FocusNext id
      in
        (AddFunctionCall id name pos :: List.concat anons, focus)

updatePreviewCursor : Model -> ID -> Int -> List RPC
updatePreviewCursor m id step =
  let baseNode = G.getNodeExn m id in
  let anonFuncNode = Debug.log "parent anon" (G.findParentAnon m baseNode) in
  case anonFuncNode of
    Just n -> [UpdateNodeCursor n.id (n.cursor + step)]
    Nothing -> []


refocus : Bool -> Focus -> Focus
refocus re default =
  case default of
    FocusNext id -> if re then FocusExact id else FocusNext id
    f -> f

submit : Model -> Bool -> EntryCursor -> String -> Modification
submit m re cursor value =
  let id = gen_id () in
  case cursor of
    Creating pos ->
      RPC <| if isValueRepr value
             then ([AddValue id value (Just pos)], FocusNext id)
             else addFunction m id value (Just pos)

    Filling n (ParamHole target param _ as hole) ->
      case String.uncons value of
        Nothing ->
          if param.optional
          then RPC ([SetConstant "null" (target.id, param.name)]
                  , FocusNext target.id |> refocus re)
          else NoChange

        Just ('$', letter) ->
          case G.fromLetter m letter of
            Just source ->
              RPC ([ SetEdge source.id (target.id, param.name)]
                   , FocusNext target.id |> refocus re)
            Nothing -> Error <| "No node named '" ++ letter ++ "'"

        _ ->
          if isValueRepr value
          then RPC ([ SetConstant value (target.id, param.name)]
                    , FocusNext target.id |> refocus re)
          else
            let (f, focus) = addFunction m id value Nothing in
              RPC (f ++ [SetEdge id (target.id, param.name)]
                  , focus |> refocus re)

    Filling n (ResultHole source as hole) ->
      case String.uncons value of
        Nothing -> NoChange

        -- TODO: this should be an opcode
        Just ('.', fieldname) ->
          RPC ([ AddFunctionCall id "." Nothing
               , SetEdge source.id (id, "value")
               , SetConstant ("\"" ++ fieldname ++ "\"") (id, "fieldname")]
              , FocusNothing)

        Just ('$', letter) ->
          case G.fromLetter m letter of
            Nothing ->
              Error <| "No node named '" ++ letter ++ "'"
            Just target ->
              -- TODO: use type
              case G.findParam target of
                Nothing -> Error "There are no argument slots available"
                Just (_, (param, _)) ->
                  RPC ([ SetEdge source.id (target.id, param.name)]
                       , FocusExact target.id)

        _ ->
          -- this is new functions only
          -- lets allow 1 thing, integer only, for now
          -- so we find the first parameter that isnt that parameter

          let (name, arg, extras) = case String.split " " value of
                              (name :: arg :: es) -> (name, Just arg, es)
                              [name] -> (name, Nothing, [])
                              [] -> ("", Nothing, [])
              (f, focus) = if isValueRepr name
                           then ([ AddFunctionCall id "_" Nothing
                                 , SetConstant name (id, "value")
                                 , SetEdge source.id (id, "ignore")
                                 ]
                                , FocusNext id)
                           else addFunction m id name Nothing
          in
          case Autocomplete.findFunction m.complete name of
            Nothing ->
              -- Unexpected, let the server reply with an error
              RPC (f, focus)
            Just fn ->
              -- tipedP: parameter to connect the previous node to
              -- arg: the 2nd word in the autocmplete box
              -- otherP: first argument that isn't tipedP
              let tipedP = Autocomplete.findParamByType fn n.liveValue.tipe
                  otherP = Autocomplete.findFirstParam fn tipedP

                  tipedEdges = case tipedP of
                    Nothing -> []
                    Just p -> [SetEdge source.id (id, p.name)]

                  argEdges = case (arg, otherP) of
                    (Nothing, _) -> Ok []
                    (Just arg, Nothing) ->
                        Err <| "No parameter exists for arg: " ++ arg
                    (Just arg, Just p) ->
                      if isValueRepr arg
                      then Ok <| [SetConstant arg (id, p.name)]
                      else
                        case String.uncons arg of
                          Just ('$', letter) ->
                            case G.fromLetter m letter of
                              Just lNode ->
                                Ok <| [SetEdge lNode.id (id, p.name)]
                              Nothing -> Err <| "No node named '"
                                             ++ letter ++ "'"
                          Just _ ->
                            Err <| "We don't currently support arguments like `" ++ arg ++ "`"
                          Nothing -> Ok [] -- empty string
              in
              if extras /= []
              then Error <| "Too many arguments: `" ++ String.join " " extras ++ "`"
              else
                case argEdges of
                  Ok edges -> RPC (f ++ tipedEdges ++ edges, focus)
                  Err err -> Error err


