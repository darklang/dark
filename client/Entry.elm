module Entry exposing (..)

-- builtins
import Task
import Result exposing (Result)

-- lib
import Dom
import List.Extra as LE
import Result.Extra as RE
import Maybe.Extra as ME

-- dark
import Defaults
import Graph as G exposing (posy, posx)
import Types exposing (..)
import Autocomplete
import Viewport
import EntryParser exposing (AST(..), ACreating(..), AExpr(..), AFillParam(..), AFillResult(..))


nodeFromHole : Hole -> Node
nodeFromHole h = case h of
                   ResultHole n -> n
                   ParamHole n _ _ -> n

holeCreatePos : Model -> Hole -> Pos
holeCreatePos m hole =
  case hole of
    ParamHole n _ i -> {x=(posx n)-50+(i*50), y=(posy n)-40}
    ResultHole n ->
      let connected = G.entireSubgraph m n
          lowest = connected
                   |> List.map (\n -> posy n)
                   |> List.maximum
                   |> Maybe.withDefault (posy n)
      in
      {x=posx n, y=lowest+40}


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
                          , AutocompleteMod (ACQuery <| "$" ++ G.toLetter m eid)]
          NoArg -> enter
          Const c -> Many [ enter
                          , AutocompleteMod (ACQuery c)]

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
             AutocompleteMod <| ACFilterByLiveValue n.liveValue
           Filling _ (ParamHole _ p _) ->
             Many [ AutocompleteMod <| ACFilterByParamType p.tipe
                  , AutocompleteMod <| ACOpen False ]
           Creating _ ->
             NoChange
       ]



updateValue : String -> Modification
updateValue target =
  Many [ AutocompleteMod <| ACQuery target, Phantom ]

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
addAnonParam : Model -> ID -> MPos -> ParamName -> List String -> (List RPC, Focus)
addAnonParam _ id pos name anon_args =
  let sid = G.gen_id ()
      argids = List.map (\_ -> G.gen_id ()) anon_args
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
      ([AddFunctionCall id name pos], FocusSame)
    Just fn ->
      -- automatically add anonymous functions
      let fn_args = List.filter (\p -> p.tipe == TFun) fn.parameters
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


----------------------
-- execute the AST
----------------------
createArg : Model -> Function -> ID -> (AExpr, Parameter) -> Result String (List RPC)
createArg m fn id (arg, param) = 
  case arg of
   AVar n -> Ok [SetEdge n.id (id, param.name)]
   AValue v -> Ok [SetConstant v (id, param.name)]
   AFnCall name args -> 
     let fnid = G.gen_id ()
     in createFn m fnid name args (Dependent Nothing) Nothing 
        |> Result.map (\rpcs -> rpcs ++ [SetEdge fnid (id, param.name)])

createFn : Model -> ID -> String -> List AExpr -> MPos -> Maybe Node -> Result String (List RPC)
createFn m id name args mpos implicit = 
  let fn = Autocomplete.findFunction m.complete name
  in case fn of
       Nothing -> Err "fn doesnt exist"
       Just fn ->
         let (fs, _) = addFunction m id name mpos
             -- get param for implicit
             impP = Maybe.andThen
               (\n -> Autocomplete.findParamByType fn n.liveValue.tipe)
               implicit
             impA = Maybe.map AVar implicit

             -- strip param used for implicit
             params = List.filter (\p -> (Just p) /= impP) fn.parameters

             -- combine
             extra = Maybe.map2 (,) impA impP |> ME.toList
             pairs = extra ++ (List.map2 (,) args params)

             argEdges = List.map (createArg m fn id) pairs
         in
          if List.length pairs < List.length extra + List.length args
          then Err <| "Too many argument and not enough parameters: " ++ (toString pairs) ++ (toString extra) ++ (toString args)
          else (Ok fs) :: argEdges
               |> RE.combine 
               |> Result.map List.concat 



execute : Model -> Bool -> AST -> Modification
execute m re ast =
  let id = G.gen_id () in
  case ast of
    ACreating pos (ACValue value) ->
      RPC <| ([AddValue id value (Root pos)]
             , FocusNext id)

    ACreating pos (ACFnCall name args) ->
      case createFn m id name args (Root pos) Nothing of
        Ok fns -> RPC (fns, FocusNext id)
        Err msg -> Error msg

    AFillParam (APBlank (target, param)) ->
      RPC ([SetConstant "null" (target.id, param.name)]
           , FocusNext target.id |> refocus re)

    AFillParam (APVar (target, param) source) ->
      RPC ([SetEdge source.id (target.id, param.name)]
          , FocusNext target.id |> refocus re)

    AFillParam (APConst (target, param) value) ->
      RPC ([SetConstant value (target.id, param.name)]
          , FocusNext target.id |> refocus re)

    AFillParam (APFnCall (target, param) name args) ->
      -- TODO: take over the positioning
      case createFn m id name args (Dependent Nothing) Nothing of
        Ok fns -> RPC (fns ++ [SetEdge id (target.id, param.name)]
                      , FocusNext target.id)
        Err msg -> Error msg

    AFillResult (ARFieldName source name) ->
      RPC ([ AddFunctionCall id "." (Dependent Nothing)
           , SetEdge source.id (id, "value")
           , SetConstant ("\"" ++ name ++ "\"") (id, "fieldname")]
          , FocusNext id)

    AFillResult (ARVar source target) ->
      -- TODO: use type
      case G.findParam target of
        Nothing -> Error "There are no argument slots available"
        Just (_, (param, _)) ->
          RPC ([ SetEdge source.id (target.id, param.name)]
              , FocusExact target.id)

    AFillResult (ARNewValue source value) ->
      RPC ([ AddFunctionCall id "_" (Dependent Nothing)
           , SetConstant value (id, "value")
           , SetEdge source.id (id, "ignore")]
          , FocusNext id)

    AFillResult (ARFnCall source name args) ->
      case createFn m id name args (Dependent Nothing) (Just source) of
        Ok fns -> RPC (fns, FocusNext id)
        Err msg -> Error msg

    AError msg ->
      Error msg

    ANothing ->
      NoChange
 

submit : Model -> Bool -> EntryCursor -> String -> Modification
submit m re cursor value =
  let pt = EntryParser.parseFully value
  in case pt of
    Ok pt -> execute m re <| EntryParser.pt2ast m cursor pt 
    Err error -> Error <| toString error
