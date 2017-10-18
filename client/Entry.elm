module Entry exposing (..)

-- builtins
import Task
import Result exposing (Result)
import Dict

-- lib
import Dom
import List.Extra as LE
import Result.Extra as RE
import Maybe.Extra as ME

-- dark
import Util
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
  in
    case LE.getAt i n.arguments of
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
enterExact m selected =
  Filling selected (G.findHole selected)
  |> cursor2mod (G.orderedNodes m)

-- Enter the next needed node, searching from here
enterNext : Model -> Node -> Modification
enterNext m n =
  cursor2mod (G.orderedNodes m) <|
    case G.findNextHole m n of
      Nothing -> Filling n (ResultHole n)
      Just hole -> Filling (nodeFromHole hole) hole

cursor2mod : NodeList -> EntryCursor -> Modification
cursor2mod ns cursor =
  Many [ Enter False cursor
       , case cursor of
           Filling n (ResultHole _) ->
             AutocompleteMod <| ACFilterByLiveValue n.liveValue
           Filling _ (ParamHole _ p _) ->
             Many [ AutocompleteMod <| ACFilterByParamType p.tipe ns
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
  let blockNode = Debug.log "parent block" (G.findParentBlock m baseNode) in
  case blockNode of
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

-- Algorithm: when we delete or create a node, we have a new set of
-- subgraphs. These subgraphs may have no root/free, or multiple
-- root/frees. All nodes are positioned already.

-- If a subgraph has no root, pick the top-left node. Should it be free
-- or root? If the deleted node was a root, than make it a root. Otherwise, make it free.

-- If a subgraph has two or more roots, pick the top-left of the roots
-- as the winner. If it has two or more frees, same deal. Use the
-- "relative" top left, but maybe skip that for now.

withNodePositioning : Model -> List RPC -> List RPC
withNodePositioning m ops = ops ++ (createNodePositioning m ops)

createNodePositioning : Model -> List RPC -> List RPC
createNodePositioning m ops =
  let newM = List.foldl model m ops
      wasDelete = List.any (\op -> case op of
                              DeleteNode id -> G.isRoot (G.getNodeExn m id)
                              _ -> False) ops
      subgraphs = G.toSubgraphs newM
      topLeftOf nodes =
        nodes
        |> List.sortWith
             (\n1 n2 -> case compare (G.posx n1) (G.posx n2) of
                          EQ -> compare (G.posy n1) (G.posy n2)
                          a -> a)
        |> Debug.log "sorted"
        |> List.head
        |> Util.deMaybe
        |> Debug.log "winner"
  in
    subgraphs
    |> List.map
        (\subgraph ->
          let roots = List.filter G.isRoot (Debug.log "subgraph" subgraph)
              rootCount = List.length (Debug.log "roots" roots)
              frees = List.filter G.isFree subgraph
              freeCount = List.length (Debug.log "frees" frees)
          in
            if rootCount + freeCount == 1
            then
              let newRoot = topLeftOf subgraph
                  oldRoot = List.head (roots ++ frees) |> Util.deMaybe
              in [UpdateNodePosition oldRoot.id (Dependent <| Just <| G.pos oldRoot), UpdateNodePosition newRoot.id (Root <| G.pos newRoot)]

            -- TODO, if we just added a parent to the root, it should become then new root. So we actually need to pick a new root here
            else if rootCount > 0
            then
              -- pick the topleft root and make it the winner, everything else is dependent
              let winner = topLeftOf roots
              in List.filterMap
                  (\n -> if n == winner
                          then Nothing
                          else Just (UpdateNodePosition n.id (Dependent Nothing)))
                  (roots ++ frees)

            else if freeCount > 0
            then -- pick the topleft free and make it the winner, everything else is dependent
              let winner = topLeftOf frees
              in List.filterMap
                  (\n -> if n == winner
                          then Nothing
                          else Just (UpdateNodePosition n.id (Dependent Nothing)))
                  frees

            else -- no root or free, promote one. To free or root? If root was deleted, to a root.
              let winner = topLeftOf subgraph in
              if wasDelete
              then [UpdateNodePosition winner.id (Root (G.pos winner))]
              else [UpdateNodePosition winner.id (Free Nothing)]
        )
    |> List.concat




-- The graph actions are on the server, the layout is on the client.
-- Awkward!! We cant use the server-side to pick winners. For example,
-- in the case that we remove a node, creating 4 new subgraphs. While
-- the server might know the new subgraphs, it won't know the positions
-- cause they layout is on the client. So we do a shitty model of the
-- RPC behaviour. We could use phantom instead.
model : RPC -> Model -> Model
model op m =
  let
    param name = { name = name
                 , tipe = TAny
                 , anon_args = []
                 , optional = False
                 , description = "fake"
                 }
    fake id pos =
      { name = "fake"
      , id = ID id
      , pos = pos
      , tipe = FunctionCall
      , liveValue = { value = "fake value"
                    , tipe = TAny
                    , json = "\" fake value \""
                    , exc = Nothing}
      , fields = []
      , arguments = []
      , blockID = Nothing
      , argIDs = []
      , visible = True
      , cursor = 0
      }
    setArg node name arg =
      let args = List.filter (\(p, a) -> p.name /= name) node.arguments
      in { node | arguments = (param name, arg) :: args }
    rmByArg cond node =
      let args = List.filter (\(_, a) -> not (cond a)) node.arguments
      in { node | arguments = args }
    update ns n = Dict.insert (n.id |> deID) n ns
    newNodes =
      case Debug.log "op" op of
        AddDatastore (ID id) _ pos ->
          fake id pos |> update m.nodes
        AddValue (ID id) _ pos ->
          fake id pos |> update m.nodes

        -- function call has args but since they dont do anything, add
        -- them later if any set_edges come through
        AddFunctionCall (ID id) name pos ->
          fake id pos |> update m.nodes

        -- I think the args can't be connected to anything else here, so
        -- nbd. But, more set_edges might connect things together. But I
        -- dont think we're dealing with that yet.
        AddAnon (ID id) pos argids _ ->
          fake id pos |> update m.nodes

        SetConstant c (id, paramname) ->
          setArg (G.getNodeExn m id) paramname (Const c) |> update m.nodes
        SetEdge source (target, paramname) ->
          setArg (G.getNodeExn m target) paramname (Edge source) |> update m.nodes

        DeleteNode id ->
          let children = G.outgoingNodes m (G.getNodeExn m id)
              newNodes = List.map (rmByArg ((==) (Edge id))) children
              new = List.foldl (flip update) m.nodes newNodes
          in new
             |> Dict.remove (id |> deID)
             |> Debug.log "removed"

        -- we could model many of these, but they can't happen in an
        -- Entry form so don't care
        other ->
          Debug.crash <| "Can't model this: " ++ (toString other)
  in
    { m | nodes = newNodes }

execute : Model -> Bool -> AST -> Modification
execute m re ast =
  let id = G.gen_id ()
      rpc = \(ops, focus) -> RPC (withNodePositioning m ops, focus) in
  case ast of
    ACreating pos (ACValue value) ->
      rpc ([AddValue id value (Root pos)]
          , FocusNext id)

    ACreating pos (ACFnCall name args) ->
      case createFn m id name args (Root pos) Nothing of
        Ok fns -> rpc (fns, FocusNext id)
        Err msg -> Error msg

    AFillParam (APBlank (target, param)) ->
      rpc ([SetConstant "null" (target.id, param.name)]
          , FocusNext target.id |> refocus re)

    AFillParam (APVar (target, param) source) ->
      rpc ([SetEdge source.id (target.id, param.name)]
          , FocusNext target.id |> refocus re)

    AFillParam (APConst (target, param) value) ->
      rpc ([SetConstant value (target.id, param.name)]
          , FocusNext target.id |> refocus re)

    AFillParam (APFnCall (target, param) name args) ->
     case createFn m id name args (Dependent Nothing) Nothing of
        Ok fns -> rpc (fns ++ [ SetEdge id (target.id, param.name)]
                      , FocusNext target.id)
        Err msg -> Error msg

    AFillResult (ARFieldName source name) ->
      rpc ([ AddFunctionCall id "." (Dependent Nothing)
           , SetEdge source.id (id, "value")
           , SetConstant ("\"" ++ name ++ "\"") (id, "fieldname")]
          , FocusNext id)

    AFillResult (ARVar source target) ->
      -- TODO: use type
      case G.findParam target of
        Nothing -> Error "There are no argument slots available"
        Just (_, (param, _)) ->
          rpc ([ SetEdge source.id (target.id, param.name)]
              , FocusExact target.id)

    AFillResult (ARNewValue source value) ->
      rpc ([ AddFunctionCall id "_" (Dependent Nothing)
           , SetConstant value (id, "value")
           , SetEdge source.id (id, "ignore")]
          , FocusNext id)

    AFillResult (ARFnCall source name args) ->
      case createFn m id name args (Dependent Nothing) (Just source) of
        Ok fns -> rpc (fns, FocusNext id)
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
