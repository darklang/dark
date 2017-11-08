module Entry exposing (..)

-- builtins
import Task
import Result exposing (Result)
import Dict
import Set

-- lib
import Dom
import Result.Extra as RE
import Maybe.Extra as ME

-- dark
import Util
import Defaults
import Graph as G exposing (posy, posx)
import Node as N
import Types exposing (..)
import Autocomplete
import Viewport
import EntryParser exposing (AST(..), ACreating(..), AExpr(..), AFillParam(..), AFillResult(..), ARef(..))


idFromHole : Hole -> ID
idFromHole h =
  case h of
    ResultHole id -> id
    ParamHole id _ _ -> id

holeCreatePos : Model -> Hole -> Pos
holeCreatePos m hole =
  let gn = G.getNodeExn m in
  case hole of
    ParamHole id _ i -> let n = gn id in {x=(posx m n)-50+(i*50), y=(posy m n)-40}
    ResultHole id ->
      let n = gn id
          connected = G.entireSubgraph m n
          lowest = connected
                   |> List.map (\n -> posy m n)
                   |> List.maximum
                   |> Maybe.withDefault (posy m n)
      in
      {x=posx m n, y=lowest+40}


---------------------
-- Nodes
---------------------
updateValue : String -> Modification
updateValue target =
  Many [ AutocompleteMod <| ACQuery target ]

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
addBlockParam : Model -> ID -> MPos -> ParamName -> List String -> (List RPC, Focus)
addBlockParam _ id pos name block_args =
  let sid = N.gen_id ()
      argids = List.map (\_ -> N.gen_id ()) block_args
      block = AddBlock sid (NoPos Nothing) argids block_args
      edge = SetEdge sid (id, name)
  in
    ([block, edge], FocusNext id)


addFunction : Model -> ID -> Name -> MPos -> (List RPC, Focus)
addFunction m id name pos =
  let fn = Autocomplete.findFunction m.complete name in
  case fn of
    -- not a real function, but hard to thread an error here, so let the
    -- server fail instead
    Nothing ->
      ([AddFunctionCall id name pos], FocusSame)
    Just fn ->
      -- automatically add blocks
      let block_args = List.filter (\p -> p.tipe == TBlock) fn.parameters
          blockpairs = List.map (\p -> addBlockParam m id pos p.name p.block_args) block_args
          blockarg = blockpairs |> List.head |> Maybe.map Tuple.second
          blocks = blockpairs |> List.unzip |> Tuple.first
          focus = case blockarg of
            Just f -> f
            Nothing -> FocusNext id
      in
        (AddFunctionCall id name pos :: List.concat blocks, focus)

updatePreviewCursor : Model -> ID -> Int -> List RPC
updatePreviewCursor m id step =
  let baseNode = G.getNodeExn m id
      blockNode = G.findParentBlock m baseNode in
  case blockNode of
    Just n -> [UpdateNodeCursor n.id (n.cursor + step)]
    Nothing -> []

refocus : Bool -> Focus -> Focus
refocus re default =
  case default of
    FocusNext id -> if re then Refocus id else default
    FocusExact id -> if re then Refocus id else default
    f -> f


----------------------
-- execute the AST
----------------------
createArg : Model -> Function -> ID -> Maybe Node -> (AExpr, Parameter) -> Result String (List RPC)
createArg m fn id placeholderNode (arg, param) =
  case arg of
   AVar r -> case r of
             ANode n -> Ok [SetEdge n.id (id, param.name)]
             APlaceholder -> Err <| "Unbound placeholder"
   AValue v -> Ok [SetConstant v (id, param.name)]
   AFnCall name args ->
     let fnid = N.gen_id ()
     in createFn m fnid name args (Dependent Nothing) Nothing placeholderNode
        |> Result.map (\rpcs -> rpcs ++ [SetEdge fnid (id, param.name)])


-- Takes a function and a list of AExprs that are to be its arguments
-- along with an implicit node, and a node to bind to `$_` args
-- and returns a list of AExprs bound to Parameters of that function
getExprParamPairs : Model -> ID -> Function -> List AExpr -> Maybe Node -> Maybe Node -> List (AExpr, Parameter)
getExprParamPairs m id fn args implicit placeholderNode =
    case (placeholderNode, (directArgsContainPlaceholder args)) of
        (Just n, True) -> fn.parameters |> List.map2 (,) (bindPlaceholderRefToNode args n)
        _ -> injectImplicit m id fn args implicit

directArgsContainPlaceholder : List AExpr -> Bool
directArgsContainPlaceholder args = argsContainPlaceholder (\_ -> False) args

nestedArgsContainPlaceholder : List AExpr -> Bool
nestedArgsContainPlaceholder args = argsContainPlaceholder (\x -> nestedArgsContainPlaceholder x) args

argsContainPlaceholder : (List AExpr -> Bool) -> List AExpr -> Bool
argsContainPlaceholder nestedfn args = args
                            |> List.filter (\x ->
                                        case x of
                                            AVar r -> case r of
                                                          APlaceholder -> True
                                                          _            -> False
                                            AFnCall _ a -> nestedfn a
                                            _      -> False)
                            |> (not << List.isEmpty)


bindPlaceholderRefToNode : List AExpr -> Node -> List AExpr
bindPlaceholderRefToNode args node = args
                                    |> List.map (\x -> case x of
                                                    AVar APlaceholder -> AVar (ANode node)
                                                    _              -> x)

injectImplicit : Model -> ID -> Function -> List AExpr -> Maybe Node -> List (AExpr, Parameter)
injectImplicit m id fn args implicit =
    let impP = Maybe.andThen
               (\n -> Autocomplete.findParamByType fn n.liveValue.tipe)
               implicit
        -- get arg for implicit
        impA = Maybe.map (AVar << ANode) implicit

        -- strip param used for implicit
        params = List.filter (\p -> (Just p) /= impP) fn.parameters

        -- combine
        extra = Maybe.map2 (,) impA impP |> ME.toList
        pairs = extra ++ (List.map2 (,) args params)
    in
        pairs


createFn : Model -> ID -> String -> List AExpr -> MPos -> Maybe Node -> Maybe Node -> Result String (List RPC)
createFn m id name args mpos implicit placeholderNode =
  let fn = Autocomplete.findFunction m.complete name
  in case fn of
       Nothing -> Err "fn doesnt exist"
       Just fn ->
         let (fs, _) = addFunction m id name mpos
             pairs = getExprParamPairs m id fn args implicit placeholderNode
             argEdges = List.map (createArg m fn id placeholderNode) pairs
         in
          if List.length pairs < List.length args + (implicit |> Maybe.map (\_ -> 1) |> Maybe.withDefault 0)
          then Err <| "Too many arguments and not enough parameters: " ++ (toString pairs) ++ (toString args)
          else (Ok fs) :: argEdges
               |> RE.combine
               |> Result.map List.concat

-- When we delete or create a node, we have a new set of subgraphs.
-- These subgraphs may have no root/free, or multiple root/frees. So we
-- need to reset them, and get the graph to be consistent.
withNodePositioning : Model -> List RPC -> List RPC
withNodePositioning m ops = ops ++ (createNodePositioning m ops)

createNodePositioning : Model -> List RPC -> List RPC
createNodePositioning m ops =
  let (newM, _) = List.foldl model (m, Set.empty) ops
      deletedNode =
        ops
        |> List.filterMap (\op -> case op of
                                 DeleteNode id -> Just (G.getNodeExn m id)
                                 _ -> Nothing)
        |> List.head
      deletedWasRoot = ME.unwrap False G.isRoot deletedNode
      subgraphs = G.toSubgraphs newM
      toDep n = UpdateNodePosition n.id (Dependent <| Just <| DPos <| G.pos m n)
      toRoot n root = UpdateNodePosition n.id (Root <| G.pos m root)
      toFree n = UpdateNodePosition n.id (G.pos m n |> Viewport.toViewport m |> Just |> Free)
      without n ns = List.filter ((/=) n) ns
      topLeftOf nodes =
        nodes
        |> List.sortWith
             (\n1 n2 -> case compare (G.posx m n1) (G.posx m n2) of
                          EQ -> compare (G.posy m n1) (G.posy m n2)
                          a -> a)
        |> List.head
        |> Util.deMaybe
  in
    subgraphs
    |> List.map
        -- Technically, we store positions on nodes. Let's ignore that
        -- for a sec. Actually, it is subgraphs that have positions. The
        -- entire subgraph (aka cluster of connected nodes) has a single
        -- position. So we could, theoretically store positions on
        -- subgraphs on the server.
        -- However, we also need to store the node off of which the
        -- subgraph "hangs", as this is difficult (and potentialy
        -- inconsistent) to figure out dynamically.
       (\subgraph ->
          let roots = subgraph |> List.filter G.isRoot
              rootCount = List.length roots
              frees = subgraph |> List.filter G.isFree
              freeCount = List.length frees
          in
            if rootCount == 1 && freeCount == 0
            then
              -- things are exactly as they should be, but we might need
              -- to update the root on which the graph hangs. Let's take
              -- the root and climb its parents.
              let root = List.head roots |> Util.deMaybe
                  newRoot = G.highestParent newM root in
              if newRoot == root
              then []
              else [toDep root, toRoot newRoot root]

            else if rootCount == 0 && freeCount == 1
            then
              -- Same, but with frees
              let free = List.head frees |> Util.deMaybe
                  newFree = G.highestParent newM free in
              if newFree == free
              then []
              else [toDep free, toFree newFree]

            -- We just merged some graphs. Let's pick the top-left root,
            -- and get rid of all other positions.
            else if rootCount >= 1
            then let winner = topLeftOf roots
                 in List.map toDep (without winner (roots ++ frees))

            -- Same, for free
            else if freeCount >= 1
            then let winner = topLeftOf frees
                 in List.map toDep (without winner (roots ++ frees))

            -- We might just have deleted the node on which this graph
            -- hung, and that stored the position. It's first child in
            -- this subgraph is the best choice. But, nodes are actually
            -- laid out here, so let's pick the top-left in the subgraph
            -- cause it's simpler.
            -- TODO: we might get multiple subgraphs in the same position...
            -- TODO: in which case we can just use the node's current position
            else if rootCount + freeCount == 0 && deletedWasRoot
            then [toRoot (topLeftOf subgraph) (deletedNode |> Util.deMaybe)]

            -- These nodes are also laid out, so pick the top-left
            else if rootCount + freeCount == 0 && not deletedWasRoot
            then [toFree (topLeftOf subgraph)]
            else
              Debug.log "no weird subgraph condition found" []
        )
      |> List.concat


-- The graph actions are on the server, the layout is on the client.
-- Awkward!! We cant use the server-side to pick winners. For example,
-- in the case that we remove a node, creating 4 new subgraphs. While
-- the server might know the new subgraphs, it won't know the positions
-- cause they layout is on the client. So we do a shitty model of the
-- RPC behaviour. We could use phantom instead.
model : RPC -> (Model, Set.Set Int) -> (Model, Set.Set Int)
model op mAndBlocks =
  let
    (m, blocks) = mAndBlocks
    param name = { name = name
                 , tipe = TAny
                 , block_args = []
                 , optional = False
                 , description = "fake"
                 }
    fake id pos tipe =
      { name = "fake"
      , id = ID id
      , pos = pos
      , tipe = tipe
      , liveValue = { value = "fake value"
                    , tipe = TAny
                    , json = "\" fake value \""
                    , exc = Nothing}
      , arguments = []
      , isBlockParent = False
      , deleteWith = []
      , cursor = 0
      , face = ""
      }
    setArg node name arg =
      let args = List.filter (\(p, a) -> p.name /= name) node.arguments
      in { node | arguments = (param name, arg) :: args }
    update ns n = Dict.insert (n.id |> deID) n ns
    newBlocks =
      case op of
        AddBlock (ID id) _ _ _ ->
          Set.insert id blocks
        _ -> blocks
    newNodes =
      case op of
        AddDatastore (ID id) _ pos ->
          fake id pos Datastore |> update m.nodes
        AddValue (ID id) _ pos ->
          fake id pos Value |> update m.nodes

        -- function call has args but since they dont do anything, add
        -- them later if any set_edges come through
        AddFunctionCall (ID id) name pos ->
          fake id pos FunctionCall |> update m.nodes

        -- We can ignore AddBlock because the arguments won't be
        -- connected to anything yet. But if that changes, this will
        -- need to be modelled.
        AddBlock (ID id) pos argids _ ->
          m.nodes

        SetConstant c (id, paramname) ->
          setArg (G.getNodeExn m id) paramname (Const c) |> update m.nodes
        SetEdge (ID source) (target, paramname) ->
          if Set.member source blocks -- ignore blocks, see AddBlock
          then m.nodes
          else setArg (G.getNodeExn m target) paramname (Edge (ID source) FnEdge) |> update m.nodes

        DeleteNode id ->
          (G.deleteNode m id).nodes


        -- we could model many of these, but they can't happen in an
        -- Entry form so don't care
        other ->
          Debug.crash <| "Can't model this: " ++ (toString other)
  in
    ({ m | nodes = newNodes }, newBlocks)

execute : Model -> Bool -> AST -> Modification
execute m re ast =
  let id = N.gen_id ()
      rpc = \(ops, focus) -> RPC (withNodePositioning m ops, focus) in
  case ast of
    ACreating pos (ACValue value) ->
      rpc ([AddValue id value (Root pos)]
          , FocusNext id)

    ACreating pos (ACFnCall name args) ->
      case createFn m id name args (Root pos) Nothing Nothing of
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
     case createFn m id name args (Dependent Nothing) Nothing Nothing of
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
      let placeholder = Just source
          implicit    = if (not << nestedArgsContainPlaceholder) args then (Just source) else Nothing
      in
      case createFn m id name args (Dependent Nothing) implicit placeholder of
        Ok fns -> rpc (fns, FocusNext id)
        Err msg -> Error msg

    AError msg ->
      Error msg

    ANothing ->
      NoChange

addDB : String -> MPos -> Modification
addDB name pos =
  let id = N.gen_id () in
  RPC ([AddDatastore id name pos], FocusExact id)

submit : Model -> Bool -> EntryCursor -> String -> Modification
submit m re cursor value =
  if String.startsWith "DB " value -- hack for now
  then
    case cursor of
      Creating pos ->
        let name = String.dropLeft 2 value |> String.trim
        in addDB name (Root pos)
      _ -> NoChange
  else
    let pt = EntryParser.parseFully value
    in case pt of
      Ok pt -> execute m re <| EntryParser.pt2ast m cursor pt
      Err error -> Error <| EntryParser.toErrorMessage <| EntryParser.addCursorToError error cursor
