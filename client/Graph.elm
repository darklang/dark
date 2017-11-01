module Graph exposing (..)

-- builtin
import Ordering
import List
import Tuple
import Dict exposing (Dict)
import Set
import Maybe

-- lib
import List.Extra as LE
import Tuple2 as T2
import Dict.Extra as DE

-- dark
import Node as N
import Types exposing (..)
import Defaults
import Viewport
import Util exposing (deMaybe, int2letter, letter2int)


-------------------------
-- positioning
-------------------------

isRoot : Node -> Bool
isRoot n =
  case n.pos of
    Root _ -> True
    _ -> False

isNotRoot : Node -> Bool
isNotRoot = not << isRoot

isDependent : Node -> Bool
isDependent n =
  case n.pos of
    Dependent _ -> True
    _ -> False



isFree : Node -> Bool
isFree n =
  case n.pos of
    Free _ -> True
    _ -> False

hasPos : Node -> Bool
hasPos n =
    case n.pos of
      NoPos _ -> False
      _       -> True

notPositioned : Node -> Bool
notPositioned = isPositioned >> not

isPositioned : Node -> Bool
isPositioned n =
  case n.pos of
    Free Nothing -> False
    Dependent Nothing -> False
    NoPos Nothing -> False
    _ -> True

hasRelativePos : Node -> Bool
hasRelativePos n =
  case n.pos of
    Free _ -> True
    Dependent (Just (DVPos _)) -> True
    _ -> False

pos : Model -> Node -> Pos
pos m n =
  case n.pos of
    Root pos -> pos
    Free (Just pos) -> Viewport.toAbsolute m pos
    Dependent (Just (DPos pos)) -> pos
    Dependent (Just (DVPos pos)) -> Viewport.toAbsolute m pos
    _ -> {x=Defaults.unsetInt, y=Defaults.unsetInt}

posx : Model -> Node -> Int
posx m n = (pos m n).x

posy : Model -> Node -> Int
posy m n = (pos m n).y

distance : Model -> Node -> Node -> Float
distance m n1 n2 =
  let xdiff = toFloat (posx m n2 - posx m n1) ^ 2
      ydiff = toFloat (posy m n2 - posy m n1) ^ 2
  in
    sqrt (xdiff + ydiff)

orderedNodes : Model -> List Node
orderedNodes m =
  m.nodes
  |> Dict.values
  |> List.map (\n -> (posx m n, posy m n, n.id |> deID))
  |> List.sortWith Ordering.natural
  |> List.map (\(_,_,id) -> getNodeExn m (ID id))

fromLetter : Model -> String -> Maybe Node
fromLetter m letter = m |> orderedNodes |> LE.getAt (letter2int letter)

toLetter : Model -> ID -> String
toLetter m id = m |> orderedNodes |> LE.findIndex (\n -> n.id == id) |> deMaybe |> int2letter

hasNode : Model -> ID -> Bool
hasNode m id = Dict.member (deID id) m.nodes

getNode : Model -> ID -> Maybe Node
getNode m id = Dict.get (deID id) m.nodes

getNodeExn : Model -> ID -> Node
getNodeExn m id =
  if Dict.member (deID id) m.nodes
  then getNode m id |> deMaybe
  else Debug.crash <| "Missing node: " ++ toString id

------------------------
-- holes
------------------------
findParam : Node -> Maybe (Int, (Parameter, Argument))
findParam n = Util.findIndex (\(_, a) -> a == NoArg) n.arguments

findHole : Node -> Hole
findHole n =
  case findParam n of
    Nothing -> ResultHole n
    Just (i, (p, _)) -> ParamHole n p i

findParamHole : Node -> Maybe Hole -> Maybe Hole
findParamHole n mh =
  case mh of
    Just h -> Just h
    Nothing ->
      findParam n
      |> Maybe.map (\(i, (p, _)) -> ParamHole n p i)

findNextArgHole : Model -> Node -> Maybe Hole
findNextArgHole m n =
  fold findParamHole Nothing n (incomingNodes m)

-- find args going up. If there aren't any, look down. Hit the end?
-- Suggest it.
findNextHole : Model -> Node -> Maybe Hole
findNextHole m start =
  let func n mh =
        case mh of
        Just h -> Just h
        Nothing ->
          case findNextArgHole m n of
            Just h -> Just h
            Nothing -> case outgoingNodes m n of
                         [] -> Just (ResultHole n)
                         _ -> Nothing
  in
    fold func Nothing start (outgoingNodes m)

findParentBlock : Model -> Node -> Maybe Node
findParentBlock m n =
  let searchParents =
        List.foldl
          (\curr accum -> case accum of
                            Just result -> Just result
                            Nothing -> findParentBlock m curr)
          Nothing
  in
      if n.isBlockParent
      then Just n
      else searchParents (incomingNodes m n)


insideBlock : Model -> Node -> Bool
insideBlock m n =
  case findParentBlock m n of
    Just n -> True
    _      -> False

------------------------
-- Subgraph
------------------------

-- Starting with `start`, traverse the graph, folding using `func`. Will not
-- visit the same node twice.
type alias IDSet = Set.Set Int
fold :  (Node -> b -> b) -> b -> Node -> (Node -> List Node) -> b
fold func acc start nextfn =
  fold_ func (Set.empty, acc) start nextfn |> Tuple.second


fold_ : (Node -> b -> b) -> (IDSet, b) -> Node -> (Node -> List Node) -> (IDSet, b)
fold_ func (seen, bAcc) start nextfn =
  -- if we've been here before, we're done. Return the inputs
  -- we have a list of next nodes.
  -- we need the new value. call (func n acc)
  -- 1 go into the node, passing the new set. When we return we take the new val and set.
  if Set.member (deID start.id) seen
  then (seen, bAcc)
  else
    let new_val = func start bAcc
        new_set = Set.insert (deID start.id) seen in
        List.foldl
          (\n (s, v) -> fold_ func (s,v) n nextfn)
          (new_set, new_val)
          (nextfn start)


-- example:

-- a
-- a - b, c, d, e, x
-- b - c, d, e
-- c - f, g, h

-- a({}, a) -> ({a}, a)
 -- b({a}, b) -> ({a,b}, b)
  -- c({a,b}, c) -> ({a,b,c}, c)
   -- f({a,b,c}, c) -> ({a,b,c,f} -> f)
    -- g({a,b,c,f}, f) -> ({a,b,c,f,g}, g)
    -- h({a,b,c,f,g), g) -> ({a,b,c,f,g,h}, h)
  -- d({a,b,c,f,g,h}, h) -> ({a,b,c,f,g,h,d}, h)

entireSubgraph : Model -> Node -> List Node
entireSubgraph m start =
  fold (\n list -> n :: list) [] start (\n -> connectedNodes m n)

toSubgraphs : Model -> List (List Node)
toSubgraphs m =
  (List.foldl
    (\node (subgraphs, set) ->
      if Set.member (node.id |> deID) set
      then (subgraphs, set)
      else
        let newSub = entireSubgraph m node
            newSet = newSub |> List.map (.id >> deID) |> Set.fromList
        in (newSub :: subgraphs, Set.union set newSet))
    ([], Set.empty)
    (m.nodes |> Dict.values))
    |> Tuple.first

  -- e({a,b,c,f,g,h,d}, h) -> ({a,b,c,f,g,h,d,e}, h)
  -- x({a,b,c,f,g,h,d,e}, h) -> ({a,b,c,f,g,h,d,e,x}, x)

moveSubgraph : Model -> ID -> Int -> Int -> (Model, Node)
moveSubgraph m id offsetX offsetY =
  let n = getNodeExn m id
      sg : List Node
      sg = entireSubgraph m n
      root = List.filter isRoot sg
      free = List.filter isFree sg
      rest = List.filter isDependent sg
      depType = if List.isEmpty root then FreeDep else RootDep
      new = List.map
        (\n ->
          let x = offsetX + posx m n
              y = offsetY + posy m n
          in updateNodePosition m RootDep n x y)
        sg
      newDict = List.map (\n -> (n.id |> deID, n)) new |> Dict.fromList
      newNodes = Dict.union newDict m.nodes
      resultNode = root ++ free |> List.head |> deMaybe
  in ({ m | nodes = newNodes}, resultNode)



-------------------
-- connected nodes
-------------------
-- We have a sorting problem in how we draw nodes. We sometimes get the
-- nice if block indentation that we want, but only if we position the
-- if first (ie. it's the first node in the list returned by
-- outgoingNodes). If not, we get the bog standard no indentation version
-- TODO: we need to come up with some actual rules for this
sortByIf : List Node -> List Node
sortByIf nodes =
  nodes
  |> List.sortWith (\a b ->
                          case (a.name, b.name) of
                          ("if", "if") -> EQ
                          ("if", _)    -> LT
                          (_, "if")    -> GT
                          _            -> EQ)


outgoingNodesWhere : (Parameter -> Argument -> Node -> Bool) -> Model -> Node -> List Node
outgoingNodesWhere cond m n =
  m.nodes
  |> Dict.values
  |> List.filterMap
       (\child ->
          child.arguments
          |> List.filter
               (\(p, a) ->
                 let parentID = N.getParentID a
                 in if parentID == Just n.id
                    then (cond p a (getNodeExn m (deMaybe parentID)))
                    else False)
          |> List.head
          |> Maybe.map (always child))
  |> sortByIf

outgoingNodes : Model -> Node -> List Node
outgoingNodes = outgoingNodesWhere (\_ _ _ -> True)

argNodes : Model -> Node -> List Node
argNodes = outgoingNodesWhere (\p a n -> N.isBlockEdge a)

childNodes : Model -> Node -> List Node
childNodes = outgoingNodesWhere (\p a n -> N.isFnEdge a)

incomingNodePairs : Model -> Node -> List (Node, ParamName)
incomingNodePairs m n =
  List.filterMap
    (\(p, a) ->
      case a of
        Edge id _ -> Just (getNodeExn m id, p.name)
        _ -> Nothing)
    n.arguments

incomingNodes : Model -> Node -> List Node
incomingNodes m n =
  incomingNodePairs m n
  |> List.map Tuple.first

connectedNodes : Model -> Node -> List Node
connectedNodes m n =
  incomingNodes m n ++ outgoingNodes m n



-- Only follows the first edge, goes as high as it can. This is intended
-- to allow more consistency. Once it hit a node with no parents, it's
-- done (even if the parent's sibling might have a higher parent).
highestParent : Model -> Node -> Node
highestParent m n =
  case incomingNodes m n |> List.head of
    Just node -> highestParent m node
    Nothing -> n

-- ported from backend
dependentNodes : Model -> Node -> List ID
dependentNodes m n =
  n.deleteWith |> LE.uniqueBy deID

deleteArg : (Argument -> Bool) -> Node -> Node
deleteArg cond n =
  let args = List.filter (\(_, a) -> not (cond a)) n.arguments
  in { n | arguments = args }

fromList : List Node -> NodeDict
fromList = DE.fromListBy (\n -> n.id |> deID)

updateAndRemove : Model -> List Node -> List Node -> Model
updateAndRemove m toUpdate toRemove =
  let updateDict = fromList toUpdate
      removeDict = fromList toRemove
      afterRemoved = Dict.diff m.nodes removeDict
      afterUpdated = Dict.union updateDict afterRemoved
  in { m | nodes = afterUpdated }

deleteNode : Model -> ID -> Model
deleteNode m id =
  let ids = dependentNodes m (getNodeExn m id)

      -- remove the nodes
      remaining = Dict.filter (\_ n -> not <| List.member n.id ids) m.nodes

      -- remove any args pointing to the nodes
      nodes = Dict.map
               (\_ n ->
                  List.foldl
                    (\id n -> deleteArg (N.isParentEdge id) n)
                    n
                    (n.id :: ids)
               )
               remaining
  in { m | nodes = nodes }

updateGraph : Model -> Model
updateGraph m = m
              -- |> collapseIfs
              -- |> collapseArgsWithSoloChildren
              |> reposition

collapseIfs : Model -> Model
-- TODO: propagate root-ness
collapseIfs m =
  let ifs = m.nodes |> Dict.values |> List.filter (\n -> n.name == "if")
      toCollapse = collapsableIfs m ifs
      toHide = toCollapse |> Dict.keys
      ifs2hidden = toCollapse
                 |> Dict.toList  -- to k,v assoc list
                 |> List.map (T2.swap)  -- to v,k assoc list
                 |> List.map (\(a, b) -> (a, [b]))  -- listify the second elem
                 |> DE.fromListDedupe (\a b -> a ++ b) -- append on dedupe
                 -- TODO: clean
                 |> Dict.map (\k v ->
                                case v of
                                  a :: b :: [] ->
                                    if isIncoming m (getNodeExn m (ID k)) (ID a)
                                    then a :: b :: []
                                    else b :: a :: []
                                  _ -> v)
      withFaces = generateFaces m ifs2hidden
      nodes = m.nodes
            |> Dict.union withFaces
            |> Dict.map (replaceArguments toCollapse)
            |> Dict.filter (\i _ -> not <| List.member i toHide)
  in
      { m | nodes = nodes }

isIncoming : Model -> Node -> ID -> Bool
isIncoming m n id = incomingNodes m n
                  |> List.map .id
                  |> List.member id

-- return a NodeDict of the ifnodes with their `face` attribute correctly
-- constructed
generateFaces : Model -> Dict Int (List Int) -> NodeDict
generateFaces m d = Dict.map (\id collapsed ->
  let ifnode = getNodeExn m (ID id)
      parents = List.map (getNodeExn m << ID) collapsed
  in
      N.generateFace ifnode parents
  ) d

-- Given an (id2hide -> id2replaceitwith) map and a (id, node) k/v pair
-- from the dict (passed as separate params bc of Dict.map's signature)
-- replace all occurrences of the id2hides with their id2replaceitwith
-- in the node's argumentd
replaceArguments : Dict Int Int -> Int -> Node -> Node
replaceArguments ids2hide _ node =
  let newArgs = List.map (\(p, a) ->
                            case a of
                              Edge id b ->
                                case Dict.get (deID id) ids2hide of
                                  Just newId -> if newId /= (deID node.id)
                                                then (p, Edge (ID newId) b)
                                                else (p, NoArg)
                                  Nothing    -> (p, a)
                              a -> (p, a)) node.arguments
  in
      { node | arguments = newArgs }


collapsableIfs : Model -> List Node -> Dict Int Int
collapsableIfs m ns = ns
                  |> List.filterMap (\n ->
                    let parents = collapsableParents m n
                    in  case parents of
                        [] -> Nothing
                        ps -> Just ps)
                  |> List.concat
                  |> Dict.fromList

collapsableParents : Model -> Node -> List (Int, Int)
collapsableParents m ifnode =
  let ifID = ifnode.id |> deID
      cond = N.getArgument "cond" ifnode
      parentsOfIf = incomingNodes m ifnode
      firstParent =
        case cond of
          Edge id _ ->
            let parent   = getNodeExn m id
                children = outgoingNodes m parent
                         |> List.filter (\n -> n /= ifnode)
            in
            case (incomingNodes m parent, children, isRoot parent) of
              ([x], [], False) -> [(deID <| parent.id, ifID)]
              ([], [], False)  -> [(deID <| parent.id, ifID)]
              _   -> []
          _ -> []
      secondParent =
        -- we want to collapse the `if`'s grandparent iff:
        -- 1) it has no children that are not direct parents of `if`
        -- 2) it has either 0 parents, OR if it has a parent that it only has
        -- 1 parent and that parent is also a parent of `if`
        case firstParent of
          [] -> []
          [(fpID, _)] ->
            let fp = getNodeExn m (ID fpID) in
            case incomingNodes m fp of
              [y] ->
                let otherKids = outgoingNodes m y
                              |> List.filter (\n -> n /= fp)
                              |> List.length
                    otherParents = incomingNodes m y
                                 |> List.filter (\n -> not (List.member n parentsOfIf))
                                 |> List.length
                in if otherKids + otherParents == 0
                    && N.isPrimitive y && isNotRoot y
                   then
                     [(deID <| y.id, ifID)]
                   else
                     []
              _   -> [] -- wildcard for the `case incomingNodes m fp`
          _ -> [] -- wildcard for `case firstParent`
  in firstParent ++ secondParent

-------------------------------------
-- Repositioning. Here be dragons
-------------------------------------


reposition : Model -> Model
reposition m =
  let m2 = repositionRoots m
      m3 = repositionFrees m2
  in m3

paramSpacing : Int
paramSpacing=15
blockIndent: Int
blockIndent=20
ySpacing : Int
ySpacing=30

type DepType = FreeDep | RootDep

repositionRoots : Model -> Model
repositionRoots m =
  let roots = List.filter isRoot (Dict.values m.nodes)
  in repositionLayout m RootDep roots

repositionFrees : Model -> Model
repositionFrees m =
  let frees = List.filter isFree (Dict.values m.nodes)
      positions = List.repeat (List.length frees) 100 |> LE.scanl1 (+)
      pairs = Util.zip frees positions
      pFrees = List.map (\(n, ypos) -> { n | pos = Free <| Just <| {vx=10, vy=10+ypos} }) pairs
  in repositionLayout m FreeDep pFrees

repositionLayout : Model -> DepType -> List Node -> Model
repositionLayout m depType roots =
  List.foldl (\n m -> posRoot m depType n (posx m n) (posy m n))
             m
             roots

seen : Model -> Node -> Bool
seen m n = case Dict.get (deID n.id) m.nodes of
  Nothing -> False
  Just n -> isPositioned n

markAsSeen : Model -> DepType -> Node -> Model
markAsSeen m dt n = position m dt n -1 -1

updateNodePosition : Model -> DepType -> Node -> Int -> Int -> Node
updateNodePosition m depType n x y =
  let pos = {x=x,y=y}
      vppos = Viewport.toViewport m pos
      newPos = case n.pos of
                 Root _ -> Root pos
                 Free _ -> Free <| Just vppos
                 Dependent _ ->
                   if depType == FreeDep
                   then Dependent <| Just <| DVPos <| vppos
                   else Dependent <| Just <| DPos <| pos
                 NoPos _ -> NoPos (Just pos)
  in { n | pos = newPos }


position : Model -> DepType -> Node -> Int -> Int -> Model
position m depType n x y =
  let newN = updateNodePosition m depType n x y
      nodes = Dict.insert (deID n.id) newN m.nodes
  in { m | nodes = nodes }

-- This algorithm is very fragile and you should be extremely careful
-- about knowing exactly what's going on.

-- Walk:
-- We walk the graph from the Root. There may be multiple roots and
-- multiple leaves, and we have to reach everything. So starting from
-- the root, we look for its children, and then children place their
-- parents, which may have more children, which may have more parents,
-- etc, etc, etc.

-- Block model:

-- The code uses MaxX and MaxY a lot. These represent a box drawn around
-- this node and all the nodes that it is responsible for. When drawing
-- your children, for example, we calculate the space used by the first
-- child, and all of its children, parents, etc, and get the MaxX and
-- MaxY for all of that. We consider that a big box around it, and when
-- we draw the second child, we're not going to use the same space ever.
-- For a node with parents, that box will create a lot of Y space, and
-- after knowing the Y space, we know where we can poistion the node
-- itself.

-- PrevY
-- PrevY indicates where our box starts. It is actually the y position
-- of the node placed bottommost in the box above us. (By y position, we
-- mean the top of the bottommost node) The algorithm requires us to
-- iterate through empty lists, and so we don't want to increase the y
-- spacing until the last minute to place ourselves.

-- MaxY
-- MaxY is very similar to prevY. However, it includes all the other
-- nodes we've been propagating through. See folds below.

-- Folds
-- We fold along lists of nodes (parents, children, args). As we do so,
-- we measure the size of the boxes as MaxX and MaxY, and propagate them
-- through the folds. When we wrote this algorithm, forgetting to
-- propagate these led to significant spacing bugs.

-- TODO put this inline in posChild
-- prevY is propagated through here, and the max of each function call
-- is the max of anything we've seen so far. However, it needs to be
-- maxed with maxY _of all the other nodes in this fold_. Don't forget
-- this.

-- X spacing:
-- We place nodes in a single pass from left-to-right. So we position a
-- node (and all its parents, children, etc [obviously excluding those
-- that happened earlier in the walk]), and only then, when we know the
-- full space used by the node (and all its children, etc), can we place
-- it's sibling. That's what the folds are for.

type alias NextX = Int
type alias PrevY = Int
type alias MaxX = Int
type alias MaxY = Int

type alias TraversalInfo = (NextX, PrevY, MaxX, MaxY, Model)
type alias SpaceInfo = (MaxX, MaxY, Model)
type alias Force = Bool
type alias Spacing = { parentX : Int
                     , argsX : Int
                     , childrenX : Int
                     , siblingX : Int
                     }

debug : Model -> String -> Node -> TraversalInfo -> ()
debug m fn n (x, y, maxX, maxY, _) =
  let _ = Debug.log (fn ++ " " ++ n.name ++ " (" ++ "seen: " ++ (seen m n |> toString) ++ ")") (x, y, maxX, maxY) in ()

------------------------
-- Position lists of nodes
------------------------

posArgs : Model -> DepType -> Node -> NextX -> PrevY -> SpaceInfo
posArgs =
  foldDependents
    (\m n -> argNodes m n)
    (\_ -> posArg)

posChildren : Model -> DepType -> Node -> NextX -> PrevY -> SpaceInfo
posChildren =
  foldDependents
    (\m n -> childNodes m n)
    (\_ -> posChild)

posParents : Model -> DepType -> Node -> NextX -> PrevY -> SpaceInfo
posParents =
  foldDependents
    (\m n -> incomingNodes m n)
    (\_ -> posParent)

type alias NodeFn = Model -> Node -> List Node
type alias PosFnHack = () -> PosFn -- The Elm compiler has an ordering bug, should be fixed in 0.19
foldDependents : NodeFn -> PosFnHack -> Model -> DepType -> Node -> NextX -> PrevY -> SpaceInfo
foldDependents nodeFn posFn m depType n x y =
  let nodes = nodeFn m n
      (_, _, maxX, maxY, m2) = List.foldl (posFn () depType) (x, y, x, y, m) nodes
  in (maxX, maxY, m2)

------------------------
-- Position nodes
------------------------

posRoot : Model -> DepType -> Node -> Int -> Int -> Model
posRoot m depType n x y =
  let (_, _, _, _, m2) = posRoot_ depType n (x, y-ySpacing, x, y-ySpacing, m)
  in m2

type alias PosFn = DepType -> Node -> TraversalInfo -> TraversalInfo
posRoot_ : PosFn
posRoot_ = posNode True {parentX = 0, argsX = blockIndent, childrenX = 0, siblingX = 0}

posArg : PosFn
posArg = posNode False {parentX = 0, argsX = 0, childrenX = 0, siblingX = paramSpacing}

posChild : PosFn
posChild = posNode False {parentX = paramSpacing, argsX = blockIndent, childrenX = 0, siblingX = paramSpacing}

posParent : PosFn
posParent = posNode False {parentX = 0, argsX = blockIndent, childrenX=blockIndent, siblingX = paramSpacing}

posNode : Force -> Spacing -> DepType -> Node -> TraversalInfo -> TraversalInfo
posNode force spacing depType n ((x, y, maxX, maxY, m) as ti) =
  if not force && seen m n
  then (x,y,maxX,maxY,m)
  else
    let m2 = markAsSeen m depType n

        (maxXps, maxYps, m3) = posParents m2 depType n (x + spacing.parentX) y

        nextY = maxYps + ySpacing
        m4 = position m3 depType n x nextY

        (maxXas, maxYas, m5) = posArgs m4 depType n (x + spacing.argsX) nextY
        (maxXcs, maxYcs, m6) = posChildren m5 depType n (x + spacing.childrenX) maxYas

        rightmostX = max5 (x + N.nodeWidth n) maxXps maxXas maxXcs maxX -- not propaged, need max of all
        bottomMostY = max maxYcs maxY -- maxYcs is propagated the whole way from y
    in
      (rightmostX + spacing.siblingX, y, rightmostX, bottomMostY, m6)

max5 : Int -> Int -> Int -> Int -> Int -> Int
max5 v w x y z = max x y |> max z |> max w |> max v



------------
-- avoiding merge conflicts by putting this here for now
------------
replaceArgEdge : Model -> IsBlockEdge -> Node -> Node -> Node -> Node
replaceArgEdge m isBlock arg toRemove toReplace =
  { arg | arguments =
    List.map
      (\(p,a) ->
        case a of
          Edge id b -> if id == toRemove.id
                       then (p, Edge toReplace.id isBlock)
                       else (p, a)
          _ -> (p,a))
      arg.arguments }



removeArg : Model -> Node -> (Node, Node)
removeArg m arg =
  let blockFn = incomingNodes m arg |> Util.hdExn
      child = outgoingNodes m arg |> Util.hdExn
      newChild = replaceArgEdge m True child arg blockFn
      toRemove = arg
      toUpdate = newChild
  in (toRemove, toUpdate)

collapseArgsWithSoloChildren : Model -> Model
collapseArgsWithSoloChildren m =
  let args = m.nodes |> Dict.values |> List.filter N.isArg
      isHideable n = outgoingNodes m n |> List.length |> (==) 1
      hideableArgs = args |> List.filter isHideable
      processed = List.map (removeArg m) hideableArgs
      (toRemove, toUpdate) = List.unzip processed
  in updateAndRemove m toUpdate toRemove


