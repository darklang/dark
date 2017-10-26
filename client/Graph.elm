module Graph exposing (..)

-- builtin
import Char
import Ordering
import List
import Tuple
import Dict
import Set
import Maybe

-- lib
import List.Extra as LE
import Maybe.Extra as ME

-- dark
import Types exposing (..)
import Defaults
import Viewport
import Util exposing (deMaybe, int2letter, letter2int)

gen_id : () -> ID
gen_id _ = ID (Util.random ())

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

isRoot : Node -> Bool
isRoot n =
  case n.pos of
    Root _ -> True
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

posx : Model -> Node -> Int
posx m n = (pos m n).x

posy : Model -> Node -> Int
posy m n = (pos m n).y

isArg : Node -> Bool
isArg n = n.tipe == Arg

isNotArg : Node -> Bool
isNotArg = not << isArg

isBlock : Node -> Bool
isBlock n = n.tipe == Block

isNotBlock : Node -> Bool
isNotBlock = not << isBlock

isFunctionCall : Node -> Bool
isFunctionCall n = n.tipe == FunctionCall

isNotFunctionCall : Node -> Bool
isNotFunctionCall = not << isFunctionCall

nodeWidth : Node -> Int
nodeWidth n =
  let
    space = 3.5
    fours = Set.fromList ['i', 'l', '[', ',', ']', 'l', ':', '/', '.', ' ', ',', '{', '}']
    fives = Set.fromList ['I', 't', Char.fromCode 34 ] -- '"'
    len name = name
             |> String.toList
             |> List.map (\c -> if c == ' '
                                then 3.5
                                else if Set.member c fours
                                     then 4.0
                                     else if Set.member c fives
                                          then 5.0
                                          else 8.0)
             |> List.sum
    paramLen = n.arguments
               |> List.map (\(p, a) ->
                 if p.tipe == TBlock then -space -- remove spaces
                 else
                   case a of
                     Const c -> if c == "null" then 8 else (len c)
                     _ -> 14)
               |> List.sum
    -- nameMultiple = case n.tipe of
    --                  Datastore -> 2
    --                  Page -> 2.2
    --                  _ -> 1
    width = 6.0 + len n.name + paramLen + (n.arguments |> List.length |> toFloat |> (+) 1.0 |> (*) space)
  in
    round(width)

nodeHeight : Node -> Int
nodeHeight n =
  case n.tipe of
    Datastore -> Defaults.nodeHeight * ( 1 + (List.length n.fields))
    _ -> Defaults.nodeHeight

nodeSize : Node -> (Int, Int)
nodeSize node =
  (nodeWidth node, nodeHeight node)

blockNodes : Model -> List Node
blockNodes m =
  m.nodes
    |> Dict.values
    |> List.filter isBlock

orderedNodes : Model -> List Node
orderedNodes m =
  m.nodes
  |> Dict.values
  |> List.filter isNotBlock
  |> List.map (\n -> (posx m n, posy m n, n.id |> deID))
  |> List.sortWith Ordering.natural
  |> List.map (\(_,_,id) -> getNodeExn m (ID id))

distance : Model -> Node -> Node -> Float
distance m n1 n2 =
  let xdiff = toFloat (posx m n2 - posx m n1) ^ 2
      ydiff = toFloat (posy m n2 - posy m n1) ^ 2
  in
    sqrt (xdiff + ydiff)


implicitPlaceholderLetter : String
implicitPlaceholderLetter = "_"

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

getArgument : ParamName -> Node -> Argument
getArgument pname n =
  case LE.find (\(p, _) -> p.name == pname) n.arguments of
    Just (_, a) -> a
    Nothing ->
      Debug.crash <| "Looking for a name which doesn't exist: " ++ pname ++ toString n

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
  let searchParents = List.foldl (\curr accum -> case accum of
    Just result -> Just result
    Nothing -> findParentBlock m curr) Nothing
  in
    case n.tipe of
      Arg -> Maybe.map (getNodeExn m) n.blockID
      _ -> searchParents (incomingNodes m n)

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
  -- e({a,b,c,f,g,h,d}, h) -> ({a,b,c,f,g,h,d,e}, h)
  -- x({a,b,c,f,g,h,d,e}, h) -> ({a,b,c,f,g,h,d,e,x}, x)




-- we have a sorting problem in how we draw nodes
-- we sometimes get the nice if block indentation that we want, but only
-- if we position the if first (ie. it's the first node in the list returned by outgoingNodes)
-- if not, we get the bog standard no indentation version
-- TODO: we need to come up with some actual rules for this
outgoingNodes : Model -> Node -> List Node
outgoingNodes m parent =
  m.nodes
    |> Dict.values
    |> List.filterMap (\child ->
                         child
                      |> incomingNodePairs m
                      |> List.map Tuple.first
                      |> LE.find (\n -> n.id == parent.id)
                      |> Maybe.map (always child))
    |> List.append (getArgsOf m parent.id)
    |> List.sortWith (\a b ->
                          case (a.name, b.name) of
                          ("if", "if") -> EQ
                          ("if", _)    -> LT
                          (_, "if")    -> GT
                          _            -> EQ)

incomingNodePairs : Model -> Node -> List (Node, ParamName)
incomingNodePairs m n = List.filterMap
                    (\(p, a) ->
                       case a of
                         Edge id -> Just (getNodeExn m id, p.name)
                         _ -> Nothing)
                    n.arguments

incomingNodes : Model -> Node -> List Node
incomingNodes m n =
  incomingNodePairs m n
  |> List.map Tuple.first
  |> List.append (getCallerOf m n.id |> ME.toList)

connectedNodes : Model -> Node -> List Node
connectedNodes m n =
  let candidates = incomingNodes m n ++ outgoingNodes m n
  in
     List.filter .visible candidates

getCallerOf : Model -> ID -> Maybe Node
getCallerOf m id =
  id
  |> getNodeExn m
  |> .blockID
  |> Maybe.andThen (\block -> getNodeExn m block
                             |> outgoingNodes m
                             |> List.head)

getArgsOf : Model -> ID -> List Node
getArgsOf m id =
  id
  |> getBlockNodesOf m
  |> List.map (\block -> block |> .argIDs |> List.map (getNodeExn m))
  |> List.concat

getBlockNodesOf : Model -> ID -> List Node
getBlockNodesOf m id =
  id
  |> getNodeExn m
  |> incomingNodePairs m
  |> List.filter (\(n, _) -> isBlock n)
  |> List.map Tuple.first

hasBlockParam : Model -> ID -> Bool
hasBlockParam m id =
  id
  |> getNodeExn m
  |> incomingNodePairs m
  |> List.any (\(n, _) -> isBlock n)

entireSubgraph : Model -> Node -> List Node
entireSubgraph m start =
  fold (\n list -> n :: list) [] start (\n -> incomingNodes m n ++ outgoingNodes m n)

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

-- Only follows the first edge, goes as high as it can. This is intended
-- to allow more consistency. Once it hit a node with no parents, it's
-- done (even if the parent's sibling might have a higher parent).
-- IGNORES BLOCKS.
highestParent : Model -> Node -> Node
highestParent m n =
  case incomingNodes m n |> List.filter isNotBlock |> List.head of
    Just node -> highestParent m node
    Nothing -> n

-- ported from backend
dependentNodes : Model -> Node -> List ID
dependentNodes m n =
  n.arguments
    |> List.filterMap (\(p, a) ->
                          case (p.tipe, a) of
                          (TBlock, Edge id) -> Just id
                          _ -> Nothing)
    |> List.append n.argIDs
    |> List.append (ME.toList n.blockID)
    |> List.append (if n.tipe == Block
                    then outgoingNodes m n |> List.map .id
                    else [])
    |> LE.uniqueBy deID
    |> Debug.log "clearing dependents"

deleteArg : (Argument -> Bool) -> Node -> Node
deleteArg cond n =
  let args = List.filter (\(_, a) -> not (cond a)) n.arguments
  in { n | arguments = args }

-- ported from backend
nodesForDeletion : Model -> ID -> List ID
nodesForDeletion m id =
  let n = getNodeExn m id
      deps = dependentNodes m n
      -- recursion will be messy, but go 3 layers down.
      transitive =
        List.map (\id -> dependentNodes m (getNodeExn m id)) deps
      transitive2 =
        transitive
        |> List.concat
        |> List.map (\id -> dependentNodes m (getNodeExn m id))
      transitive3 =
        transitive2
        |> List.concat
        |> List.map (\id -> dependentNodes m (getNodeExn m id))
  in
      transitive3
      |> List.concat
      |> (++) deps
      |> (::) id
      |> LE.uniqueBy deID


deleteNode : Model -> ID -> Model
deleteNode m id =
  let ids = nodesForDeletion m id

      -- remove the nodes
      remaining = Dict.filter (\_ n -> not <| List.member n.id ids) m.nodes

      -- remove any args pointing to the nodes
      nodes = Dict.map
               (\_ n ->
                  List.foldl
                    (\id n -> deleteArg ((==) (Edge id)) n)
                    n
                    (n.id :: ids)
               )
               remaining
  in { m | nodes = nodes }



-- Considerations to postioning the graph
--
-- there may be a set of nodes a,b,c, where a->b and b->c and a->c.
-- This is challenging because as we don't want to draw b over the line
-- between a->c, nor do we want to put b and c on the same horizontal
-- line.

-- We largely want the bulk of the code to be aligned in a single
-- pipeline. This can be challenging

-- don't consider the width of the parent in the positioning of the
-- children.

-- graph algorithm:
-- - place root node
--   - call repositionDown on the root

-- repositionDown node:
-- - go down a level
-- - for each argument of the parent
--   - call repositionChild (x+=blockindent), returning the x position of the next argument
--   - return the maximum depth of the rendered nodes
-- - for each child of the parent
--   - call repositionChild (y=max depth), returning the x position of the next child

-- repositionChild node:
--   - for each parent of the node
--     - call repositionUp, returning the new y position for this node
--   - place node
--   - callRepositionDown on this node

-- repositionUp node:
--   - for each unpositioned parent of the node
--     - repositionUp parent
--   - place node
--   - return new depth
-- TODO: hard to tell when two lines overlap each other
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
      positions = List.repeat (List.length frees) 100 |> LE.scanl1 (+) |> Debug.log "positions"
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

position : Model -> DepType -> Node -> Int -> Int -> Model
position m depType n x y =
  let _ = Debug.log ("Positioning " ++ (toString n.id)) (x, y) in
  let pos = {x=x,y=y}
      newPos = case n.pos of
                 Root _ -> Root pos
                 Free _ -> Free <| Just (Viewport.toViewport m pos)
                 Dependent _ -> if depType == FreeDep
                                then Dependent <| Just <| DVPos <| (Viewport.toViewport m pos)
                                else Dependent <| Just <| DPos <| pos
                 NoPos _ -> NoPos (Just pos)
      nodes = Dict.insert (deID n.id) { n | pos = newPos} m.nodes
  in {m | nodes = nodes }

type NextX = NX Int
nx : NextX -> Int
nx p =
  case p of
    NX b -> b

type MaxX = MX Int
mx : MaxX -> Int
mx p =
  case p of
    MX b -> b

type MaxY = MY Int
my : MaxY -> Int
my p =
  case p of
    MY b -> b

type PrevX = PX Int
px : PrevX -> Int
px p =
  case p of
    PX b -> b

type PrevY = PY Int
py : PrevY -> Int
py p =
  case p of
    PY b -> b


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
-- PrevY indicates where our box starts. It is actually the y position of the node placed
-- The algorithm requires us to iterate through empty lists, and so we don't
-- want to increase the y spacing until the last minute to place ourselves.

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

type alias TraversalInfo = (NextX, PrevY, MaxX, MaxY, Model)
type alias SpaceInfo = (MaxX, MaxY, Model)
type alias Force = Bool
type alias Spacing = { parentX : Int
                     , argsX: Int
                     , childrenX: Int
                     , siblingX: Int}

debug : Model -> String -> Node -> TraversalInfo -> ()
debug m fn n (x, y, maxX, maxY, _) =
  let _ = Debug.log (fn ++ " " ++ n.name ++ " (" ++ "seen: " ++ (seen m n |> toString) ++ ")") (x, y, maxX, maxY) in ()

posNode : Force -> Spacing -> DepType -> Node -> TraversalInfo -> TraversalInfo
posNode force spacing depType n ((x, y, maxX, maxY, m) as ti) =
  if not force && seen m n then (x,y,maxX,maxY,m)
  else
    let m2 = markAsSeen m depType n in
    let (maxXps, maxYps, m3) = posParents m2 depType n (NX <| nx x + spacing.parentX) y
        nextY = (my maxYps) + ySpacing
        m4 = position m3 depType n (nx x) nextY

        (maxXas, maxYas, m5) = posArgs m4 depType n (NX <| nx x+spacing.argsX) (PY nextY)
        (maxXcs, maxYcs, m6) = posChildren m5 depType n (NX <| (nx x) + spacing.childrenX) ((PY << my) maxYas)

        newX = (max5 (nx x + nodeWidth n) (mx maxXps) (mx maxX) (mx maxXas) (mx maxXcs))
        maxDrawnX = MX newX
        overallMaxY = MY (max5 (my maxYps) (my maxY) (py y) (my maxYas) (my maxYcs))
    in (NX <| newX+spacing.siblingX, y, maxDrawnX, overallMaxY, m6)

posArgs : Model -> DepType -> Node -> NextX -> PrevY -> SpaceInfo
posArgs m depType n x y =
  let args = outgoingNodes m n |> List.filter isArg
      (_, _, maxX, maxY, m2) = List.foldl (posArg depType) (x, y, (MX << nx) x, (MY << py) y, m) args
      -- _ = debug m "posArgs" n (x, y, MX -1, MY -1, m)
  in (maxX, maxY, m2)

-- if there are no children to pos, then passed NextY is returned as MaxY
posChildren : Model -> DepType -> Node -> NextX -> PrevY -> SpaceInfo
posChildren m depType n x y =
  let children = outgoingNodes m n |> List.filter isNotArg
      (_, _, maxX, maxY, m2) = List.foldl (posChild depType) (x, y, (MX << nx) x, (MY << py) y, m) children
  in (maxX, maxY, m2)

posParents : Model -> DepType -> Node -> NextX -> PrevY -> SpaceInfo
posParents m depType n x y =
  let parents = incomingNodes m n |> List.filter isNotBlock
      (_, _, maxX, maxY, m2) = List.foldl (posParent depType) (x, y, (MX << nx) x, (MY << py) y, m) parents
  in (maxX, maxY, m2)

posRoot : Model -> DepType -> Node -> Int -> Int -> Model
posRoot m depType n x y =
  let (_, _, _, _, m2) = posRoot_ depType n (NX x, PY (y-ySpacing), MX x, MY (y-ySpacing), m)
  in m2

posRoot_ : DepType -> Node -> TraversalInfo -> TraversalInfo
posRoot_ = posNode True {parentX = 0, argsX = blockIndent, childrenX = 0, siblingX=0}


posArg : DepType -> Node -> TraversalInfo -> TraversalInfo
posArg = posNode False {parentX = 0, argsX = 0, childrenX = 0, siblingX=paramSpacing}

posChild : DepType -> Node -> TraversalInfo -> TraversalInfo
posChild = posNode False {parentX = paramSpacing, argsX = blockIndent, childrenX = 0, siblingX=paramSpacing}

posParent : DepType -> Node -> TraversalInfo -> TraversalInfo
posParent = posNode False {parentX = 0, argsX = blockIndent, childrenX=blockIndent, siblingX=paramSpacing}

max3 : Int -> Int -> Int -> Int
max3 x y z = max x y |> max z
max4 : Int -> Int -> Int -> Int -> Int
max4 w x y z = max x y |> max z |> max w
max5 : Int -> Int -> Int -> Int -> Int -> Int
max5 v w x y z = max x y |> max z |> max w |> max v

validate : Model -> Result (List String) (List ())
validate m =
  m.nodes
  |> Dict.values
  |> List.map
      (\n -> if isBlock n
              then Ok ()
              else if notPositioned n
              then Err ("unpositioned node", n)
              else if posx m n == Defaults.unsetInt || posy m n == Defaults.unsetInt
              then Err ("in hell", n)
              else Ok ()
              -- TODO no nodes overlap
              -- TODO any rules about space between nodes
      )
  |> Util.combineResult
  |> Result.mapError (List.map (\(name, node) -> name ++ ": " ++ (toString node)))

