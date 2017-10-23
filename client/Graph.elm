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

pos : Node -> Pos
pos n =
  case n.pos of
    Root pos -> pos
    Free (Just pos) -> pos
    Dependent (Just pos) -> pos
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

posx : Node -> Int
posx n = (pos n).x

posy : Node -> Int
posy n = (pos n).y

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
    |> List.map (\n -> (posx n, posy n, n.id |> deID))
    |> List.sortWith Ordering.natural
    |> List.map (\(_,_,id) -> getNodeExn m (ID id))

distance : Node -> Node -> Float
distance n1 n2 =
  let xdiff = toFloat (posx n2 - posx n1) ^ 2
      ydiff = toFloat (posy n2 - posy n1) ^ 2
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

-- TODO: hard to tell when two lines overlap each other


type alias Layout = List LRoot

-- A root: args, children, cant have parents
-- Some nodes are "Roots", that is, they have an explicit location.
-- Almost all other nodes hang from them and do not have locations. We
-- infer those locations of the hanger-ons in repositionChildren, going
-- from the roots. However, when a child (non-root) node adds an
-- attached parent (eg an if statement adds a condition, or a + adds
-- another + as a param), then the new nodes need to be positioned too.
-- So we go up til we find an already positioned node, then reposition
-- off that.
-- Classic example:
--  code = toAsciiCode; if code > 110 then code - 13 else code + 13
-- THis code is shaped like this: toAScii -> `>`, toAscii -> `if`,
-- `>` -> `if`. If we place the `if` first, then we have no place for
-- the `>`.
-- Note: a topological sort would be another solution here.
type LRoot = LRoot Node (List LArg) (List LChild)

-- A normal node, usually a function call. Has args, children, unattached parents
type LChild = LChild Node (List LArg) (List LChild) (List LParent)

-- Args belong to blocks, and can only have children
type LArg = LArg Node (List LChild)

-- A parent is not reachable from the root, but it can reach children of
-- the root. Technically these could have children, but let's simplify
-- for now. We could do this by passing a set of already seen nodes.
type LParent = LParent Node (List LParent)

type alias NextX = Int
type alias NextY = Int
type alias MaxX = Int
type alias MaxY = Int

--       roots = List.filter isRoot (Dict.values m.nodes)
--       frees = List.filter isFree (Dict.values m.nodes)
-- repositionLayout m (List.map (root2layout m) roots)
--       m3 = repositionLayout m (List.map (root2layout m) frees)

repositionRoots : Model -> Model
repositionRoots m =
  let roots = List.filter isRoot (Dict.values m.nodes)
  in repositionLayout m (List.map (root2layout m) roots)

repositionFrees : Model -> Model
repositionFrees m =
  m
  -- let frees = List.filter isFree (Dict.values m.nodes)
  -- in repositionLayout m (List.map (root2layout m) frees)


repositionLayout : Model -> Layout -> Model
repositionLayout m roots =
  List.foldl (\r m -> let (LRoot n _ _) = r
                      in posRoot m r (posx n) (posy n)) m roots

seen : Model -> Node -> Bool
seen m n = case Dict.get (deID n.id) m.nodes of
  Nothing -> False
  Just n -> isPositioned n

position : Model -> Node -> Int -> Int -> Model
position m n x y =
  let pos = {x=x,y=y}
      newPos = case n.pos of
                 Root _ -> Root pos
                 Free _ -> Free (Just pos)
                 Dependent _ -> Dependent (Just pos)
                 NoPos _ -> NoPos (Just pos)
      nodes = Dict.insert (deID n.id) { n | pos = newPos} m.nodes
  in {m | nodes = nodes }

max3 : Int -> Int -> Int -> Int
max3 x y z = max x y |> max z
max4 : Int -> Int -> Int -> Int -> Int
max4 w x y z = max x y |> max z |> max w

posRoot : Model -> LRoot -> Int -> Int -> Model
posRoot m (LRoot n args children) x y =
  let m2 = position m n x y
      (_, nextY, _, _, m3) = posArgs m2 args x y
      (_, _, _, _, m4) = posChildren m3 children x nextY
  in m4

type alias TraversalInfo = (NextX, NextY, MaxX, MaxY, Model)
posArg : LArg -> TraversalInfo -> TraversalInfo
posArg (LArg n children) (x, y, maxX, maxY, m) =
  if seen m n then (x,y,x,y,m)
  else
    let m2 = position m n x y in
    let (_, _, maxXcs, maxYcs, m3) = posChildren m2 children x y
        newX = (max3 (x + nodeWidth n) maxXcs maxX) + paramSpacing
    in (newX, y, newX, max maxYcs maxY, m3)

posArgs : Model -> List LArg -> NextX -> NextY -> TraversalInfo
posArgs m args x y =
  if List.length args == 0
  then (x, y, x, y, m)
  else List.foldl posArg (x+blockIndent, y+ySpacing, x+blockIndent, y+ySpacing, m) args

posChild : LChild -> TraversalInfo -> TraversalInfo
posChild (LChild n args children parents) (x, y, maxX, maxY, m) =
  if seen m n then (x,y,maxX,maxY,m)
  else
    let (_, nextY, maxXps, maxYps, m2) = posParents m parents (x+paramSpacing) y
        m3 = position m2 n x nextY

        -- blocks should be calculated before children, as we need to
        -- know how deep they are before we can start to position other
        -- outgoing nodes below them.
        (_, _, maxXas, maxYas, m4) = posArgs m3 args x maxYps
        (_, _, maxXcs, maxYcs, m5) = posChildren m4 children x maxYas
        newX = max4 maxXps maxXas maxXcs (x + nodeWidth n)
    in (newX+paramSpacing, y, newX, max maxYcs y, m5)

posChildren : Model -> List LChild -> NextX -> NextY -> TraversalInfo
posChildren m children x y =
  if List.length children == 0
  then (x,y,x,y,m)
  else List.foldl posChild (x, y, x, y+ySpacing, m) children

posParent : LParent -> TraversalInfo -> TraversalInfo
posParent (LParent n parents) (x, y, maxX, maxY, m) =
  if seen m n then (x,y,x,y,m)
  else
    -- TODO see if we can take the first position away
    let m2 = position m n x y in -- don't have position yet, but dont want to visit twice
    let (_, nextY, maxXps, maxYps, m3) = posParents m2 parents x y
        m4 = position m3 n x nextY
        newX = (max3 (x + nodeWidth n) maxXps maxX) + paramSpacing
    in (newX, y, newX, max maxYps maxY, m4)

posParents : Model -> List LParent -> NextX -> NextY -> TraversalInfo
posParents m parents x y =
  if List.length parents == 0
  then (x,y,x,y,m)
  else
    let (nextX, nextY, maxX, maxY, m2) = List.foldl posParent (x, y, x, y, m) parents
    in (nextX, maxY+ySpacing, maxX, maxY+ySpacing, m2)


root2layout : Model -> Node -> LRoot
root2layout m n =
  let outgoing = outgoingNodes m n
      args = List.filter isArg outgoing
      children = List.filter isNotArg outgoing
  in
    LRoot n
      (List.map (arg2layout m) args)
      (List.map (child2layout m) children)

-- we have a sorting problem in out arg2layout
-- we sometimes get the nice if block indentation that we want, but only
-- if we position the if first (ie. it's the first node in the list returned by outgoingNodes)
-- if not, we get the bog standard no indentation version
-- TODO: we need to come up with some actual rules for this
arg2layout : Model -> Node -> LArg
arg2layout m n =
  let outgoing = outgoingNodes m n
      sortedOutgoingNodes = List.sortWith (\a b ->
                              case (a.name, b.name) of
                              ("if", "if") -> EQ
                              ("if", _)    -> LT
                              (_, "if")    -> GT
                              _            -> EQ) outgoing
  in LArg n (List.map (child2layout m) sortedOutgoingNodes)

parent2layout : Model -> Node -> LParent
parent2layout m n =
  LParent n (List.map (parent2layout m) (incomingNodes m n))

child2layout : Model -> Node -> LChild
child2layout m n =
  let parents = incomingNodes m n -- TODO: filter by parents already posed
      outgoing = outgoingNodes m n
      args = List.filter isArg outgoing
      children = List.filter isNotArg outgoing
  in
  LChild n
    (List.map (arg2layout m) args)
    (List.map (child2layout m) children)
    (List.map (parent2layout m) parents)



validate : Model -> Result (List String) (List ())
validate m =
  m.nodes
  |> Dict.values
  |> List.map
      (\n -> if isBlock n
              then Ok ()
              else if notPositioned n
              then Err ("unpositioned node", n)
              else if posx n == Defaults.unsetInt || posy n == Defaults.unsetInt
              then Err ("in hell", n)
              else Ok ()
              -- TODO no nodes overlap
              -- TODO any rules about space between nodes
      )
  |> Util.combineResult
  |> Result.mapError (List.map (\(name, node) -> name ++ ": " ++ (toString node)))

