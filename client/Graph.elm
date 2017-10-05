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
-- import Tuple3

-- dark
import Types exposing (..)
import Defaults
import Util exposing (deMaybe)

gen_id : () -> ID
gen_id _ = ID (Util.random ())

hasNoPos : Node -> Bool
hasNoPos n = n.pos == {x=Defaults.unsetPosition,y=Defaults.unsetPosition}

hasPos : Node -> Bool
hasPos = hasNoPos >> not

isArg : Node -> Bool
isArg n = n.tipe == Arg

isNotArg : Node -> Bool
isNotArg n = n.tipe /= Arg

isAnon : Node -> Bool
isAnon n = n.tipe == FunctionDef

isNotAnon : Node -> Bool
isNotAnon n = n.tipe /= FunctionDef



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
    paramLen = args n
               |> List.map (\(p, a) ->
                 if p.tipe == "Function" then -space -- remove spaces
                 else
                   case a of
                     Const c -> if c == "null" then 8 else (len c)
                     _ -> 14)
               |> List.sum
    -- nameMultiple = case n.tipe of
    --                  Datastore -> 2
    --                  Page -> 2.2
    --                  _ -> 1
    width = 6.0 + len n.name + paramLen + (args n |> List.length |> toFloat |> (+) 1.0 |> (*) space)
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



anonNodes : Model -> List Node
anonNodes m =
  m.nodes
    |> Dict.values
    |> List.filter (\n -> n.tipe == FunctionDef)

orderedNodes : Model -> List Node
orderedNodes m =
  m.nodes
    |> Dict.values
    |> List.filter (\n -> n.tipe /= FunctionDef)
    |> List.map (\n -> (n.pos.x, n.pos.y, n.id |> deID))
    |> List.sortWith Ordering.natural
    |> List.map (\(_,_,id) -> getNodeExn m (ID id))

distance : Node -> Node -> Float
distance n1 n2 =
  let xdiff = toFloat (n2.pos.x - n1.pos.x) ^ 2
      ydiff = toFloat (n2.pos.y - n1.pos.y) ^ 2
  in
    sqrt (xdiff + ydiff)

int2letter : Int -> String
int2letter i = 'a' |> Char.toCode |> (+) i |> Char.fromCode |> String.fromChar

letter2int : String -> Int
letter2int s = s |> String.uncons |> Maybe.withDefault ('!', "") |> Tuple.first |> Char.toCode |> (-) (Char.toCode 'a') |> (*) (-1)

fromLetter : Model -> String -> Maybe Node
fromLetter m letter = m |> orderedNodes |> LE.getAt (letter2int letter)

toLetter : Model -> ID -> String
toLetter m id = m |> orderedNodes |> LE.findIndex (\n -> n.id == id) |> deMaybe |> int2letter

getNode : Model -> ID -> Maybe Node
getNode m id = Dict.get (deID id) m.nodes

getNodeExn : Model -> ID -> Node
getNodeExn m id = getNode m id |> deMaybe

getArgument : ParamName -> Node -> Argument
getArgument pname n =
  case LE.find (\(p, _) -> p.name == pname) (args n) of
    Just (_, a) -> a
    Nothing ->
      Debug.crash <| "Looking for a name which doesn't exist: " ++ pname ++ toString n

args : Node -> List (Parameter, Argument)
args n =
  Util.zip n.parameters n.arguments

findParam : Node -> Maybe (Int, (Parameter, Argument))
findParam n = Util.findIndex (\(_, a) -> a == NoArg) (args n)

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

findParentAnon : Model -> Node -> Maybe Node
findParentAnon m n =
  let searchParents = List.foldl (\curr accum -> case accum of
    Just result -> Just result
    Nothing -> findParentAnon m curr) Nothing
  in
    case n.tipe of
      Arg -> Maybe.map (getNodeExn m) n.anonID
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
                    (args n)

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
  |> .anonID
  |> Maybe.andThen (\anon -> getNodeExn m anon
                             |> outgoingNodes m
                             |> List.head)

getArgsOf : Model -> ID -> List Node
getArgsOf m id =
  id
  |> getAnonNodesOf m
  |> List.map (\anon -> anon |> .argIDs |> List.map (getNodeExn m))
  |> List.concat

getAnonNodesOf : Model -> ID -> List Node
getAnonNodesOf m id =
  id
  |> getNodeExn m
  |> incomingNodePairs m
  |> List.filter (\(n, _) -> n.tipe == FunctionDef)
  |> List.map Tuple.first

hasAnonParam : Model -> ID -> Bool
hasAnonParam m id =
  id
  |> getNodeExn m
  |> incomingNodePairs m
  |> List.any (\(n, _) -> n.tipe == FunctionDef)

entireSubgraph : Model -> Node -> List Node
entireSubgraph m start =
  fold (\n list -> n :: list) [] start (connectedNodes m)

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
  let roots = List.filter hasPos (Dict.values m.nodes)
      result = repositionLayout m (graph2layout m)

      -- result = List.foldl
      --          (\r m2 ->
      --            let _ = Debug.log ("starting at root: " ++ r.name) r.pos in
      --            repositionDown m2 r r.pos |> Tuple3.third)
      --          m
      --          roots
      --
      _ = Dict.map (\k n -> if hasNoPos n
                               then Debug.log "missed one" n
                               else n)
                   result.nodes
  in
     result


paramSpacing : Int
paramSpacing=15
blockIndent: Int
blockIndent=20
ySpacing : Int
ySpacing=30


setPos : Model -> Node -> Pos -> Model
setPos m n pos =
  let newNode = { n | pos = pos }
  in if hasPos n
        then m
        else { m | nodes = Dict.insert (n.id |> deID) newNode m.nodes}


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
repositionUp : Model -> Node -> Pos -> (Model, Pos, Int)
repositionUp m n pos =
  -- we only want to increase Y when this returns. However, the fold
  -- goes across the parents, so we don't want to increase Y on the
  -- fold.
  let parents = List.filter isNotAnon <| incomingNodes m n
      unpositioned = List.filter hasNoPos parents
      (mPositioned, parentPos, maxY) =
        List.foldl
          (\par (m,p,maxInY) ->
            let (mPos, newPos, maxUpY) = repositionUp m par p
            in (mPos, { x=newPos.x+nodeWidth par+paramSpacing,y=pos.y}
                      , max maxInY (maxUpY + ySpacing)))
          (m, pos, pos.y)
          unpositioned
      nPos = {x=pos.x, y=maxY} -- when we are finished
  in
    (setPos mPositioned n nPos, nPos, maxY) -- when we come down

-- given a `root` starting point, and `pos` (the position of the root),
-- return the new position of the node, alongside the (unupdated) node,
-- the maximum depth that has been achieved, and the maximum width found
-- when exploring depth-first from the root.
repositionDown : Model -> Node -> Pos -> (Int, Int, Model)
repositionDown m root pos =
  let -- TODO: hard to tell when two lines overlap each other
      debug4 str (a,b,c,d) = let _ = Debug.log str (a,b,c) in (a,b,c,d)
      debug3 str (a,b,c) = let _ = Debug.log str (a,b) in (a,b,c)

      -- a function to fold across sets of nodes. It keeps track of
      -- where to place nodes, as well as the maximum height that has
      -- been achieved, which we use to place nodes below it later
      rePos n (startX, startY, _, mStart) =
        let
          startPos = {x=startX, y=startY}
          (mParents, newPos, maxUpY) = repositionUp mStart n {x=startPos.x+paramSpacing, y=startPos.y}
          (maxX, maxDownY, mChildren) = repositionDown mParents n {newPos | y=maxUpY}
          newX = max maxX (startX + nodeWidth n + paramSpacing)
        in
          -- next node in this row should be over to the right, on the
          -- same line.
          (newX, startY, maxDownY, mChildren)

      -- anonymous functions need to get placed with the function
      -- they're with, so just update them to this node's position.
      mAnons =
        List.foldl
          (\anon model -> setPos model anon pos)
          m
          (getAnonNodesOf m root.id)

      -- blocks should be indented
      startingX = if hasAnonParam m root.id then pos.x + blockIndent else pos.x
      startingY = pos.y + ySpacing

      -- blocks should be calculated first, as we need to know how deep
      -- they are before we can start to position other outgoing nodes
      -- below them.
      argChildren = List.filter isArg (outgoingNodes mAnons root)
      (argX, _, argY, mArgs) =
        List.foldl
          rePos
          (startingX, startingY, startingY, mAnons)
          argChildren

      -- position back on the baseline, but below all the other nodes
      nonArgChildren = List.filter isNotArg (outgoingNodes mArgs root)
      (nonArgX, _, nonArgY, mNonArgs) =
        List.foldl
          rePos
          (pos.x, argY, argY, mArgs)
          nonArgChildren

  in
    (max argX nonArgX, max argY nonArgY, mNonArgs)



type alias Layout = List LRoot
-- A root: args, children, cant have parents

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

repositionLayout : Model -> Layout -> Model
repositionLayout m roots =
  let pRoots = roots
               |> List.map (\r -> let (LRoot n _ _) = r
                                  in posRoot m r n.pos)
               |> Debug.log "roots"
               |> List.concat
               |> List.map (\n -> (n.id |> deID, n))
               |> Dict.fromList
               |> Debug.log "new nodes"
      newNodes = Dict.union pRoots m.nodes
  in
    { m | nodes = newNodes }

inSet : IDSet -> Node -> Bool
inSet set n = Set.member (deID n.id) set
addSet : IDSet -> Node -> IDSet
addSet set n = Set.insert (deID n.id) set
max3 : Int -> Int -> Int -> Int
max3 x y z = max x y |> max z
max4 : Int -> Int -> Int -> Int -> Int
max4 w x y z = max x y |> max z |> max w

posRoot : Model -> LRoot -> Pos -> List Node
posRoot m (LRoot n args children) pos =
  let set = addSet Set.empty n
      (_, nextY, _, _, aNodes, set2) = posArgs m set args pos.x pos.y
      (_, _, _, _, cNodes, _) = posChildren m set2 children pos.x nextY
      newN = { n | pos = pos }
  in n :: aNodes ++ cNodes

type alias TraversalInfo = (NextX, NextY, MaxX, MaxY, List Node, IDSet)
posArg : Model -> LArg -> TraversalInfo -> TraversalInfo
posArg m (LArg n children) (x, y, maxX, maxY, nodes, set) =
  if inSet set n then (x,y,x,y,nodes,set)
  else
    let set2 = addSet set n in
    let (_, _, maxXcs, maxYcs, cNodes, set3) = posChildren m set2 children x y
        newX = (max3 (x + nodeWidth n) maxXcs maxX) + paramSpacing
    in (newX, y, newX, max maxYcs maxY, {n | pos={x=x,y=y}} :: nodes ++ cNodes, set3)

posArgs : Model -> IDSet -> List LArg -> NextX -> NextY -> TraversalInfo
posArgs m set args x y =
  if List.length args == 0
  then (x, y, x, y, [], set)
  else List.foldl (posArg m) (x+blockIndent, y+ySpacing, x+blockIndent, y+ySpacing, [], set) args

posChild : Model -> LChild -> TraversalInfo -> TraversalInfo
posChild m (LChild n args children parents) (x, y, maxX, maxY, nodes, set) =
  if inSet set n then (x,y,maxX,maxY,nodes, set)
  else
    let (_, nextY, maxXps, maxYps, pNodes, set2) = posParents m set parents (x+paramSpacing) y
        newN = { n | pos={x=x, y=nextY} }
        set3 = addSet set2 n
        (_, _, maxXas, maxYas, aNodes, set4) = posArgs m set3 args x maxYps
        (_, _, maxXcs, maxYcs, cNodes, set5) = posChildren m set4 children x maxYas
        newX = max4 maxXps maxXas maxXcs (x + nodeWidth n)
    in (newX+paramSpacing, y, newX, max maxYcs y, newN :: nodes ++ aNodes ++ cNodes ++ pNodes, set5)

posChildren : Model -> IDSet -> List LChild -> NextX -> NextY -> TraversalInfo
posChildren m set children x y =
  if List.length children == 0
  then (x,y,x,y,[], set)
  else List.foldl (posChild m) (x, y+ySpacing, x, y+ySpacing, [], set) children

posParent : Model -> LParent -> TraversalInfo -> TraversalInfo
posParent m (LParent n parents) (x, y, maxX, maxY, nodes, set) =
  if inSet set n then (x,y,x,y,nodes,set)
  else
    let set2 = addSet set n in
    let (_, nextY, maxXps, maxYps, pNodes, set3) = posParents m set2 parents x y
        newN = { n | pos={x=x,y=nextY} }
        newX = (max3 (x + nodeWidth n) maxXps maxX) + paramSpacing
    in (newX, y, newX, max maxYps maxY, newN :: nodes ++ pNodes, set3)

posParents : Model -> IDSet -> List LParent -> NextX -> NextY -> TraversalInfo
posParents m set parents x y =
  if List.length parents == 0
  then
  (x,y,x,y,[],set)
  else
    let (nextX, nextY, maxX, maxY, pNodes, set2) = List.foldl (posParent m) (x, y, x, y, [], set) parents
    in (nextX, maxY+ySpacing, maxX, maxY+ySpacing, pNodes, set2)


--
graph2layout : Model -> Layout
graph2layout m =
  let roots = List.filter hasPos (Dict.values m.nodes)
  in List.map (root2layout m) roots

root2layout : Model -> Node -> LRoot
root2layout m n =
  let outgoing = outgoingNodes m n
      args = List.filter isArg outgoing
      children = List.filter isNotArg outgoing
  in
    LRoot n
      (List.map (arg2layout m) args)
      (List.map (child2layout m) children)

arg2layout : Model -> Node -> LArg
arg2layout m n =
  LArg n (List.map (child2layout m) (outgoingNodes m n))

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



