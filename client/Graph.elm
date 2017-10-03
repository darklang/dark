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
import Tuple3

-- dark
import Types exposing (..)
import Defaults
import Util exposing (deMaybe)

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
                      |> LE.find ((==) parent)
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

reposition : Model -> NodeDict -> NodeDict
reposition m nodes =
  let roots = List.filter (\n -> n.pos.x /= -42) (Dict.values nodes)
      rePosed = roots
              |> List.map (\r -> repositionChildren m r r.pos |> Tuple3.third)
              |> List.concat

      result = List.foldl (\(node, pos) dict ->
                   Dict.update
                     (node.id |> deID)
                     (Maybe.map (\n -> { n | pos=pos}))
                     dict)
                nodes
                rePosed
      _ = Dict.map (\k n -> if n.pos == {x=-42,y=-42}
                               then Debug.log "missed one" n
                               else n)
                   result
  in
     result

-- given a `root` starting point, and `pos` (the position of the root),
-- return the new position of the node, alongside the (unupdated) node,
-- the maximum depth that has been achieved, and the maximum width found
-- when exploring depth-first from the root.
repositionChildren : Model -> Node -> Pos -> (Int, Int, List (Node, Pos))
repositionChildren m root pos =
  let
      -- a function to fold across sets of nodes. It keeps track of
      -- where to place nodes, as well as the maximum height that has
      -- been achieved, which we use to place nodes below it later
      rePosChild child (startX, startY, _, accumulatedNodes) =
        let
            newPos = {x=startX, y=startY}
            endX = startX + nodeWidth child
            _ = Debug.log "starting on child" (child.name, newPos)
            (maxChildX, maxChildY, children) = repositionChildren m child newPos
            maxX = max (endX+20) maxChildX
        in
           -- next node in this row should be over to the right, on the
           -- same line.
          (maxX, startY, maxChildY, (child, newPos) :: (children ++ accumulatedNodes))

      (argChildren, nonArgChildren) =
        List.partition (\n -> n.tipe == Arg) (outgoingNodes m root)

      -- blocks should be indented
      startingX = if hasAnonParam m root.id then pos.x + 30 else pos.x

      -- blocks should be calculated first, as we need to know how deep
      -- they are before we can start to position other outgoing nodes
      -- below them.
      (maxArgX, _, maxArgY, reArgChildren) =
        Debug.log ("after arg fold:" ++ root.name)
          (List.foldl rePosChild (startingX, pos.y+40, pos.y+40, []) argChildren)

      -- position back on the baseline, but below all the other nodes
      (maxNonArgX, _, maxNonArgY, reNonArgChildren) =
        Debug.log ("after non-arg fold:" ++ root.name)
          (List.foldl rePosChild (pos.x, maxArgY, maxArgY, []) nonArgChildren)

      -- anonymous functions need to get placed with the function
      -- they're with, so just update them to this node's position.
      anons = List.map (\anon -> (anon, pos)) (getAnonNodesOf m root.id)

      _ = Debug.log "placing node" (root.name, pos)
  in
    Debug.log ("result: " ++ root.name)
    (max maxArgX maxNonArgX, max maxArgY maxNonArgY, reArgChildren ++ reNonArgChildren ++ anons)

