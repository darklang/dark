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
  List.map2 (,) n.parameters n.arguments

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
                      |> incomingNodes m
                      |> LE.find ((==) parent)
                      |> Maybe.map (always child))

incomingNodePairs : Model -> Node -> List (Node, ParamName)
incomingNodePairs m n = List.filterMap
                    (\(p, a) ->
                       case a of
                         Edge id -> Just (getNodeExn m id, p.name)
                         _ -> Nothing)
                    (Util.zip n.parameters n.arguments)

incomingNodes : Model -> Node -> List Node
incomingNodes m n = incomingNodePairs m n |> List.map Tuple.first

connectedNodes : Model -> Node -> List Node
connectedNodes m n =
  let caller = getCallerOf m n.id
      arg = getFirstArgOf m n.id
      candidates = ME.toList caller
                   ++ ME.toList arg
                   ++ incomingNodes m n
                   ++ outgoingNodes m n
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

getFirstArgOf : Model -> ID -> Maybe Node
getFirstArgOf m id =
  id
  |> getAnonNodeOf m
  |> Maybe.andThen (\anon -> anon |> .argIDs |> List.head)
  |> Maybe.map (\arg -> getNodeExn m arg)

getAnonNodeOf : Model -> ID -> Maybe Node
getAnonNodeOf m id =
  id
  |> getNodeExn m
  |> incomingNodePairs m
  |> List.filter (\(n, _) -> n.tipe == FunctionDef)
  |> List.head
  |> Maybe.map Tuple.first

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
      rePosed = List.map (\r -> repositionChildren m r r.pos) roots |> List.concat
  in
     List.foldl (\(node, pos) dict ->
                   Dict.update
                     (node.id |> deID)
                     (Maybe.map (\n -> { n | pos=pos}))
                     dict)
                nodes
                rePosed


repositionChildren : Model -> Node -> Pos -> List (Node, Pos)
repositionChildren m root pos =
  let children = outgoingNodes m root
      rePos n prevWidth =
        (prevWidth + nodeWidth n, (n, {x=prevWidth, y=pos.y+40}))
      (_, repositioned) = List.foldr
                            (\n (width, list) ->
                               let (w, new) = rePos n width
                               in (w, new :: list))
                            (pos.x, [])
                            children
  in
     repositioned
     |> List.map (\(n, pos) -> repositionChildren m n pos)
     |> List.concat
     |> (++) repositioned

