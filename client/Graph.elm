module Graph exposing (..)

-- builtin
import Char
import Ordering
import List
import Tuple
import Dict
import Set

-- lib
import List.Extra as LE

-- dark
import Types exposing (..)
import Util exposing (deMaybe)



orderedNodes : Model -> List Node
orderedNodes m =
  m.nodes
    |> Dict.values
    |> List.map (\n -> (n.pos.x, n.pos.y, n.id |> deID))
    |> List.sortWith Ordering.natural
    |> List.map (\(_,_,id) -> Dict.get id m.nodes |> deMaybe)

distance : Node -> Node -> Float
distance n1 n2 =
  let xdiff = toFloat (n2.pos.x - n1.pos.x) ^ 2
      ydiff = toFloat (n2.pos.y - n1.pos.y) ^ 2
  in
    sqrt (xdiff + ydiff)

int2letter : Int -> String
int2letter i = 'a' |> Char.toCode |> (+) i |> Char.fromCode |> String.fromChar

letter2int : String -> Int
letter2int s = s |> String.uncons |> deMaybe |> Tuple.first |> Char.toCode |> (-) (Char.toCode 'a') |> (*) (-1)

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
      Debug.crash <| "Looking for a name which doesn't exist: " ++ pname ++ (toString n)

args : Node -> List (Parameter, Argument)
args n =
  List.map2 (,) n.parameters n.arguments


findHole : Model -> Node -> Hole
findHole _ n =
  case Util.findIndex (\(_, a) -> a == NoArg) (args n) of
    Nothing -> ResultHole n
    Just (i, (p, _)) -> ParamHole n p i

findArgHole : Model -> Node -> Maybe Hole
findArgHole _ n =
  case Util.findIndex (\(_, a) -> a == NoArg) (args n) of
    Nothing -> Nothing
    Just (i, (p, _)) -> Just (ParamHole n p i)

-- follow the graph to find all available holes. (I could have skipped
-- the seen-tracking, but I guess it's gottas be built sometime unless
-- we go to Elm.Graph)
type alias IDSet = Set.Set Int
findNextArgHole : Model -> Node -> Maybe Hole
findNextArgHole m n =
  find n (incomingNodes m) (findArgHole m)

-- find args going up. If there aren't any, look down. Hit the end?
-- Suggest it.
findNextHole : Model -> Node -> Maybe Hole
findNextHole m start =
  let findfn n =
        case findNextArgHole m n of
          Nothing -> case outgoingNodes m n of
                       [] -> Just (ResultHole n)
                       _ -> Nothing
          h -> h
  in
    find start (outgoingNodes m) findfn

find : Node -> (Node -> List Node) -> (Node -> Maybe a) -> Maybe a
find starting nextfn findfn =
  find_ starting nextfn findfn Set.empty |> Tuple.second

find_ : Node -> (Node -> List Node) -> (Node -> Maybe a) -> IDSet -> (IDSet, Maybe a)
find_ starting nextfn findfn seen =
  case findfn starting of
    Just x -> (seen, Just x)
    Nothing ->
      List.foldl
        (\node (set, x) ->
           case x of
             -- found something, return
             Just _ -> (set, x)
             -- not found, keep looking
             Nothing -> if Set.member (deID node.id) set
                        then (set, x)
                        else find_ node nextfn findfn set)
          (Set.insert (deID starting.id) seen, Nothing)
          (nextfn starting)




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
  (incomingNodes m n) ++ (outgoingNodes m n)

slotIsConnected : Model -> ID -> ParamName -> Bool
slotIsConnected m target param =
  target
    |> getNodeExn m
    |> getArgument param
    |> (/=) NoArg
