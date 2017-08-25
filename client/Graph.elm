module Graph exposing (..)

-- builtin
import Char
import Ordering
import List
import Tuple
import Dict

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
  case LE.find (\(p, a) -> p.name == pname) (args n) of
    Just (p, a) -> a
    Nothing ->
      Debug.crash <| "Looking for a name which doesn't exist: " ++ pname ++ (toString n)

args : Node -> List (Parameter, Argument)
args n =
  (List.map2 (,) n.parameters n.arguments)


findHole : Model -> Node -> Hole
findHole m n =
  case Util.findIndex (\(_, a) -> a == NoArg) (args n) of
    Nothing -> ResultHole n
    Just (i, (p, _)) -> ParamHole n p i

incomingNodes : Model -> Node -> List (Node, ParamName)
incomingNodes m n = List.filterMap
                    (\(p, a) ->
                       case a of
                         Edge id -> Just (getNodeExn m id, p.name)
                         _ -> Nothing)
                    (Util.zip n.parameters n.arguments)

outgoingNodes : Model -> Node -> List Node
outgoingNodes m parent =
  m.nodes
    |> Dict.values
    |> List.filterMap (\child ->
                         child
                      |> incomingNodes m
                      |> List.map Tuple.first
                      |> LE.find ((==) parent)
                      |> Maybe.map (always child))

connectedNodes : Model -> Node -> List Node
connectedNodes m n =
  (incomingNodes m n |> List.map Tuple.first) ++ (outgoingNodes m n)


slotIsConnected : Model -> ID -> ParamName -> Bool
slotIsConnected m target param =
  target
    |> getNodeExn m
    |> getArgument param
    |> (/=) NoArg
