module Graph exposing ( orderedNodes
                      , fromLetter
                      , int2letter
                      , getNode
                      , getNodeExn
                      , distance
                      , findHole
                      , incomingEdges
                      , incomingNodes
                      , outgoingEdges
                      , outgoingNodes
                      , slotIsConnected
                      , connectedNodes)

import Array
import Char
import Ordering
import List
import Tuple
import Dict

import List.Extra as LE

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
fromLetter m letter = m |> orderedNodes |> Array.fromList |> Array.get (letter2int letter)

getNode : Model -> ID -> Maybe Node
getNode m id = Dict.get (deID id) m.nodes

getNodeExn : Model -> ID -> Node
getNodeExn m id = getNode m id |> deMaybe

incomingEdges : Model -> Node -> List Edge
incomingEdges m target = List.filter (\e -> e.target == target.id) m.edges

outgoingEdges : Model -> Node -> List Edge
outgoingEdges m source = List.filter (\e -> e.source == source.id) m.edges

incomingNodes : Model -> Node -> List Node
incomingNodes m n = n
                  |> incomingEdges m
                  |> List.map .source
                  |> List.map (getNodeExn m)

outgoingNodes : Model -> Node -> List Node
outgoingNodes m n = n
                  |> outgoingEdges m
                  |> List.map .target
                  |> List.map (getNodeExn m)

-- TODO: if a node has the same incoming and outgoing node, this will
-- break. But, we shouldn't allow cycles like that anyway, except in
-- some cases...
connectedNodes : Model -> Node -> List Node
connectedNodes m n = (incomingNodes m n) ++ (outgoingNodes m n)


findHole : Model -> Node -> Hole
findHole m n =
  let incoming = incomingEdges m n
      used_params = List.map .param incoming ++ Dict.keys n.constants
      all_params = List.indexedMap (,) n.parameters
      unused = LE.find
               (\(i, p) -> not <| List.member p.name used_params)
                 all_params
  in
    case unused of
      Nothing -> ResultHole n
      Just (i, p) -> ParamHole n p i


slotIsConnected : Model -> ID -> ParamName -> Bool
slotIsConnected m target param =
  let node = getNodeExn m target in
  List.any (\e -> e.target == target && e.param == param) m.edges
    || Dict.member param node.constants
