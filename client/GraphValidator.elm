module GraphValidator exposing (..)

-- built-in
import Dict

-- dark
import Types exposing (..)
import Node as N
import Graph as G
import Defaults
import Util

validate : Model -> Result (List String) (List ())
validate m =
  let posPartitionedNodes = m.nodes
                          |> Dict.values
                          |> List.map
                              (\n -> if G.notPositioned n
                                     then Err ("unpositioned node", n)
                                     else if G.posx m n == Defaults.unsetInt || G.posy m n == Defaults.unsetInt
                                     then Err ("in hell", n)
                                     else Ok n
                              )
                          |> Util.combineResult
      positionedNodes  = case posPartitionedNodes of
                          Ok ns -> ns
                          Err _ -> []
      unpositionedNodes = case posPartitionedNodes of
                           Err ns -> List.map (\(e, n) -> e ++ ": " ++ (toString n)) ns
                           _      -> []
      nodesThatOverlap  = m
                       |> overlappingNodes
                       |> List.filter (\(a, b) -> (List.member a positionedNodes) && (List.member b positionedNodes))
                       |> List.map (\t -> "overlapping nodes: " ++ (ppNodes t))
      errs = unpositionedNodes ++ nodesThatOverlap
  in if List.length errs == 0
      then Ok [()]
      else Err errs

overlappingNodes : Model -> List (Node, Node)
overlappingNodes m = m.nodes
                   |> Dict.values
                   |> List.filter G.hasPos
                   |> List.map (bounds m)
                   |> Util.uniqueCombinations
                   |> List.filter checkOverlap
                   |> List.map (toNodes m)

type alias NodeBounds = { id: ID
                        , top: Int
                        , bottom: Int
                        , left: Int
                        , right: Int
                        }

toNode : Model -> NodeBounds -> Node
toNode m nb = G.getNodeExn m nb.id

toNodes : Model -> (NodeBounds, NodeBounds) -> (Node, Node)
toNodes m (n1, n2) = (toNode m n1, toNode m n2)

checkOverlap : (NodeBounds, NodeBounds) -> Bool
checkOverlap (n1, n2) =
    let horizontalOverlap = (n1.left <= n2.right) && (n1.right >= n2.left)
        verticalOverlap   = (n1.bottom >= n2.top) && (n1.top <= n2.bottom)
    in
        horizontalOverlap && verticalOverlap

bounds : Model -> Node -> NodeBounds
bounds m n =
    let id              = n.id
        topLeft         = G.pos m n
        (width, height) = N.nodeSize n
        bottomRight     = {x = topLeft.x + width, y = topLeft.y + height}
    in
    { id = id, top = topLeft.y, bottom = bottomRight.y, right = bottomRight.x, left = topLeft.x }

ppNodePairs : List (Node, Node) -> String
ppNodePairs np = String.join "\n" (List.map ppNodes np)

ppNodes : (Node, Node) -> String
ppNodes (x, y) = "([" ++ x.name ++ ", " ++ (toString <| deID <| x.id) ++ "], [" ++ y.name ++ ", " ++ (toString <| deID <| y.id) ++ "])"
