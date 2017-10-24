module Overlap exposing (..)

import Dict
import List

-- dark
import Types exposing (..)
import Util
import Graph as G

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
        (width, height) = G.nodeSize n
        bottomRight     = {x = topLeft.x + width, y = topLeft.y + height}
    in
    { id = id, top = topLeft.y, bottom = bottomRight.y, right = bottomRight.x, left = topLeft.x }

ppNodePairs : List (Node, Node) -> String
ppNodePairs np = String.join "\n" (List.map ppNodes np)

ppNodes : (Node, Node) -> String
ppNodes (x, y) = "([" ++ x.name ++ ", " ++ (toString <| deID <| x.id) ++ "], [" ++ y.name ++ ", " ++ (toString <| deID <| y.id) ++ "])"
