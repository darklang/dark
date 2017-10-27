module GraphValidator exposing (..)

-- built-in
import Dict

-- dark
import Types exposing (..)
import Node as N
import Overlap as O
import Graph as G
import Defaults
import Util

validate : Model -> Result (List String) (List ())
validate m =
  let posPartitionedNodes = m.nodes
                          |> Dict.values
                          |> List.map
                              (\n -> if N.isBlock n
                                      then Ok n
                                      else if G.notPositioned n
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
      overlappingNodes = m
                       |> O.overlappingNodes
                       |> List.filter (\(a, b) -> (List.member a positionedNodes) && (List.member b positionedNodes))
                       |> List.map (\t -> "overlapping nodes: " ++ (O.ppNodes t))
      errs = unpositionedNodes ++ overlappingNodes
  in if List.length errs == 0
      then Ok [()]
      else Err errs
