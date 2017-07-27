module Graph exposing ( orderedNodes
                      , fromLetter
                      , int2letter
                      , getNode
                      , findHole)

import Array
import Char
import Ordering
import List
import Tuple
import Dict

import List.Extra

import Types exposing (..)
import Util exposing (deMaybe)



orderedNodes : Model -> List Node
orderedNodes m =
  m.nodes
    |> Dict.values
    |> List.map (\n -> (n.pos.x, n.pos.y, n.id |> deID))
    |> List.sortWith Ordering.natural
    |> List.map (\(_,_,id) -> Dict.get id m.nodes |> deMaybe)


int2letter : Int -> String
int2letter i = i |> (+) 97 |> Char.fromCode |> String.fromChar

letter2int : String -> Int
letter2int s = s |> String.uncons |> deMaybe |> Tuple.first |> Char.toCode |> (-) 97 |> (*) (-1)

fromLetter : Model -> String -> Node
fromLetter m letter = m |> orderedNodes |> Array.fromList |> Array.get (letter2int letter) |> deMaybe

getNode : Model -> ID -> Node
getNode m id = Dict.get (deID id) m.nodes |> deMaybe

findHole : Model -> Maybe ID -> Hole
findHole model mID =
  case mID of
    Nothing -> NoHole
    Just id ->
      let n = getNode model id
          incoming = List.filter (\e -> e.target == n.id) model.edges
          used_params = List.map .param incoming
          all_params = List.indexedMap (,) n.parameters
          unused = List.Extra.find
                   (\(i, p) -> not <| List.member p used_params)
                   all_params
      in
        case unused of
          Nothing -> ResultHole n
          Just (i, p) -> ParamHole n p i
