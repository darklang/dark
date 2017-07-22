module Util exposing (timestamp, windowSize, deMaybe, orderedNodes)

import Dict
import List
import Ordering

import Native.Window
import Native.Timestamp

import Types exposing (..)

timestamp : () -> Int
timestamp a = Native.Timestamp.timestamp a

windowSize : () -> (Int, Int)
windowSize a = let size = Native.Window.size a
               in (size.width, size.height)


deMaybe x = case x of
              Nothing -> Debug.crash "not possible"
              Just y -> y


orderedNodes : Model -> List Node
orderedNodes m =
  m.nodes
    |> Dict.values
    |> List.map (\n -> (n.pos.x, n.pos.y, n.id |> deID))
    |> List.sortWith Ordering.natural
    |> List.map (\(_,_,id) -> Dict.get id m.nodes |> deMaybe)
