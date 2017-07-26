module Util exposing (timestamp, windowSize, deMaybe, orderedNodes, letter2int, int2letter, fromLetter, rematch)

import Dict
import Array
import Char
import Tuple
import List
import Ordering
import Regex

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


int2letter : Int -> String
int2letter i = i |> (+) 97 |> Char.fromCode |> String.fromChar

letter2int : String -> Int
letter2int s = s |> String.uncons |> Debug.log "uncons" |> deMaybe |> Tuple.first |> Char.toCode |> Debug.log "code" |> (-) 97 |> (*) (-1)

fromLetter : Model -> String -> Node
fromLetter m letter = m |> orderedNodes |> Array.fromList |> Debug.log "ordered" |> Array.get (letter2int letter) |> Debug.log "fromArray"|> deMaybe

rematch : String -> String -> Bool
rematch re s = Regex.contains (Regex.regex re) s
