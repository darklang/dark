module Util exposing (..)

-- builtin
import Regex

-- lib
import List.Extra as LE

-- dark
import Native.Window
import Native.Timestamp
import Native.Random

timestamp : () -> Int
timestamp a = Native.Timestamp.timestamp a

windowSize : () -> (Int, Int)
windowSize a = let size = Native.Window.size a
               in (size.width, size.height)

random : () -> Int
random a = Native.Random.random a

deMaybe : Maybe a -> a
deMaybe x = case x of
              Nothing -> Debug.crash "not possible"
              Just y -> y

rematch : String -> String -> Bool
rematch re s = Regex.contains (Regex.regex re) s

findIndex : (a -> Bool) -> List a -> Maybe (Int, a)
findIndex fn l =
  LE.find (\(_, a) -> fn a) (List.indexedMap (,) l)


zip : List a -> List b -> List (a, b)
zip a b = List.map2 (,) a b
