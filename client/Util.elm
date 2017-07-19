module Util exposing (timestamp, windowSize, deMaybe)

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
