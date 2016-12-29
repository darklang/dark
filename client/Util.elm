module Util exposing (timestamp, windowSize)

import Native.Window
import Native.Timestamp

timestamp : () -> Int
timestamp a = Native.Timestamp.timestamp a

windowSize : () -> (Int, Int)
windowSize a = let size = Native.Window.size a
               in (size.width, size.height)
