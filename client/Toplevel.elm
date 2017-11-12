module Toplevel exposing (..)

-- builtin

-- lib
import List.Extra as LE

-- dark
import Types exposing (..)
import Util exposing (deMaybe)
import AST

getTL : Model -> TLID -> Toplevel
getTL m id =
  LE.find (\tl -> tl.id == id) m.toplevels
  |> deMaybe

firstHole : Model -> TLID -> HID
firstHole m id =
  getTL m id |> .ast |> AST.findFirstHole
