module Canvas exposing (..)

-- builtins

-- lib
import List.Extra as LE

-- dark
import Types exposing (..)
import Util exposing (deMaybe)

-------------------
-- Positioning
-------------------
nextPosition : Pos -> Pos
nextPosition {x, y} =
  if x > 900 then
    {x=100, y=y+100}
  else
    {x=x+100, y=y}

paramOffset : Node -> String -> Pos
paramOffset node param =
  let
    index = deMaybe (LE.findIndex (\p -> p.name == param) node.parameters)
  in
    {x=index*10, y=-2}
