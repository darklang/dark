module Canvas exposing (..)

-- builtins
import Dict

-- lib
import Mouse
import List.Extra as LE

-- dark
import Types exposing (..)
import Util exposing (deMaybe)

-------------------
-- Dragging
-------------------
updateDragPosition : Pos -> Offset -> ID -> NodeDict -> NodeDict
updateDragPosition pos off (ID id) nodes =
  Dict.update id (Maybe.map (\n -> {n | pos = {x=pos.x+off.x, y=pos.y+off.y}})) nodes

-------------------
-- Positioning
-------------------
nextPosition : Pos -> Pos
nextPosition {x, y} =
  if x > 900 then
    {x=100, y=y+100}
  else
    {x=x+100, y=y}

findOffset : Pos -> Mouse.Position -> Offset
findOffset pos mpos =
 {x=pos.x - mpos.x, y= pos.y - mpos.y, offsetCheck=1}

paramOffset : Node -> String -> Pos
paramOffset node param =
  let
    index = deMaybe (LE.findIndex (\p -> p.name == param) node.parameters)
  in
    {x=index*10, y=-2}
