module Canvas exposing (..)

-- builtins
import Dict

-- lib
import Task
import Dom
import Mouse
import List.Extra

-- dark
import Defaults
import Types exposing (..)
import Util exposing (deMaybe)

-------------------
-- Focus
-------------------
focusEntry : Cmd Msg
focusEntry = Dom.focus Defaults.entryID |> Task.attempt FocusResult

focusRepl : Cmd Msg
focusRepl = Cmd.none -- Dom.focus Defaults.replID |> Task.attempt FocusResult

unfocusRepl : Cmd Msg
unfocusRepl = Dom.blur Defaults.replID |> Task.attempt FocusResult

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
    index = deMaybe (List.Extra.elemIndex param node.parameters)
  in
    {x=index*10, y=-2}
