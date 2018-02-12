module Viewport exposing (..)

import Navigation

-- dark
import Defaults
import Types exposing (..)

toViewport : Model -> Pos -> VPos
toViewport m pos =
  let d = Defaults.defaultModel Defaults.defaultEditor |> .center in
  { vx = d.x + pos.x - m.center.x, vy = d.y + pos.y - m.center.y}

toAbsolute : Model -> VPos -> Pos
toAbsolute m pos =
  let d = Defaults.defaultModel Defaults.defaultEditor |> .center in
  { x = pos.vx + m.center.x - d.x, y = pos.vy + m.center.y - d.y}

moveUp : Pos -> Modification
moveUp c =
  {x=c.x, y=c.y - Defaults.moveSize } |> moveTo

moveDown : Pos -> Modification
moveDown c =
  {x=c.x, y=c.y + Defaults.moveSize } |> moveTo

moveLeft : Pos -> Modification
moveLeft c =
  {x=c.x - Defaults.moveSize, y=c.y } |> moveTo

moveRight : Pos -> Modification
moveRight c =
  {x=c.x + Defaults.moveSize, y=c.y } |> moveTo

moveTo : Pos -> Modification
moveTo p =
  MakeCmd (Navigation.modifyUrl (urlForPos p))

urlForPos : Pos -> String
urlForPos pos =
  let x = "X" ++ (toString pos.x)
      y = "Y" ++ (toString pos.y)
  in
      "/admin/ui#" ++ x ++ "&" ++ y
