module Viewport exposing (..)

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

moveUp : Pos -> Pos
moveUp c =
  {x=c.x, y=c.y - Defaults.moveSize }

moveDown : Pos -> Pos
moveDown c =
  {x=c.x, y=c.y + Defaults.moveSize }

moveLeft : Pos -> Pos
moveLeft c =
  {x=c.x - Defaults.moveSize, y=c.y }

moveRight : Pos -> Pos
moveRight c =
  {x=c.x + Defaults.moveSize, y=c.y }
