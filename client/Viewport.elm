module Viewport exposing (..)

-- dark
import Defaults
import Types exposing (..)

toViewport : Model -> Pos -> VPos
toViewport m pos =
  let d = Defaults.defaultModel {} |> .center in
  { vx = d.x + pos.x - m.center.x, vy = d.y + pos.y - m.center.y}

toAbsolute : Model -> VPos -> Pos
toAbsolute m pos =
  let d = Defaults.defaultModel {} |> .center in
  { x = pos.vx + m.center.x - d.x, y = pos.vy + m.center.y - d.y}

moveUp : Model -> Model
moveUp m =
  { m | center = {x=m.center.x, y=m.center.y - Defaults.moveSize }}

moveDown : Model -> Model
moveDown m =
  { m | center = {x=m.center.x, y=m.center.y + Defaults.moveSize }}

moveLeft : Model -> Model
moveLeft m =
  { m | center = {x=m.center.x - Defaults.moveSize, y=m.center.y }}

moveRight : Model -> Model
moveRight m =
  { m | center = {x=m.center.x + Defaults.moveSize, y=m.center.y }}
