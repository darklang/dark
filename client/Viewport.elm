module Viewport exposing (..)

-- dark
import Defaults
import Types exposing (..)

pagePos : Page -> Pos
pagePos page =
  case page of
    Toplevels pos -> pos
    Fn _ pos -> pos

toViewport : Model -> Pos -> VPos
toViewport m pos =
  let default = pagePos (Defaults.defaultModel |> .currentPage)
      center = pagePos m.currentPage
  in
  { vx = default.x + pos.x - center.x
  , vy = default.y + pos.y - center.y}

toAbsolute : Model -> VPos -> Pos
toAbsolute m pos =
  let default = pagePos (Defaults.defaultModel |> .currentPage)
      center = pagePos m.currentPage
  in
  { x = pos.vx + center.x - default.x
  , y = pos.vy + center.y - default.y}


pageUp : Pos -> Modification
pageUp c =
  {x=c.x, y=c.y - Defaults.pageHeight } |> moveTo

pageDown : Pos -> Modification
pageDown c =
  {x=c.x, y=c.y + Defaults.pageHeight } |> moveTo

pageLeft : Pos -> Modification
pageLeft c =
  {x=c.x - Defaults.pageWidth, y=c.y } |> moveTo

pageRight : Pos -> Modification
pageRight c =
  {x=c.x + Defaults.pageWidth, y=c.y } |> moveTo





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
moveTo pos =
  SetCenter pos


