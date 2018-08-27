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

addPos: Pos -> Pos -> Pos
addPos a b = { x = a.x + b.x, y = a.y + b.y }

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

centerOn : Pos -> Pos
centerOn pos =
  addPos (Pos (-1*pos.x) (-1*pos.y)) Defaults.initialPos

mouseMove : Model -> (List Int) -> Modification
mouseMove m deltaCoords =
  let d =
        case deltaCoords of
          x::y::_ -> { x=x, y=y }
          _ -> { x=0, y=0 }
      c = m.canvas
      newCanvas =
        case m.currentPage of
          Toplevels _ -> { c | offset = Pos (c.offset.x - d.x) (c.offset.y + d.y) }
          Fn _ _ -> { c | fnOffset =  Pos (c.fnOffset.x - d.x) (c.fnOffset.y + d.y) }
  in TweakModel (\m -> { m | canvas = newCanvas })
