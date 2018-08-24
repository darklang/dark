module Viewport exposing (..)

import Dom.Scroll
import Task

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

mouseMove : Model -> (List Int) -> Modification
mouseMove m deltaCoords =
  let pos = pagePos m.currentPage
      delta =
        case deltaCoords of
          x::y::_ -> { x=x, y=y }
          _ -> { x=0, y=0 }
  in if (m.sidebar.isScrollable && (abs delta.x) < 10)
  then
    let y = m.sidebar.yPos + (toFloat delta.y)
        ny = if y < 0 then 0 else y
        scroll = Dom.Scroll.toY "leftsidebar" ny
    in Many
      [ MakeCmd (Task.attempt SidebarScrollTo scroll)
      , SidebarUpdateY ny
      ]
  else moveTo {
        x = pos.x + delta.x
        , y = pos.y + delta.y
      }

