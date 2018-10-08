open Types

let addPos a b = {x= a.x + b.x; y= a.y + b.y}

let subPos a b = {x= a.x - b.x; y= a.y - b.y}

let pagePos page = match page with Toplevels pos -> pos | Fn (_, pos) -> pos

let toAbsolute m pos =
  let topleft =
    match m.currentPage with
    | Toplevels _ -> m.canvas.offset
    | Fn (_, _) -> m.canvas.fnOffset
  in
  addPos {x= pos.vx; y= pos.vy} topleft

let toCenteredOn pos = subPos pos Defaults.centerPos

let toCenter pos = addPos pos Defaults.centerPos

let pageUp m = moveCanvasBy m 0 (-1 * Defaults.pageHeight)

let pageDown m = moveCanvasBy m 0 Defaults.pageHeight

let pageLeft m = moveCanvasBy m (-1 * Defaults.pageWidth) 0

let pageRight m = moveCanvasBy m Defaults.pageWidth 0

let moveUp m = moveCanvasBy m 0 (-1 * Defaults.moveSize)

let moveDown m = moveCanvasBy m 0 Defaults.moveSize

let moveLeft m = moveCanvasBy m (-1 * Defaults.moveSize) 0

let moveRight m = moveCanvasBy m Defaults.moveSize 0

let moveToOrigin m = MoveCanvasTo (m.canvas, m.currentPage, Defaults.origin)

let moveCanvasBy m x y =
  let c = m.canvas in
  let offset =
    match m.currentPage with
    | Toplevels _ -> c.offset
    | Fn (_, _) -> c.fnOffset
  in
  let pos = addPos offset {x; y} in
  MoveCanvasTo (c, m.currentPage, pos)
