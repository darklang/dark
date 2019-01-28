open Types

let addPos (a : pos) (b : pos) : pos = {x = a.x + b.x; y = a.y + b.y}

let subPos (a : pos) (b : pos) : pos = {x = a.x - b.x; y = a.y - b.y}

let pagePos (page : page) : pos =
  match page with Toplevels pos -> pos | Fn (_, pos) -> pos


let toAbsolute (m : model) (pos : vPos) : pos =
  let topleft =
    match m.currentPage with
    | Toplevels _ ->
        m.canvas.offset
    | Fn (_, _) ->
        m.canvas.fnOffset
  in
  addPos {x = pos.vx; y = pos.vy} topleft


let toCenteredOn (pos : pos) : pos = subPos pos Defaults.centerPos

let toCenter (pos : pos) : pos = addPos pos Defaults.centerPos

let moveCanvasBy (m : model) (x : int) (y : int) : modification =
  let c = m.canvas in
  let offset =
    match m.currentPage with
    | Toplevels _ ->
        c.offset
    | Fn (_, _) ->
        c.fnOffset
  in
  let pos = addPos offset {x; y} in
  MoveCanvasTo (c, m.currentPage, pos)


let pageUp (m : model) : modification =
  moveCanvasBy m 0 (-1 * Defaults.pageHeight)


let pageDown (m : model) : modification = moveCanvasBy m 0 Defaults.pageHeight

let pageLeft (m : model) : modification =
  moveCanvasBy m (-1 * Defaults.pageWidth) 0


let pageRight (m : model) : modification = moveCanvasBy m Defaults.pageWidth 0

let moveUp (m : model) : modification =
  moveCanvasBy m 0 (-1 * Defaults.moveSize)


let moveDown (m : model) : modification = moveCanvasBy m 0 Defaults.moveSize

let moveLeft (m : model) : modification =
  moveCanvasBy m (-1 * Defaults.moveSize) 0


let moveRight (m : model) : modification = moveCanvasBy m Defaults.moveSize 0

let moveToOrigin (m : model) : modification =
  MoveCanvasTo (m.canvas, m.currentPage, Defaults.origin)
