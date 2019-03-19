open Types

let addPos (a : pos) (b : pos) : pos = {x = a.x + b.x; y = a.y + b.y}

let subPos (a : pos) (b : pos) : pos = {x = a.x - b.x; y = a.y - b.y}

let pagePos (page : page) : pos =
  match page with Architecture pos -> pos | _ -> Defaults.centerPos


let toAbsolute (m : model) (pos : vPos) : pos =
  let topleft = m.canvasProps.offset in
  addPos {x = pos.vx; y = pos.vy} topleft


let toCenteredOn (pos : pos) : pos = subPos pos Defaults.centerPos

let toCenter (pos : pos) : pos = addPos pos Defaults.centerPos

let moveCanvasBy (m : model) (x : int) (y : int) : modification =
  let offset = m.canvasProps.offset in
  let pos = addPos offset {x; y} in
  MoveCanvasTo pos


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

let moveToOrigin : modification = MoveCanvasTo Defaults.origin

let isEnclosed (outer : box) (inner : box) : bool =
  let oOrigin, oSize = outer in
  let oLeft, oTop = (oOrigin.x, oOrigin.y) in
  let oRight = oLeft + oSize.w in
  let oBottom = oTop + oSize.h in
  let iOrigin, iSize = inner in
  let iLeft, iTop = (iOrigin.x, iOrigin.y) in
  let iRight = iLeft + iSize.w in
  let iBottom = iTop + iSize.h in
  (oLeft < iLeft && iRight < oRight) && oTop < iTop && iBottom < oBottom
