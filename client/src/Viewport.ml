open Types

let addPos (a : pos) (b : pos) : pos = {x = a.x + b.x; y = a.y + b.y}

let subPos (a : pos) (b : pos) : pos = {x = a.x - b.x; y = a.y - b.y}

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

(* Gets coordinates of the corners of outer box and inner box.
  Inner box is enclosed withing outer box, if and only if
  all 4 corners (points) of the inner box lay within the outer box boundaries.
 *)
let isEnclosed (outer : box) (inner : box) : bool =
  let ptInside px py =
    let oOrigin, oSize = outer in
    let ox1, oy1 = (oOrigin.x, oOrigin.y) in
    let ox2 = ox1 + oSize.w in
    let oy2 = oy1 + oSize.h in
    ox1 < px && px < ox2 && oy1 < py && py < oy2
  in
  let iOrigin, iSize = inner in
  let ix1, iy1 = (iOrigin.x, iOrigin.y) in
  let ix2 = ix1 + iSize.w in
  let iy2 = iy1 + iSize.h in
  ptInside ix1 iy1 || ptInside ix1 iy2 || ptInside ix2 iy1 || ptInside ix2 iy2


(* Centers the toplevel on canvas based on windowWidth and sidebarWidth 
  Default values (when we can't find get elements from dom) are based on
  min-widths defined in app.less. At some point we will want to find a
  less volatile method for the constant definitions.
*)
let centerCanvasOn (tl : toplevel) (props : canvasProps) : pos =
  let windowWidth = props.viewportSize.w in
  let sidebarWidth =
    let sidebar = Native.Ext.querySelector "#sidebar-left" in
    match sidebar with Some e -> Native.Ext.clientWidth e | None -> 320
  in
  let tlWidth =
    let tle =
      Native.Ext.querySelector (".toplevel.tl-" ^ Prelude.showTLID tl.id)
    in
    match tle with Some e -> Native.Ext.clientWidth e | None -> 245
  in
  let availWidth = (windowWidth - tlWidth) / 3 in
  let offsetLeft = sidebarWidth + availWidth in
  {x = tl.pos.x - offsetLeft; y = tl.pos.y - 200}
