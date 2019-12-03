open Types
open Tc
open Prelude
module TL = Toplevel

let addPos (a : pos) (b : pos) : pos = {x = a.x + b.x; y = a.y + b.y}

let subPos (a : pos) (b : pos) : pos = {x = a.x - b.x; y = a.y - b.y}

let toAbsolute (m : model) (pos : vPos) : pos =
  let topleft = m.canvasProps.offset in
  addPos {x = pos.vx; y = pos.vy} topleft


let toCenteredOn (pos : pos) : pos = subPos pos Defaults.centerPos

let toCenter (pos : pos) : pos = addPos pos Defaults.centerPos

let moveCanvasBy (m : model) (x : int) (y : int) : modification =
  let dx, dy =
    if VariantTesting.variantIsActive m GridLayout
    then (x / 2, y / 2)
    else (x, y)
  in
  let offset = m.canvasProps.offset in
  let pos = addPos offset {x = dx; y = dy} in
  MoveCanvasTo (pos, DontAnimateTransition)


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

let moveToOrigin : modification =
  MoveCanvasTo (Defaults.origin, DontAnimateTransition)


(* Centers the toplevel on canvas based on windowWidth and sidebarWidth 
  Default values (when we can't find get elements from dom) are based on
  min-widths defined in app.less. At some point we will want to find a
  less volatile method for the constant definitions.
*)
let centerCanvasOn (tl : toplevel) : pos =
  let windowWidth = Native.Window.viewportWidth in
  let sidebarWidth =
    Native.Ext.querySelector "#sidebar-left"
    |> Option.map ~f:Native.Ext.clientWidth
    |> recoverOpt "can't find sidebar HTML body" ~default:320
  in
  let tlWidth =
    let tle =
      Native.Ext.querySelector (".toplevel.tl-" ^ Prelude.showTLID (TL.id tl))
    in
    match tle with Some e -> Native.Ext.clientWidth e | None -> 245
  in
  let availWidth = (windowWidth - tlWidth) / 3 in
  let offsetLeft = sidebarWidth + availWidth in
  {x = (TL.pos tl).x - offsetLeft; y = (TL.pos tl).y - 200}


(* Checks to see is the token's dom element within viewport,
if not returns the new targetX and/or targetY coordindates we should transform the canvas by *)
let moveToToken (id : id) (tl : toplevel) : int option * int option =
  let tokenSelector = ".id-" ^ Prelude.deID id in
  let tlSelector = ".tl-" ^ Prelude.deTLID (TL.id tl) in
  match Native.Ext.querySelector tokenSelector with
  | Some tokenDom ->
      let sidebarWidth =
        Native.Ext.querySelector "#sidebar-left"
        |> Option.map ~f:Native.Ext.clientWidth
        |> recoverOpt "can't find sidebar HTML body" ~default:320
      in
      let viewport : Native.rect =
        { id = "#canvas"
        ; top = 0
        ; left = sidebarWidth
        ; right = Native.Window.viewportWidth
        ; bottom = Native.Window.viewportHeight }
      in
      let tokenBox = Native.Ext.getBoundingClient tokenDom tokenSelector in
      let tlBox =
        Native.Ext.querySelector tlSelector
        |> Option.map ~f:(fun dom ->
               Native.Ext.getBoundingClient dom tlSelector )
        |> Option.valueExn
      in
      let tlPos = TL.pos tl in
      (* If the token's DOM element is out of viewport, we want to shift the canvas transform to bring it within view. To make the transition seem smooth, ideally we want only either move by y-axis or x-axis. Sometimes we might have to both by both axis. But since the only use case for this function at the moment is to find code from several statements before where you are currently looking at, the most likely case is the that the token we are looking for is above the top fold, therefore we are likely going to move by only the y-axis. *)
      let xTarget =
        if tokenBox.right > viewport.left && tokenBox.left < viewport.right
        then None
        else
          let offsetLeft = tokenBox.left - tlBox.left in
          Some (tlPos.x - (sidebarWidth + offsetLeft))
      in
      let yTarget =
        if tokenBox.bottom > viewport.top && tokenBox.top < viewport.bottom
        then None
        else
          let offsetTop = tokenBox.top - tlBox.top in
          Some (tlPos.y - (50 + offsetTop))
      in
      (xTarget, yTarget)
  | None ->
      (None, None)
