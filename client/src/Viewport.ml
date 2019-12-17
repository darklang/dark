open Types
open Tc
open Prelude
module TL = Toplevel

let addPos (a : pos) (b : pos) : pos = {x = a.x + b.x; y = a.y + b.y}

let subPos (a : pos) (b : pos) : pos = {x = a.x - b.x; y = a.y - b.y}

let toAbsolute (pos : vPos) : pos =
  let topleft =
    let x, y = Native.Ext.appScrollPos () in
    {x; y}
  in
  addPos {x = pos.vx; y = pos.vy} topleft


let centerTop : vPos = {vx = (Native.Window.viewportWidth / 2) - 200; vy = 200}

let toCenteredOn (pos : pos) : pos = subPos pos Defaults.centerPos

let toCenter (pos : pos) : pos = addPos pos Defaults.centerPos

let pageUp () : modification =
  MoveCanvasBy (0, -1 * Native.Window.viewportHeight)


let pageDown () : modification = MoveCanvasBy (0, Native.Window.viewportHeight)

let pageLeft () : modification =
  MoveCanvasBy (-1 * Native.Window.viewportWidth, 0)


let pageRight () : modification = MoveCanvasBy (Native.Window.viewportWidth, 0)

let moveUp () : modification = MoveCanvasBy (0, -1 * Defaults.moveSize)

let moveDown () : modification = MoveCanvasBy (0, Defaults.moveSize)

let moveLeft () : modification = MoveCanvasBy (-1 * Defaults.moveSize, 0)

let moveRight () : modification = MoveCanvasBy (Defaults.moveSize, 0)

let sidebarWidth () : int =
  Native.Ext.querySelector "#sidebar-left"
  |> Option.map ~f:Native.Ext.clientWidth
  |> recoverOpt "can't find sidebar HTML body" ~default:320


(* Checks to see is the token's dom element within viewport, if not returns the the dx,dy to scroll by to bring the element within view *)
let moveToToken (id : id) : int * int =
  let tokenSelector = ".id-" ^ Prelude.deID id in
  match Native.Ext.querySelector tokenSelector with
  | Some tokenDom ->
      let sidebarWidth = sidebarWidth () in
      let viewport : Native.rect =
        { id = "#canvas"
        ; top = 0
        ; left = sidebarWidth
        ; right = Native.Window.viewportWidth
        ; bottom = Native.Window.viewportHeight }
      in
      let tokenBox = Native.Ext.getBoundingClient tokenDom tokenSelector in
      (* let tlPos = TL.pos tl in *)
      (* If the token's DOM element is out of viewport, we want to shift the canvas transform to bring it within view. To make the transition seem smooth, ideally we want only either move by y-axis or x-axis. Sometimes we might have to both by both axis. But since the only use case for this function at the moment is to find code from several statements before where you are currently looking at, the most likely case is the that the token we are looking for is above the top fold, therefore we are likely going to move by only the y-axis. *)
      let xTarget =
        if tokenBox.left < viewport.left
        then -1 * (viewport.left - tokenBox.left + 20)
        else if tokenBox.right > viewport.right
        then tokenBox.right - viewport.right + 20
        else 0
      in
      let yTarget =
        if tokenBox.top < viewport.top
        then -1 * (viewport.top - tokenBox.top + 20)
        else if tokenBox.bottom > viewport.bottom
        then tokenBox.bottom - viewport.bottom + 20
        else 0
      in
      (xTarget, yTarget)
  | None ->
      (0, 0)


let isToplevelVisible ?(isFullyInside = true) (tlid : tlid) : bool =
  let viewport : Native.rect =
    let sidebar = sidebarWidth () in
    { id = "#canvas"
    ; top = 0
    ; left = sidebar
    ; right = Native.Window.viewportWidth
    ; bottom = Native.Window.viewportHeight }
  in
  let id = Prelude.showTLID tlid in
  Native.Ext.querySelector (".toplevel.tl-" ^ id)
  |> Option.map ~f:(fun tl ->
         let rect = Native.Ext.getBoundingClient tl id in
         if isFullyInside
         then
           rect.top > viewport.top
           && rect.left > viewport.left
           && rect.right < viewport.right
           && rect.bottom < viewport.bottom
         else
           rect.top > viewport.top
           || rect.left > viewport.left
           || rect.right < viewport.right
           || rect.bottom < viewport.bottom )
  |> Option.withDefault ~default:false


let moveCanvasBy (x : int) (y : int) : msg Tea.Cmd.t =
  Tea_cmd.call (fun _ -> Native.Ext.appScrollBy x y)


let moveCanvasTo (x : int) (y : int) : msg Tea.Cmd.t =
  Tea_cmd.call (fun _ -> Native.Ext.appScrollTo x y ~smooth:false)


let moveCanvasToPos (pos : pos) : msg Tea.Cmd.t =
  let mx, my = Native.Ext.appScrollLimits () in
  let o = Defaults.tlSpacing in
  let x =
    let sidebarOffset = sidebarWidth () in
    Util.clamp (pos.x - sidebarOffset - o.x) 0 mx
  in
  let y = Util.clamp (pos.y - o.y) 0 my in
  Tea_cmd.call (fun _ -> Native.Ext.appScrollTo x y ~smooth:true)
