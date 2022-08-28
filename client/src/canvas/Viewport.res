open Prelude

module TL = Toplevel
module Cmd = Tea.Cmd

type model = AppTypes.model
type modification = AppTypes.modification

let addPos = (a: Pos.t, b: Pos.t): Pos.t => {x: a.x + b.x, y: a.y + b.y}

let subPos = (a: Pos.t, b: Pos.t): Pos.t => {x: a.x - b.x, y: a.y - b.y}

let toAbsolute = (m: model, pos: VPos.t): Pos.t => {
  let topleft = m.canvasProps.offset
  addPos({x: pos.vx, y: pos.vy}, topleft)
}

let toCenteredOn = (pos: Pos.t): Pos.t => subPos(pos, Pos.center)

let toCenter = (pos: Pos.t): Pos.t => addPos(pos, Pos.center)

let moveCanvasBy = (m: model, x: int, y: int): modification => {
  let (dx, dy) = (x, y)
  let offset = m.canvasProps.offset
  let pos = addPos(offset, {x: dx, y: dy})
  MoveCanvasTo(pos, DontAnimateTransition)
}

let pageUp = (m: model): modification => moveCanvasBy(m, 0, -1 * Defaults.pageHeight)

let pageDown = (m: model): modification => moveCanvasBy(m, 0, Defaults.pageHeight)

let pageLeft = (m: model): modification => moveCanvasBy(m, -1 * Defaults.pageWidth, 0)

let pageRight = (m: model): modification => moveCanvasBy(m, Defaults.pageWidth, 0)

let moveUp = (m: model): modification => moveCanvasBy(m, 0, -1 * Defaults.moveSize)

let moveDown = (m: model): modification => moveCanvasBy(m, 0, Defaults.moveSize)

let moveLeft = (m: model): modification => moveCanvasBy(m, -1 * Defaults.moveSize, 0)

let moveRight = (m: model): modification => moveCanvasBy(m, Defaults.moveSize, 0)

/* Centers the toplevel on canvas based on windowWidth and sidebarWidth
  Default values (when we can't find get elements from dom) are based on
  min-widths defined in app.less. At some point we will want to find a
  less volatile method for the constant definitions.
*/
let centerCanvasOn = (tl: toplevel): Pos.t => {
  let windowWidth = Webapi.Dom.window->Webapi.Dom.Window.innerWidth
  let sidebarWidth =
    Native.Ext.querySelector("#sidebar-left")
    |> Option.map(~f=Native.Ext.clientWidth)
    |> Option.unwrap(~default=320)

  let tlWidth = {
    let tle = Native.Ext.querySelector(".toplevel.tl-" ++ TLID.toString(TL.id(tl)))

    switch tle {
    | Some(e) => Native.Ext.clientWidth(e)
    | None => 245
    }
  }

  let availWidth = (windowWidth - tlWidth) / 3
  let offsetLeft = sidebarWidth + availWidth
  {x: TL.pos(tl).x - offsetLeft, y: TL.pos(tl).y - 200}
}

// Checks to see is the token's dom element within viewport, if not returns the new targetX and/or targetY to move the user to, in the canvas
let moveToToken = (id: id, tl: toplevel): (option<int>, option<int>) => {
  let tokenSelector = ".id-" ++ ID.toString(id)
  let tlSelector = ".tl-" ++ TLID.toString(TL.id(tl))
  switch Native.Ext.querySelector(tokenSelector) {
  | Some(tokenDom) =>
    let sidebarWidth =
      Native.Ext.querySelector("#sidebar-left")
      |> Option.map(~f=Native.Ext.clientWidth)
      |> recoverOpt("can't find sidebar HTML body", ~default=320)

    let window = Webapi.Dom.window
    let viewport: Native.rect = {
      id: "#canvas",
      top: 0,
      left: sidebarWidth,
      right: Webapi.Dom.Window.innerWidth(window),
      bottom: Webapi.Dom.Window.innerHeight(window),
    }

    let tokenBox = Native.Ext.getBoundingClient(tokenDom, tokenSelector)
    let tlBox =
      Native.Ext.querySelector(tlSelector)
      |> Option.map(~f=dom => Native.Ext.getBoundingClient(dom, tlSelector))
      |> Option.unwrapUnsafe

    let tlPos = TL.pos(tl)
    // If the token's DOM element is out of viewport, we want to shift the canvas transform to bring it within view. To make the transition seem smooth, ideally we want only either move by y-axis or x-axis. Sometimes we might have to both by both axis. But since the only use case for this function at the moment is to find code from several statements before where you are currently looking at, the most likely case is the that the token we are looking for is above the top fold, therefore we are likely going to move by only the y-axis.
    let xTarget = if tokenBox.right > viewport.left && tokenBox.left < viewport.right {
      None
    } else {
      let offsetLeft = tokenBox.left - tlBox.left
      Some(tlPos.x - (sidebarWidth + offsetLeft))
    }

    let yTarget = if tokenBox.bottom > viewport.top && tokenBox.top < viewport.bottom {
      None
    } else {
      let offsetTop = tokenBox.top - tlBox.top
      Some(tlPos.y - (50 + offsetTop))
    }

    (xTarget, yTarget)
  | None => (None, None)
  }
}

let findNewPos = (m: model): Pos.t => {
  open Native
  switch m.currentPage {
  | Architecture | FocusedHandler(_) | FocusedDB(_) | SettingsModal(_) =>
    let o = m.canvasProps.offset
    // We add padding to the viewport range, to ensure we don't have new handlers too far from eachother.
    let padRight = 400
    let padBottom = 400
    let minX = switch m.sidebarState.mode {
    | DetailedMode => 320 + o.x
    | AbridgedMode => o.x
    }

    let window = Webapi.Dom.window
    let viewportWidth = Webapi.Dom.Window.innerWidth(window)
    let viewportHeight = Webapi.Dom.Window.innerHeight(window)

    let maxX = minX + (viewportWidth - padRight)
    let minY = o.y
    let maxY = minY + (viewportHeight - padBottom)
    {x: Random.range(minX, maxX), y: Random.range(minY, maxY)}
  | FocusedPackageManagerFn(_) | FocusedFn(_) | FocusedType(_) =>
    /* if the sidebar is open, the users can't see the livevalues, which
     * confused new users. Given we can't get z-index to work, moving it to the
     * side a little seems the best solution for now. */
    let xOffset = switch m.sidebarState.mode {
    | DetailedMode => 320
    | AbridgedMode => 0
    }

    let offset: Pos.t = {x: xOffset, y: 0}
    addPos(Pos.center, offset)
  }
}

let enablePan = (enablePan: bool, m: model): (model, AppTypes.cmd) => (
  {...m, canvasProps: {...m.canvasProps, enablePan: enablePan}},
  Cmd.none,
)
