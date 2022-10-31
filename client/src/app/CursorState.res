open Types

type cursorState = AppTypes.CursorState.t

let rec unwrap = (s: cursorState): cursorState =>
  switch s {
  | DraggingTL(_, _, _, nested) | PanningCanvas({prevCursorState: nested, _}) => unwrap(nested)
  | Omnibox(_) | Entering(_) | Selecting(_) | Deselected | FluidEntering(_) => s
  }

let tlidOf = (s: cursorState): option<TLID.t> =>
  switch unwrap(s) {
  | Selecting(tlid, _) => Some(tlid)
  | Entering(tlid, _) => Some(tlid)
  | Omnibox(_) => None
  | Deselected => None
  | FluidEntering(tlid) => Some(tlid)
  // NOTE: These have no id because unwrapCursorState should unwrap them
  | DraggingTL(_) | PanningCanvas(_) => None
  }

let idOf = (s: cursorState): option<id> =>
  switch unwrap(s) {
  | Selecting(_, id) => id
  | Omnibox(_) => None
  | Entering(_, id) => Some(id)
  | Deselected => None
  | FluidEntering(_) => None
  // NOTE: These have no id because unwrapCursorState should unwrap them
  | DraggingTL(_) | PanningCanvas(_) => None
  }

let focusEntry = (m: AppTypes.Model.t): AppTypes.cmd =>
  switch unwrap(m.cursorState) {
  | Entering(_) | Omnibox(_) => Tea_html_cmds.focus(Defaults.entryID)
  | Selecting(_) | Deselected | FluidEntering(_) | DraggingTL(_) | PanningCanvas(_) => Tea.Cmd.none
  }

// Based on Tea_html_cmds, applies offset after focus
let focusWithOffset = (id, offset) =>
  Tea.Cmd.call(_ => {
    let ecb = _ignored => {
      let elem =
        Webapi.Dom.document
        ->Webapi.Dom.Document.getElementById(id)
        ->Belt.Option.flatMap(Webapi.Dom.HtmlInputElement.ofElement)
      switch elem {
      | Some(input) =>
        input->Webapi.Dom.HtmlInputElement.setSelectionRange(offset, offset)
        input->Webapi.Dom.HtmlInputElement.focus
      | None => ()
      }
    }

    // One to get out of the current render frame
    let cb = _ignored => ignore(Webapi.requestAnimationFrame(ecb))
    // And another to properly focus
    ignore(Webapi.requestAnimationFrame(cb))
  })

let focusEntryWithOffset = (m: AppTypes.model, offset: int): AppTypes.cmd =>
  switch unwrap(m.cursorState) {
  | Entering(_) | Omnibox(_) => focusWithOffset(Defaults.entryID, offset)
  | Selecting(_) | Deselected | FluidEntering(_) | DraggingTL(_) | PanningCanvas(_) => Tea.Cmd.none
  }

let setCursorState = (cursorState: cursorState, m: AppTypes.model): (
  AppTypes.model,
  AppTypes.cmd,
) => {
  let m = {...m, cursorState: cursorState}
  /* TODO: Move the focusEntry part of this out, to happen
   * once all modifications have been applied. It's currently
   * too easy to call this only to have a later modification
   * change the model in such a way that the resulting view
   * no longer contains the thing we want to focus. */
  (m, focusEntry(m))
}

let rec toString = (cursorState: cursorState): string =>
  switch cursorState {
  | Deselected => "Deselected"
  | Selecting(tlid, Some(id)) => `Selecting(${TLID.toString(tlid)}, ${ID.toString(id)})`
  | Selecting(tlid, None) => `Selecting(${TLID.toString(tlid)}, None)`
  | Entering(tlid, id) => `Entering(${TLID.toString(tlid)}, ${ID.toString(id)})`
  | DraggingTL(tlid, pos, hasMoved, prev) =>
    `DraggingTL(${TLID.toString(tlid)}, ${VPos.toString(pos)}, ${Tc.Bool.toString(
        hasMoved,
      )}, ${toString(prev)})`
  | PanningCanvas(p) =>
    `PanningCanvas(${VPos.toString(p.viewportStart)}, ${VPos.toString(p.viewportCurr)}, ${toString(
        p.prevCursorState,
      )})`
  | Omnibox(pos) =>
    `Omnibox(${pos->Tc.Option.map(~f=Pos.toString)->Tc.Option.unwrap(~default="None")})`
  | FluidEntering(tlid) => `FluidEntering(${TLID.toString(tlid)})`
  }
