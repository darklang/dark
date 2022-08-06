open Prelude
module Cmd = Tea.Cmd
module Navigation = Tea.Navigation
module TL = Toplevel

type page = AppTypes.Page.t

type modification = AppTypes.modification
type model = AppTypes.model
module Mod = AppTypes.Modification

let tlidOf = (page: page): option<TLID.t> =>
  switch page {
  | SettingsModal(_) | Architecture => None
  | FocusedPackageManagerFn(tlid)
  | FocusedFn(tlid, _)
  | FocusedHandler(tlid, _, _)
  | FocusedDB(tlid, _)
  | FocusedType(tlid) =>
    Some(tlid)
  }

let calculatePanOffset = (m: model, tl: toplevel, page: page): model => {
  let center = switch page {
  | FocusedHandler(_, _, center) | FocusedDB(_, center) => center
  | _ => false
  }

  let offset = if center {
    Viewport.centerCanvasOn(tl)
  } else {
    m.canvasProps.offset
  }

  let panAnimation = if offset != m.canvasProps.offset {
    AppTypes.CanvasProps.AnimateTransition
  } else {
    DontAnimateTransition
  }

  let boId = {
    let idInToplevel = id =>
      switch TL.find(tl, id) {
      | Some(_) => Some(id)
      | None => None
      }

    switch m.cursorState {
    | Selecting(tlid, sid) if tlid == TL.id(tl) =>
      switch sid {
      | Some(id) => idInToplevel(id)
      | None => None
      }
    | _ => None
    }
  }

  {
    ...m,
    currentPage: page,
    cursorState: Selecting(TL.id(tl), boId),
    canvasProps: {...m.canvasProps, offset: offset, panAnimation: panAnimation, lastOffset: None},
  }
}

let setPageTraceID = (oldPage: page, traceID: traceID): page =>
  switch oldPage {
  | FocusedHandler(tlid, _, center) => FocusedHandler(tlid, Some(traceID), center)
  | FocusedFn(tlid, _) => FocusedFn(tlid, Some(traceID))
  | _ => oldPage
  }

let getTraceID = (page: page): option<traceID> =>
  switch page {
  | FocusedPackageManagerFn(_)
  | FocusedDB(_)
  | FocusedType(_)
  | SettingsModal(_)
  | Architecture =>
    None
  | FocusedFn(_, traceId) | FocusedHandler(_, traceId, _) => traceId
  }

let updatePossibleTrace = (m: model, page: page): (model, AppTypes.cmd) => {
  let tlid = tlidOf(page) |> Option.unwrap(~default=gtlid())
  switch getTraceID(page) {
  | Some(tid) =>
    let m = {
      let trace = TLID.Dict.fromList(list{
        (tlid, list{(tid, Error(AnalysisTypes.TraceError.NoneYet))}),
      })

      Analysis.updateTraces(m, trace)
    }

    let m = Analysis.setSelectedTraceID(m, tlid, tid)
    Analysis.analyzeFocused(m)
  | None => (m, Cmd.none)
  }
}

let setPage = (m: model, oldPage: page, newPage: page): model =>
  switch (oldPage, newPage) {
  | (SettingsModal(_), FocusedPackageManagerFn(tlid))
  | (Architecture, FocusedPackageManagerFn(tlid))
  | (FocusedHandler(_), FocusedPackageManagerFn(tlid))
  | (FocusedDB(_), FocusedPackageManagerFn(tlid))
  | (SettingsModal(_), FocusedFn(tlid, _))
  | (Architecture, FocusedFn(tlid, _))
  | (FocusedHandler(_), FocusedFn(tlid, _))
  | (FocusedDB(_), FocusedFn(tlid, _))
  | (SettingsModal(_), FocusedType(tlid))
  | (Architecture, FocusedType(tlid))
  | (FocusedHandler(_), FocusedType(tlid))
  | (FocusedDB(_), FocusedType(tlid)) => /* Going from non-fn/type page to fn/type page.
     * Save the canvas position; set offset to origin
     */
    {
      ...m,
      currentPage: newPage,
      canvasProps: {
        ...m.canvasProps,
        lastOffset: Some(m.canvasProps.offset),
        offset: Pos.origin,
      },
      cursorState: Selecting(tlid, None),
    }
  | (FocusedFn(oldtlid, otid), FocusedFn(newtlid, tid)) if oldtlid === newtlid && otid != tid => {
      ...m,
      currentPage: newPage,
      cursorState: Selecting(newtlid, None),
    }
  | (FocusedPackageManagerFn(oldtlid), FocusedPackageManagerFn(newtlid))
  | (FocusedType(oldtlid), FocusedPackageManagerFn(newtlid))
  | (FocusedPackageManagerFn(oldtlid), FocusedType(newtlid))
  | (FocusedFn(oldtlid, _), FocusedFn(newtlid, _))
  | (FocusedType(oldtlid), FocusedFn(newtlid, _))
  | (FocusedFn(oldtlid, _), FocusedType(newtlid))
  | (FocusedPackageManagerFn(oldtlid), FocusedFn(newtlid, _))
  | (FocusedFn(oldtlid, _), FocusedPackageManagerFn(newtlid))
  | (FocusedType(oldtlid), FocusedType(newtlid)) =>
    /* Going between fn pages
     * Check they are not the same user function;
     * reset offset to origin, just in case user moved around on the fn page
     */
    if oldtlid === newtlid {
      m
    } else {
      {
        ...m,
        currentPage: newPage,
        canvasProps: {...m.canvasProps, offset: Pos.origin},
        cursorState: Selecting(newtlid, None),
      }
    }
  | (FocusedPackageManagerFn(_), FocusedHandler(tlid, _, _))
  | (FocusedPackageManagerFn(_), FocusedDB(tlid, _))
  | (FocusedFn(_), FocusedHandler(tlid, _, _))
  | (FocusedFn(_), FocusedDB(tlid, _))
  | (FocusedType(_), FocusedHandler(tlid, _, _))
  | (FocusedType(_), FocusedDB(tlid, _)) =>
    /* Going from Fn/Type to focused DB/hanlder
     * Jump to position where the toplevel is located
     */
    let tl = TL.get(m, tlid)
    let offset =
      Option.map(~f=\">>"(TL.pos, Viewport.toCenteredOn), tl) |> recoverOpt(
        "tl not found",
        ~default=m.canvasProps.offset,
      )

    {
      ...m,
      currentPage: newPage,
      cursorState: Selecting(tlid, None),
      canvasProps: {...m.canvasProps, offset: offset, lastOffset: None},
    }
  | (SettingsModal(_), FocusedHandler(tlid, _, _))
  | (SettingsModal(_), FocusedDB(tlid, _))
  | (Architecture, FocusedHandler(tlid, _, _))
  | (Architecture, FocusedDB(tlid, _)) =>
    /* Going from Architecture to focused db/handler
     * Figure out if you can stay where you are or animate pan to toplevel pos
     */
    TL.get(m, tlid)
    |> Option.map(~f=tl => calculatePanOffset(m, tl, newPage))
    |> recoverOpt("switching to missing tl", ~default=m)
  | (FocusedHandler(oldtlid, otid, _), FocusedHandler(newtlid, tid, _))
    if oldtlid === newtlid && otid != tid => {...m, currentPage: newPage}
  | (FocusedHandler(otlid, _, _), FocusedHandler(tlid, _, _))
  | (FocusedHandler(otlid, _, _), FocusedDB(tlid, _))
  | (FocusedDB(otlid, _), FocusedHandler(tlid, _, _))
  | (FocusedDB(otlid, _), FocusedDB(tlid, _)) =>
    /* Going from focused db/handler to another focused db/handler
     * Check it is a different tl;
     * figure out if you can stay where you are or animate pan to toplevel pos
     */
    if otlid == tlid {
      m
    } else {
      TL.get(m, tlid)
      |> Option.map(~f=tl => calculatePanOffset(m, tl, newPage))
      |> recoverOpt("switching to missing tl", ~default=m)
    }
  | (FocusedPackageManagerFn(_), Architecture)
  | (FocusedFn(_), Architecture)
  | (FocusedType(_), Architecture) =>
    /* Going from fn back to Architecture
     * Return to the previous position you were on the canvas
     */
    let offset = switch m.canvasProps.lastOffset {
    | Some(lo) => lo
    | None => m.canvasProps.offset
    }

    {
      ...m,
      currentPage: newPage,
      canvasProps: {...m.canvasProps, offset: offset, lastOffset: None},
    }
  | (_, SettingsModal(tab)) =>
    let settingsView = SettingsView.update(m.settingsView, OpenSettingsView(tab))

    {...m, settingsView: settingsView}
  | (_, Architecture) => /* Anything else to Architecture
     * Stay where you are, Deselect
     */
    {...m, currentPage: newPage, cursorState: Deselected}
  }

/* Go back to Architecture view if user is on the type/fn page
 and then deletes same UserType/UserFunction */
let maybeChangeFromPage = (tlid: TLID.t, page: page): list<modification> =>
  switch tlidOf(page) {
  | Some(ptlid) if ptlid == tlid => list{SetPage(Architecture)}
  | _ => list{}
  }

let getPageFromTLID = (m: model, tlid: TLID.t): page => {
  let hasKey = dict => List.any(~f=key => key == tlid, Map.keys(dict))
  if hasKey(m.deletedHandlers) || hasKey(m.handlers) {
    FocusedHandler(tlid, None, true)
  } else if hasKey(m.dbs) || hasKey(m.deletedDBs) {
    FocusedDB(tlid, true)
  } else if hasKey(m.userFunctions) || hasKey(m.deletedUserFunctions) {
    FocusedFn(tlid, None)
  } else if hasKey(m.userTypes) || hasKey(m.deleteduserTypes) {
    FocusedType(tlid)
  } else {
    Architecture
  }
}
