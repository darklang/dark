open Prelude

let canonicalizeCursorState = CursorState.unwrap

let toModel = (e: AppTypes.SavedSettings.t): AppTypes.model => {
  let m = AppTypes.Model.default
  {
    ...m,
    editorSettings: e.editorSettings,
    cursorState: e.cursorState |> canonicalizeCursorState,
    tlTraceIDs: e.tlTraceIDs,
    handlerProps: e.handlerProps,
    canvasProps: {...m.canvasProps, offset: e.canvasPos},
    lastReload: e.lastReload,
    sidebarState: e.sidebarState,
    showTopbar: e.showTopbar,
    firstVisitToThisCanvas: e.firstVisitToThisCanvas,
    tooltipState: {
      ...m.tooltipState,
      userTutorial: {step: e.userTutorial, tlid: e.userTutorialTLID},
    },
  }
}

let model2editor = (m: AppTypes.model): AppTypes.SavedSettings.t => {
  editorSettings: m.editorSettings,
  cursorState: m.cursorState,
  tlTraceIDs: // what trace is selected
  m.tlTraceIDs,
  handlerProps: m.handlerProps,
  canvasPos: m.canvasProps.offset,
  lastReload: m.lastReload,
  sidebarState: m.sidebarState,
  showTopbar: m.showTopbar,
  firstVisitToThisCanvas: m.firstVisitToThisCanvas,
  userTutorial: m.tooltipState.userTutorial.step,
  userTutorialTLID: m.tooltipState.userTutorial.tlid,
}

let fromString = (json: option<string>): AppTypes.SavedSettings.t =>
  switch json {
  | None =>
    Debug.loG("no serialized editor", None)
    AppTypes.SavedSettings.default
  | Some(json) =>
    try json |> Json.parseOrRaise |> AppTypes.SavedSettings.decode catch {
    | e =>
      Debug.loG("error parsing serialized editor", (e, json))
      AppTypes.SavedSettings.default
    }
  }

let toString = (se: AppTypes.SavedSettings.t): string =>
  se |> AppTypes.SavedSettings.encode |> Json.stringify

let save = (m: AppTypes.Model.t): unit => {
  let state = m |> model2editor |> toString
  Dom.Storage.setItem("editorState-" ++ m.canvasName, state, Dom.Storage.localStorage)
}

let load = (canvasName: string): AppTypes.SavedSettings.t =>
  Dom.Storage.localStorage |> Dom.Storage.getItem("editorState-" ++ canvasName) |> fromString
