open Prelude

let canonicalizeCursorState = CursorState.unwrap

let toModel = (e: savedSettings): model => {
  let m = Defaults.defaultModel
  {
    ...m,
    editorSettings: e.editorSettings,
    cursorState: e.cursorState |> canonicalizeCursorState,
    tlTraceIDs: e.tlTraceIDs,
    featureFlags: e.featureFlags,
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

let model2editor = (m: model): savedSettings => {
  editorSettings: m.editorSettings,
  cursorState: m.cursorState,
  tlTraceIDs: // what trace is selected
  m.tlTraceIDs,
  featureFlags: // which flags are expanded
  m.featureFlags,
  handlerProps: m.handlerProps,
  canvasPos: m.canvasProps.offset,
  lastReload: m.lastReload,
  sidebarState: m.sidebarState,
  showTopbar: m.showTopbar,
  firstVisitToThisCanvas: m.firstVisitToThisCanvas,
  userTutorial: m.tooltipState.userTutorial.step,
  userTutorialTLID: m.tooltipState.userTutorial.tlid,
}

let fromString = (json: option<string>): savedSettings =>
  switch json {
  | None =>
    Debug.loG("no serialized editor", None)
    Defaults.defaultSavedSettings
  | Some(json) =>
    try json |> Json.parseOrRaise |> Decoders.savedSettings catch {
    | e =>
      Debug.loG("error parsing serialized editor", (e, json))
      Defaults.defaultSavedSettings
    }
  }

let toString = (se: savedSettings): string => se |> Encoders.savedSettings |> Json.stringify

let save = (m: model): unit => {
  let state = m |> model2editor |> toString
  Dom.Storage.setItem("editorState-" ++ m.canvasName, state, Dom.Storage.localStorage)
}

let load = (canvasName: string): savedSettings =>
  Dom.Storage.localStorage |> Dom.Storage.getItem("editorState-" ++ canvasName) |> fromString
