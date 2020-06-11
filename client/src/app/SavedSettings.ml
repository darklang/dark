open Prelude

let canonicalizeCursorState = CursorState.unwrap

let canonicalizeHandlerProps props =
  StrDict.map props ~f:(fun v ->
      { v with
        handlerState =
          ( match v.handlerState with
          | HandlerExpanded ->
              HandlerExpanded
          | HandlerPrepCollapse ->
              HandlerCollapsed
          | HandlerCollapsing ->
              HandlerCollapsed
          | HandlerCollapsed ->
              HandlerCollapsed
          | HandlerExpanding ->
              HandlerExpanded ) })


let toModel (e : savedSettings) : model =
  let m = Defaults.defaultModel in
  { m with
    editorSettings = e.editorSettings
  ; cursorState = e.cursorState |> canonicalizeCursorState
  ; tlTraceIDs = e.tlTraceIDs
  ; featureFlags = e.featureFlags
  ; handlerProps = e.handlerProps |> canonicalizeHandlerProps
  ; canvasProps = {m.canvasProps with offset = e.canvasPos}
  ; lastReload = e.lastReload
  ; sidebarState = e.sidebarState
  ; showTopbar = e.showTopbar
  ; firstVisitToThisCanvas = e.firstVisitToThisCanvas
  ; tooltipState =
      { m.tooltipState with
        userTutorial = {m.tooltipState.userTutorial with step = e.userTutorial}
      } }


let model2editor (m : model) : savedSettings =
  { editorSettings = m.editorSettings
  ; cursorState = m.cursorState
  ; tlTraceIDs = (* what trace is selected *)
                 m.tlTraceIDs
  ; featureFlags = (* which flags are expanded *)
                   m.featureFlags
  ; handlerProps = m.handlerProps
  ; canvasPos = m.canvasProps.offset
  ; lastReload = m.lastReload
  ; sidebarState = m.sidebarState
  ; showTopbar = m.showTopbar
  ; firstVisitToThisCanvas = m.firstVisitToThisCanvas
  ; userTutorial = m.tooltipState.userTutorial.step }


let fromString (json : string option) : savedSettings =
  match json with
  | None ->
      Debug.loG "no serialized editor" None ;
      Defaults.defaultSavedSettings
  | Some json ->
    ( try json |> Json.parseOrRaise |> Decoders.savedSettings
      with e ->
        Debug.loG "error parsing serialized editor" e ;
        Defaults.defaultSavedSettings )


let toString (se : savedSettings) : string =
  se |> Encoders.savedSettings |> Json.stringify


let save (m : model) : unit =
  let state = m |> model2editor |> toString in
  Dom.Storage.setItem
    ("editorState-" ^ m.canvasName)
    state
    Dom.Storage.localStorage


let load (canvasName : string) : savedSettings =
  Dom.Storage.localStorage
  |> Dom.Storage.getItem ("editorState-" ^ canvasName)
  |> fromString
