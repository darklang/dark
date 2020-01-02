open Types
open Tc

let fromString (json : string option) : serializableEditor =
  match json with
  | None ->
      Debug.loG "no serialized editor" None ;
      Defaults.defaultEditor
  | Some json ->
    ( try json |> Json.parseOrRaise |> Decoders.serializableEditor
      with e ->
        Debug.loG "error parsing serialized editor" e ;
        Defaults.defaultEditor )


let canonicalizeCursorState (cs : cursorState) : cursorState =
  Prelude.unwrapCursorState cs


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


let toString (se : serializableEditor) : string =
  se |> Encoders.serializableEditor |> Json.stringify


let editor2model (e : serializableEditor) : model =
  let m = Defaults.defaultModel in
  { m with
    editorSettings = e.editorSettings
  ; cursorState = e.cursorState |> canonicalizeCursorState
  ; routingTableOpenDetails = e.routingTableOpenDetails
  ; tlTraceIDs = e.tlTraceIDs
  ; featureFlags = e.featureFlags
  ; handlerProps = e.handlerProps |> canonicalizeHandlerProps
  ; canvasProps = {m.canvasProps with offset = e.canvasPos}
  ; lastReload = e.lastReload
  ; sidebarOpen = e.sidebarOpen
  ; showTopbar = e.showTopbar }


let model2editor (m : model) : serializableEditor =
  { editorSettings = m.editorSettings
  ; cursorState = m.cursorState
  ; routingTableOpenDetails =
      (* state of the routing table *)
      m.routingTableOpenDetails
  ; tlTraceIDs = (* what trace is selected *)
                 m.tlTraceIDs
  ; featureFlags = (* which flags are expanded *)
                   m.featureFlags
  ; handlerProps = m.handlerProps
  ; canvasPos = m.canvasProps.offset
  ; lastReload = m.lastReload
  ; sidebarOpen = m.sidebarOpen
  ; showTopbar = m.showTopbar }


let serialize (m : model) : unit =
  let state = m |> model2editor |> toString in
  Dom.Storage.setItem
    ("editorState-" ^ m.canvasName)
    state
    Dom.Storage.localStorage
