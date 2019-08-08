open Types
open Tc

let showTLID = Prelude.showTLID

let tlidOf = Prelude.tlidOf

let fromString (json : string option) : serializableEditor =
  match json with
  | None ->
      Debug.loG "no serialized editor" None ;
      Defaults.defaultEditor
  | Some json ->
    ( try json |> Json.parseOrRaise |> Decoders.serializableEditor with e ->
        Debug.loG "error parsing serialized editor" e ;
        Defaults.defaultEditor )


let rec stripDragging (cs : cursorState) : cursorState =
  match cs with Dragging (_, _, _, state) -> stripDragging state | _ -> cs


let toString (se : serializableEditor) : string =
  se |> Encoders.serializableEditor |> Json.stringify


let editor2model (e : serializableEditor) : model =
  let m = Defaults.defaultModel in
  let finalHandlerStates props =
    props
    |> StrDict.map ~f:(fun v ->
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
                   HandlerExpanded ) } )
  in
  { m with
    timersEnabled = e.timersEnabled
  ; cursorState = e.cursorState |> stripDragging
  ; routingTableOpenDetails = e.routingTableOpenDetails
  ; tlCursors = e.tlCursors
  ; featureFlags = e.featureFlags
  ; handlerProps = finalHandlerStates e.handlerProps
  ; canvasProps = {m.canvasProps with offset = e.canvasPos}
  ; lastReload = e.lastReload }


let model2editor (m : model) : serializableEditor =
  { timersEnabled = m.timersEnabled (* are timers enabled *)
  ; cursorState = m.cursorState
  ; routingTableOpenDetails =
      m.routingTableOpenDetails (* state of the routing table *)
  ; tlCursors = m.tlCursors (* what trace cursor is selected *)
  ; featureFlags = m.featureFlags (* which flags are expanded *)
  ; handlerProps = m.handlerProps
  ; canvasPos = m.canvasProps.offset
  ; lastReload = m.lastReload }


let getHandlerProps (tlid : tlid) (m : model) : handlerProp =
  TLIDDict.get ~tlid m.handlerProps
  |> Option.withDefault ~default:Defaults.defaultHandlerProp


let setHandlerLock (tlid : tlid) (lock : bool) (m : model) : model =
  let updateProps prop =
    match prop with
    | Some p ->
        Some {p with handlerLock = lock}
    | None ->
        Some {Defaults.defaultHandlerProp with handlerLock = lock}
  in
  let props = m.handlerProps |> TLIDDict.update ~tlid ~f:updateProps in
  {m with handlerProps = props}


let setHandlerState (tlid : tlid) (state : handlerState) (m : model) : model =
  let updateProps prop =
    match prop with
    | Some p ->
        Some {p with handlerState = state}
    | None ->
        Some {Defaults.defaultHandlerProp with handlerState = state}
  in
  let props = m.handlerProps |> TLIDDict.update ~tlid ~f:updateProps in
  {m with handlerProps = props}


let setHandlerMenu (tlid : tlid) (show : bool) (m : model) : model =
  let updateProps prop =
    match prop with
    | Some p ->
        Some {p with showActions = show}
    | None ->
        Some {Defaults.defaultHandlerProp with showActions = show}
  in
  let props = m.handlerProps |> TLIDDict.update ~tlid ~f:updateProps in
  {m with handlerProps = props}


let serialize (m : model) : unit =
  let state = m |> model2editor |> toString in
  Dom.Storage.setItem
    ("editorState-" ^ m.canvasName)
    state
    Dom.Storage.localStorage


let closeMenu (m : model) : model =
  match tlidOf m.cursorState with
  | Some tlid ->
      setHandlerMenu tlid false m
  | None ->
      m
