open Prelude

(* Dark *)
module B = BlankOr

type viewProps = ViewUtils.viewProps

type domEventList = ViewUtils.domEventList

let inUnit = ViewUtils.intAsUnit

let fontAwesome = ViewUtils.fontAwesome

let viewText = ViewBlankOr.viewText

let handlerIsExeComplete (vp : viewProps) : bool =
  match vp.handlerProp with Some hp -> hp.execution = Complete | None -> false


(* If a handler's execution has failed, we want to display an X instead of a
 * check. We define failure here as DIncomplete, DError, and DErrorRail *)
let handlerIsExeFail (vp : viewProps) : bool =
  if not (handlerIsExeComplete vp)
  then false
  else
    let outermostId =
      match vp.tl with
      | TLHandler handler ->
          Some (FluidAST.toID handler.ast)
      | _ ->
          None
    in
    outermostId
    |> Option.andThen ~f:(Analysis.getLiveValue' vp.analysisStore)
    |> Option.map ~f:(fun outermostResult ->
           match outermostResult with
           | DIncomplete _ | DError _ | DErrorRail _ ->
               true
           | _ ->
               false)
    |> Option.withDefault ~default:false


let triggerHandlerButton (vp : viewProps) (spec : handlerSpec) : msg Html.html =
  match (spec.space, spec.name, spec.modifier) with
  (* Hide button if spec is not filled out because trace id
   is needed to recover handler traces on refresh. *)
  | F (_, a), F (_, b), F (_, c)
    when List.any ~f:(fun s -> String.length s = 0) [a; b; c] ->
      Vdom.noNode
  | F _, F _, F _ ->
      if vp.permission = Some ReadWrite
      then
        let hasData =
          Analysis.selectedTraceID vp.tlTraceIDs vp.traces vp.tlid
          |> Option.andThen ~f:(fun trace_id ->
                 List.find ~f:(fun (id, _) -> id = trace_id) vp.traces
                 |> Option.andThen ~f:(fun (_, data) -> data |> Result.toOption))
          |> Option.is_some
        in
        let classes =
          Html.classList
            [ ("handler-trigger", true)
            ; ("is-executing", vp.isExecuting)
            ; ("inactive", not hasData)
            ; ("complete", handlerIsExeComplete vp)
            ; ("failed", handlerIsExeFail vp) ]
        in
        let attrs =
          if hasData
          then
            [ Html.title "Replay this execution"
            ; ViewUtils.eventNoPropagation
                ~key:("lh" ^ "-" ^ TLID.toString vp.tlid)
                "click"
                (fun _ -> TriggerHandler vp.tlid)
            ; ViewUtils.onAnimationEnd
                ~key:("exe" ^ "-" ^ TLID.toString vp.tlid)
                ~listener:(fun name ->
                  if name = "fadeIn"
                  then SetHandlerExeIdle vp.tlid
                  else IgnoreMsg "trigger-animation-end") ]
          else
            [ Html.title "Need input data to replay execution"
            ; Html.noProp
            ; Html.noProp ]
        in
        Html.div (classes :: attrs) [fontAwesome "redo"]
      else Vdom.noNode
  | _, _, _ ->
      Vdom.noNode


let externalLink (vp : viewProps) (name : string) =
  let urlPath =
    let currentTraceData =
      Analysis.selectedTraceID vp.tlTraceIDs vp.traces vp.tlid
      |> Option.andThen ~f:(fun trace_id ->
             List.find ~f:(fun (id, _) -> id = trace_id) vp.traces
             |> Option.andThen ~f:(fun (_, data) -> data |> Result.toOption))
    in
    match currentTraceData with
    | Some data ->
        Runtime.pathFromInputVars data.input |> Option.withDefault ~default:name
    | None ->
        name
  in
  "//" ^ Tea.Http.encodeUri vp.canvasName ^ "." ^ vp.userContentHost ^ urlPath


let viewMenu (vp : viewProps) (spec : handlerSpec) : msg Html.html =
  let tlid = vp.tlid in
  let actions =
    let commonAction : TLMenu.menuItem =
      { title = "Delete"
      ; key = "del-tl-"
      ; icon = Some "times"
      ; action = (fun _ -> ToplevelDelete tlid)
      ; disableMsg = None }
    in
    match (spec.space, spec.modifier, spec.name) with
    | F (_, "HTTP"), F (_, meth), F (_, name) ->
        let curlAction : TLMenu.menuItem =
          { title = "Copy request as cURL"
          ; key = "copy-curl-"
          ; icon = Some "copy"
          ; action = (fun m -> CopyCurl (tlid, m.mePos))
          ; disableMsg = None }
        in
        let httpActions = [curlAction; commonAction] in
        if meth = "GET"
        then
          let url = externalLink vp name in
          let newTabAction : TLMenu.menuItem =
            { title = "Open in new tab"
            ; key = "new-tab-"
            ; icon = Some "external-link-alt"
            ; action = (fun _ -> NewTabFromTLMenu (url, tlid))
            ; disableMsg = None }
          in
          newTabAction :: httpActions
        else httpActions
    | _ ->
        [commonAction]
  in
  TLMenu.viewMenu vp.menuState tlid actions


let viewEventSpec
    (vp : viewProps) (spec : handlerSpec) (dragEvents : domEventList) :
    msg Html.html =
  let viewEventName =
    viewText ~enterable:true ~classes:["toplevel-name"] EventName vp spec.name
  in
  let viewEventSpace =
    viewText ~enterable:true ~classes:["space"] EventSpace vp spec.space
  in
  let viewEventModifier =
    let viewMod =
      viewText
        ~enterable:true
        ~classes:["modifier"]
        EventModifier
        vp
        spec.modifier
    in
    match (spec.space, spec.modifier, spec.name) with
    | F (_, "HTTP"), _, _ | F (_, "CRON"), _, _ ->
        Html.div [Html.class' "modifier"] [viewMod]
    | _ ->
        Vdom.noNode
  in
  let baseClass = "spec-header" in
  let classes =
    match (spec.space, spec.modifier) with
    | F (_, "HTTP"), F (_, "GET") ->
        baseClass ^ " http-get"
    | F (_, "HTTP"), F (_, "POST") ->
        baseClass ^ " http-post"
    | F (_, "HTTP"), F (_, "PUT") ->
        baseClass ^ " http-put"
    | F (_, "HTTP"), F (_, "DELETE") ->
        baseClass ^ " http-delete"
    | F (_, "HTTP"), F (_, "PATCH") ->
        baseClass ^ " http-patch"
    | F (_, "HTTP"), F (_, "OPTIONS") ->
        baseClass ^ " http-options"
    | F (_, "CRON"), _ ->
        baseClass ^ " cron"
    | F (_, "WORKER"), _ ->
        baseClass ^ " worker"
    | F (_, "REPL"), _ ->
        baseClass ^ " repl"
    | _ ->
        baseClass
  in
  let viewActions =
    let triggerBtn = triggerHandlerButton vp spec in
    Html.div [Html.class' "handler-actions"] [triggerBtn; viewMenu vp spec]
  in
  let viewType =
    Html.div [Html.class' "toplevel-type"] [viewEventSpace; viewEventModifier]
  in
  Html.div
    (Html.class' classes :: dragEvents)
    [viewType; viewEventName; viewActions]


let handlerAttrs (tlid : TLID.t) (state : handlerState) : msg Vdom.property list
    =
  let sid = TLID.toString tlid in
  let codeHeight id =
    let open Webapi.Dom in
    Document.querySelector (".toplevel.tl-" ^ id ^ " .handler-body") document
    |> Option.map ~f:(fun el -> Element.scrollHeight el)
    |> Option.withDefault ~default:0
  in
  match state with
  | HandlerExpanding ->
      let h = inUnit (codeHeight sid) "px" in
      [ Html.class' "handler-body expand"
      ; Html.style "height" h
      ; ViewUtils.onTransitionEnd ~key:("hdlexp-" ^ sid) ~listener:(fun prop ->
            if prop = "opacity"
            then UpdateHandlerState (tlid, HandlerExpanded)
            else IgnoreMsg "handler-expanding") ]
  | HandlerExpanded ->
      [ Html.class' "handler-body expand"
      ; Html.style "height" "auto"
      ; Vdom.noProp ]
  | HandlerPrepCollapse ->
      let h = inUnit (codeHeight sid) "px" in
      [ Html.class' "handler-body"
      ; Html.style "height" h
      ; ViewUtils.onTransitionEnd ~key:("hdlpcol-" ^ sid) ~listener:(fun prop ->
            if prop = "opacity"
            then UpdateHandlerState (tlid, HandlerCollapsing)
            else IgnoreMsg "handler-prep-collapse") ]
  | HandlerCollapsing ->
      [ Html.class' "handler-body"
      ; Html.style "height" "0"
      ; ViewUtils.onTransitionEnd
          ~key:("hdlcolng-" ^ sid)
          ~listener:(fun prop ->
            if prop = "height"
            then UpdateHandlerState (tlid, HandlerCollapsed)
            else IgnoreMsg "handler-collapsing") ]
  | HandlerCollapsed ->
      [Html.class' "handler-body"; Html.style "height" "0"; Vdom.noProp]


let view (vp : viewProps) (h : handler) (dragEvents : domEventList) :
    msg Html.html list =
  let attrs = handlerAttrs vp.tlid (ViewUtils.getHandlerState vp) in
  let ast = Html.div attrs (FluidView.view vp dragEvents) in
  let header = viewEventSpec vp h.spec dragEvents in
  [header; ast]
