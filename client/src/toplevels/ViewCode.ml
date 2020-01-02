open Tc
open Prelude
open Types

(* Tea *)
module Svg = Tea.Svg

(* Dark *)
module B = BlankOr

type viewState = ViewUtils.viewState

type domEventList = ViewUtils.domEventList

let inUnit = ViewUtils.intAsUnit

let toggleButton = ViewUtils.toggleIconButton

let idConfigs = ViewBlankOr.idConfigs

let fontAwesome = ViewUtils.fontAwesome

let viewText = ViewBlankOr.viewText

let wc = ViewBlankOr.wc

let text = ViewBlankOr.text

let enterable = ViewBlankOr.Enterable

let handlerIsExecuting (vs : viewState) : bool =
  match vs.handlerProp with
  | Some hp ->
      hp.execution = Executing
  | None ->
      false


let handlerIsExeComplete (vs : viewState) : bool =
  match vs.handlerProp with Some hp -> hp.execution = Complete | None -> false


(* If a handler's execution has failed, we want to display an X instead of a
 * check. We define failure here as DIncomplete, DError, and DErrorRail *)
let handlerIsExeFail (vs : viewState) : bool =
  if not (handlerIsExeComplete vs)
  then false
  else
    let outermostId =
      match vs.tl with
      | TLHandler handler ->
          Some (FluidExpression.id handler.ast)
      | _ ->
          None
    in
    outermostId
    |> Option.andThen ~f:(Analysis.getLiveValue' vs.analysisStore)
    |> Option.map ~f:(fun outermostResult ->
           match outermostResult with
           | DIncomplete _ | DError _ | DErrorRail _ ->
               true
           | _ ->
               false)
    |> Option.withDefault ~default:false


let view (vs : viewState) (e : fluidExpr) =
  [Html.div [Html.class' "fluid-ast"] (Fluid.viewAST ~vs e)]


let triggerHandlerButton (vs : viewState) (spec : handlerSpec) : msg Html.html =
  match (spec.space, spec.name, spec.modifier) with
  (* Hide button if spec is not filled out because trace id
   is needed to recover handler traces on refresh. *)
  | F (_, a), F (_, b), F (_, c)
    when List.any ~f:(fun s -> String.length s = 0) [a; b; c] ->
      Vdom.noNode
  | F _, F _, F _ ->
      if vs.permission = Some ReadWrite
      then
        let hasData =
          Analysis.selectedTrace vs.tlTraceIDs vs.traces vs.tlid
          |> Option.andThen ~f:(fun trace_id ->
                 List.find ~f:(fun (id, _) -> id = trace_id) vs.traces
                 |> Option.andThen ~f:(fun (_, data) -> data))
          |> Option.is_some
        in
        let classes =
          Html.classList
            [ ("handler-trigger", true)
            ; ("is-executing", handlerIsExecuting vs)
            ; ("inactive", not hasData)
            ; ("complete", handlerIsExeComplete vs)
            ; ("failed", handlerIsExeFail vs) ]
        in
        let attrs =
          if hasData
          then
            [ Html.title "Replay this execution"
            ; ViewUtils.eventNoPropagation
                ~key:("lh" ^ "-" ^ showTLID vs.tlid)
                "click"
                (fun _ -> TriggerHandler vs.tlid)
            ; ViewUtils.onAnimationEnd
                ~key:("exe" ^ "-" ^ showTLID vs.tlid)
                ~listener:(fun name ->
                  if name = "fadeIn"
                  then SetHandlerExeIdle vs.tlid
                  else IgnoreMsg) ]
          else [Html.title "Need input data to replay execution"]
        in
        Html.div (classes :: attrs) [fontAwesome "redo"]
      else Vdom.noNode
  | _, _, _ ->
      Vdom.noNode


let externalLink (vs : viewState) (name : string) =
  let urlPath =
    let currentTraceData =
      Analysis.selectedTrace vs.tlTraceIDs vs.traces vs.tlid
      |> Option.andThen ~f:(fun trace_id ->
             List.find ~f:(fun (id, _) -> id = trace_id) vs.traces
             |> Option.andThen ~f:(fun (_, data) -> data))
    in
    match currentTraceData with
    | Some data ->
        Runtime.pathFromInputVars data.input |> Option.withDefault ~default:name
    | None ->
        name
  in
  Html.a
    [ Html.class' "external"
    ; Html.href
        ( "//"
        ^ Tea.Http.encodeUri vs.canvasName
        ^ "."
        ^ vs.userContentHost
        ^ urlPath )
    ; Html.target "_blank"
    ; ViewUtils.eventNoPropagation
        ~key:("hide-tl-opts" ^ showTLID vs.tlid)
        "click"
        (fun _ -> SetHandlerActionsMenu (vs.tlid, false)) ]
    [fontAwesome "external-link-alt"; Html.text "Open in new tab"]


let viewMenu (vs : viewState) (spec : handlerSpec) : msg Html.html =
  let strTLID = showTLID vs.tlid in
  let showMenu =
    match vs.handlerProp with Some hp -> hp.showActions | None -> false
  in
  let actions =
    let commonActions =
      [ Html.div
          [ ViewUtils.eventNoPropagation
              ~key:("del-tl-" ^ strTLID)
              "click"
              (fun _ -> ToplevelDelete vs.tlid) ]
          [fontAwesome "times"; Html.text "Delete handler"] ]
    in
    match (spec.space, spec.modifier, spec.name) with
    | F (_, "HTTP"), F (_, meth), F (_, name) ->
        let curlAction =
          Html.div
            [ ViewUtils.eventNoPropagation
                ~key:("del-tl-" ^ strTLID)
                "click"
                (fun m -> CopyCurl (vs.tlid, m.mePos)) ]
            [fontAwesome "copy"; Html.text "Copy request as cURL"]
        in
        let httpActions = curlAction :: commonActions in
        if meth = "GET"
        then externalLink vs name :: httpActions
        else httpActions
    | _ ->
        commonActions
  in
  let toggleMenu =
    toggleButton
      ~name:"toggle-btn"
      ~activeIcon:"bars"
      ~inactiveIcon:"bars"
      ~msg:(fun _ -> SetHandlerActionsMenu (vs.tlid, not showMenu))
      ~active:showMenu
      ~key:("toggle-tl-menu-" ^ strTLID)
  in
  Html.div
    [Html.classList [("more-actions", true); ("show", showMenu)]]
    [ toggleMenu
    ; Html.div
        [ Html.class' "actions"
        ; ViewUtils.eventNoPropagation
            ~key:("hide-tl-opts" ^ strTLID)
            "mouseleave"
            (fun _ -> SetHandlerActionsMenu (vs.tlid, false)) ]
        actions ]


let viewEventSpec
    (vs : viewState) (spec : handlerSpec) (dragEvents : domEventList) :
    msg Html.html =
  let viewEventName =
    let configs = (enterable :: idConfigs) @ [wc "handler-name"] in
    viewText EventName vs configs spec.name
  in
  let viewEventSpace =
    let configs = (enterable :: idConfigs) @ [wc "space"] in
    viewText EventSpace vs configs spec.space
  in
  let viewEventModifier =
    let configs = (enterable :: idConfigs) @ [wc "modifier"] in
    let viewMod = viewText EventModifier vs configs spec.modifier in
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
    let triggerBtn = triggerHandlerButton vs spec in
    Html.div [Html.class' "handler-actions"] [triggerBtn; viewMenu vs spec]
  in
  let viewType =
    Html.div [Html.class' "handler-type"] [viewEventSpace; viewEventModifier]
  in
  Html.div
    (Html.class' classes :: dragEvents)
    [viewType; viewEventName; viewActions]


let handlerAttrs (tlid : tlid) (state : handlerState) : msg Vdom.property list =
  let sid = showTLID tlid in
  let codeHeight id =
    let e =
      Native.Ext.querySelector (".toplevel.tl-" ^ id ^ " .handler-body")
    in
    match e with Some el -> Native.Ext.scrollHeight el | None -> 0
  in
  match state with
  | HandlerExpanding ->
      let h = inUnit (codeHeight sid) "px" in
      [ Html.class' "handler-body expand"
      ; Html.style "height" h
      ; ViewUtils.onTransitionEnd ~key:("hdlexp-" ^ sid) ~listener:(fun prop ->
            if prop = "opacity"
            then UpdateHandlerState (tlid, HandlerExpanded)
            else IgnoreMsg) ]
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
            else IgnoreMsg) ]
  | HandlerCollapsing ->
      [ Html.class' "handler-body"
      ; Html.style "height" "0"
      ; ViewUtils.onTransitionEnd
          ~key:("hdlcolng-" ^ sid)
          ~listener:(fun prop ->
            if prop = "height"
            then UpdateHandlerState (tlid, HandlerCollapsed)
            else IgnoreMsg) ]
  | HandlerCollapsed ->
      [Html.class' "handler-body"; Html.style "height" "0"; Vdom.noProp]


let viewHandler (vs : viewState) (h : handler) (dragEvents : domEventList) :
    msg Html.html list =
  let attrs = handlerAttrs vs.tlid (ViewUtils.getHandlerState vs) in
  let ast = Html.div attrs (view vs h.ast) in
  let header = viewEventSpec vs h.spec dragEvents in
  [header; ast]
