open Prelude

module Html = Tea.Html
module Attrs = Tea.Attrs
module Events = Tea.Events

// Dark
module B = BlankOr

module Msg = AppTypes.Msg
type msg = AppTypes.msg

type viewProps = ViewUtils.viewProps

type domEventList = ViewUtils.domEventList

let fontAwesome = Icons.fontAwesome

let viewText = ViewBlankOr.viewText

let handlerIsExeComplete = (vp: viewProps): bool =>
  switch vp.handlerProp {
  | Some(hp) => hp.execution == Complete
  | None => false
  }

/* If a handler's execution has failed, we want to display an X instead of a
 * check. We define failure here as DIncomplete, DError, and DErrorRail */
let handlerIsExeFail = (vp: viewProps): bool =>
  if !handlerIsExeComplete(vp) {
    false
  } else {
    let outermostId = switch vp.tl {
    | TLHandler(handler) => Some(FluidAST.toID(handler.ast))
    | _ => None
    }

    outermostId
    |> Option.andThen(~f=Analysis.getLiveValue'(vp.analysisStore))
    |> Option.map(~f=outermostResult =>
      switch outermostResult {
      | RT.Dval.DIncomplete(_) | DError(_) | DErrorRail(_) => true
      | _ => false
      }
    )
    |> Option.unwrap(~default=false)
  }

let triggerHandlerButton = (vp: viewProps, spec: PT.Handler.Spec.t): Html.html<msg> =>
  switch spec {
  /* Hide button if spec is not filled out because trace id
   is needed to recover handler traces on refresh. */
  | PT.Handler.Spec.UnknownHandler(_)
  | PT.Handler.Spec.HTTP("", _, _)
  | PT.Handler.Spec.HTTP(_, "", _)
  | PT.Handler.Spec.Worker("", _)
  | PT.Handler.Spec.OldWorker("", _, _)
  | PT.Handler.Spec.OldWorker(_, "", _)
  | PT.Handler.Spec.Cron("", _, _)
  | PT.Handler.Spec.Cron(_, None, _)
  | PT.Handler.Spec.REPL("", _) => Vdom.noNode
  | _ =>
    if vp.permission == Some(ReadWrite) {
      let hasData =
        Analysis.selectedTraceID(vp.tlTraceIDs, vp.traces, vp.tlid)
        |> Option.andThen(~f=trace_id =>
          List.find(~f=((id, _)) => id == trace_id, vp.traces) |> Option.andThen(~f=((_, data)) =>
            data |> Result.toOption
          )
        )
        |> Option.is_some

      let classes = Attrs.classList(list{
        ("handler-trigger", true),
        ("is-executing", vp.isExecuting),
        ("inactive", !hasData),
        ("complete", handlerIsExeComplete(vp)),
        ("failed", handlerIsExeFail(vp)),
      })

      let attrs = if hasData {
        list{
          Attrs.title("Replay this execution"),
          EventListeners.eventNoPropagation(
            ~key="lh" ++ ("-" ++ TLID.toString(vp.tlid)),
            "click",
            _ => Msg.TriggerHandler(vp.tlid),
          ),
          EventListeners.onAnimationEnd(
            ~key="exe" ++ ("-" ++ TLID.toString(vp.tlid)),
            ~listener=name =>
              if name == "fadeIn" {
                Msg.SetHandlerExeIdle(vp.tlid)
              } else {
                AppTypes.Msg.IgnoreMsg("trigger-animation-end")
              },
          ),
        }
      } else {
        list{Attrs.title("Need input data to replay execution"), Attrs.noProp, Attrs.noProp}
      }

      Html.div(list{classes, ...attrs}, list{fontAwesome("redo")})
    } else {
      Vdom.noNode
    }
  }

let externalLink = (vp: viewProps, name: string) => {
  let urlPath = {
    let currentTraceData =
      Analysis.selectedTraceID(vp.tlTraceIDs, vp.traces, vp.tlid) |> Option.andThen(~f=trace_id =>
        List.find(~f=((id, _)) => id == trace_id, vp.traces) |> Option.andThen(~f=((_, data)) =>
          data |> Result.toOption
        )
      )

    switch currentTraceData {
    | Some(data) => Runtime.pathFromInputVars(data.input) |> Option.unwrap(~default=name)
    | None => name
    }
  }

  "//" ++ (Tea.Http.encodeUri(vp.canvasName) ++ ("." ++ (vp.userContentHost ++ urlPath)))
}

let viewMenu = (vp: viewProps, spec: PT.Handler.Spec.t): Html.html<msg> => {
  let tlid = vp.tlid
  let actions = {
    let commonAction: TLMenu.menuItem = {
      title: "Delete",
      key: "del-tl-",
      icon: Some("times"),
      action: _ => ToplevelDelete(tlid),
      disableMsg: None,
    }

    switch spec {
    | PT.Handler.Spec.HTTP(name, meth, _) =>
      let curlAction: TLMenu.menuItem = {
        title: "Copy request as cURL",
        key: "copy-curl-",
        icon: Some("copy"),
        action: m => CopyCurl(tlid, m.mePos),
        disableMsg: None,
      }

      let httpActions = list{curlAction, commonAction}
      if meth == "GET" {
        let url = externalLink(vp, name)
        let newTabAction: TLMenu.menuItem = {
          title: "Open in new tab",
          key: `new-tab-${url}`,
          icon: Some("external-link-alt"),
          action: _ => NewTabFromTLMenu(url, tlid),
          disableMsg: None,
        }

        list{newTabAction, ...httpActions}
      } else {
        httpActions
      }
    | _ => list{commonAction}
    }
  }

  TLMenu.viewMenu(vp.menuState, tlid, actions)
}

let viewEventSpec = (vp: viewProps, spec: PT.Handler.Spec.t, dragEvents: domEventList): Html.html<
  msg,
> => {
  let eventName = PT.Handler.Spec.name(spec)
  let viewEventName = viewText(
    ~enterable=true,
    ~classes=list{"toplevel-name"},
    EventName,
    vp,
    eventName,
  )

  let eventSpace = PT.Handler.Spec.space(spec)
  let viewEventSpace = viewText(~enterable=true, ~classes=list{"space"}, EventSpace, vp, eventSpace)

  let viewEventModifier = {
    switch PT.Handler.Spec.modifier(spec) {
    | Some(mod) =>
      let viewMod = viewText(~enterable=true, ~classes=list{"modifier"}, EventModifier, vp, mod)
      Html.div(list{Attrs.class("modifier")}, list{viewMod})
    | _ => Vdom.noNode
    }
  }

  let baseClass = "spec-header"
  let classes = switch spec {
  | PT.Handler.Spec.HTTP(_, "GET", _) => baseClass ++ " http-get"
  | PT.Handler.Spec.HTTP(_, "POST", _) => baseClass ++ " http-post"
  | PT.Handler.Spec.HTTP(_, "PUT", _) => baseClass ++ " http-put"
  | PT.Handler.Spec.HTTP(_, "DELETE", _) => baseClass ++ " http-delete"
  | PT.Handler.Spec.HTTP(_, "PATCH", _) => baseClass ++ " http-patch"
  | PT.Handler.Spec.HTTP(_, "OPTIONS", _) => baseClass ++ " http-options"
  | PT.Handler.Spec.Cron(_) => baseClass ++ " cron"
  | PT.Handler.Spec.Worker(_) => baseClass ++ " worker"
  | PT.Handler.Spec.REPL(_) => baseClass ++ " repl"
  | _ => baseClass
  }

  let viewActions = {
    let triggerBtn = triggerHandlerButton(vp, spec)
    Html.div(list{Attrs.class("handler-actions")}, list{triggerBtn, viewMenu(vp, spec)})
  }

  let viewType = Html.div(
    list{Attrs.class("toplevel-type")},
    list{viewEventSpace, viewEventModifier},
  )

  Html.div(list{Attrs.class(classes), ...dragEvents}, list{viewType, viewEventName, viewActions})
}

let handlerAttrs: list<Vdom.property<msg>> = list{
  Attrs.class("handler-body expand"),
  Attrs.style("height", "auto"),
  Vdom.noProp,
}

let view = (vp: viewProps, h: PT.Handler.t, dragEvents: domEventList): list<Html.html<msg>> => {
  let attrs = handlerAttrs
  let ast = Html.div(attrs, FluidView.view(vp, dragEvents))
  let header = viewEventSpec(vp, h.spec, dragEvents)
  list{header, ast}
}
