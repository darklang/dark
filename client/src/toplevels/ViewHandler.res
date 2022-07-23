open Prelude

// Dark
module B = BlankOr

type viewProps = ViewUtils.viewProps

type domEventList = ViewUtils.domEventList

let inUnit = ViewUtils.intAsUnit

let fontAwesome = ViewUtils.fontAwesome

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

let triggerHandlerButton = (vp: viewProps, spec: PT.Handler.Spec.t): Html.html<AppTypes.msg> =>
  switch (spec.space, spec.name, spec.modifier) {
  /* Hide button if spec is not filled out because trace id
   is needed to recover handler traces on refresh. */
  | (F(_, a), F(_, b), F(_, c))
    if List.any(~f=s => String.length(s) == 0, list{a, b, c}) => Vdom.noNode
  | (F(_), F(_), F(_)) =>
    if vp.permission == Some(ReadWrite) {
      let hasData =
        Analysis.selectedTraceID(vp.tlTraceIDs, vp.traces, vp.tlid)
        |> Option.andThen(~f=trace_id =>
          List.find(~f=((id, _)) => id == trace_id, vp.traces) |> Option.andThen(~f=((_, data)) =>
            data |> Result.toOption
          )
        )
        |> Option.is_some

      let classes = Html.classList(list{
        ("handler-trigger", true),
        ("is-executing", vp.isExecuting),
        ("inactive", !hasData),
        ("complete", handlerIsExeComplete(vp)),
        ("failed", handlerIsExeFail(vp)),
      })

      let attrs = if hasData {
        list{
          Html.title("Replay this execution"),
          ViewUtils.eventNoPropagation(
            ~key="lh" ++ ("-" ++ TLID.toString(vp.tlid)),
            "click",
            _ => TriggerHandler(vp.tlid),
          ),
          ViewUtils.onAnimationEnd(~key="exe" ++ ("-" ++ TLID.toString(vp.tlid)), ~listener=name =>
            if name == "fadeIn" {
              SetHandlerExeIdle(vp.tlid)
            } else {
              AppTypes.Msg.IgnoreMsg("trigger-animation-end")
            }
          ),
        }
      } else {
        list{Html.title("Need input data to replay execution"), Html.noProp, Html.noProp}
      }

      Html.div(list{classes, ...attrs}, list{fontAwesome("redo")})
    } else {
      Vdom.noNode
    }
  | (_, _, _) => Vdom.noNode
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

let viewMenu = (vp: viewProps, spec: PT.Handler.Spec.t): Html.html<AppTypes.msg> => {
  let tlid = vp.tlid
  let actions = {
    let commonAction: TLMenu.menuItem = {
      title: "Delete",
      key: "del-tl-",
      icon: Some("times"),
      action: _ => ToplevelDelete(tlid),
      disableMsg: None,
    }

    switch (spec.space, spec.modifier, spec.name) {
    | (F(_, "HTTP"), F(_, meth), F(_, name)) =>
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
          key: "new-tab-",
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
  AppTypes.msg,
> => {
  let viewEventName = viewText(
    ~enterable=true,
    ~classes=list{"toplevel-name"},
    EventName,
    vp,
    spec.name,
  )

  let viewEventSpace = viewText(~enterable=true, ~classes=list{"space"}, EventSpace, vp, spec.space)

  let viewEventModifier = {
    let viewMod = viewText(
      ~enterable=true,
      ~classes=list{"modifier"},
      EventModifier,
      vp,
      spec.modifier,
    )

    switch (spec.space, spec.modifier, spec.name) {
    | (F(_, "HTTP"), _, _) | (F(_, "CRON"), _, _) =>
      Html.div(list{Html.class'("modifier")}, list{viewMod})
    | _ => Vdom.noNode
    }
  }

  let baseClass = "spec-header"
  let classes = switch (spec.space, spec.modifier) {
  | (F(_, "HTTP"), F(_, "GET")) => baseClass ++ " http-get"
  | (F(_, "HTTP"), F(_, "POST")) => baseClass ++ " http-post"
  | (F(_, "HTTP"), F(_, "PUT")) => baseClass ++ " http-put"
  | (F(_, "HTTP"), F(_, "DELETE")) => baseClass ++ " http-delete"
  | (F(_, "HTTP"), F(_, "PATCH")) => baseClass ++ " http-patch"
  | (F(_, "HTTP"), F(_, "OPTIONS")) => baseClass ++ " http-options"
  | (F(_, "CRON"), _) => baseClass ++ " cron"
  | (F(_, "WORKER"), _) => baseClass ++ " worker"
  | (F(_, "REPL"), _) => baseClass ++ " repl"
  | _ => baseClass
  }

  let viewActions = {
    let triggerBtn = triggerHandlerButton(vp, spec)
    Html.div(list{Html.class'("handler-actions")}, list{triggerBtn, viewMenu(vp, spec)})
  }

  let viewType = Html.div(
    list{Html.class'("toplevel-type")},
    list{viewEventSpace, viewEventModifier},
  )

  Html.div(list{Html.class'(classes), ...dragEvents}, list{viewType, viewEventName, viewActions})
}

let handlerAttrs: list<Vdom.property<AppTypes.msg>> = list{
  Html.class'("handler-body expand"),
  Html.style("height", "auto"),
  Vdom.noProp,
}

let view = (vp: viewProps, h: PT.Handler.t, dragEvents: domEventList): list<
  Html.html<AppTypes.msg>,
> => {
  let attrs = handlerAttrs
  let ast = Html.div(attrs, FluidView.view(vp, dragEvents))
  let header = viewEventSpec(vp, h.spec, dragEvents)
  list{header, ast}
}
