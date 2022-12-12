open Prelude

module Html = Tea.Html
module Attrs = Tea.Attrs

// Dark
module B = BlankOr
module TL = Toplevel

module Msg = AppTypes.Msg
type msg = AppTypes.msg

let pauseWorkerButton = (vp: ViewUtils.viewProps, name: string): Html.html<msg> => {
  let strTLID = TLID.toString(vp.tlid)
  let schedule =
    vp.workerStats
    |> Option.andThen(~f=(ws: AnalysisTypes.WorkerStats.t) => ws.schedule)
    |> Option.unwrap(~default=AnalysisTypes.WorkerState.Running)

  switch schedule {
  | Paused =>
    Html.div(
      list{
        EventListeners.eventNoPropagation(~key="run-" ++ strTLID, "click", _ => Msg.RunWorker(
          name,
        )),
        Attrs.class("restart-worker"),
        Attrs.title("Run worker"),
      },
      list{Icons.fontAwesome("play-circle")},
    )
  | Blocked =>
    Html.div(
      list{
        Attrs.class("blocked-worker"),
        Attrs.title("Worker disabled by Dark. Please get in touch to discuss why."),
      },
      list{Icons.fontAwesome("ban")},
    )
  | Running =>
    Html.div(
      list{
        EventListeners.eventNoPropagation(~key="pause-" ++ strTLID, "click", _ => Msg.PauseWorker(
          name,
        )),
        Attrs.class("pause-worker"),
        Attrs.title("Pause worker"),
      },
      list{Icons.fontAwesome("pause-circle")},
    )
  }
}

let viewTrace = (
  vp: ViewUtils.viewProps,
  traceID: traceID,
  value: option<AnalysisTypes.InputValueDict.t>,
  timestamp: option<string>,
  isActive: bool,
  isHover: bool,
  isUnfetchable: bool,
): Html.html<msg> => {
  let tl = vp.tl
  let tlid = TL.id(tl)
  let classes = list{
    ("active", isActive),
    ("mouseovered", isHover),
    ("traceid-" ++ traceID, true),
    ("unfetchable", isUnfetchable),
  }

  let eventKey = constructor => constructor ++ ("-" ++ (TLID.toString(tlid) ++ ("-" ++ traceID)))

  let events = if isUnfetchable {
    list{
      EventListeners.eventNoPropagation(
        ~key=eventKey("dml"),
        "mouseleave",
        x => Msg.TraceMouseLeave(tlid, traceID, x),
      ),
    }
  } else {
    list{
      EventListeners.eventNoPropagation(~key=eventKey("dc"), "click", x => Msg.TraceClick(
        tlid,
        traceID,
        x,
      )),
      EventListeners.eventNoPropagation(
        ~key=eventKey("dme"),
        "mouseenter",
        x => Msg.TraceMouseEnter(tlid, traceID, x),
      ),
      EventListeners.eventNoPropagation(
        ~key=eventKey("dml"),
        "mouseleave",
        x => Msg.TraceMouseLeave(tlid, traceID, x),
      ),
    }
  }

  let valueDiv = if isUnfetchable {
    Vdom.noNode
  } else {
    switch value {
    | None => Icons.fontAwesome("spinner")
    | Some(v) =>
      let asString = Runtime.inputValueAsString(tl, v)
      let asString = if String.length(asString) == 0 {
        "No input parameters"
      } else {
        Util.hideSecrets(asString, vp.secretValues)
      }

      Html.div(list{Vdom.noProp}, list{Html.text(asString)})
    }
  }

  let timestampDiv = switch timestamp {
  | None | Some("1970-01-01T00:00:00Z") => Vdom.noNode
  | Some(ts) =>
    let human = Js.Date.now() -. Js.Date.parseAsFloat(ts) |> Util.humanReadableTimeElapsed

    Html.div(list{Attrs.title(ts)}, list{Html.text("Made " ++ (human ++ " ago"))})
  }

  let dotHtml = if isHover && !isActive {
    list{Html.div(list{Attrs.class("empty-dot")}, list{Vdom.noNode})}
  } else {
    list{Html.div(list{Attrs.class("dot")}, list{Html.text(`•`)})}
  }

  let viewData = Html.div(list{Attrs.class("data")}, list{timestampDiv, valueDiv})
  let unfetchableAltText = if isUnfetchable {
    Attrs.title("Trace is too large for the editor to load")
  } else {
    Vdom.noProp
  }

  let props = list{Attrs.classList(classes), unfetchableAltText, ...events}
  Html.li(props, Belt.List.concat(dotHtml, list{viewData}))
}

let viewTraces = (vp: ViewUtils.viewProps): list<Html.html<msg>> => {
  let traceToHtml = ((traceID, traceData): AnalysisTypes.Trace.t) => {
    let value = Option.map(~f=td => td.input, traceData |> Result.to_option)

    let timestamp = Option.map(
      ~f=(td: AnalysisTypes.TraceData.t) => td.timestamp,
      traceData |> Result.toOption,
    )

    // Note: the isActive and hoverID tlcursors are very different things
    let isActive = Analysis.selectedTraceID(vp.tlTraceIDs, vp.traces, vp.tlid) == Some(traceID)

    let isHover = vp.hovering == Some(vp.tlid, ATraceID(traceID))
    let isUnfetchable = switch traceData {
    | Error(MaximumCallStackError) => true
    | _ => false
    }

    viewTrace(vp, traceID, value, timestamp, isActive, isHover, isUnfetchable)
  }

  List.map(~f=traceToHtml, vp.traces)
}

let viewData = (vp: ViewUtils.viewProps): list<Html.html<msg>> => {
  let requestEls = viewTraces(vp)
  let tlSelected = switch CursorState.tlidOf(vp.cursorState) {
  | Some(tlid) if tlid == vp.tlid => true
  | Some(_) | None => false
  }

  let showWorkerStats = tlSelected && Option.isSome(vp.workerStats)
  let workQStats = if showWorkerStats {
    let count =
      vp.workerStats
      |> Option.map(~f=(ws: AnalysisTypes.WorkerStats.t) => ws.count)
      |> Option.unwrap(~default=0)

    Html.div(
      list{Attrs.class("worker-stats")},
      list{
        Html.span(list{Attrs.class("label")}, list{Html.text("Pending events")}),
        Html.span(
          list{Attrs.classList(list{("count", true), ("active", count > 0)})},
          list{Html.text(string_of_int(count))},
        ),
      },
    )
  } else {
    Vdom.noNode
  }

  let maxHeight = if Some(vp.tlid) == CursorState.tlidOf(vp.cursorState) {
    "max-content"
  } else {
    let height =
      Webapi.Dom.document->Webapi.Dom.Document.querySelector(
        ".tl-" ++ (TLID.toString(vp.tlid) ++ " .ast"),
      )
      |> Option.andThen(~f=e => Some(Webapi.Dom.Element.clientHeight(e) + 20))
      |> Option.unwrap(~default=100)

    string_of_int(height) ++ "px"
  }

  let selectedValue = switch vp.cursorState {
  | Selecting(_, Some(id)) => Analysis.getLiveValue'(vp.analysisStore, id)
  | _ => None
  }

  let pauseBtn =
    vp.tl
    |> TL.asHandler
    |> Option.andThen(~f=(h: PT.Handler.t) =>
      switch h.spec {
      | PT.Handler.Spec.Worker(name, _) => Some(pauseWorkerButton(vp, name))
      | _ => None
      }
    )
    |> Option.unwrap(~default=Vdom.noNode)

  list{
    Html.div(
      list{
        Attrs.classList(list{
          ("view-data", true),
          ("show-worker-stats", showWorkerStats),
          ("live-view-selection-active", selectedValue != None),
        }),
        Attrs.style("max-height", maxHeight),
      },
      list{pauseBtn, workQStats, Html.ul(list{Attrs.class("request-cursor")}, requestEls)},
    ),
  }
}
