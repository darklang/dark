open Prelude

// Dark
module B = BlankOr
module TL = Toplevel

let pauseWorkerButton = (vp: ViewUtils.viewProps, name: string): Html.html<msg> => {
  let strTLID = TLID.toString(vp.tlid)
  let schedule =
    vp.workerStats
    |> Option.andThen(~f=(ws: Types.workerStats) => ws.schedule)
    |> Option.unwrap(~default="run")

  switch schedule {
  | "pause" =>
    Html.div(
      list{
        ViewUtils.eventNoPropagation(~key="run-" ++ strTLID, "click", _ => RunWorker(name)),
        Html.class'("restart-worker"),
        Html.title("Run worker"),
      },
      list{ViewUtils.fontAwesome("play-circle")},
    )
  | "block" =>
    Html.div(
      list{
        Html.class'("blocked-worker"),
        Html.title("Worker disabled by Dark. Please get in touch to discuss why."),
      },
      list{ViewUtils.fontAwesome("ban")},
    )
  | "run" =>
    Html.div(
      list{
        ViewUtils.eventNoPropagation(~key="pause-" ++ strTLID, "click", _ => PauseWorker(name)),
        Html.class'("pause-worker"),
        Html.title("Pause worker"),
      },
      list{ViewUtils.fontAwesome("pause-circle")},
    )
  | _ => Vdom.noNode
  }
}

let viewTrace = (
  vp: ViewUtils.viewProps,
  traceID: traceID,
  value: option<inputValueDict>,
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
      ViewUtils.eventNoPropagation(~key=eventKey("dml"), "mouseleave", x => TraceMouseLeave(
        tlid,
        traceID,
        x,
      )),
    }
  } else {
    list{
      ViewUtils.eventNoPropagation(~key=eventKey("dc"), "click", x => TraceClick(tlid, traceID, x)),
      ViewUtils.eventNoPropagation(~key=eventKey("dme"), "mouseenter", x => TraceMouseEnter(
        tlid,
        traceID,
        x,
      )),
      ViewUtils.eventNoPropagation(~key=eventKey("dml"), "mouseleave", x => TraceMouseLeave(
        tlid,
        traceID,
        x,
      )),
    }
  }

  let valueDiv = if isUnfetchable {
    Vdom.noNode
  } else {
    switch value {
    | None => ViewUtils.fontAwesome("spinner")
    | Some(v) =>
      let asString = Runtime.inputValueAsString(tl, v)
      let asString = if String.length(asString) == 0 {
        "No input parameters"
      } else {
        Util.hideSecrets(vp.secretValues, asString)
      }

      Html.div(list{Vdom.noProp}, list{Html.text(asString)})
    }
  }

  let timestampDiv = switch timestamp {
  | None | Some("1970-01-01T00:00:00Z") => Vdom.noNode
  | Some(ts) =>
    let human = Js.Date.now() -. Js.Date.parseAsFloat(ts) |> Util.humanReadableTimeElapsed

    Html.div(list{Html.title(ts)}, list{Html.text("Made " ++ (human ++ " ago"))})
  }

  let dotHtml = if isHover && !isActive {
    list{Html.div(list{Html.class'("empty-dot")}, list{Vdom.noNode})}
  } else {
    list{Html.div(list{Html.class'("dot")}, list{Html.text(`•`)})}
  }

  let viewData = Html.div(list{Html.class'("data")}, list{timestampDiv, valueDiv})
  let unfetchableAltText = if isUnfetchable {
    Html.title("Trace is too large for the editor to load")
  } else {
    Vdom.noProp
  }

  let props = list{Html.classList(classes), unfetchableAltText, ...events}
  Html.li(props, Belt.List.concat(dotHtml, list{viewData}))
}

let viewTraces = (vp: ViewUtils.viewProps): list<Html.html<msg>> => {
  let traceToHtml = ((traceID, traceData): trace) => {
    let value = Option.map(~f=td => td.input, traceData |> Result.to_option)

    let timestamp = Option.map(~f=(td: traceData) => td.timestamp, traceData |> Result.toOption)

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
    let count = vp.workerStats |> Option.map(~f=ws => ws.count) |> Option.unwrap(~default=0)

    Html.div(
      list{Html.class'("worker-stats")},
      list{
        Html.span(list{Html.class'("label")}, list{Html.text("Pending events")}),
        Html.span(
          list{Html.classList(list{("count", true), ("active", count > 0)})},
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
      Native.Ext.querySelector(".tl-" ++ (TLID.toString(vp.tlid) ++ " .ast"))
      |> Option.andThen(~f=e => Some(Native.Ext.clientHeight(e) + 20))
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
      switch (h.spec.space, h.spec.name) {
      | (F(_, "WORKER"), F(_, name)) => Some(pauseWorkerButton(vp, name))
      | _ => None
      }
    )
    |> Option.unwrap(~default=Vdom.noNode)

  list{
    Html.div(
      list{
        Html.classList(list{
          ("view-data", true),
          ("show-worker-stats", showWorkerStats),
          ("live-view-selection-active", selectedValue != None),
        }),
        Html.style("max-height", maxHeight),
      },
      list{pauseBtn, workQStats, Html.ul(list{Html.class'("request-cursor")}, requestEls)},
    ),
  }
}
