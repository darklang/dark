open Prelude

(* Dark *)
module B = BlankOr
module TL = Toplevel

let pauseWorkerButton (vs : ViewUtils.viewState) (name : string) : msg Html.html
    =
  let strTLID = showTLID vs.tlid in
  let schedule =
    vs.workerStats
    |> Option.andThen ~f:(fun (ws : Types.workerStats) -> ws.schedule)
    |> Option.withDefault ~default:"run"
  in
  match schedule with
  | "pause" ->
      Html.div
        [ ViewUtils.eventNoPropagation ~key:("run-" ^ strTLID) "click" (fun _ ->
              RunWorker name)
        ; Html.class' "restart-worker"
        ; Html.title "Run worker" ]
        [ViewUtils.fontAwesome "play-circle"]
  | "block" ->
      Html.div
        [ Html.class' "blocked-worker"
        ; Html.title
            "Worker disabled by Dark. Please get in touch to discuss why." ]
        [ViewUtils.fontAwesome "ban"]
  | "run" ->
      Html.div
        [ ViewUtils.eventNoPropagation
            ~key:("pause-" ^ strTLID)
            "click"
            (fun _ -> PauseWorker name)
        ; Html.class' "pause-worker"
        ; Html.title "Pause worker" ]
        [ViewUtils.fontAwesome "pause-circle"]
  | _ ->
      Vdom.noNode


let viewTrace
    (tl : toplevel)
    (traceID : traceID)
    (value : inputValueDict option)
    (timestamp : string option)
    (isActive : bool)
    (isHover : bool)
    (isUnfetchable : bool)
    (tipe : tipe) : msg Html.html =
  let tlid = TL.id tl in
  let classes =
    [ ("active", isActive)
    ; ("mouseovered", isHover)
    ; ("tipe-" ^ Runtime.tipe2str tipe, true)
    ; ("traceid-" ^ traceID, true)
    ; ("unfetchable", isUnfetchable) ]
  in
  let eventKey constructor =
    constructor ^ "-" ^ showTLID tlid ^ "-" ^ traceID
  in
  let events =
    if isUnfetchable
    then
      [ ViewUtils.eventNoPropagation ~key:(eventKey "dml") "mouseleave" (fun x ->
            TraceMouseLeave (tlid, traceID, x)) ]
    else
      [ ViewUtils.eventNoPropagation ~key:(eventKey "dc") "click" (fun x ->
            TraceClick (tlid, traceID, x))
      ; ViewUtils.eventNoPropagation
          ~key:(eventKey "dme")
          "mouseenter"
          (fun x -> TraceMouseEnter (tlid, traceID, x))
      ; ViewUtils.eventNoPropagation
          ~key:(eventKey "dml")
          "mouseleave"
          (fun x -> TraceMouseLeave (tlid, traceID, x)) ]
  in
  let valueDiv =
    match value with
    | None ->
        ViewUtils.fontAwesome "spinner"
    | Some v ->
        let asString = Runtime.inputValueAsString tl v in
        let asString =
          if String.length asString = 0 then "No input parameters" else asString
        in
        Html.div [Vdom.noProp] [Html.text asString]
  in
  let timestampDiv =
    match timestamp with
    | None | Some "1970-01-01T00:00:00Z" ->
        Vdom.noNode
    | Some ts ->
        let human =
          Js.Date.now () -. Js.Date.parseAsFloat ts
          |> Util.humanReadableTimeElapsed
        in
        Html.div [Html.title ts] [Html.text ("Made " ^ human ^ " ago")]
  in
  let dotHtml =
    if isHover && not isActive
    then [Html.div [Html.class' "empty-dot"] [Vdom.noNode]]
    else [Html.div [Html.class' "dot"] [Html.text {js|•|js}]]
  in
  let viewData = Html.div [Html.class' "data"] [timestampDiv; valueDiv] in
  let unfetchableAltText =
    if isUnfetchable
    then Html.title "Trace is too large for the editor to load"
    else Vdom.noProp
  in
  let props = Html.classList classes :: unfetchableAltText :: events in
  Html.li props (dotHtml @ [viewData])


let viewTraces (vs : ViewUtils.viewState) (astID : id) : msg Html.html list =
  let traceToHtml ((traceID, traceData) : trace) =
    let value =
      Option.map ~f:(fun td -> td.input) (traceData |> Result.to_option)
    in
    let timestamp =
      Option.map
        ~f:(fun (td : traceData) -> td.timestamp)
        (traceData |> Result.toOption)
    in
    (* Note: the isActive and hoverID tlcursors are very different things *)
    let isActive =
      Analysis.selectedTrace vs.tlTraceIDs vs.traces vs.tlid = Some traceID
    in
    let isHover = vs.hovering = Some (vs.tlid, ID traceID) in
    let isUnfetchable =
      match traceData with Error MaximumCallStackError -> true | _ -> false
    in
    let astTipe =
      Analysis.getTipeOf' vs.analysisStore astID
      |> Option.withDefault ~default:TIncomplete
    in
    viewTrace
      vs.tl
      traceID
      value
      timestamp
      isActive
      isHover
      isUnfetchable
      astTipe
  in
  List.map ~f:traceToHtml vs.traces


let viewData (vs : ViewUtils.viewState) (ast : fluidExpr) : msg Html.html list =
  let astID = FluidExpression.id ast in
  let requestEls = viewTraces vs astID in
  let tlSelected =
    match tlidOf vs.cursorState with
    | Some tlid when tlid = vs.tlid ->
        true
    | Some _ | None ->
        false
  in
  let showWorkerStats = tlSelected && Option.isSome vs.workerStats in
  let workQStats =
    if showWorkerStats
    then
      let count =
        vs.workerStats
        |> Option.map ~f:(fun ws -> ws.count)
        |> Option.withDefault ~default:0
      in
      Html.div
        [Html.class' "worker-stats"]
        [ Html.span [Html.class' "label"] [Html.text "Pending events"]
        ; Html.span
            [Html.classList [("count", true); ("active", count > 0)]]
            [Html.text (string_of_int count)] ]
    else Vdom.noNode
  in
  let maxHeight =
    if Some vs.tlid = tlidOf vs.cursorState
    then "max-content"
    else
      let height =
        Native.Ext.querySelector (".tl-" ^ showTLID vs.tlid ^ " .ast")
        |> Option.andThen ~f:(fun e -> Some (Native.Ext.clientHeight e + 20))
        |> Option.withDefault ~default:100
      in
      string_of_int height ^ "px"
  in
  let selectedValue =
    match vs.cursorState with
    | Selecting (_, Some id) ->
        Analysis.getLiveValue' vs.analysisStore id
    | _ ->
        None
  in
  let pauseBtn =
    vs.tl
    |> TL.asHandler
    |> Option.andThen ~f:(fun h ->
           match (h.spec.space, h.spec.name) with
           | F (_, "WORKER"), F (_, name) ->
               Some (pauseWorkerButton vs name)
           | _ ->
               None)
    |> Option.withDefault ~default:Vdom.noNode
  in
  [ Html.div
      [ Html.classList
          [ ("view-data", true)
          ; ("show-worker-stats", showWorkerStats)
          ; ("live-view-selection-active", selectedValue <> None) ]
      ; Html.style "max-height" maxHeight ]
      [pauseBtn; workQStats; Html.ul [Html.class' "request-cursor"] requestEls]
  ]
