open Tc
open Prelude
open Types

(* Dark *)
module B = Blank
module TL = Toplevel

let viewInput
    (tl : toplevel)
    (traceID : traceID)
    (value : inputValueDict option)
    (timestamp : string option)
    (isActive : bool)
    (isHover : bool)
    (tipe : tipe) : msg Html.html =
  let tlid = TL.id tl in
  let activeClass = if isActive then ["active"] else [] in
  let hoverClass = if isHover then ["mouseovered"] else [] in
  let tipeClass = ["tipe-" ^ Runtime.tipe2str tipe] in
  let classes = activeClass @ hoverClass @ tipeClass |> String.join ~sep:" " in
  let eventKey constructor =
    constructor ^ "-" ^ showTLID tlid ^ "-" ^ traceID
  in
  let events =
    [ ViewUtils.eventNoPropagation ~key:(eventKey "dc") "click" (fun x ->
          TraceClick (tlid, traceID, x) )
    ; ViewUtils.eventNoPropagation ~key:(eventKey "dme") "mouseenter" (fun x ->
          TraceMouseEnter (tlid, traceID, x) )
    ; ViewUtils.eventNoPropagation ~key:(eventKey "dml") "mouseleave" (fun x ->
          TraceMouseLeave (tlid, traceID, x) ) ]
  in
  let valueDiv =
    match value with
    | None ->
        ViewUtils.fontAwesome "spinner"
    | Some v ->
        let asString = Runtime.inputValueAsString tl v in
        let asString =
          if String.length asString = 0 && TL.spaceOf tl <> Some HSCron
          then "No input parameters"
          else asString
        in
        Html.div [] [Html.text asString]
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
    then [Html.div [Html.class' "empty-dot"] []]
    else [Html.div [] [Html.text {js|•|js}]]
  in
  let viewData = Html.div [Html.class' "data"] [timestampDiv; valueDiv] in
  Html.li (Html.class' classes :: events) (dotHtml @ [viewData])


let viewInputs (vs : ViewUtils.viewState) (ID astID : id) : msg Html.html list
    =
  let traceToHtml ((traceID, traceData) : trace) =
    let value = Option.map ~f:(fun td -> td.input) traceData in
    let timestamp =
      Option.map ~f:(fun (td : traceData) -> td.timestamp) traceData
    in
    (* Note: the isActive and hoverID tlcursors are very different things *)
    let isActive =
      Analysis.cursor' vs.tlCursors vs.traces vs.tlid = Some traceID
    in
    let isHover = vs.hovering = Some (vs.tlid, ID traceID) in
    let astTipe =
      StrDict.get ~key:traceID vs.analyses
      |> Option.map ~f:(fun x -> x.liveValues)
      |> Option.andThen ~f:(StrDict.get ~key:astID)
      |> Option.map ~f:Runtime.typeOf
      |> Option.withDefault ~default:TIncomplete
    in
    viewInput vs.tl traceID value timestamp isActive isHover astTipe
  in
  List.map ~f:traceToHtml vs.traces


let viewData (vs : ViewUtils.viewState) (ast : expr) : msg Html.html list =
  let astID = B.toID ast in
  let requestEls = viewInputs vs astID in
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
    | Selecting (_, Some (ID id)) ->
        StrDict.get ~key:id vs.currentResults.liveValues
    | _ ->
        None
  in
  [ Html.div
      [ Html.classList
          [ ("view-data", true)
          ; ("live-view-selection-active", selectedValue <> None) ]
      ; Html.style "max-height" maxHeight ]
      [Html.ul [Html.class' "request-cursor"] requestEls] ]
