open Tc
open Prelude
open Types

(* Dark *)
module B = Blank

let viewInput
    (tlid : tlid)
    (traceID : traceID)
    (value : string)
    (timestamp : string)
    (isActive : bool)
    (isHover : bool)
    (tipe : tipe) : msg Html.html =
  let activeClass = if isActive then ["active"] else [] in
  let hoverClass = if isHover then ["mouseovered"] else [] in
  let tipeClassName = "tipe-" ^ Runtime.tipe2str tipe in
  let tipeClass = [tipeClassName] in
  let classes = activeClass @ hoverClass @ tipeClass in
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
  let viewData =
    Html.div [Html.class' "data"] [Html.text (value ^ "\nMade at: " ^ timestamp ^ " ago") ]
  in
  Html.li
    ([classes |> String.join ~sep:" " |> Html.class'] @ events )
    [Html.text {js|•|js}; viewData]


let asValue (inputValue : inputValueDict) : string =
  Runtime.inputValueAsString inputValue


let viewInputs (vs : ViewUtils.viewState) (ID astID : id) : msg Html.html list
    =
  let traceToHtml ((traceID, traceData) : trace) =
    let value =
      Option.map ~f:(fun td -> asValue td.input) traceData
      |> Option.withDefault ~default:"<loading>"
      |> fun s -> if String.length s = 0 then "No input parameters" else s
    in
    let timestamp =
      Option.map ~f:(fun (td : traceData) -> td.timestamp) traceData
      |> Option.map ~f:(fun tstr ->
             Js.Date.now () -. Js.Date.parseAsFloat tstr
             |> Util.humanReadableTimeElapsed )
      |> Option.withDefault ~default:""
    in
    (* Note: the isActive and hoverID tlcursors are very different things *)
    let isActive =
      Analysis.cursor' vs.tlCursors vs.traces vs.tl.id = Some traceID
    in
    let isHover = vs.hovering = Some (vs.tl.id, ID traceID) in
    let astTipe =
      StrDict.get ~key:traceID vs.analyses
      |> Option.map ~f:(fun x -> x.liveValues)
      |> Option.andThen ~f:(StrDict.get ~key:astID)
      |> Option.map ~f:Runtime.typeOf
      |> Option.withDefault ~default:TIncomplete
    in
    viewInput vs.tl.id traceID value timestamp isActive isHover astTipe
  in
  List.map ~f:traceToHtml vs.traces


let viewData (vs : ViewUtils.viewState) (ast : expr) : msg Html.html list =
  let astID = B.toID ast in
  let requestEls = viewInputs vs astID in
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
          ; ("live-view-selection-active", selectedValue <> None) ] ]
      [Html.ul [Html.class' "request-cursor"] requestEls] ]
