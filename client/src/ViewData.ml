open Tc
open Prelude
open Types

(* Dark *)
module B = Blank

let timeDifference (time : float) : string =
  let msPerMinute = 60.0 *. 1000.0 in
  let msPerHour = msPerMinute *. 60.0 in
  let msPerDay = msPerHour *. 24.0 in
  let msPerMonth = msPerDay *. 30.0 in
  let msPerYear = msPerDay *. 365.0 in
  let rec f time =
    if time /. msPerYear > 1.0
    then
      let suffix = if time /. msPerYear > 2.0 then "years" else "year" in
      ( (time /. msPerYear |> int_of_float |> string_of_int)
      ^ " "
      ^ suffix
      ^ ", " )
      ^ f (mod_float time msPerYear)
    else if time /. msPerMonth > 1.0
    then
      let suffix = if time /. msPerMonth > 2.0 then "months" else "month" in
      ( (time /. msPerMonth |> int_of_float |> string_of_int)
      ^ " "
      ^ suffix
      ^ ", " )
      ^ f (mod_float time msPerMonth)
    else if time /. msPerDay > 1.0
    then
      let suffix = if time /. msPerDay > 2.0 then "days" else "day" in
      ( (time /. msPerDay |> int_of_float |> string_of_int)
      ^ " "
      ^ suffix
      ^ ", " )
      ^ f (mod_float time msPerDay)
    else if time /. msPerHour > 1.0
    then
      let suffix = if time /. msPerHour > 2.0 then "hours" else "hour" in
      ( (time /. msPerHour |> int_of_float |> string_of_int)
      ^ " "
      ^ suffix
      ^ ", " )
      ^ f (mod_float time msPerHour)
    else if time /. msPerMinute > 1.0
    then
      let suffix = if time /. msPerMinute > 2.0 then "minutes" else "minute" in
      ((time /. msPerMinute |> int_of_float |> string_of_int) ^ " " ^ suffix)
      ^ f (mod_float time msPerMinute)
    else ""
  in
  let diff = f time in
  if diff = "" then "less than a minute" else diff


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
  Html.li
    ( [ Vdom.attribute
          ""
          "data-content"
          (value ^ "\nMade at: " ^ timestamp ^ " ago") ]
    @ [classes |> String.join ~sep:" " |> Html.class']
    @ events )
    [Html.text {js|•|js}]


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
             Js.Date.now () -. Js.Date.parseAsFloat tstr |> timeDifference )
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
