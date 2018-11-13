open Tea
open! Porting
open Prelude
open Types
module Attrs = Html.Attributes
module B = Blank
module RT = Runtime

let viewInput (tlid : tlid) (idx : int) (value : string) (isActive : bool)
    (isHover : bool) (tipe : tipe) : msg Html.html =
  let activeClass = if isActive then [Html.class' "active"] else [Vdom.noProp] in
  let hoverClass = if isHover then [Html.class' "mouseovered"] else [Vdom.noProp] in
  let tipeClassName = "tipe-" ^ RT.tipe2str tipe in
  let tipeClass = [Html.class' tipeClassName] in
  let classes = activeClass @ hoverClass @ tipeClass in
  let eventKey constructor tlid id =
    constructor ^ "-" ^ (showTLID tlid) ^ "-" ^ (string_of_int id)
  in
  let events =
    [ ViewUtils.eventNoPropagation ~key:(eventKey "dc" tlid idx) "click" (fun x -> DataClick (tlid, idx, x))
    ; ViewUtils.eventNoPropagation ~key:(eventKey "dme" tlid idx) "mouseenter" (fun x -> DataMouseEnter (tlid, idx, x))
    ; ViewUtils.eventNoPropagation ~key:(eventKey "dml" tlid idx) "mouseleave" (fun x -> DataMouseLeave (tlid, idx, x)) ]
  in
  Html.li
    ([Vdom.attribute "" "data-content" value] @ classes @ events)
    [Html.text {js|•|js}]

let asValue (inputValue : inputValueDict) : string =
  RT.inputValueAsString inputValue

let viewInputs (vs : ViewUtils.viewState) (ID astID : id) : msg Html.html list =
  let traceToHtml idx trace =
    let value = asValue trace.input in
    (* Note: the isActive and hoverID tlcursors are very different things *)
    let isActive = Analysis.cursor_ vs.tlCursors vs.tl.id = idx in
    let hoverID = tlCursorID vs.tl.id idx in
    let isHover = vs.hovering = Some hoverID in
    let astTipe =
      StrDict.get trace.traceID vs.analyses
      |> Option.map (fun x -> x.liveValues)
      |> Option.andThen (StrDict.get astID)
      |> Option.map RT.typeOf
      |> Option.withDefault TIncomplete
    in
    viewInput vs.tl.id idx value isActive isHover astTipe
  in
  List.indexedMap traceToHtml vs.traces

let viewData (vs : ViewUtils.viewState) (ast : expr) : msg Html.html list =
  let astID = B.toID ast in
  let requestEls = viewInputs vs astID in
  let selectedValue =
    match vs.cursorState with
    | Selecting (tlid, Some (ID id)) ->
        StrDict.get id vs.currentResults.liveValues
    | _ -> None
  in
  [ Html.div
      [ Html.classList
          [ ("view-data", true)
          ; ("live-view-selection-active", selectedValue <> None) ] ]
      [Html.ul [Html.class' "request-cursor"] requestEls] ]
