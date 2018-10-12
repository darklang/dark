open Tea
open! Porting
module B = Blank
module Attrs = Html.Attributes
open Prelude
module RT = Runtime
open Types
open ViewUtils

let viewInput (tlid : tlid) (idx : int) (value : string) (isActive : bool)
    (isHover : bool) (tipe : tipe) : msg Html.html =
  let activeClass = if isActive then [Attrs.class_ "active"] else [] in
  let hoverClass = if isHover then [Attrs.class_ "mouseovered"] else [] in
  let tipeClassName = "tipe-" ^ RT.tipe2str tipe in
  let tipeClass = [Attrs.class_ tipeClassName] in
  let classes = activeClass ^ hoverClass ^ tipeClass in
  let events =
    [ eventNoPropagation "click" (DataClick (tlid, idx))
    ; eventNoPropagation "mouseenter" (DataMouseEnter (tlid, idx))
    ; eventNoPropagation "mouseleave" (DataMouseLeave (tlid, idx)) ]
  in
  Html.li
    ([Attrs.attribute "data-content" value] ^ classes ^ events)
    [Html.text "\226\128\162"]

let asValue (inputValue : inputValueDict) : string =
  RT.inputValueAsString inputValue

let viewInputs (vs : viewState) (ID astID : id) : msg Html.html list =
  let traceToHtml idx trace =
    let value = asValue trace.input in
    let _ = "comment" in
    let isActive = Analysis.cursor_ vs.tlCursors vs.tl.id = idx in
    let _ = "comment" in
    let hoverID = tlCursorID vs.tl.id idx in
    let isHover = vs.hovering = Some hoverID in
    let astTipe =
      Dict.get trace.id vs.analyses
      |> Option.map (fun x -> x.liveValues)
      |> Option.andThen (Dict.get astID)
      |> Option.map RT.typeOf
      |> Option.withDefault TIncomplete
    in
    viewInput vs.tl.id idx value isActive isHover astTipe
  in
  List.indexedMap traceToHtml vs.traces

let viewData (vs : viewState) (ast : expr) : msg Html.html list =
  let astID = B.toID ast in
  let requestEls = viewInputs vs astID in
  let selectedValue =
    match vs.cursorState with
    | Selecting (tlid, Some (ID id)) ->
        Dict.get id vs.currentResults.liveValues
    | _ -> None
  in
  [ Html.div
      [ Attrs.classList
          [ ("view-data", true)
          ; ("live-view-selection-active", selectedValue <> None) ] ]
      [Html.ul [Attrs.class_ "request-cursor"] requestEls] ]
