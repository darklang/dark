open Tea
open! Porting
module Attrs = Html.Attributes
module Events = Html.Events
open Prelude
module TL = Toplevel
open Types
open ViewUtils

let view m =
  let attributes =
    [ Attrs.id "grid"
    ; Events.onWithOptions "mouseup"
        {stopPropagation= false; preventDefault= true}
        (decodeClickEvent GlobalClick) ]
  in
  let footer = [ViewScaffold.viewError m.error; ViewScaffold.viewButtons m] in
  let routing = ViewRoutingTable.viewRoutingTable m in
  let body = viewCanvas m in
  let content = [routing; body] ^ footer in
  Html.div attributes content

let viewCanvas m =
  let entry = ViewEntry.viewEntry m in
  let asts =
    match m.currentPage with
    | Toplevels _ -> List.map (viewTL m) m.toplevels
    | Fn (tlid, _) -> (
      match List.find (fun f -> f.tlid = tlid) m.userFunctions with
      | Some func -> [viewTL m (TL.ufToTL m func)]
      | None -> List.map (viewTL m) m.toplevels )
  in
  let _ = "comment" in
  let canvasTransform =
    let offset =
      match m.currentPage with
      | Toplevels _ -> m.canvas.offset
      | Fn (_, _) -> m.canvas.fnOffset
    in
    let x = string_of_int (-offset.x) in
    let y = string_of_int (-offset.y) in
    ((("translate(" ^ x) ^ "px, ") ^ y) ^ "px)"
  in
  let allDivs = asts ^ entry in
  Html.div
    [Attrs.id "canvas"; Attrs.style [("transform", canvasTransform)]]
    allDivs

let viewTL m tl =
  let id = deTLID tl.id in
  let recalc () = viewTL_ m tl.id in
  let _ = "comment" in
  let isDB = match tl.data with TLDB _ -> true | _ -> false in
  let pos =
    match m.currentPage with
    | Toplevels _ -> tl.pos
    | Fn (tLID, _) -> Defaults.centerPos
  in
  let html =
    if Some tl.id = tlidOf m.cursorState || isDB then
      let _ = Util.cacheClear id in
      recalc ()
    else
      match Util.cacheGet id with
      | Some html -> html
      | None ->
          let result = recalc () in
          let _ = Util.cacheSet id result in
          result
  in
  placeHtml m pos html

let viewTL_ m tlid =
  let tl = TL.getTL m tlid in
  let vs = createVS m tl in
  let body, data =
    match tl.data with
    | TLHandler h -> (ViewCode.viewHandler vs h, ViewData.viewData vs h.ast)
    | TLDB db -> (ViewDB.viewDB vs db, [])
    | TLFunc f -> ([ViewFunction.viewFunction vs f], ViewData.viewData vs f.ast)
  in
  let events =
    [ eventNoPropagation "mousedown" (ToplevelMouseDown tl.id)
    ; eventNoPropagation "mouseup" (ToplevelMouseUp tl.id)
    ; eventNoPropagation "click" (ToplevelClick tl.id) ]
  in
  let selected =
    if Some tl.id = tlidOf m.cursorState then "selected" else ""
  in
  let boxClasses =
    match m.cursorState with
    | Dragging (tlid_, _, _, _) -> if tlid_ = tl.id then ["dragging"] else []
    | _ -> []
  in
  let class_ =
    [ selected
    ; "tl-" ^ string_of_int (deTLID tl.id)
    ; "toplevel"
    ; "cursor-" ^ string_of_int (Analysis.cursor m tl.id) ]
    |> String.join " "
  in
  let documentation =
    if Some tl.id = tlidOf m.cursorState then
      m.complete |> Autocomplete.highlighted
      |> Option.andThen Autocomplete.documentationForItem
      |> Option.map (fun desc ->
             [ Html.div
                 [Attrs.class_ "documentation-box"]
                 [Html.p [] [Html.text desc]] ] )
    else None
  in
  let top = match documentation with Some doc -> doc | _ -> data in
  let html =
    Html.div
      [Attrs.class_ <| String.join " " (boxClasses ^ ["sidebar-box"; selected])]
      [Html.div (Attrs.class_ class_ :: events) (body ^ top)]
  in
  html
