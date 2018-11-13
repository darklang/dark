open Tea
open! Porting
module Attrs = Html.Attributes
open Prelude
module TL = Toplevel
open Types

type viewState = ViewUtils.viewState
type htmlConfig = ViewBlankOr.htmlConfig
let idConfigs = ViewBlankOr.idConfigs
let fontAwesome = ViewUtils.fontAwesome
let viewText = ViewBlankOr.viewText
let wc = ViewBlankOr.wc
let text = ViewBlankOr.text
let div = ViewBlankOr.div
let nested = ViewBlankOr.nested
let atom = ViewBlankOr.atom
let keyword = ViewBlankOr.keyword


let viewTL_ (m : model) (tlid : tlid) : msg Html.html =
  let tl = TL.getTL m tlid in
  let vs = ViewUtils.createVS m tl in
  let body, data =
    match tl.data with
    | TLHandler h -> (ViewCode.viewHandler vs h, ViewData.viewData vs h.ast)
    | TLDB db -> (ViewDB.viewDB vs db, [])
    | TLFunc f ->
        ([ViewFunction.viewFunction vs f], ViewData.viewData vs f.ufAST)
  in
  let events =
    [ ViewUtils.eventNoPropagation
        ~key:("tlmd-" ^ showTLID tl.id)
        "mousedown"
        (fun x -> ToplevelMouseDown (tl.id, x))
    ; ViewUtils.eventNoPropagation
        ~key:("tlmu-" ^ showTLID tl.id)
        "mouseup"
        (fun x -> ToplevelMouseUp (tl.id, x))
    ; ViewUtils.eventNoPropagation
        ~key:("tlc-" ^ showTLID tl.id)
        "click"
        (fun x -> ToplevelClick (tl.id, x)) ]
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
                 [Html.class' "documentation-box"]
                 [Html.p [] [Html.text desc]] ] )
    else None
  in
  let top = match documentation with Some doc -> doc | _ -> data in
  let html =
    Html.div
      [Html.class' <| String.join " " (boxClasses @ ["sidebar-box"; selected])]
      [Html.div (Html.class' class_ :: events) (body @ top)]
  in
  html

let viewTL (m : model) (tl : toplevel) : msg Html.html =
  let id = deTLID tl.id in
  let recalc () = viewTL_ m tl.id in
  (* Allow the DB locked status to update *)
  let isDB = match tl.data with TLDB _ -> true | _ -> false in
  let pos =
    match m.currentPage with
    | Toplevels _ -> tl.pos
    | Fn (tLID, _) -> Defaults.centerPos
  in
  let html =
    if Some tl.id = tlidOf m.cursorState || isDB then begin
      Util.cacheClear id;
      recalc ()
    end
    else
      match Util.cacheGet id with
      | Some html -> html
      | None ->
          let result = recalc () in
          Util.cacheSet id result;
          result
  in
  ViewUtils.placeHtml m pos html

let viewCanvas (m : model) : msg Html.html =
  let entry = ViewEntry.viewEntry m in
  let asts =
    match m.currentPage with
    | Toplevels _ ->
      m.toplevels
      (* TEA's vdom assumes lists have the same ordering, and diffs incorrectly
       * if not (though only when using our Util cache). This leads to the
       * clicks going to the wrong toplevel. Sorting solves it, though I don't
       * know exactly how. *)
      |> List.sortBy (fun tl -> deTLID (tl.id))
      |> List.map (viewTL m)
    | Fn (tlid, _) -> (
      match List.find (fun f -> f.ufTLID = tlid) m.userFunctions with
      | Some func -> [viewTL m (TL.ufToTL m func)]
      | None -> List.map (viewTL m) m.toplevels ) (* TODO(ian): change to crash *)
  in
  let canvasTransform =
    let offset =
      match m.currentPage with
      | Toplevels _ -> m.canvas.offset
      | Fn (_, _) -> m.canvas.fnOffset
    in
    let x = string_of_int (-offset.x) in
    let y = string_of_int (-offset.y) in
    "translate(" ^ x ^ "px, " ^ y ^ "px)"
  in
  let allDivs = asts @ entry in
  Html.div
    [Html.id "canvas"; Html.style "transform" canvasTransform]
    allDivs



let view (m : model) : msg Html.html =
  let attributes =
    [ Html.id "grid"
    ; Html.onWithOptions "mouseup"
        {stopPropagation= false; preventDefault= true}
        (Decoders.wrapDecoder (ViewUtils.decodeClickEvent (fun x -> GlobalClick x)))]
  in
  let footer = [ViewScaffold.viewError m.error; ViewScaffold.viewButtons m] in
  let routing = ViewRoutingTable.viewRoutingTable m in
  let body = viewCanvas m in
  let content = [routing; body] @ footer in
  Html.div attributes content


