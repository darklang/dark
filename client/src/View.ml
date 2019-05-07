open Tc
open Prelude
open Types

(* Dark *)
module TL = Toplevel
module P = Pointer

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

let viewTL_ (m : model) (tl : toplevel) : msg Html.html =
  let vs = ViewUtils.createVS m tl in
  let body, data =
    match tl.data with
    | TLHandler h ->
        (ViewCode.viewHandler vs h, ViewData.viewData vs h.ast)
    | TLDB db ->
        (ViewDB.viewDB vs db, [])
    | TLFunc f ->
        ([ViewFunction.viewFunction vs f], ViewData.viewData vs f.ufAST)
    | TLTipe t ->
        ([ViewUserType.viewUserTipe vs t], [])
  in
  let refs = ViewIntrospect.refersToViews tl.id vs.toReferences in
  let uses = ViewIntrospect.usedInViews vs.inReferences in
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
        (fun x -> ToplevelClick (tl.id, x))
    ; ViewUtils.eventNoPropagation
        ~key:("tldblc-" ^ showTLID vs.tl.id)
        "dblclick"
        (fun _ -> ToplevelDoubleClick vs.tl.id) ]
  in
  let selected = Some tl.id = tlidOf m.cursorState in
  let boxClasses =
    let dragging =
      match m.cursorState with
      | Dragging (tlid_, _, _, _) ->
          tlid_ = tl.id
      | _ ->
          false
    in
    [("sidebar-box", true); ("selected", selected); ("dragging", dragging)]
  in
  let class_ =
    ["toplevel"; "tl-" ^ deTLID tl.id; (if selected then "selected" else "")]
    |> String.join ~sep:" "
  in
  let documentation =
    match (tlidOf m.cursorState, idOf m.cursorState) with
    | Some tlid, Some id when tlid = tl.id ->
        let acFnDocString =
          m.complete
          |> Autocomplete.highlighted
          |> Option.andThen ~f:Autocomplete.documentationForItem
          |> Option.map ~f:(fun desc ->
                 [ Html.div
                     [Html.class' "documentation-box"]
                     [Html.p [] [Html.text desc]] ] )
        in
        let selectedFnDocString =
          let fn =
            TL.get m tlid
            |> Option.andThen ~f:TL.asHandler
            |> Option.map ~f:(fun x -> x.ast)
            |> Option.andThen ~f:(fun ast -> AST.find id ast)
            |> Option.andThen ~f:(function
                   | PExpr (F (_, FnCall (F (_, name), _, _))) ->
                       Some name
                   | PFnCallName (F (_, name)) ->
                       Some name
                   | _ ->
                       None )
            |> Option.andThen ~f:(fun name ->
                   m.complete.functions
                   |> List.find ~f:(fun f -> name = f.fnName) )
          in
          match fn with
          | Some fn ->
              Some
                [ Html.div
                    [Html.class' "documentation-box"]
                    [Html.p [] [Html.text fn.fnDescription]] ]
          | None ->
              None
        in
        let selectedParamDocString =
          let param =
            TL.get m tlid
            |> Option.andThen ~f:TL.asHandler
            |> Option.map ~f:(fun x -> x.ast)
            |> Option.andThen ~f:(fun ast -> AST.getParamIndex ast id)
            |> Option.andThen ~f:(fun (name, index) ->
                   m.complete.functions
                   |> List.find ~f:(fun f -> name = f.fnName)
                   |> Option.map ~f:(fun x -> x.fnParameters)
                   |> Option.andThen ~f:(List.getAt ~index) )
          in
          match param with
          | Some p ->
              let header =
                p.paramName ^ " : " ^ Runtime.tipe2str p.paramTipe
              in
              Some
                [ Html.div
                    [Html.class' "documentation-box"]
                    [ Html.p [] [Html.text header]
                    ; Html.p [] [Html.text p.paramDescription] ] ]
          | _ ->
              None
        in
        acFnDocString
        |> Option.orElse selectedParamDocString
        |> Option.orElse selectedFnDocString
    | _ ->
        None
  in
  let top = match documentation with Some doc -> doc | _ -> [] in
  let pos =
    match m.currentPage with
    | Architecture | FocusedHandler _ | FocusedDB _ ->
        tl.pos
    | FocusedFn _ | FocusedType _ ->
        Defaults.centerPos
  in
  let liveValue =
    match unwrapCursorState m.cursorState with
    | Selecting (_, Some _id) ->
        true
    | _ ->
        false
  in
  let html =
    Html.div
      (* -- see comment in css *)
      [Html.classList boxClasses]
      [ Html.div
          [Html.classList [("use-wrapper", true); ("fade", liveValue)]]
          [uses; refs]
      ; Html.div (Html.class' class_ :: events) (body @ data @ top) ]
  in
  ViewUtils.placeHtml pos html


let tlCacheKey (m : model) tl =
  if Some tl.id = tlidOf m.cursorState
  then None
  else
    let hovered =
      match List.head m.hovering with
      | Some (tlid, id) when tlid = tl.id ->
          Some id
      | _ ->
          None
    in
    let tracesLoaded =
      Analysis.getTraces m tl.id
      |> List.map ~f:(fun (_, traceData) -> Option.isSome traceData)
    in
    Some (tl, Analysis.cursor m tl.id, hovered, tracesLoaded)


let tlCacheKeyDB (m : model) tl =
  if Some tl.id = tlidOf m.cursorState
  then None
  else Some (tl, DB.isLocked m tl.id)


let tlCacheKeyTipe (m : model) tl =
  if Some tl.id = tlidOf m.cursorState then None else Some tl


let viewTL m tl =
  match tl.data with
  | TLTipe _ ->
      Cache.cache2m tlCacheKeyTipe viewTL_ m tl
  | TLDB _ ->
      Cache.cache2m tlCacheKeyDB viewTL_ m tl
  | TLFunc _ | TLHandler _ ->
      Cache.cache2m tlCacheKey viewTL_ m tl


let viewCanvas (m : model) : msg Html.html =
  let entry = ViewEntry.viewEntry m in
  let asts =
    match m.currentPage with
    | Architecture | FocusedHandler _ | FocusedDB _ ->
        m.toplevels
        (* TEA's vdom assumes lists have the same ordering, and diffs incorrectly
       * if not (though only when using our Util cache). This leads to the
       * clicks going to the wrong toplevel. Sorting solves it, though I don't
       * know exactly how. TODO: we removed the Util cache so it might work. *)
        |> List.sortBy ~f:(fun tl -> deTLID tl.id)
        |> List.map ~f:(viewTL m)
    | FocusedFn tlid ->
      ( match List.find ~f:(fun f -> f.ufTLID = tlid) m.userFunctions with
      | Some func ->
          [viewTL m (TL.ufToTL func)]
      | None ->
          [] )
    | FocusedType tlid ->
      ( match List.find ~f:(fun t -> t.utTLID = tlid) m.userTipes with
      | Some tipe ->
          [viewTL m (TL.utToTL tipe)]
      | None ->
          [] )
  in
  let canvasTransform =
    let offset = m.canvasProps.offset in
    let x = string_of_int (-offset.x) in
    let y = string_of_int (-offset.y) in
    "translate(" ^ x ^ "px, " ^ y ^ "px)"
  in
  let styles =
    [ ( "transition"
      , if m.canvasProps.panAnimation then "transform 0.5s" else "unset" )
    ; ("transform", canvasTransform) ]
  in
  let allDivs = asts @ entry in
  let overlay =
    let show =
      match m.currentPage with
      | FocusedHandler _ | FocusedDB _ ->
          true
      | _ ->
          false
    in
    Html.div [Html.classList [("overlay", true); ("show", show)]] []
  in
  let pageClass =
    match m.currentPage with
    | Architecture ->
        "arch"
    | FocusedHandler _ ->
        "focused-handler"
    | FocusedDB _ ->
        "focused-db"
    | FocusedFn _ ->
        "focused-fn"
    | FocusedType _ ->
        "focused-type"
  in
  Html.div
    [ Html.id "canvas"
    ; Html.class' pageClass
    ; Html.styles styles
    ; ViewUtils.onTransitionEnd ~key:"canvas-pan-anim" ~listener:(fun prop ->
          if prop = "transform" then CanvasPanAnimationEnd else IgnoreMsg ) ]
    (overlay :: allDivs)


let view (m : model) : msg Html.html =
  let activeVariantsClass =
    match VariantTesting.activeCSSClasses m with
    | "" ->
        Vdom.noProp
    | str ->
        Html.class' str
  in
  let attributes =
    [ Html.id "app"
    ; activeVariantsClass
    ; Html.onWithOptions
        ~key:"app-mu"
        "mouseup"
        {stopPropagation = false; preventDefault = true}
        (Decoders.wrapDecoder
           (ViewUtils.decodeClickEvent (fun x -> GlobalClick x))) ]
  in
  let footer = [ViewScaffold.viewError m.error; ViewScaffold.viewButtons m] in
  let routing = ViewRoutingTable.viewRoutingTable m in
  let body = viewCanvas m in
  let ast = TL.selectedAST m |> Option.withDefault ~default:(Blank.new_ ()) in
  let fluidStatus =
    if VariantTesting.isFluid m.tests
    then [Fluid.viewStatus (Fluid.fromExpr ast) m.fluidState]
    else []
  in
  let content = [routing; body] @ fluidStatus @ footer in
  Html.div attributes content
