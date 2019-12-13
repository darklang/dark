open Tc
open Prelude
open Types

(* Dark *)
module TL = Toplevel
module P = Pointer
module TD = TLIDDict

let fontAwesome = ViewUtils.fontAwesome

let viewTL_ (m : model) (tl : toplevel) : msg Html.html =
  let tlid = TL.id tl in
  let vs = ViewUtils.createVS m tl in
  let dragEvents =
    if VariantTesting.isFluid m.tests
    then
      [ ViewUtils.eventNoPropagation
          ~key:("tlmd-" ^ showTLID tlid)
          "mousedown"
          (fun x -> TLDragRegionMouseDown (tlid, x))
      ; ViewUtils.eventNoPropagation
          ~key:("tlmu-" ^ showTLID tlid)
          "mouseup"
          (fun x -> TLDragRegionMouseUp (tlid, x)) ]
    else []
  in
  let body, data =
    match tl with
    | TLHandler h ->
        (ViewCode.viewHandler vs h dragEvents, ViewData.viewData vs h.ast)
    | TLDB db ->
        (ViewDB.viewDB vs db dragEvents, [])
    | TLFunc f ->
        ([ViewFunction.viewFunction vs f], ViewData.viewData vs f.ufAST)
    | TLTipe t ->
        ([ViewUserType.viewUserTipe vs t], [])
    | TLGroup g ->
        ([ViewGroup.viewGroup m vs g dragEvents], [])
  in
  let usages =
    ViewIntrospect.allUsagesView tlid vs.usedInRefs vs.refersToRefs
  in
  (* JS click events are mousedown->mouseup->click so previously ToplevelClick was being called after mouseup when selecting in fluid and would reset the fluid cursor state, we changed it to mouseup to prevent this *)
  let events =
    if VariantTesting.isFluid m.tests
    then
      [ ViewUtils.eventNoPropagation
          ~key:("tlc-" ^ showTLID tlid)
          "mouseup"
          (fun x ->
            match Entry.getFluidSelectionRange () with
            | None ->
                ToplevelClick (tlid, x)
            | Some (selBegin, selEnd) when selBegin = selEnd ->
                ToplevelClick (tlid, x)
            | Some range ->
                (* Persist fluid selection when clicking in handler *)
                FluidMsg (FluidMouseUp (tlid, Some range)) ) ]
    else
      [ ViewUtils.eventNoPropagation
          ~key:("tlmd-" ^ showTLID tlid)
          "mousedown"
          (fun x -> TLDragRegionMouseDown (tlid, x))
      ; ViewUtils.eventNoPropagation
          ~key:("tlmu-" ^ showTLID tlid)
          "mouseup"
          (fun x -> TLDragRegionMouseUp (tlid, x))
      ; ViewUtils.eventNoPropagation
          ~key:("tlc-" ^ showTLID tlid)
          "click"
          (fun x -> ToplevelClick (tlid, x)) ]
  in
  (* This is a bit ugly - DBs have a larger 'margin' (not CSS margin) between
   * the encompassing toplevel div and the db div it contains, than  handlers.
   * Which leads to it being easy to hit "why won't this drag" if you click in
   * that margin. So for TLDBs, we want the whole toplevel div to be a draggable
   * region (and so we want to attach the dragEvents handlers to the that div,
   * and not just the db div).
   *
   * We do not want that for handlers, because:
   * - if you click _just_ outside of a text region, you probably want
   * click-drag-select behavior, not drag-handler behavior
   * - the 'margin' is smaller, so you are less likely to hit the "why won't
   * this drag" behavior mentioned above *)
  let events = events @ match tl with TLDB _ -> dragEvents | _ -> [] in
  let avatars = Avatar.viewAvatars m.avatarsList tlid in
  let selected = Some tlid = tlidOf m.cursorState in
  let hovering = ViewUtils.isHoverOverTL vs in
  let boxClasses =
    let dragging =
      match m.cursorState with
      | Dragging (tlid_, _, _, _) ->
          tlid_ = tlid
      | _ ->
          false
    in
    [("selected", selected); ("dragging", dragging); ("hovering", hovering)]
  in
  (* Need to add aditional css class to remove backgroun color *)
  let isGroup = match tl with TLGroup _ -> "group" | _ -> "" in
  let ellensHack =
    if m.username = "ellen"
    then
      match tl with
      | TLHandler {spec}
        when Blank.toMaybe spec.name = Some "sendDM"
             || Blank.toMaybe spec.name = Some "sendText" ->
          "ellenDemoSendDMHack"
      | TLDB {dbName} when Blank.toMaybe dbName = Some "Visits" ->
          "ellenDemoSendDMHack"
      | _ ->
          ""
    else ""
  in
  let class_ =
    [ "toplevel"
    ; "tl-" ^ deTLID tlid
    ; (if selected then "selected" else "")
    ; isGroup
    ; ellensHack ]
    |> String.join ~sep:" "
  in
  let id =
    if VariantTesting.isFluid m.tests
    then
      TL.getAST tl
      |> Option.map ~f:(Fluid.fromExpr m.fluidState)
      |> Option.andThen ~f:(Fluid.getToken m.fluidState)
      |> Option.map ~f:(fun ti -> FluidToken.tid ti.token)
      |> Option.orElse (idOf m.cursorState)
    else idOf m.cursorState
  in
  let documentation =
    match (tlidOf m.cursorState, id) with
    | Some tlid_, Some id when tlid_ = tlid ->
        let acFnDocString =
          let regular =
            m.complete
            |> Autocomplete.highlighted
            |> Option.andThen ~f:Autocomplete.documentationForItem
          in
          let desc =
            if VariantTesting.isFluid m.tests
            then
              m.fluidState.ac
              |> FluidAutocomplete.highlighted
              |> Option.andThen ~f:FluidAutocomplete.documentationForItem
              |> Option.orElse regular
            else regular
          in
          Option.map desc ~f:(fun desc ->
              [ Html.div
                  [Html.class' "documentation-box"]
                  [Html.p [] [Html.text desc]] ] )
        in
        let selectedFnDocString =
          let fn =
            TL.getAST tl
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
            |> Option.andThen ~f:TL.getAST
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
    | Architecture | FocusedHandler _ | FocusedDB _ | FocusedGroup _ ->
        TL.pos tl
    | FocusedFn _ | FocusedType _ ->
        Defaults.centerPos
  in
  let hasFf =
    TL.getAST tl
    |> Option.map ~f:AST.allData
    |> Option.withDefault ~default:[]
    |> List.any ~f:(function PFFMsg _ -> true | _ -> false)
  in
  let html =
    [ Html.div (Html.class' class_ :: events) (top @ body @ data)
    ; avatars
    ; Html.div [Html.classList [("use-wrapper", true); ("fade", hasFf)]] usages
    ]
  in
  ViewUtils.placeHtml pos boxClasses html m


let tlCacheKey (m : model) tl =
  let tlid = TL.id tl in
  if Some tlid = tlidOf m.cursorState
  then None
  else
    let hovered =
      match List.head m.hovering with
      | Some (tlid_, id) when tlid_ = tlid ->
          Some id
      | _ ->
          None
    in
    let tracesLoaded =
      Analysis.getTraces m tlid
      |> List.map ~f:(fun (_, traceData) -> Option.isSome traceData)
    in
    let avatarsList = Avatar.filterAvatarsByTlid m.avatarsList tlid in
    let props = TLIDDict.get ~tlid m.handlerProps in
    let workerSchedule =
      tl |> TL.asHandler |> Option.andThen ~f:(Handlers.getWorkerSchedule m)
    in
    Some
      ( tl
      , Analysis.getSelectedTraceID m tlid
      , hovered
      , tracesLoaded
      , avatarsList
      , props
      , workerSchedule )


let tlCacheKeyDB (m : model) tl =
  let tlid = TL.id tl in
  if Some tlid = tlidOf m.cursorState
  then None
  else
    let avatarsList = Avatar.filterAvatarsByTlid m.avatarsList tlid in
    Some (tl, DB.isLocked m tlid, avatarsList)


let tlCacheKeyTipe (m : model) tl =
  let tlid = TL.id tl in
  if Some tlid = tlidOf m.cursorState
  then None
  else
    let avatarsList = Avatar.filterAvatarsByTlid m.avatarsList tlid in
    Some (tl, avatarsList)


let tlCacheKeyGroup (m : model) tl =
  let tlid = TL.id tl in
  if Some tlid = tlidOf m.cursorState
  then None
  else
    let avatarsList = Avatar.filterAvatarsByTlid m.avatarsList tlid in
    Some (tl, avatarsList)


let viewTL m tl =
  match tl with
  | TLGroup _ ->
      Cache.cache2m tlCacheKeyGroup viewTL_ m tl
  | TLTipe _ ->
      Cache.cache2m tlCacheKeyTipe viewTL_ m tl
  | TLDB _ ->
      Cache.cache2m tlCacheKeyDB viewTL_ m tl
  | TLFunc _ | TLHandler _ ->
      Cache.cache2m tlCacheKey viewTL_ m tl


let viewCanvas (m : model) : msg Html.html =
  let allDivs =
    match m.currentPage with
    | Architecture | FocusedHandler _ | FocusedDB _ | FocusedGroup _ ->
        m
        |> TL.structural
        |> TD.values
        (* TEA's vdom assumes lists have the same ordering, and diffs incorrectly
       * if not (though only when using our Util cache). This leads to the
       * clicks going to the wrong toplevel. Sorting solves it, though I don't
       * know exactly how. TODO: we removed the Util cache so it might work. *)
        |> List.sortBy ~f:(fun tl -> deTLID (TL.id tl))
        (* Filter out toplevels that are not in a group *)
        |> List.filter ~f:(fun tl -> not (Groups.isInGroup (TL.id tl) m.groups))
        |> List.map ~f:(viewTL m)
    | FocusedFn tlid ->
      ( match TD.get ~tlid m.userFunctions with
      | Some func ->
          [viewTL m (TL.ufToTL func)]
      | None ->
          [] )
    | FocusedType tlid ->
      ( match TD.get ~tlid m.userTipes with
      | Some tipe ->
          [viewTL m (TL.utToTL tipe)]
      | None ->
          [] )
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
    | FocusedGroup _ ->
        "focused-group"
  in
  Html.div
    [ Html.id "canvas"
    ; Html.class' pageClass
    ]
    (allDivs)


let viewMinimap (data : string option) : msg Html.html =
  match data with
  | Some src ->
      Html.div
        [ Html.id "minimap"
        ; Html.class' "minimap"
        ; ViewUtils.eventNoPropagation ~key:"return-to-arch" "click" (fun _ ->
              GoToArchitecturalView ) ]
        [Html.img [Html.src src; Vdom.prop "alt" "architecture preview"] []]
  | None ->
      Vdom.noNode


let viewToast (t : toast) : msg Html.html =
  let msg = Option.withDefault ~default:"" t.toastMessage in
  let classes =
    if Option.isSome t.toastMessage then "toast show" else "toast"
  in
  let styleOverrides =
    match t.toastPos with
    | Some {vx; vy} ->
        Html.styles
          [ ("top", string_of_int (vy - 10) ^ "px")
          ; ("left", string_of_int (vx + 10) ^ "px") ]
    | None ->
        Vdom.noProp
  in
  Html.div
    [ Html.class' classes
    ; ViewUtils.onAnimationEnd ~key:"toast" ~listener:(fun _ -> ResetToast)
    ; styleOverrides ]
    [Html.text msg]


let accountView (m : model) : msg Html.html =
  let logout =
    Html.a
      [ ViewUtils.eventNoPropagation ~key:"logout" "mouseup" (fun _ ->
            LogoutOfDark )
      ; Html.class' "action-link" ]
      [Html.text "Logout"]
  in
  Html.div
    [Html.class' "my-account"]
    [ m |> Avatar.myAvatar |> Avatar.avatarDiv
    ; Html.div [Html.class' "account-actions"] [logout] ]


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
  let footer =
    [ ViewScaffold.viewIntegrationTestButton m.integrationTestState
    ; ViewScaffold.readOnlyMessage m
    ; viewMinimap m.canvasProps.minimap
    ; ViewScaffold.viewError m.error ]
  in
  let sidebar = ViewSidebar.viewSidebar m in
  let body = viewCanvas m in
  let entry = ViewEntry.viewEntry m in
  let activeAvatars = Avatar.viewAllAvatars m.avatarsList in
  let ast = TL.selectedAST m |> Option.withDefault ~default:(Blank.new_ ()) in
  let fluidStatus =
    if m.editorSettings.showFluidDebugger
    then [Fluid.viewStatus (Fluid.fromExpr m.fluidState ast) m.fluidState]
    else []
  in
  let viewDocs =
    [ Html.a
        [ Html.class' "doc-container"
        ; Html.href "https://ops-documentation.builtwithdark.com/user-manual"
        ; Html.target "_blank"
        ; ViewUtils.eventNoPropagation ~key:"doc" "mouseup" (fun _ -> IgnoreMsg)
        ]
        [fontAwesome "book"; Html.text "Docs"] ]
  in
  let content =
    ViewTopbar.html m
    @ [sidebar; body; activeAvatars; accountView m; viewToast m.toast; entry]
    @ fluidStatus
    @ footer
    @ viewDocs
  in
  Html.div attributes content
