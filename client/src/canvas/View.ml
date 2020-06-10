open Prelude

(* Dark *)
module TL = Toplevel
module P = Pointer
module TD = TLIDDict
module E = FluidExpression

let appID = "app"

let fontAwesome = ViewUtils.fontAwesome

let docsURL = "https://ops-documentation.builtwithdark.com/user-manual"

let functionRefsURL = "https://ops-documentation.builtwithdark.com/?pretty=1"

let keyboardRefsURL = "https://darklang.github.io/docs/keyboard-mapping"

let viewTL_ (m : model) (tl : toplevel) : msg Html.html =
  let tlid = TL.id tl in
  let vs = ViewUtils.createVS m tl in
  let dragEvents =
    [ ViewUtils.eventNoPropagation
        ~key:("tlmd-" ^ TLID.toString tlid)
        "mousedown"
        (fun x -> TLDragRegionMouseDown (tlid, x))
    ; ViewUtils.eventNoPropagation
        ~key:("tlmu-" ^ TLID.toString tlid)
        "mouseup"
        (fun x -> TLDragRegionMouseUp (tlid, x)) ]
  in
  let body, data, draggable =
    match tl with
    | TLHandler h ->
        (ViewHandler.view vs h dragEvents, ViewData.viewData vs, true)
    | TLDB db ->
        (ViewDB.viewDB vs db dragEvents, [], true)
    | TLPmFunc f ->
        ([ViewUserFunction.view vs (PackageFn f) false], [], false)
    | TLFunc f ->
        ( [ViewUserFunction.view vs (UserFunction f) m.tooltipState.fnSpace]
        , ViewData.viewData vs
        , false )
    | TLTipe t ->
        ([ViewUserType.viewUserTipe vs t], [], false)
    | TLGroup g ->
        ([ViewGroup.viewGroup m vs g dragEvents], [], true)
  in
  let usages =
    ViewIntrospect.allUsagesView tlid vs.usedInRefs vs.refersToRefs
  in
  (* we capture and ignore mouseup here, otherwise clicking things inside the
   * toplevel (but not inside another element with a mouseup handler like
   * .fluid-editor) will bubble up into a "focus canvas" message and deselect
   * our toplevel. Since we capture mouseup, we must capture mousedown as well,
   * otherwise mousedown will begin canvas panning, but mouseup won't stop it. *)
  let events =
    [ ViewUtils.nothingMouseEvent "mouseup"
    ; ViewUtils.nothingMouseEvent "mousedown" ]
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
  let events =
    events
    @ match tl with TLDB _ -> dragEvents | _ -> [Vdom.noProp; Vdom.noProp]
  in
  let avatars = Avatar.viewAvatars m.avatarsList tlid in
  let selected = Some tlid = CursorState.tlidOf m.cursorState in
  let hovering = ViewUtils.isHoverOverTL vs in
  let boxClasses =
    let dragging =
      match m.cursorState with
      | DraggingTL (tlid_, _, _, _) ->
          tlid_ = tlid
      | _ ->
          false
    in
    [("selected", selected); ("dragging", dragging); ("hovering", hovering)]
  in
  (* Need to add aditional css class to remove background color *)
  let classes =
    [ ("toplevel", true)
    ; ("tl-" ^ TLID.toString tlid, true)
    ; ("selected", selected)
    ; ("group", match tl with TLGroup _ -> true | _ -> false) ]
  in
  let id =
    FluidTokenizer.ASTInfo.getToken vs.astInfo
    |> Option.map ~f:(fun ti -> FluidToken.tid ti.token)
    |> Option.orElse (CursorState.idOf m.cursorState)
  in
  let top =
    let p (text : string) = Html.p [] [Html.text text] in
    let viewDoc desc =
      Html.div
        ( Html.classList [("documentation-box", true); ("draggable", draggable)]
        :: dragEvents )
        desc
    in
    match (CursorState.tlidOf m.cursorState, id) with
    | Some tlid_, Some id when tlid_ = tlid ->
        let acFnDocString =
          let regular =
            m.complete
            |> Autocomplete.highlighted
            |> Option.andThen ~f:Autocomplete.documentationForItem
          in
          let desc =
            m.fluidState.ac
            |> FluidAutocomplete.highlightedWithValidity
            |> Option.andThen ~f:FluidAutocomplete.documentationForItem
            |> Option.orElse regular
          in
          Option.map desc ~f:(fun desc -> viewDoc desc)
        in
        let selectedFnDocString =
          let fnAndRail =
            TL.getAST tl
            |> Option.andThen ~f:(fun ast -> FluidAST.find id ast)
            |> Option.andThen ~f:(function
                   | E.EFnCall (_, name, _, sendToRail)
                   | EBinOp (_, name, _, _, sendToRail) ->
                       Some (name, sendToRail)
                   | _ ->
                       None)
            |> Option.andThen ~f:(fun (name, sendToRail) ->
                   Functions.find name m.functions
                   |> Option.map ~f:(fun f -> (f, sendToRail)))
          in
          match fnAndRail with
          | Some (fn, sendToRail) ->
              Some
                (viewDoc
                   ( PrettyDocs.convert fn.fnDescription
                   @ [ViewErrorRailDoc.hintForFunction fn (Some sendToRail)] ))
          | None ->
              None
        in
        let selectedParamDocString =
          let param =
            TL.get m tlid
            |> Option.andThen ~f:TL.getAST
            |> Option.andThen ~f:(AST.getParamIndex id)
            |> Option.andThen ~f:(fun (name, index) ->
                   m.functions
                   |> Functions.find name
                   |> Option.map ~f:(fun x -> x.fnParameters)
                   |> Option.andThen ~f:(List.getAt ~index))
          in
          match param with
          | Some pm ->
              let header =
                pm.paramName ^ " : " ^ Runtime.tipe2str pm.paramTipe
              in
              Some (viewDoc [p header; p pm.paramDescription])
          | _ ->
              None
        in
        let cmdDocString =
          if FluidCommands.isOpenOnTL m.fluidState.cp tlid
          then
            FluidCommands.highlighted m.fluidState.cp
            |> Option.map ~f:(fun c -> viewDoc [p c.doc])
          else None
        in
        acFnDocString
        |> Option.orElse cmdDocString
        |> Option.orElse selectedParamDocString
        |> Option.orElse selectedFnDocString
        |> Option.withDefault ~default:Vdom.noNode
    | _ ->
        Vdom.noNode
  in
  let pos =
    match m.currentPage with
    | Architecture
    | FocusedHandler _
    | FocusedDB _
    | FocusedGroup _
    | SettingsModal _ ->
        TL.pos tl
    | FocusedPackageManagerFn _ | FocusedFn _ | FocusedType _ ->
        Defaults.centerPos
  in
  let hasFF = vs.astInfo.featureFlagTokenInfos <> [] in
  let html =
    [ Html.div
      (* this unique key ensures that when switching between toplevels the entire
       * vdom node is rebuilt so that we don't get yelled at about the fact that
       * the property list or body nodes may be of differing length. Eg, DBs and
       * Fns have different property list lengths *)
        ~unique:(TLID.toString tlid)
        (Html.classList classes :: events)
        ((top :: body) @ data)
    ; avatars
    ; Html.div
        [ Html.classList [("use-wrapper", true); ("fade", hasFF)]
          (* Block opening the omnibox here by preventing canvas pan start *)
        ; ViewUtils.nothingMouseEvent "mousedown" ]
        usages ]
  in
  ViewUtils.placeHtml pos boxClasses html


let tlCacheKey (m : model) tl =
  let tlid = TL.id tl in
  if Some tlid = CursorState.tlidOf m.cursorState
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
      |> List.map ~f:(fun (_, traceData) ->
             match traceData with
             | Ok _ | Error MaximumCallStackError ->
                 true
             | _ ->
                 false)
    in
    let avatarsList = Avatar.filterAvatarsByTlid m.avatarsList tlid in
    let props = TLIDDict.get ~tlid m.handlerProps in
    let menuIsOpen = TLMenu.isOpen m tlid in
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
      , menuIsOpen
      , workerSchedule
      , m.editorSettings.showHandlerASTs )


let tlCacheKeyDB (m : model) tl =
  let tlid = TL.id tl in
  if Some tlid = CursorState.tlidOf m.cursorState
  then None
  else
    let avatarsList = Avatar.filterAvatarsByTlid m.avatarsList tlid in
    let menuIsOpen = TLMenu.isOpen m tlid in
    Some (tl, DB.isLocked m tlid, avatarsList, menuIsOpen)


let tlCacheKeyTipe (m : model) tl =
  let tlid = TL.id tl in
  if Some tlid = CursorState.tlidOf m.cursorState
  then None
  else
    let avatarsList = Avatar.filterAvatarsByTlid m.avatarsList tlid in
    Some (tl, avatarsList)


let tlCacheKeyGroup (m : model) tl =
  let tlid = TL.id tl in
  if Some tlid = CursorState.tlidOf m.cursorState
  then None
  else
    let avatarsList = Avatar.filterAvatarsByTlid m.avatarsList tlid in
    Some (tl, avatarsList)


let viewTL m tl =
  match tl with
  | TLGroup _ ->
      ViewCache.cache2m tlCacheKeyGroup viewTL_ m tl
  | TLTipe _ ->
      ViewCache.cache2m tlCacheKeyTipe viewTL_ m tl
  | TLDB _ ->
      ViewCache.cache2m tlCacheKeyDB viewTL_ m tl
  | TLPmFunc _ | TLFunc _ | TLHandler _ ->
      ViewCache.cache2m tlCacheKey viewTL_ m tl


(** [zeroOutAppScrollImmediate ()] immediately forces the scroll of #app to 0,0.
 * Prefer [zeroOutAppScroll] if possible. *)
let zeroOutAppScrollImmediate () : unit =
  let open Webapi.Dom in
  Document.getElementById appID document
  |> Option.map ~f:(fun app -> Element.scrollTo 0.0 0.0 app)
  |> recoverOpt "zeroOutAppScroll" ~default:()


(** [zeroOutAppScroll] returns a Tea.Cmd.t that forces the scroll of #app to 0,0.
 * We need the invariant of #app scrolled to 0,0 to be maintained in order for #canvas translate to work.
 * See https://www.notion.so/darklang/Positioning-Bug-8831a3e00a234e55856a85861512876e
 * for more information about this constraint and what happens if it is broken. *)
let zeroOutAppScroll : msg Tea.Cmd.t =
  Tea.Cmd.call (fun _ -> zeroOutAppScrollImmediate ())


(** [isAppScrollZero ()] returns true if the scroll of #app is 0,0 and false otherwise.
 * We need the invariant of #app scrolled to 0,0 to be maintained in order for #canvas translate to work.
 * See https://www.notion.so/darklang/Positioning-Bug-8831a3e00a234e55856a85861512876e
 * for more information about this constraint and what happens if it is broken. *)
let isAppScrollZero () : bool =
  let open Webapi.Dom in
  Document.getElementById appID document
  |> Option.map ~f:(fun app ->
         Element.scrollLeft app = 0.0 && Element.scrollTop app = 0.0)
  (* Technically recoverOpt might be better here, but in some situations, #app doesn't exist yet *)
  |> Option.withDefault ~default:true


let viewCanvas (m : model) : msg Html.html =
  let allDivs =
    match m.currentPage with
    | Architecture
    | FocusedHandler _
    | FocusedDB _
    | FocusedGroup _
    | SettingsModal _ ->
        m
        |> TL.structural
        |> TD.values
        (* TEA's vdom assumes lists have the same ordering, and diffs incorrectly
       * if not (though only when using our Util cache). This leads to the
       * clicks going to the wrong toplevel. Sorting solves it, though I don't
       * know exactly how. TODO: we removed the Util cache so it might work. *)
        |> List.sortBy ~f:(fun tl -> TLID.toString (TL.id tl))
        (* Filter out toplevels that are not in a group *)
        |> List.filter ~f:(fun tl -> not (Groups.isInGroup (TL.id tl) m.groups))
        |> List.map ~f:(viewTL m)
    | FocusedPackageManagerFn tlid ->
      ( match TD.get ~tlid m.functions.packageFunctions with
      | Some func ->
          [viewTL m (TL.pmfToTL func)]
      | None ->
          [] )
    | FocusedFn (tlid, _) ->
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
  (* BEGIN HACK *)
  (* This is a last-ditch effort to fix the position bug.
   * If recover doesn't happen in prod, we can remove this
   * for a performance boost. *)
  if isAppScrollZero ()
  then ()
  else recover "forcibly corrected position bug" (zeroOutAppScrollImmediate ()) ;
  (* END HACK *)
  (* Note that the following translation is container relative,
  * so we must ensure that none of the parent elements are scrolled or otherwise moved. *)
  let animationStyle =
    ( "transition"
    , if m.canvasProps.panAnimation = AnimateTransition
      then "transform 0.5s"
      else "unset" )
  in
  let canvasStyles, overlayStyles =
    let offset = m.canvasProps.offset in
    ( (* The canvas is transformed to the inverse offset to move it such that the browser viewport
       * can act as a "camera" looking at the region of the nodes we've scrolled to. *)
      [ animationStyle
      ; ( "transform"
        , Printf.sprintf "translate(%dpx, %dpx)" (-offset.x) (-offset.y) ) ]
    , (* The overlay is inverse-transformed from the canvas so that it is always in the viewport.
       * We "undo" the transformation of the canvas since the overlay is a child of the canvas. *)
      [ animationStyle
      ; ("transform", Printf.sprintf "translate(%dpx, %dpx)" offset.x offset.y)
      ] )
  in
  let overlay =
    let show =
      match m.currentPage with
      | FocusedHandler _ | FocusedDB _ ->
          true
      | Architecture ->
        ( match CursorState.unwrap m.cursorState with
        | Entering (Creating _) ->
            true
        | _ ->
            false )
      | _ ->
          false
    in
    Html.div
      [ Html.classList [("overlay", true); ("show", show)]
      ; (if show then Html.styles overlayStyles else Vdom.noProp) ]
      []
  in
  let pageClass =
    match m.currentPage with
    | SettingsModal _ ->
        "settings-modal"
    | Architecture ->
        "arch"
    | FocusedHandler _ ->
        "focused-handler"
    | FocusedDB _ ->
        "focused-db"
    | FocusedPackageManagerFn _ ->
        "focused-package-manager-fn"
    | FocusedFn _ ->
        "focused-fn"
    | FocusedType _ ->
        "focused-type"
    | FocusedGroup _ ->
        "focused-group"
  in
  Html.div
    [ Html.id "canvas"
    ; Html.class' ("canvas " ^ pageClass)
    ; Html.styles canvasStyles
    ; ViewUtils.onTransitionEnd ~key:"canvas-pan-anim" ~listener:(fun prop ->
          if prop = "transform"
          then CanvasPanAnimationEnd
          else IgnoreMsg "canvas-pan-end") ]
    (overlay :: allDivs)


let viewMinimap (data : string option) (currentPage : page) (showTooltip : bool)
    : msg Html.html =
  match data with
  | Some src ->
      let helpIcon =
        match currentPage with
        | FocusedFn _ ->
            Html.div
              [ Html.class' "help-icon"
              ; ViewUtils.eventNoPropagation ~key:"ept" "mouseenter" (fun _ ->
                    ToolTipMsg (OpenFnTooltip true))
              ; ViewUtils.eventNoPropagation ~key:"epf" "mouseleave" (fun _ ->
                    ToolTipMsg (OpenFnTooltip false)) ]
              [fontAwesome "question-circle"]
        | _ ->
            Vdom.noNode
      in
      let tooltip =
        Tooltips.generateContent FnMiniMap
        |> Tooltips.viewToolTip ~shouldShow:showTooltip
      in

      Html.div
        [Html.id "minimap"; Html.class' "minimap"]
        [ tooltip
        ; Html.div
            [Html.class' "minimap-content"]
            [ helpIcon
            ; Html.img
                [ Html.src src
                ; Vdom.prop "alt" "architecture preview"
                ; ViewUtils.eventNoPropagation
                    ~key:"return-to-arch"
                    "click"
                    (fun _ -> GoToArchitecturalView) ]
                [] ] ]
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
      [ Html.class' "account-action-btn"
      ; Html.href "https://login.darklang.com/logout" ]
      [Html.text "Logout"]
  in
  let docs =
    Html.a
      [ Html.class' "account-action-btn"
      ; Html.href docsURL
      ; Html.target "_blank"
      ; ViewUtils.eventNoPropagation ~key:"account-doc" "click" (fun _ ->
            UpdateSegment OpenDocs) ]
      [Html.text "Documentation"]
  in
  let functionRefs =
    Html.a
      [ Html.class' "account-action-btn"
      ; Html.href functionRefsURL
      ; Html.target "_blank"
      ; ViewUtils.eventNoPropagation ~key:"account-fn-ref" "click" (fun _ ->
            UpdateSegment OpenFnRef) ]
      [Html.text "Function Reference"]
  in
  let keyboardRefs =
    Html.a
      [ Html.class' "account-action-btn"
      ; Html.href keyboardRefsURL
      ; Html.target "_blank"
      ; ViewUtils.eventNoPropagation ~key:"account-fn-ref" "click" (fun _ ->
            UpdateSegment OpenKeyboardRef) ]
      [Html.text "Keyboard Reference"]
  in
  let tutorial =
    Html.p
      [ Html.class' "account-action-btn"
      ; ViewUtils.eventNoPropagation ~key:"tutorial" "click" (fun _ ->
            TutorialMsg ReopenTutorial) ]
      [Html.text "Hello World tutorial"]
  in
  let spacer = Html.div [Html.class' "account-action-spacer"] [] in
  let newCanvas =
    Html.p
      [ Html.class' "account-action-btn"
      ; ViewUtils.eventNoPropagation ~key:"open-settings" "click" (fun _ ->
            SettingsViewMsg (OpenSettingsView NewCanvas)) ]
      [Html.text "New Canvas"]
  in
  let canvasInfo =
    Html.p
      [ Html.class' "account-action-btn"
      ; ViewUtils.eventNoPropagation ~key:"open-settings" "click" (fun _ ->
            SettingsViewMsg (OpenSettingsView CanvasInfo)) ]
      [Html.text "About"]
  in
  let settings =
    Html.p
      [ Html.class' "account-action-btn"
      ; ViewUtils.eventNoPropagation ~key:"open-settings" "click" (fun _ ->
            SettingsViewMsg (OpenSettingsView UserSettings)) ]
      [Html.text "Account"]
  in
  let privacy =
    Html.p
      [ Html.class' "account-action-btn"
      ; ViewUtils.eventNoPropagation ~key:"open-settings" "click" (fun _ ->
            SettingsViewMsg (OpenSettingsView Privacy)) ]
      [Html.text "Privacy"]
  in
  let share =
    Html.p
      [ Html.class' "account-action-btn invite"
      ; ViewUtils.eventNoPropagation ~key:"open-invite" "click" (fun _ ->
            SettingsViewMsg
              (OpenSettingsView
                 (InviteUser SettingsViewTypes.defaultInviteFields))) ]
      [Html.text "Share Dark"]
  in
  let tooltip =
    UserTutorial.generateTooltipContent Welcome m.username
    |> Tooltips.viewToolTip
         ~shouldShow:(m.tooltipState.userTutorial = Some Welcome)
  in
  Html.div
    [ Html.class' "my-account"
      (* Block opening the omnibox here by preventing canvas pan start *)
    ; ViewUtils.nothingMouseEvent "mousedown" ]
    [ m |> Avatar.myAvatar |> Avatar.avatarDiv
    ; tooltip
    ; Html.div
        [Html.class' "account-actions"]
        [ newCanvas
        ; canvasInfo
        ; settings
        ; privacy
        ; share
        ; logout
        ; spacer
        ; docs
        ; functionRefs
        ; keyboardRefs
        ; tutorial ] ]


let view (m : model) : msg Html.html =
  let eventListeners =
    (* We don't want propagation because we don't want to double-handle these events and
     * window has its own listeners. *)
    [ ViewUtils.eventNeither ~key:"app-md" "mousedown" (fun mouseEvent ->
          AppMouseDown mouseEvent)
    ; ViewUtils.eventNeither ~key:"app-mu" "mouseup" (fun mouseEvent ->
          AppMouseUp mouseEvent)
    ; ViewUtils.scrollEventNeither ~key:"app-scroll" "scroll" (fun _ ->
          AppScroll) ]
  in
  let attributes =
    [Html.id appID; Html.class' ("app " ^ VariantTesting.activeCSSClasses m)]
    @ eventListeners
  in
  let footer =
    [ ViewScaffold.viewIntegrationTestButton m.integrationTestState
    ; ViewScaffold.readOnlyMessage m
    ; viewMinimap m.canvasProps.minimap m.currentPage m.tooltipState.fnSpace
    ; ViewScaffold.viewError m.error ]
  in
  let sidebar = ViewSidebar.viewSidebar m in
  let body = viewCanvas m in
  let entry = ViewEntry.viewEntry m in
  let activeAvatars = Avatar.viewAllAvatars m.avatarsList in
  let ast =
    TL.selectedAST m
    |> Option.withDefault ~default:(FluidAST.ofExpr (E.EBlank (gid ())))
  in
  let fluidStatus =
    if m.editorSettings.showFluidDebugger
    then [FluidDebugger.view m ast]
    else [Vdom.noNode]
  in
  let viewDocs =
    [ Html.a
        [ Html.class' "doc-container"
        ; Html.href docsURL
        ; Html.target "_blank"
          (* Block opening the omnibox here by preventing canvas pan start *)
        ; ViewUtils.nothingMouseEvent "mousedown"
        ; ViewUtils.eventNoPropagation ~key:"doc" "click" (fun _ ->
              UpdateSegment OpenDocs) ]
        [fontAwesome "book"; Html.text "Docs"] ]
  in
  let tutorial =
    if m.integrationTestState = NoIntegrationTest
    then UserTutorial.view m.username m.canvasName m.firstVisitToThisCanvas
    else Vdom.noNode
  in
  let modal =
    ViewModal.unsupportedBrowser
      ~show:(m.integrationTestState = NoIntegrationTest && m.unsupportedBrowser)
  in
  let settingsModal =
    if m.settingsView.opened then SettingsView.html m else Vdom.noNode
  in
  let content =
    (FullstoryView.html m :: ViewTopbar.html m)
    @ [ sidebar
      ; body
      ; activeAvatars
      ; accountView m
      ; viewToast m.toast
      ; entry
      ; modal
      ; settingsModal
      ; InsertSecret.view m.insertSecretModal
      ; tutorial ]
    @ fluidStatus
    @ footer
    @ viewDocs
  in
  Html.div attributes content
