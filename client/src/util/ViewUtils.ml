open Prelude
module Svg = Tea.Svg
module Regex = Util.Regex
module TL = Toplevel
module TD = TLIDDict
module E = FluidExpression

type editorState =
  { tlid : TLID.t
  ; panelId : ID.t option
  ; expression : FluidExpression.t
  ; tokens : FluidToken.tokenInfo list }

type viewState =
  { tl : toplevel
  ; ast : FluidAST.t
  ; tokens : FluidToken.tokenInfo list
  ; panels : FluidPanel.Group.t
  ; cursorState : cursorState
  ; tlid : TLID.t
  ; isAdmin : bool
  ; hovering : (TLID.t * ID.t) option
  ; ac : autocomplete
  ; showEntry : bool
  ; showLivevalue : bool
  ; dbLocked : bool
  ; analysisStore : analysisStore (* for current selected trace *)
  ; traces : trace list
  ; dbStats : dbStatsStore
  ; ufns : userFunction list
  ; fns : function_ list
  ; executingFunctions : ID.t list
  ; tlTraceIDs : tlTraceIDs
  ; testVariants : variantTest list
  ; handlerProp : handlerProp option
  ; canvasName : string
  ; userContentHost : string
  ; refersToRefs : (toplevel * ID.t list) list
  ; usedInRefs : toplevel list
  ; hoveringRefs : ID.t list
  ; fluidState : fluidState
  ; avatarsList : avatar list
  ; permission : permission option
  ; workerStats : workerStats option
  ; menuState : menuState
  ; isExecuting : bool
  ; fnProps : fnProps }

(* ----------------------------- *)
(* Events *)
(* ----------------------------- *)
type domEvent = msg Vdom.property

type domEventList = domEvent list

let createVS (m : model) (tl : toplevel) : viewState =
  let tlid = TL.id tl in
  let hp =
    match tl with TLHandler _ -> TD.get ~tlid m.handlerProps | _ -> None
  in
  let traceID = Analysis.getSelectedTraceID m tlid in
  let ast =
    TL.getAST tl |> Option.withDefault ~default:(FluidAST.ofExpr (E.newB ()))
  in
  let tokens = FluidPrinter.tokenize (FluidAST.toExpr ast) in
  let panels =
    (* There are cases like page load where we create a bunch of viewStates at once,
     * but we only have a single fluidState in the model. If we were to blindly
     * apply this map for every toplevel, we'd share the fluidState and create
     * incorrect panels. So, we check the toplevel of the panels with the
     * toplevel that's being created and don't create the panel if it doesn't
     * match. We do create the FluidPanel.Group.t with all the same TLID, so
     * they /should/ always all have the same TLID, but this code doesn't know
     * that, so we have to filter them out individually.
     *
     * Once we have a fluidState per TLID, this can be removed. *)
    FluidPanel.Group.filter ~f:(fun s -> s.tlid = tlid) m.fluidState.panels
  in
  { tl
  ; ast
  ; tokens
  ; tlid
  ; cursorState = CursorState.unwrap m.cursorState
  ; hovering =
      m.hovering
      |> List.filter ~f:(fun (tlid, _) -> tlid = tlid)
      |> List.head
      |> Option.andThen ~f:(fun ((_, i) as res) ->
             match CursorState.idOf m.cursorState with
             | Some cur ->
                 if cur = i then None else Some res
             | _ ->
                 Some res)
  ; ac = m.complete
  ; showEntry = true
  ; showLivevalue = false
  ; isAdmin = m.isAdmin
  ; dbLocked = DB.isLocked m tlid
  ; ufns = m.userFunctions |> TLIDDict.values
  ; fns = m.builtInFunctions
  ; analysisStore =
      Option.map traceID ~f:(Analysis.getStoredAnalysis m)
      |> Option.withDefault ~default:LoadableNotInitialized
  ; traces = Analysis.getTraces m tlid
  ; dbStats = m.dbStats
  ; executingFunctions =
      List.filter ~f:(fun (tlid_, _) -> tlid_ = tlid) m.executingFunctions
      |> List.map ~f:(fun (_, id) -> id)
  ; tlTraceIDs = m.tlTraceIDs
  ; testVariants = m.tests
  ; panels
  ; handlerProp = hp
  ; canvasName = m.canvasName
  ; userContentHost = m.userContentHost
  ; refersToRefs =
      ( if CursorState.tlidOf m.cursorState = Some tlid
      then Introspect.allRefersTo tlid m
      else [] )
  ; usedInRefs =
      ( if CursorState.tlidOf m.cursorState = Some tlid
      then Introspect.allUsedIn tlid m
      else [] )
  ; hoveringRefs =
      TD.get ~tlid m.handlerProps
      |> Option.map ~f:(fun x -> x.hoveringReferences)
      |> Option.withDefault ~default:[]
  ; fluidState = m.fluidState
  ; avatarsList =
      ( match m.currentPage with
      | FocusedHandler (tlid_, _)
      | FocusedType tlid_
      | FocusedFn tlid_
      | FocusedDB (tlid_, _)
      | FocusedGroup (tlid_, _)
        when tlid_ = tlid ->
          m.avatarsList
      | _ ->
          [] )
  ; permission = m.permission
  ; workerStats =
      (* Right now we patch because worker execution link depends on name instead of TLID. When we fix our worker association to depend on TLID instead of name, then we will get rid of this patchy hack. *)
      (let count = TLIDDict.get ~tlid m.workerStats in
       let asWorkerSchedule = Handlers.getWorkerSchedule m in
       let schedule =
         tl |> TL.asHandler |> Option.andThen ~f:asWorkerSchedule
       in
       match (count, schedule) with
       | None, None ->
           None
       | Some c, None ->
           Some c
       | None, Some _ ->
           Some {Defaults.defaultWorkerStats with schedule}
       | Some c, Some _ ->
           Some {c with schedule})
  ; menuState =
      TLIDDict.get ~tlid m.tlMenus
      |> Option.withDefault ~default:Defaults.defaultMenu
  ; isExecuting =
      (* Converge can execute for functions & handlers *)
      ( match tl with
      | TLFunc _ ->
          List.any ~f:(fun (fTLID, _) -> fTLID = tlid) m.executingFunctions
      | TLHandler _ ->
        (* Doing explicit match here just to be safe, even though we can probably assume you can't have handlerProp without it being a handler from code above. *)
        (match hp with Some p -> p.execution = Executing | _ -> false)
      | TLDB _ | TLTipe _ | TLGroup _ ->
          false )
  ; fnProps = m.currentUserFn }


let fontAwesome (name : string) : msg Html.html =
  Html.i [Html.class' ("fa fa-" ^ name)] []


let decodeTransEvent (fn : string -> 'a) j : 'a =
  let open Json.Decode in
  fn (field "propertyName" string j)


let decodeAnimEvent (fn : string -> 'a) j : 'a =
  let open Json.Decode in
  fn (field "animationName" string j)


(* Generic event, the the listener handle and do what it wants with the event object *)
let onEvent
    ~(event : string)
    ~(key : string)
    ?(preventDefault = true)
    (listener : Web.Node.event -> msg) : msg Vdom.property =
  Tea.Html.onCB event key (fun evt ->
      if preventDefault then evt##preventDefault () ;
      Some (listener evt))


let eventBoth ~(key : string) (event : string) (constructor : mouseEvent -> msg)
    : msg Vdom.property =
  Tea.Html.onWithOptions
    ~key
    event
    {stopPropagation = false; preventDefault = false}
    (Decoders.wrapDecoder (Decoders.clickEvent constructor))


let eventPreventDefault
    ~(key : string) (event : string) (constructor : mouseEvent -> msg) :
    msg Vdom.property =
  Tea.Html.onWithOptions
    ~key
    event
    {stopPropagation = false; preventDefault = true}
    (Decoders.wrapDecoder (Decoders.clickEvent constructor))


let eventNeither
    ~(key : string) (event : string) (constructor : mouseEvent -> msg) :
    msg Vdom.property =
  Tea.Html.onWithOptions
    ~key
    event
    {stopPropagation = true; preventDefault = true}
    (Decoders.wrapDecoder (Decoders.clickEvent constructor))


let eventNoPropagation
    ~(key : string) (event : string) (constructor : mouseEvent -> msg) :
    msg Vdom.property =
  Tea.Html.onWithOptions
    ~key
    event
    {stopPropagation = true; preventDefault = false}
    (Decoders.wrapDecoder (Decoders.clickEvent constructor))


let onTransitionEnd ~(key : string) ~(listener : string -> msg) :
    msg Vdom.property =
  Tea.Html.onWithOptions
    ~key
    "transitionend"
    {stopPropagation = false; preventDefault = true}
    (Decoders.wrapDecoder (decodeTransEvent listener))


let onAnimationEnd ~(key : string) ~(listener : string -> msg) :
    msg Vdom.property =
  Tea.Html.onWithOptions
    ~key
    "animationend"
    {stopPropagation = false; preventDefault = true}
    (Decoders.wrapDecoder (decodeAnimEvent listener))


let nothingMouseEvent (name : string) : msg Vdom.property =
  eventNoPropagation ~key:"" name (fun _ -> IgnoreMsg)


let placeHtml (pos : pos) (classes : 'a list) (html : msg Html.html list) :
    msg Html.html =
  let styles =
    Html.styles
      [("left", string_of_int pos.x ^ "px"); ("top", string_of_int pos.y ^ "px")]
  in
  Html.div [Html.classList (("node", true) :: classes); styles] html


let inCh (w : int) : string = w |> string_of_int |> fun s -> s ^ "ch"

let widthInCh (w : int) : msg Vdom.property = w |> inCh |> Html.style "width"

let svgIconFn (color : string) : msg Html.html =
  Svg.svg
    [ Svg.Attributes.viewBox "0 0 16 16"
    ; Svg.Attributes.width "16"
    ; Svg.Attributes.height "16" ]
    [ Svg.g
        [Svg.Attributes.fill color]
        [ Svg.path
            [ Svg.Attributes.d
                "M5,5.62A4.38,4.38,0,0,1,9.44,1.31h.35V3.63H9.44a2,2,0,0,0-2.1,2V6.78H9.79V9.12H7.34V11A4.38,4.38,0,0,1,2.9,15.31H2.55V13H2.9A2,2,0,0,0,5,11V9.12H3.84V6.78H5Z"
            ]
            []
        ; Svg.path
            [ Svg.Attributes.d
                "M12.89,9.91l.76.75-1.48,1.48,1.48,1.48-.76.76L11.41,12.9,9.93,14.38l-.75-.76,1.48-1.48L9.18,10.66l.75-.75,1.48,1.48Z"
            ]
            [] ] ]


let createHandlerProp (hs : handler list) : handlerProp TD.t =
  hs
  |> List.map ~f:(fun h -> (h.hTLID, Defaults.defaultHandlerProp))
  |> TD.fromList


let getHandlerState (vs : viewState) : handlerState =
  match vs.handlerProp with
  | Some p ->
      p.handlerState
  | None ->
      Defaults.defaultHandlerProp.handlerState


let isHandlerExpanded (vs : viewState) : bool =
  let state = getHandlerState vs in
  match state with HandlerExpanded | HandlerExpanding -> true | _ -> false


let isHoverOverTL (vs : viewState) : bool =
  match vs.hovering with
  | Some (tlid, _id) when tlid = TL.id vs.tl ->
      true
  | _ ->
      false


let intAsUnit (i : int) (u : string) : string = string_of_int i ^ u
