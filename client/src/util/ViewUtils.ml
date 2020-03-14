open Prelude
module Svg = Tea.Svg
module Regex = Util.Regex
module TL = Toplevel
module TD = TLIDDict
module E = FluidExpression

type viewState =
  { tl : toplevel
  ; ast : FluidAST.t
  ; tokens : FluidToken.tokenInfo list
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
  ; featureFlags : flagsVS
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
  { tl
  ; ast
  ; tokens = FluidPrinter.tokenize (FluidAST.toExpr ast)
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
  ; featureFlags = m.featureFlags
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

(* The same as static/icons/tipe.svg *)
let svgIconTipe (color : string) : msg Html.html =
  Svg.svg
    [ Svg.Attributes.viewBox "0 0 32 32"
    ; Svg.Attributes.width "16"
    ; Svg.Attributes.height "16" ]
    [ Svg.g
        [Svg.Attributes.fill color]
        [ Svg.path
            [ Svg.Attributes.d
                "M1.736,17.64 L9.60628974,12.6206826 C10.0719374,12.3237134 10.6901605,12.4604543 10.9871297,12.926102 C11.0895746,13.0867356 11.144,13.2732916 11.144,13.4638123 L11.144,14.2066955 C11.144,14.9075596 10.7771302,15.5572866 10.1769863,15.9192781 L5.096,18.984 L10.1967092,22.1415819 C10.7856313,22.5061527 11.144,23.1494787 11.144,23.8421122 L11.144,24.6001877 C11.144,25.1524725 10.6962847,25.6001877 10.144,25.6001877 C9.95347933,25.6001877 9.76692337,25.5457624 9.60628974,25.4433174 L1.736,20.424 L1.70504715,20.3986101 C0.950130257,19.7793695 0.840142878,18.6653948 1.45938346,17.9104779 C1.5414168,17.810471 1.63417757,17.7197687 1.736,17.64 L1.736,17.64 Z M13.248,10.808 L8.264,10.808 C7.71171525,10.808 7.264,10.3602847 7.264,9.808 L7.264,7.2 C7.264,6.64771525 7.71171525,6.2 8.264,6.2 L23.736,6.2 C24.2882847,6.2 24.736,6.64771525 24.736,7.2 L24.736,9.808 C24.736,10.3602847 24.2882847,10.808 23.736,10.808 L18.752,10.808 L18.752,26 C18.752,26.5522847 18.3042847,27 17.752,27 L14.248,27 C13.6957153,27 13.248,26.5522847 13.248,26 L13.248,10.808 Z M30.264,20.424 L22.3937103,25.4433174 C21.9280626,25.7402866 21.3098395,25.6035457 21.0128703,25.137898 C20.9104254,24.9772644 20.856,24.7907084 20.856,24.6001877 L20.856,23.8573045 C20.856,23.1564404 21.2228698,22.5067134 21.8230137,22.1447219 L26.904,19.08 L21.8032908,15.9224181 C21.2143687,15.5578473 20.856,14.9145213 20.856,14.2218878 L20.856,13.4638123 C20.856,12.9115275 21.3037153,12.4638123 21.856,12.4638123 C22.0465207,12.4638123 22.2330766,12.5182376 22.3937103,12.6206826 L30.264,17.64 L30.264,17.64 C31.0326214,18.2421458 31.1675765,19.3533722 30.5654307,20.1219936 C30.485662,20.223816 30.3949598,20.3165768 30.2949529,20.3986101 L30.264,20.424 Z"
            ]
            [] ] ]


(* The same as static/icons/fn.svg *)
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

let fnForToken (state : fluidState) token : function_ option =
  match token with
  | TBinOp (_, fnName)
  | TFnVersion (_, _, _, fnName)
  | TFnName (_, _, _, fnName, _) ->
      Some (Functions.findByNameInList fnName state.ac.functions)
  | _ ->
      None
