open Prelude
module Regex = Util.Regex
module TL = Toplevel
module TD = TLIDDict
module E = FluidExpression
module ASTInfo = FluidTokenizer.ASTInfo

type viewProps =
  { tl : toplevel
  ; functions : Functions.t
  ; astInfo : ASTInfo.t
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
  ; fnProps : fnProps
  ; showHandlerASTs : bool
  ; secretValues : string list }

(* ----------------------------- *)
(* Events *)
(* ----------------------------- *)
type domEvent = msg Vdom.property

type domEventList = domEvent list

let createVS (m : model) (tl : toplevel) : viewProps =
  let tlid = TL.id tl in
  let hp =
    match tl with TLHandler _ -> Map.get ~key:tlid m.handlerProps | _ -> None
  in
  let traceID = Analysis.getSelectedTraceID m tlid in
  let ast =
    TL.getAST tl |> Option.unwrap ~default:(FluidAST.ofExpr (E.newB ()))
  in
  let analysisStore =
    Option.map traceID ~f:(Analysis.getStoredAnalysis m)
    |> Option.unwrap ~default:LoadableNotInitialized
  in
  let props = FluidUtil.propsFromModel m in
  let astInfo = ASTInfo.make props ast m.fluidState in
  { tl
  ; astInfo
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
  ; functions = m.functions
  ; analysisStore
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
      Map.get ~key:tlid m.handlerProps
      |> Option.map ~f:(fun x -> x.hoveringReferences)
      |> Option.unwrap ~default:[]
  ; fluidState = m.fluidState
  ; avatarsList =
      ( match m.currentPage with
      | FocusedHandler (tlid_, _, _)
      | FocusedType tlid_
      | FocusedFn (tlid_, _)
      | FocusedDB (tlid_, _)
        when tlid_ = tlid ->
          m.avatarsList
      | _ ->
          [] )
  ; permission = m.permission
  ; workerStats =
      (* Right now we patch because worker execution link depends on name instead of TLID. When we fix our worker association to depend on TLID instead of name, then we will get rid of this patchy hack. *)
      (let count = Map.get ~key:tlid m.workerStats in
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
      Map.get ~key:tlid m.tlMenus |> Option.unwrap ~default:Defaults.defaultMenu
  ; isExecuting =
      (* Converge can execute for functions & handlers *)
      ( match tl with
      | TLFunc _ ->
          List.any ~f:(fun (fTLID, _) -> fTLID = tlid) m.executingFunctions
      | TLHandler _ ->
        (* Doing explicit match here just to be safe, even though we can probably assume you can't have handlerProp without it being a handler from code above. *)
        (match hp with Some p -> p.execution = Executing | _ -> false)
      | TLPmFunc _ | TLDB _ | TLTipe _ ->
          false )
  ; fnProps = m.currentUserFn
  ; showHandlerASTs = m.editorSettings.showHandlerASTs
  ; secretValues = m.secrets |> List.map ~f:SecretTypes.getSecretValue }


let fontAwesome (name : string) : msg Html.html =
  Html.i [Html.class' ("fa fa-" ^ name)] []


let darkIcon (name : string) : msg Html.html =
  Html.i [Html.class' ("di di-" ^ name)] []


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


let scrollEventNeither
    ~(key : string) (event : string) (constructor : scrollEvent -> msg) :
    msg Vdom.property =
  Tea.Html.onWithOptions
    ~key
    event
    {stopPropagation = true; preventDefault = true}
    (Decoders.wrapDecoder (Decoders.scrollEvent constructor))


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
  eventNoPropagation ~key:"" name (fun _ ->
      (* For fluid, we need to know about most (all?) mouseups *)
      if name = "mouseup" then IgnoreMouseUp else IgnoreMsg name)


let placeHtml (pos : pos) (classes : 'a list) (html : msg Html.html list) :
    msg Html.html =
  let styles =
    Html.styles
      [("left", string_of_int pos.x ^ "px"); ("top", string_of_int pos.y ^ "px")]
  in
  Html.div [Html.classList (("node", true) :: classes); styles] html


let inCh (w : int) : string = w |> string_of_int |> fun s -> s ^ "ch"

let widthInCh (w : int) : msg Vdom.property = w |> inCh |> Html.style "width"

let createHandlerProp (hs : handler list) : handlerProp TD.t =
  hs
  |> List.map ~f:(fun h -> (h.hTLID, Defaults.defaultHandlerProp))
  |> TD.fromList


let isHoverOverTL (vp : viewProps) : bool =
  match vp.hovering with
  | Some (tlid, _id) when tlid = TL.id vp.tl ->
      true
  | _ ->
      false


let intAsUnit (i : int) (u : string) : string = string_of_int i ^ u

let fnForToken (functions : Functions.t) token : function_ option =
  match token with
  | TBinOp (_, fnName, _)
  | TFnVersion (_, _, _, fnName)
  | TFnName (_, _, _, fnName, _) ->
      Functions.find fnName functions
  | _ ->
      None
