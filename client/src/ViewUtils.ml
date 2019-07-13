open Tc
open Types
open Prelude
module Svg = Tea.Svg
module Regex = Util.Regex

type viewState =
  { tl : toplevel
  ; cursorState : cursorState
  ; tlid : tlid
  ; hovering : (tlid * id) option
  ; ac : autocomplete
  ; handlerSpace : handlerSpace
  ; showEntry : bool
  ; showLivevalue : bool
  ; dbLocked : bool
  ; currentResults : analysisResults (* for current selected cursor/trace *)
  ; traces : trace list
  ; analyses : analyses
  ; dbStats : dbStatsStore
  ; ufns : userFunction list
  ; fns : function_ list
  ; relatedBlankOrs : id list
  ; tooWide : bool
  ; executingFunctions : id list
  ; executingHandlers : StrSet.t
  ; tlCursors : tlCursors
  ; testVariants : variantTest list
  ; featureFlags : flagsVS
  ; handlerProp : handlerProp option
  ; canvasName : string
  ; userContentHost : string
  ; inReferences : usedIn list
  ; toReferences : refersTo list
  ; usagesOfHoveredReference : id list
  ; fluidState : Types.fluidState
  ; avatarsList : avatar list }

let usagesOfBindingAtCursor (tl : toplevel) (cs : cursorState) : id list =
  match unwrapCursorState cs with
  | Entering (Filling (_, id)) ->
    ( match Toplevel.find tl id with
    | Some (PVarBind (F (_, var))) as pd ->
      ( match Toplevel.getParentOf tl (deOption "impossible" pd) with
      | Some (PExpr e) ->
        ( match e with
        | F (_, Let (_, _, body)) ->
            AST.uses var body |> List.map ~f:Blank.toID
        | F (_, Lambda (_, body)) ->
            AST.uses var body |> List.map ~f:Blank.toID
        | _ ->
            [] )
      | _ ->
          [] )
    | Some (PPattern (F (_, _)) as pd) ->
        let parent = Toplevel.getParentOf tl pd in
        let caseContainingPattern (p, _) =
          Pattern.extractById p (Pointer.toID pd) |> Option.isSome
        in
        let relatedVariableIds (p, body) =
          Pattern.variableNames p
          |> List.map ~f:(fun var ->
                 AST.uses var body |> List.map ~f:Blank.toID )
          |> List.concat
        in
        ( match parent with
        | Some (PExpr (F (_, Match (_, cases)))) ->
            cases
            |> List.filter ~f:caseContainingPattern
            |> List.map ~f:relatedVariableIds
            |> List.concat
        | _ ->
            [] )
    | _ ->
        [] )
  | _ ->
      []


let usagesOfHoveredReference (tl : toplevel) (hp : handlerProp option) :
    id list =
  match tl.data with
  | TLHandler h ->
      let body = h.ast in
      hp
      |> Option.andThen ~f:(fun p -> p.hoveringVariableName)
      |> Option.andThen ~f:(fun v ->
             Some (AST.uses v body |> List.map ~f:Blank.toID) )
      |> Option.withDefault ~default:[]
  | _ ->
      []


let createVS (m : model) (tl : toplevel) : viewState =
  let hp =
    match tl.data with
    | TLHandler _ ->
        StrDict.get ~key:(showTLID tl.id) m.handlerProps
    | _ ->
        None
  in
  { tl
  ; cursorState = unwrapCursorState m.cursorState
  ; tlid = tl.id
  ; hovering =
      m.hovering
      |> List.filter ~f:(fun (tlid, _) -> tlid = tl.id)
      |> List.head
      |> Option.andThen ~f:(fun ((_, i) as res) ->
             match idOf m.cursorState with
             | Some cur ->
                 if cur = i then None else Some res
             | _ ->
                 Some res )
  ; ac = m.complete
  ; showEntry = true
  ; showLivevalue = true
  ; handlerSpace = Toplevel.spaceOf tl |> Option.withDefault ~default:HSOther
  ; dbLocked = DB.isLocked m tl.id
  ; ufns = m.userFunctions |> TLIDDict.values
  ; fns = m.builtInFunctions
  ; currentResults = Analysis.getCurrentAnalysisResults m tl.id
  ; traces = Analysis.getTraces m tl.id
  ; analyses = m.analyses
  ; dbStats = m.dbStats
  ; relatedBlankOrs = usagesOfBindingAtCursor tl m.cursorState
  ; tooWide = false
  ; executingFunctions =
      List.filter ~f:(fun (tlid, _) -> tlid = tl.id) m.executingFunctions
      |> List.map ~f:(fun (_, id) -> id)
  ; executingHandlers =
      ( if StrSet.member ~value:(deTLID tl.id) m.executingHandlers
      then StrSet.ofList [deTLID tl.id]
      else StrSet.empty )
  ; tlCursors = m.tlCursors
  ; testVariants = m.tests
  ; featureFlags = m.featureFlags
  ; handlerProp = hp
  ; canvasName = m.canvasName
  ; userContentHost = m.userContentHost
  ; inReferences =
      ( match m.currentPage with
      | FocusedDB (tlid_, _) when tlid_ = tl.id ->
          Introspect.allIn tlid_ m
      | _ ->
          [] )
  ; toReferences =
      ( match m.currentPage with
      | FocusedHandler (tlid_, _) when tlid_ = tl.id ->
          Introspect.allTo tlid_ m
      | _ ->
          [] )
  ; usagesOfHoveredReference = usagesOfHoveredReference tl hp
  ; fluidState = m.fluidState
  ; avatarsList =
      ( match m.currentPage with
      | FocusedHandler (tlid_, _)
      | FocusedType tlid_
      | FocusedFn tlid_
      | FocusedDB (tlid_, _)
        when tlid_ = tl.id ->
          Introspect.presentAvatars m
      | _ ->
          [] ) }


let fontAwesome (name : string) : msg Html.html =
  Html.i [Html.class' ("fa fa-" ^ name)] []


let decodeClickEvent (fn : mouseEvent -> 'a) j : 'a =
  let module JSD = Json_decode_extended in
  fn
    { mePos =
        {vx = JSD.field "pageX" JSD.int j; vy = JSD.field "pageY" JSD.int j}
    ; button = JSD.field "button" JSD.int j }


let decodeTransEvent (fn : string -> 'a) j : 'a =
  let module JSD = Json_decode_extended in
  fn (JSD.field "propertyName" JSD.string j)


let eventNeither
    ~(key : string) (event : string) (constructor : mouseEvent -> msg) :
    msg Vdom.property =
  Patched_tea_html.onWithOptions
    ~key
    event
    {stopPropagation = true; preventDefault = true}
    (Decoders.wrapDecoder (decodeClickEvent constructor))


let eventNoPropagation
    ~(key : string) (event : string) (constructor : mouseEvent -> msg) :
    msg Vdom.property =
  Patched_tea_html.onWithOptions
    ~key
    event
    {stopPropagation = true; preventDefault = false}
    (Decoders.wrapDecoder (decodeClickEvent constructor))


let onTransitionEnd ~(key : string) ~(listener : string -> msg) :
    msg Vdom.property =
  Patched_tea_html.onWithOptions
    ~key
    "transitionend"
    {stopPropagation = false; preventDefault = true}
    (Decoders.wrapDecoder (decodeTransEvent listener))


let nothingMouseEvent (name : string) : msg Vdom.property =
  eventNoPropagation ~key:"" name (fun _ -> IgnoreMsg)


let placeHtml (pos : pos) (classes : 'a list) (html : msg Html.html list) :
    msg Html.html =
  Html.div
    [ Html.classList (("node", true) :: classes)
    ; Html.styles
        [ ("left", string_of_int pos.x ^ "px")
        ; ("top", string_of_int pos.y ^ "px") ] ]
    html


let inCh (w : int) : string = w |> string_of_int |> fun s -> s ^ "ch"

let widthInCh (w : int) : msg Vdom.property = w |> inCh |> Html.style "width"

let blankOrLength ~(f : 'a -> int) (b : 'a blankOr) : int =
  match b with Blank _ -> 6 | F (_, value) -> f value


let strBlankOrLength (b : string blankOr) : int =
  blankOrLength ~f:String.length b


let visualStringLength (string : string) : int =
  string
  |> Regex.replace ~re:(Regex.regex "\t") ~repl:"        "
  (* replace tabs with 8 ch for ch counting *)
  |> String.length


let rec approxWidth (e : expr) : int =
  match e with Blank _ -> 6 | F (_, ne) -> approxNWidth ne


and approxNWidth (ne : nExpr) : int =
  match ne with
  | Value v ->
      (* -- TODO: calculate visual width here *)
      v |> String.length
  | Variable name ->
      String.length name
  | Let (lhs, rhs, body) ->
      max
        (strBlankOrLength lhs + approxWidth rhs + 4 (* "let" *)
        + 3)
        (* " = " *)
        (approxWidth body)
  | If (cond, ifbody, elsebody) ->
      3
      (* "if " *)
      |> ( + ) (approxWidth cond)
      |> max (approxWidth ifbody + 2)
      (* indent *)
      |> max (approxWidth elsebody + 2)
      (* indent *)
  | FnCall (name, exprs, _) ->
      let sizes =
        exprs
        |> List.map ~f:approxWidth
        |> List.map ~f:(( + ) 1)
        (* the space in between *)
        |> List.sum
      in
      strBlankOrLength name + sizes
  | Constructor (name, exprs) ->
      strBlankOrLength name
      + List.sum (List.map ~f:approxWidth exprs)
      (* spaces *)
      + 1
      + List.length exprs
  | Lambda (_, expr) ->
      max (approxWidth expr) 7
      (* "| var -->" *)
      (* TODO: deal with more vars *)
  | Thread exprs ->
      exprs
      |> List.map ~f:approxWidth
      |> List.maximum
      |> Option.withDefault ~default:2
      |> ( + ) 1
      (* the pipe *)
  | FieldAccess (obj, field) ->
      approxWidth obj + 1 (* "." *)
      + strBlankOrLength field
  | ListLiteral exprs ->
      exprs
      |> List.map ~f:approxWidth
      |> List.map ~f:(( + ) 2)
      (* ", " *)
      |> List.sum
      |> ( + ) 4
      (* "[  ]" *)
  | ObjectLiteral pairs ->
      pairs
      |> List.map ~f:(fun (k, v) -> strBlankOrLength k + approxWidth v)
      |> List.map ~f:(( + ) 2)
      (* ": " *)
      |> List.map ~f:(( + ) 2)
      (* ", " *)
      |> List.maximum
      |> Option.withDefault ~default:0
      |> ( + ) 4
      (* "{  }" *)
  | FeatureFlag (_, _, a, b) ->
      (* probably want both taking the same size *)
      max (approxWidth a) (approxWidth b) + 1
      (* the flag *)
  | Match (matchExpr, cases) ->
      let rec pWidth p =
        match p with
        | PLiteral l ->
            String.length l
        | PVariable v ->
            String.length v
        | PConstructor (name, args) ->
            String.length name
            + List.sum (List.map ~f:(blankOrLength ~f:pWidth) args)
            + List.length args
      in
      let caseWidth (p, e) =
        blankOrLength ~f:pWidth p
        + 2
        (* indent *)
        + 4
        (* arrow and spaces *)
        + approxWidth e
      in
      List.maximum ((6 + approxWidth matchExpr) :: List.map ~f:caseWidth cases)
      |> Option.withDefault ~default:0
  | FluidPartial (str, oldExpr) ->
      max (String.length str) (approxWidth oldExpr)
  | FluidRightPartial (str, oldExpr) ->
      String.length str + approxWidth oldExpr


let splitFnName (fnName : fnName) : string option * string * string =
  let pattern = Js.Re.fromString "^((\\w+)::)?([^_]+)(_v(\\d+))?$" in
  let mResult = Js.Re.exec_ pattern fnName in
  match mResult with
  | Some result ->
      let captures =
        result
        |> Js.Re.captures
        |> Belt.List.fromArray
        |> List.map ~f:Js.toOption
      in
      ( match captures with
      | [_; _; mod_; Some fn; _; Some v] ->
          (mod_, fn, v)
      | [_; _; mod_; Some fn; _; None] ->
          (mod_, fn, "0")
      | _ ->
          Debug.crash "invalid fn name" )
  | None ->
      (None, fnName, "0")


let viewFnName (parens : bool) (fnName : fnName) : msg Html.html =
  let mod_, name, version = splitFnName fnName in
  let name = if parens then "(" ^ name ^ ")" else name in
  let classes = if mod_ = None then ["atom"] else [] in
  let versionTxt = if version = "0" then "" else "v" ^ version in
  let modHtml =
    match mod_ with
    | Some name ->
        [ Html.div [Html.class' "module"] [Html.text name]
        ; Html.div [Html.class' "moduleseparator"] [Html.text "::"] ]
    | _ ->
        []
  in
  Html.div
    [Html.class' "namegroup atom"]
    ( modHtml
    @ [ Html.div
          [ Html.class'
              (String.join ~sep:" " (classes @ ["versioned-function"; "fnname"]))
          ]
          [ Html.span [Html.class' "name"] [Html.text name]
          ; Html.span [Html.class' "version"] [Html.text versionTxt] ] ] )


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


let createHandlerProp (tls : toplevel list) : handlerProp StrDict.t =
  let insertProps tlid props =
    props
    |> StrDict.insert ~key:(showTLID tlid) ~value:Defaults.defaultHandlerProp
  in
  tls
  |> List.foldl
       ~f:(fun tl props ->
         match tl.data with
         | TLHandler _ ->
             insertProps tl.id props
         | _ ->
             props )
       ~init:StrDict.empty


let isHandlerLocked (vs : viewState) : bool =
  match vs.handlerProp with
  | Some p ->
      p.handlerLock
  | None ->
      Defaults.defaultHandlerProp.handlerLock


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
  | Some (tlid, _id) when tlid = vs.tl.id ->
      true
  | _ ->
      false


let toggleIconButton
    ~(name : string)
    ~(activeIcon : string)
    ~(inactiveIcon : string)
    ~(msg : mouseEvent -> msg)
    ~(active : bool)
    ~(key : string) : msg Html.html =
  let icon = if active then activeIcon else inactiveIcon in
  Html.div
    [ Html.classList [(name, true); ("active", active)]
    ; eventNoPropagation ~key "click" msg ]
    [fontAwesome icon]


let intAsUnit (i : int) (u : string) : string = string_of_int i ^ u
