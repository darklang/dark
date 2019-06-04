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
  ; ufns = m.userFunctions
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
      | FocusedHandler (tlid_, _) when tlid_ = tl.id ->
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
  match b with Partial _ | Blank _ -> 6 | F (_, value) -> f value


let strBlankOrLength (b : string blankOr) : int =
  blankOrLength ~f:String.length b


let visualStringLength (string : string) : int =
  string
  |> Regex.replace ~re:(Regex.regex "\t") ~repl:"        "
  (* replace tabs with 8 ch for ch counting *)
  |> String.length


let rec approxWidth (e : expr) : int =
  match e with Partial _ | Blank _ -> 6 | F (_, ne) -> approxNWidth ne


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


let svgIconFunction : msg Html.html =
  Svg.svg
    [ Svg.Attributes.viewBox "0 0 32 32"
    ; Svg.Attributes.width "100%"
    ; Svg.Attributes.height "100%" ]
    [ Svg.g
        []
        [ Svg.path
            [ Svg.Attributes.d
                "M10.3991713,11.0045906 C10.4680726,6.519536 14.0347707,2.93878054 18.371547,3.00079346 C18.718632,3.00079346 19,3.28216147 19,3.6292465 L19,6.68064887 C19,7.0277339 18.718632,7.30910191 18.371547,7.30910191 C17.3884,7.25819781 16.4281495,7.62690969 15.7153239,8.32902429 C15.0024983,9.0311389 14.5995974,10.0050955 14.6008287,11.0231609 L14.6008287,13.1587449 L18,13.1587449 C18.5522847,13.1587449 19,13.6064601 19,14.1587449 L19,16.5041939 C19,17.0564786 18.5522847,17.5041939 18,17.5041939 L14.6008287,17.5041939 L14.6008287,20.9954094 C14.5319274,25.480464 10.9652293,29.0612195 6.62845304,28.9992065 C6.28136801,28.9992065 6,28.7178385 6,28.3707535 L6,25.3379214 C6,24.9908364 6.28136801,24.7094684 6.62845304,24.7094684 C7.61159997,24.7603725 8.57185048,24.3916606 9.28467608,23.689546 C9.99750168,22.9874314 10.4004026,22.0134748 10.3991713,20.9954094 L10.3991713,17.5041939 L7.62845304,17.5041939 C7.07616829,17.5041939 6.62845304,17.0564786 6.62845304,16.5041939 L6.62845304,14.1587449 C6.62845304,13.6064601 7.07616829,13.1587449 7.62845304,13.1587449 L10.3991713,13.1587449 L10.3991713,11.0045906 Z M25.3199105,19.6711409 C25.6930344,20.0393553 25.6970143,20.6403286 25.3288,21.0134524 C25.3273282,21.0149438 25.3258516,21.0164303 25.32437,21.0179119 L23.3512304,22.9910515 L25.3199105,24.9597315 C25.6955136,25.3353346 25.6955136,25.9443075 25.3199105,26.3199105 C24.9443075,26.6955136 24.3353346,26.6955136 23.9597315,26.3199105 L21.9910515,24.3512304 L20.0179119,26.32437 C19.6472347,26.6950472 19.0462482,26.6950472 18.675571,26.32437 C18.6740894,26.3228884 18.6726127,26.3214019 18.6711409,26.3199105 C18.2997632,25.9435811 18.3017683,25.3380527 18.67563,24.964191 L20.6487696,22.9910515 L18.6711409,21.0134228 C18.30048,20.6427619 18.30048,20.0418018 18.6711409,19.6711409 C19.0418018,19.30048 19.6427619,19.30048 20.0134228,19.6711409 L21.9910515,21.6487696 L23.964191,19.67563 C24.3380527,19.3017683 24.9435811,19.2997632 25.3199105,19.6711409 Z"
            ]
            [] ] ]


let svgIconHTTP : msg Html.html =
  Svg.svg
    [ Svg.Attributes.viewBox "0 0 32 32"
    ; Svg.Attributes.width "100%"
    ; Svg.Attributes.height "100%"
    ; Svg.Attributes.preserveAspectRatio "none" ]
    [ Svg.g
        []
        [ Svg.path
            [ Svg.Attributes.d
                "M3.08,10.728 C3.08,10.2959978 3.15599924,9.89200188 3.308,9.516 C3.46000076,9.13999812 3.67199864,8.8120014 3.944,8.532 C4.21600136,8.2519986 4.54399808,8.02800084 4.928,7.86 C5.31200192,7.69199916 5.73599768,7.608 6.2,7.608 C6.66400232,7.608 7.08799808,7.69199916 7.472,7.86 C7.85600192,8.02800084 8.18399864,8.2519986 8.456,8.532 C8.72800136,8.8120014 8.93999924,9.13999812 9.092,9.516 C9.24400076,9.89200188 9.32,10.2959978 9.32,10.728 C9.32,11.1600022 9.24400076,11.5639981 9.092,11.94 C8.93999924,12.3160019 8.72800136,12.6479986 8.456,12.936 C8.18399864,13.2240014 7.85600192,13.4479992 7.472,13.608 C7.08799808,13.7680008 6.66400232,13.848 6.2,13.848 C5.73599768,13.848 5.31200192,13.7680008 4.928,13.608 C4.54399808,13.4479992 4.21600136,13.2240014 3.944,12.936 C3.67199864,12.6479986 3.46000076,12.3160019 3.308,11.94 C3.15599924,11.5639981 3.08,11.1600022 3.08,10.728 Z M3.08,19.168 C3.08,18.7359978 3.15599924,18.3320019 3.308,17.956 C3.46000076,17.5799981 3.67199864,17.2520014 3.944,16.972 C4.21600136,16.6919986 4.54399808,16.4680008 4.928,16.3 C5.31200192,16.1319992 5.73599768,16.048 6.2,16.048 C6.66400232,16.048 7.08799808,16.1319992 7.472,16.3 C7.85600192,16.4680008 8.18399864,16.6919986 8.456,16.972 C8.72800136,17.2520014 8.93999924,17.5799981 9.092,17.956 C9.24400076,18.3320019 9.32,18.7359978 9.32,19.168 C9.32,19.6000022 9.24400076,20.0039981 9.092,20.38 C8.93999924,20.7560019 8.72800136,21.0879986 8.456,21.376 C8.18399864,21.6640014 7.85600192,21.8879992 7.472,22.048 C7.08799808,22.2080008 6.66400232,22.288 6.2,22.288 C5.73599768,22.288 5.31200192,22.2080008 4.928,22.048 C4.54399808,21.8879992 4.21600136,21.6640014 3.944,21.376 C3.67199864,21.0879986 3.46000076,20.7560019 3.308,20.38 C3.15599924,20.0039981 3.08,19.6000022 3.08,19.168 Z M13.8132525,26 L11.4510574,26 C10.8987726,26 10.4510574,25.5522847 10.4510574,25 C10.4510574,24.8783377 10.4732584,24.7576966 10.5165696,24.6440047 L17.7569439,5.63802228 C17.9035279,5.25323914 18.2709822,4.99764056 18.682725,4.99405551 L21.1242456,4.97279716 C21.6765095,4.9679886 22.1281058,5.41178876 22.1329144,5.96405258 C22.1340087,6.08973502 22.1114001,6.21449729 22.0662728,6.33180373 L14.7465729,25.3590445 C14.5980196,25.7452022 14.2269986,26 13.8132525,26 Z M19.8908474,26 L17.5300755,26 C16.9777908,26 16.5300755,25.5522847 16.5300755,25 C16.5300755,24.8785503 16.5521991,24.7581165 16.5953628,24.6445959 L23.8325686,5.61074458 C23.9812779,5.21963902 24.3577632,4.96246901 24.7761699,4.96618818 L27.1427132,4.98722412 C27.6949761,4.99213312 28.1386942,5.44381022 28.1337852,5.99607315 C28.1327303,6.1147438 28.1105603,6.23227929 28.0683124,6.34317994 L20.8253351,25.3559953 C20.6776186,25.7437513 20.3057869,26 19.8908474,26 Z"
            ]
            [] ] ]


let svgIconDB : msg Html.html =
  Svg.svg
    [ Svg.Attributes.viewBox "0 0 32 32"
    ; Svg.Attributes.width "100%"
    ; Svg.Attributes.height "100%"
    ; Svg.Attributes.preserveAspectRatio "none" ]
    [ Svg.g
        []
        [ Svg.path
            [ Svg.Attributes.d
                "M27,12 C24.0666667,13.4666667 20.4,14.2 16,14.2 C11.6,14.2 7.93333333,13.4666667 5,12 L5,8.09615385 C5,7.21153846 7.64,5 16,5 C24.36,5 27,7.21153846 27,8.09615385 L27,12 Z M27,14.64 L27,18.16 C24.0666667,19.6266667 20.4,20.36 16,20.36 C11.6,20.36 7.93333333,19.6266667 5,18.16 L5,14.64 C7.34666667,16.1066667 11.0133333,16.84 16,16.84 C20.9866667,16.84 24.6533333,16.1066667 27,14.64 Z M27,20.8 L27,24.9038462 C27,25.7884615 23.48,28 15.56,28 C7.64,28 5,25.7884615 5,24.9038462 L5,20.8 C7.34666667,22.2666667 11.0133333,23 16,23 C20.9866667,23 24.6533333,22.2666667 27,20.8 Z"
            ]
            [] ] ]


let svgIconDeleted : msg Html.html =
  Svg.svg
    [ Svg.Attributes.viewBox "0 0 32 32"
    ; Svg.Attributes.width "100%"
    ; Svg.Attributes.height "100%"
    ; Svg.Attributes.preserveAspectRatio "none" ]
    [ Svg.g
        []
        [ Svg.path
            [ Svg.Attributes.d
                "M25.2857143,5.33333823 L19.9285714,5.33333823 L19.5089286,4.55417299 C19.3276084,4.21439976 18.9558067,3.9997351 18.5491071,4.00000734 L13.4464286,4.00000734 C13.0404939,3.99855089 12.6693935,4.21381187 12.4910714,4.55417299 L12.0714286,5.33333823 L6.71428571,5.33333823 C6.31979661,5.33333823 6,5.63181452 6,6 L6,7.33333456 C6,7.70152371 6.31979661,8 6.71428571,8 L25.2857143,8 C25.6802034,8 26,7.70152371 26,7.33333456 L26,6 C26,5.63181452 25.6802034,5.33333823 25.2857143,5.33333823 Z M7.99375,25.890625 C8.06799532,27.0762278 9.05113724,27.9998476 10.2390625,28 L21.7609375,28 C22.9488628,27.9998476 23.9320047,27.0762278 24.00625,25.890625 L25,10 L7,10 L7.99375,25.890625 Z"
            ]
            [] ] ]


let svgIconStatic : msg Html.html =
  Svg.svg
    [ Svg.Attributes.viewBox "0 0 32 32"
    ; Svg.Attributes.width "100%"
    ; Svg.Attributes.height "100%"
    ; Svg.Attributes.preserveAspectRatio "none" ]
    [ Svg.g
        []
        [ Svg.path
            [ Svg.Attributes.d
                "M25.6607377,9.51783869 C26.156274,10.013375 26.5714531,11.0044476 26.5714531,11.7142699 L26.5714531,27.1428597 C26.5714531,27.852682 25.9955596,28.4285755 25.2857373,28.4285755 L7.28571582,28.4285755 C6.57589354,28.4285755 6,27.852682 6,27.1428597 L6,5.71426272 C6,5.00444045 6.57589354,4.42854691 7.28571582,4.42854691 L19.2857301,4.42854691 C19.9955524,4.42854691 20.986625,4.84372597 21.4821613,5.33926228 L25.6607377,9.51783869 Z M23.1428776,20.7142806 L18.8571582,16.4285612 L13.7142949,21.5714245 L12.0000072,19.8571367 L9.42857552,22.4285612 L9.42857552,25 L23.1428776,25 L23.1428776,20.7142806 Z M12.0000072,18.142849 C13.4196517,18.142849 14.5714388,16.9910619 14.5714388,15.5714173 C14.5714388,14.1517728 13.4196517,12.9999857 12.0000072,12.9999857 C10.5803626,12.9999857 9.42857552,14.1517728 9.42857552,15.5714173 C9.42857552,16.9910619 10.5803626,18.142849 12.0000072,18.142849 Z M19.7143021,6.24997765 L19.7143021,11.2856979 L24.7500224,11.2856979 C24.6696651,11.0580191 24.5491293,10.8303403 24.4553791,10.7365901 L20.2634099,6.54462086 C20.1696597,6.45087074 19.9419809,6.33033489 19.7143021,6.24997765 Z"
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
