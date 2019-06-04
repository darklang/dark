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


<<<<<<< HEAD
=======
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


let svgIconFof : msg Html.html =
  Svg.svg
    [ Svg.Attributes.viewBox "0 0 32 32"
    ; Svg.Attributes.width "100%"
    ; Svg.Attributes.height "100%"
    ; Svg.Attributes.preserveAspectRatio "none" ]
    [ Svg.g
        []
        [ Svg.path
            [ Svg.Attributes.d
                "M4,6 L28,6 C29.1045695,6 30,6.8954305 30,8 L30,24 C30,25.1045695 29.1045695,26 28,26 L4,26 C2.8954305,26 2,25.1045695 2,24 L2,8 C2,6.8954305 2.8954305,6 4,6 Z M4,10 L4,22 C4,23.1045695 4.8954305,24 6,24 L26,24 C27.1045695,24 28,23.1045695 28,22 L28,10 L4,10 Z M22,9 C22.5522847,9 23,8.55228475 23,8 C23,7.44771525 22.5522847,7 22,7 C21.4477153,7 21,7.44771525 21,8 C21,8.55228475 21.4477153,9 22,9 Z M25,9 C25.5522847,9 26,8.55228475 26,8 C26,7.44771525 25.5522847,7 25,7 C24.4477153,7 24,7.44771525 24,8 C24,8.55228475 24.4477153,9 25,9 Z M28,9 C28.5522847,9 29,8.55228475 29,8 C29,7.44771525 28.5522847,7 28,7 C27.4477153,7 27,7.44771525 27,8 C27,8.55228475 27.4477153,9 28,9 Z M11.09,19.8 C10.53,19.8 10.39,19.71 10.39,19.6 L10.39,18.4 L7.2,18.4 C7.09,18.4 7,18.21 7,17.83 C7,17.46 7.05,17.19 7.12,17.1 L10.11,13.3 C10.18,13.21 10.3,13.1 10.8,13.1 C11.49,13.1 11.62,13.23 11.62,13.32 C11.62,13.37 11.61,13.4 11.57,13.45 L8.66,17.2 L10.39,17.2 L10.39,15.86 C10.39,15.74 10.42,15.66 10.51,15.55 L11.43,14.38 C11.5,14.29 11.55,14.25 11.64,14.25 C11.75,14.25 11.79,14.33 11.79,14.44 L11.79,19.6 C11.79,19.71 11.64,19.8 11.09,19.8 Z M16.02,19.9 C14.11,19.9 13.04,18.75 13.04,16.43 C13.04,14.11 14.31,13 16.02,13 C17.73,13 19,14.11 19,16.43 C19,18.75 17.84,19.9 16.02,19.9 Z M16.02,18.75 C17.03,18.75 17.66,18.15 17.66,16.45 C17.66,14.73 16.98,14.17 16.02,14.17 C15.06,14.17 14.38,14.73 14.38,16.45 C14.38,18.15 14.96,18.75 16.02,18.75 Z M23.89,19.8 C23.33,19.8 23.19,19.71 23.19,19.6 L23.19,18.4 L20,18.4 C19.89,18.4 19.8,18.21 19.8,17.83 C19.8,17.46 19.85,17.19 19.92,17.1 L22.91,13.3 C22.98,13.21 23.1,13.1 23.6,13.1 C24.29,13.1 24.42,13.23 24.42,13.32 C24.42,13.37 24.41,13.4 24.37,13.45 L21.46,17.2 L23.19,17.2 L23.19,15.86 C23.19,15.74 23.22,15.66 23.31,15.55 L24.23,14.38 C24.3,14.29 24.35,14.25 24.44,14.25 C24.55,14.25 24.59,14.33 24.59,14.44 L24.59,19.6 C24.59,19.71 24.44,19.8 23.89,19.8 Z"
            ]
            [] ] ]


let svgIconTypes : msg Html.html =
  Svg.svg
    [ Svg.Attributes.viewBox "0 0 32 32"
    ; Svg.Attributes.width "100%"
    ; Svg.Attributes.height "100%"
    ; Svg.Attributes.preserveAspectRatio "none" ]
    [ Svg.g
        []
        [ Svg.path
            [ Svg.Attributes.d
                "M1.736,17.64 L9.60628974,12.6206826 C10.0719374,12.3237134 10.6901605,12.4604543 10.9871297,12.926102 C11.0895746,13.0867356 11.144,13.2732916 11.144,13.4638123 L11.144,14.2066955 C11.144,14.9075596 10.7771302,15.5572866 10.1769863,15.9192781 L5.096,18.984 L10.1967092,22.1415819 C10.7856313,22.5061527 11.144,23.1494787 11.144,23.8421122 L11.144,24.6001877 C11.144,25.1524725 10.6962847,25.6001877 10.144,25.6001877 C9.95347933,25.6001877 9.76692337,25.5457624 9.60628974,25.4433174 L1.736,20.424 L1.70504715,20.3986101 C0.950130257,19.7793695 0.840142878,18.6653948 1.45938346,17.9104779 C1.5414168,17.810471 1.63417757,17.7197687 1.736,17.64 L1.736,17.64 Z M13.248,10.808 L8.264,10.808 C7.71171525,10.808 7.264,10.3602847 7.264,9.808 L7.264,7.2 C7.264,6.64771525 7.71171525,6.2 8.264,6.2 L23.736,6.2 C24.2882847,6.2 24.736,6.64771525 24.736,7.2 L24.736,9.808 C24.736,10.3602847 24.2882847,10.808 23.736,10.808 L18.752,10.808 L18.752,26 C18.752,26.5522847 18.3042847,27 17.752,27 L14.248,27 C13.6957153,27 13.248,26.5522847 13.248,26 L13.248,10.808 Z M30.264,20.424 L22.3937103,25.4433174 C21.9280626,25.7402866 21.3098395,25.6035457 21.0128703,25.137898 C20.9104254,24.9772644 20.856,24.7907084 20.856,24.6001877 L20.856,23.8573045 C20.856,23.1564404 21.2228698,22.5067134 21.8230137,22.1447219 L26.904,19.08 L21.8032908,15.9224181 C21.2143687,15.5578473 20.856,14.9145213 20.856,14.2218878 L20.856,13.4638123 C20.856,12.9115275 21.3037153,12.4638123 21.856,12.4638123 C22.0465207,12.4638123 22.2330766,12.5182376 22.3937103,12.6206826 L30.264,17.64 L30.264,17.64 C31.0326214,18.2421458 31.1675765,19.3533722 30.5654307,20.1219936 C30.485662,20.223816 30.3949598,20.3165768 30.2949529,20.3986101 L30.264,20.424 Z"
            ]
            [] ] ]


let svgIconCron : msg Html.html =
  Svg.svg
    [ Svg.Attributes.viewBox "0 0 32 32"
    ; Svg.Attributes.width "100%"
    ; Svg.Attributes.height "100%"
    ; Svg.Attributes.preserveAspectRatio "none" ]
    [ Svg.g
        []
        [ Svg.path
            [ Svg.Attributes.d
                "M8.67175293,12.3472748 C7.31077576,11.7559662 5.83293152,11.1901855 6.04554749,10.8881073 C6.04496543,10.8855466 6.04438995,10.8829476 6.04382103,10.8803104 C7.96500179,6.81363069 12.1039216,4 16.9,4 C23.527417,4 28.9,9.372583 28.9,16 C28.9,22.627417 23.527417,28 16.9,28 C10.6093362,28 5.44922468,23.1595305 4.94107378,17 L4,17 C3.72385763,17 3.5,16.7761424 3.5,16.5 C3.5,16.3788884 3.54395912,16.2618934 3.62371165,16.1707477 L6.02182754,13.4300438 C6.20366857,13.2222255 6.51954987,13.2011668 6.72736819,13.3830078 C6.74407358,13.397625 6.75978702,13.4133384 6.77440423,13.4300438 L9.17252012,16.1707477 C9.35436115,16.378566 9.33330239,16.6944473 9.12548408,16.8762883 C9.03433833,16.9560409 8.91734335,17 8.79623177,17 L7.95492878,17 C8.05829604,17.9350747 8.3049167,18.8269742 8.67235806,19.6532658 L8.96557899,19.4839746 C9.30038382,19.2906749 9.72849711,19.4053875 9.92179677,19.7401924 L10.2217968,20.2598076 C10.4150964,20.5946125 10.3003838,21.0227257 9.96557899,21.2160254 L9.68351959,21.3788725 C10.231625,22.1130382 10.8891296,22.7607519 11.6318787,23.2978589 L11.7839746,23.034421 C11.9772743,22.6996162 12.4053875,22.5849036 12.7401924,22.7782032 L13.2598076,23.0782032 C13.5946125,23.2715029 13.7093251,23.6996162 13.5160254,24.034421 L13.3727764,24.2825355 C14.1931173,24.6323352 15.0760034,24.8638223 16,24.9555623 L16,23.7 C16,23.3134007 16.3134007,23 16.7,23 L17.3,23 C17.6865993,23 18,23.3134007 18,23.7 L18,24.9334515 C18.9113283,24.822397 19.7806063,24.5751284 20.5868584,24.212621 L20.4839746,24.034421 C20.2906749,23.6996162 20.4053875,23.2715029 20.7401924,23.0782032 L21.2598076,22.7782032 C21.5946125,22.5849036 22.0227257,22.6996162 22.2160254,23.034421 L22.3083692,23.1943652 C23.0205991,22.6580962 23.6506416,22.0185197 24.1762323,21.2979002 L24.034421,21.2160254 C23.6996162,21.0227257 23.5849036,20.5946125 23.7782032,20.2598076 L24.0782032,19.7401924 C24.2715029,19.4053875 24.6996162,19.2906749 25.034421,19.4839746 L25.1680075,19.5611008 C25.5131716,18.7607867 25.7455359,17.90041 25.8450712,17 L24.7,17 C24.3134007,17 24,16.6865993 24,16.3 L24,15.7 C24,15.3134007 24.3134007,15 24.7,15 L25.8450712,15 C25.7455359,14.09959 25.5131716,13.2392133 25.1680075,12.4388992 L25.034421,12.5160254 C24.6996162,12.7093251 24.2715029,12.5946125 24.0782032,12.2598076 L23.7782032,11.7401924 C23.5849036,11.4053875 23.6996162,10.9772743 24.034421,10.7839746 L24.1762323,10.7020998 C23.6506416,9.98148031 23.0205991,9.34190384 22.3083692,8.80563481 L22.2160254,8.96557899 C22.0227257,9.30038382 21.5946125,9.41509643 21.2598076,9.22179677 L20.7401924,8.92179677 C20.4053875,8.72849711 20.2906749,8.30038382 20.4839746,7.96557899 L20.5868584,7.78737904 C19.7806063,7.4248716 18.9113283,7.17760295 18,7.0665485 L18,8.3 C18,8.68659932 17.6865993,9 17.3,9 L16.7,9 C16.3134007,9 16,8.68659932 16,8.3 L16,7.04443772 C15.0760034,7.13617769 14.1931173,7.36766485 13.3727764,7.71746445 L13.5160254,7.96557899 C13.7093251,8.30038382 13.5946125,8.72849711 13.2598076,8.92179677 L12.7401924,9.22179677 C12.4053875,9.41509643 11.9772743,9.30038382 11.7839746,8.96557899 L11.6318787,8.7021411 C10.8891296,9.23924809 10.231625,9.88696181 9.68351959,10.6211275 L9.96557899,10.7839746 C10.3003838,10.9772743 10.4150964,11.4053875 10.2217968,11.7401924 L9.92179677,12.2598076 C9.72849711,12.5946125 9.30038382,12.7093251 8.96557899,12.5160254 L8.67235806,12.3467342 Z M17.8559254,14.1503631 L19.910935,13.2544282 C20.0375007,13.1992486 20.1848343,13.2571185 20.2400139,13.3836841 C20.2677962,13.4474084 20.2677905,13.5198232 20.2399982,13.5835431 L19.3458145,15.6336565 C19.4455354,15.9035779 19.5,16.1954285 19.5,16.5 C19.5,17.8807119 18.3807119,19 17,19 C15.6192881,19 14.5,17.8807119 14.5,16.5 C14.5,15.1192881 15.6192881,14 17,14 C17.3006571,14 17.5889181,14.0530736 17.8559254,14.1503631 Z"
            ]
            [] ] ]


let svgIconUndefined : msg Html.html =
  Svg.svg
    [ Svg.Attributes.viewBox "0 0 32 32"
    ; Svg.Attributes.width "100%"
    ; Svg.Attributes.height "100%"
    ; Svg.Attributes.preserveAspectRatio "none" ]
    [ Svg.g
        []
        [ Svg.path
            [ Svg.Attributes.d
                "M12.8557965,17.1810594 C12.8680346,16.7856932 12.9087691,16.4776734 12.978,16.257 C13.1593342,15.6789971 13.4143317,15.1633356 13.743,14.71 C14.0716683,14.2566644 14.4456646,13.8430019 14.865,13.469 C15.2843354,13.0949981 15.6809981,12.7380017 16.055,12.398 C16.4290019,12.0579983 16.7406654,11.7293349 16.99,11.412 C17.2393346,11.0946651 17.364,10.7546685 17.364,10.392 C17.364,9.77999694 17.1543354,9.34366797 16.735,9.083 C16.3156646,8.82233203 15.8453359,8.692 15.324,8.692 C14.6666634,8.692 14.105669,8.82799864 13.641,9.1 C13.445235,9.21459415 13.180825,9.39255316 12.8477699,9.63387702 L12.8477778,9.633888 C12.4634082,9.91239315 11.9362136,9.88232304 11.5860045,9.56191904 L9.80925419,7.93643788 C9.40175856,7.56362273 9.37364447,6.93105585 9.74645962,6.52356022 C9.77181855,6.49584233 9.79871922,6.46957543 9.82703371,6.44488442 C10.6762904,5.70430948 11.3922792,5.18401468 11.975,4.884 C13.1196724,4.29466372 14.3719932,4 15.732,4 C16.7066715,4 17.6359956,4.10766559 18.52,4.323 C19.4040044,4.53833441 20.1689968,4.87833101 20.815,5.343 C21.4610032,5.80766899 21.9766647,6.39699643 22.362,7.111 C22.7473353,7.82500357 22.94,8.66932846 22.94,9.644 C22.94,10.3920037 22.809668,11.0323307 22.549,11.565 C22.288332,12.0976693 21.9653353,12.5793312 21.58,13.01 C21.1946647,13.4406688 20.7810022,13.8373315 20.339,14.2 C19.8969978,14.5626685 19.4890019,14.9366647 19.115,15.322 C18.7409981,15.7073353 18.4406678,16.132331 18.214,16.597 C18.1291119,16.7710206 18.066875,17.0171662 18.0272893,17.3354366 L18.0272793,17.3354354 C17.9650042,17.8361292 17.5394873,18.212 17.0349356,18.212 L13.8553677,18.2120499 C13.3030554,18.2120499 12.8553178,17.7643123 12.8553178,17.212 C12.8553178,17.2016846 12.8554774,17.1913699 12.8557965,17.1810594 Z M11.788,24.822 C11.788,23.7339946 12.1393298,22.8443368 12.842,22.153 C13.5446702,21.4616632 14.4399946,21.116 15.528,21.116 C16.6160054,21.116 17.5113298,21.4616632 18.214,22.153 C18.9166702,22.8443368 19.268,23.7339946 19.268,24.822 C19.268,25.9100054 18.9166702,26.8109964 18.214,27.525 C17.5113298,28.2390036 16.6160054,28.596 15.528,28.596 C14.4399946,28.596 13.5446702,28.2390036 12.842,27.525 C12.1393298,26.8109964 11.788,25.9100054 11.788,24.822 Z"
            ]
            [] ] ]


>>>>>>> seperate style files and show icons on sidebar collapse
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
