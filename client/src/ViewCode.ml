open Tc
open Prelude
open Types

(* Tea *)
module Svg = Tea.Svg

(* Dark *)
module B = Blank

type viewState = ViewUtils.viewState

let isLocked = ViewUtils.isHandlerLocked

let isExpanded = ViewUtils.isHandlerExpanded

let inUnit = ViewUtils.intAsUnit

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

let withROP = ViewBlankOr.withROP

let enterable = ViewBlankOr.Enterable

let viewFieldName (vs : viewState) (c : htmlConfig list) (f : string blankOr) :
    msg Html.html =
  let configs =
    c
    @ [enterable]
    @ [ViewBlankOr.ClickSelectAs (B.toID f)]
    @ [ViewBlankOr.MouseoverAs (B.toID f)]
    @ ViewBlankOr.withFeatureFlag vs f
  in
  ViewBlankOr.viewBlankOr text Field vs configs f


let viewVarBind (vs : viewState) (c : htmlConfig list) (v : string blankOr) :
    msg Html.html =
  let configs = (enterable :: idConfigs) @ c in
  ViewBlankOr.viewBlankOr text VarBind vs configs v


let viewConstructorName
    (vs : viewState) (c : htmlConfig list) (v : string blankOr) : msg Html.html
    =
  let configs = idConfigs @ c in
  ViewBlankOr.viewBlankOr text VarBind vs configs v


let viewKey (vs : viewState) (c : htmlConfig list) (k : string blankOr) :
    msg Html.html =
  let configs = (enterable :: idConfigs) @ c in
  ViewBlankOr.viewBlankOr text Key vs configs k


let rec viewNPattern
    (vs : viewState) (config : htmlConfig list) (np : nPattern) : msg Html.html
    =
  match np with
  | PLiteral l ->
      text vs (enterable :: config) l
  | PVariable v ->
      text vs (enterable :: config) v
  | PConstructor (name, params) ->
      let ps = List.map ~f:(viewPattern vs []) params in
      div
        vs
        []
        ( text vs (atom :: enterable :: wc "constructor-pattern" :: config) name
        :: ps )


and viewPattern (vs : viewState) (c : htmlConfig list) (p : pattern) =
  let configs = idConfigs @ c in
  ViewBlankOr.viewBlankOr viewNPattern Pattern vs configs p


let isExecuting (vs : viewState) (id : id) : bool =
  List.member ~value:id vs.executingFunctions


type ('a, 'b, 'c, 'd) x =
  { class_ : 'a
  ; event : 'b
  ; title : 'c
  ; icon : 'd }

let depthString (n : int) : string = "precedence-" ^ string_of_int n

let rec viewExpr
    (depth : int) (vs : viewState) (c : htmlConfig list) (e : expr) :
    msg Html.html =
  let width = ViewUtils.approxWidth e in
  let widthClass =
    [wc ("width-" ^ string_of_int width)]
    @ if width > 120 then [wc "too-wide"] else []
  in
  let configs =
    idConfigs
    @ c
    @ ViewBlankOr.withFeatureFlag vs e
    @ ViewBlankOr.withEditFn vs e
    @ widthClass
  in
  let id = B.toID e in
  ViewBlankOr.viewBlankOr (viewNExpr depth id) Expr vs configs e


and viewNExpr
    (d : int) (id : id) (vs : viewState) (config : htmlConfig list) (e : nExpr)
    : msg Html.html =
  let vExpr d_ = viewExpr d_ vs [] in
  let n c = div vs (nested :: c) in
  let a c = text vs (atom :: c) in
  let kw = keyword vs in
  let all = idConfigs @ config in
  let cs = ViewBlankOr.ClickSelect in
  let mo = ViewBlankOr.Mouseover in
  let ent = ViewBlankOr.Enterable in
  let incD = d + 1 in
  match e with
  | Value v ->
      let cssClass =
        v |> Decoders.typeOfLiteralString |> show_tipe |> String.toLower
      in
      let value =
        (* TODO: remove *)
        if Decoders.typeOfLiteralString v = TStr
        then Util.transformToStringEntry v
        else v
      in
      let tooWide = if vs.tooWide then [wc "short-strings"] else [] in
      let c = ent :: wc cssClass :: wc "value" :: atom :: (all @ tooWide) in
      div
        vs
        c
        [ Html.div [Html.class' "quote quote-start"] []
        ; Html.text value
        ; Html.div [Html.class' "quote quote-end"] [] ]
  | Variable name ->
      if List.member ~value:id vs.relatedBlankOrs
      then a (ent :: wc "variable" :: wc "related-change" :: all) vs.ac.value
      else a (ent :: wc "variable" :: all) name
  | Let (lhs, rhs, body) ->
      let bodyID = B.toID body in
      let showRHSInstead =
        B.isBlank body && idOf vs.cursorState = Some bodyID
      in
      let rhsConfig =
        if showRHSInstead then [wc "display-livevalue"] else []
      in
      let bodyViewState =
        if showRHSInstead then {vs with showLivevalue = false} else vs
      in
      n
        (wc "letexpr" :: all)
        [ kw [] "let"
        ; viewVarBind vs [wc "letvarname"] lhs
        ; a [wc "letbind"] "="
        ; n [wc "letrhs"; cs] [viewExpr d vs rhsConfig rhs]
        ; n [wc "letbody"] [viewExpr d bodyViewState [] body] ]
  | If (cond, ifbody, elsebody) ->
      n
        (wc "ifexpr" :: all)
        [ kw [] "if"
        ; n [wc "cond"] [vExpr d cond]
        ; n [wc "ifbody"] [vExpr 0 ifbody]
        ; kw [] "else"
        ; n [wc "elsebody"] [vExpr 0 elsebody] ]
  | FnCall (Blank _, _, _) ->
      Debug.crash "fn with blank"
  | FnCall ((F (_, name) as nameBo), exprs, sendToRail) ->
      let width = ViewUtils.approxNWidth e in
      let viewTooWideArg p d_ e_ =
        let c =
          [wc "arg-on-new-line"; ViewBlankOr.WithParamName p.paramName]
        in
        viewExpr d_ {vs with tooWide = true} c e_
      in
      let ve p = if width > 120 then viewTooWideArg p else vExpr in
      let fn =
        vs.ac.functions
        |> List.find ~f:(fun f -> f.fnName = name)
        |> Option.withDefault
             ~default:
               { fnName = "fnLookupError"
               ; fnParameters = []
               ; fnDescription = "default, fn error"
               ; fnReturnTipe = TError
               ; fnPreviewExecutionSafe = true
               ; fnInfix = false
               ; fnDeprecated = false }
      in
      let previous =
        match vs.tl.data with
        | TLHandler h ->
            h.ast |> AST.threadPrevious id |> Option.toList
        | TLFunc f ->
            f.ufAST |> AST.threadPrevious id |> Option.toList
        | TLTipe _ ->
            []
        | TLDB db ->
          ( match db.activeMigration with
          | None ->
              []
          | Some am ->
              [am.rollforward; am.rollback]
              |> List.filterMap ~f:(fun m -> AST.threadPrevious id m) )
      in
      (* buttons *)
      let allExprs = previous @ exprs in
      let isComplete v =
        v
        |> ViewBlankOr.getLiveValue vs.currentResults.liveValues
        |> fun v_ ->
        match v_ with
        | None ->
            false
        | Some (DError _) ->
            false
        | Some DIncomplete ->
            false
        | Some _ ->
            true
      in
      let paramsComplete = List.all ~f:(isComplete << B.toID) allExprs in
      let resultHasValue = isComplete id in
      let buttonUnavailable = not paramsComplete in
      let showButton = not fn.fnPreviewExecutionSafe in
      let buttonNeeded = not resultHasValue in
      let showExecuting = isExecuting vs id in
      let exeIcon = "play" in
      let events =
        [ ViewUtils.eventNoPropagation
            ~key:("efb-" ^ showTLID vs.tl.id ^ "-" ^ showID id ^ "-" ^ name)
            "click"
            (fun _ -> ExecuteFunctionButton (vs.tl.id, id, name))
        ; ViewUtils.nothingMouseEvent "mouseup"
        ; ViewUtils.nothingMouseEvent "mousedown"
        ; ViewUtils.nothingMouseEvent "dblclick" ]
      in
      let {class_; event; title; icon} =
        if name = "Password::check" || name = "Password::hash"
        then
          { class_ = "execution-button-unsafe"
          ; event = []
          ; title = "Cannot run interactively for security reasons."
          ; icon = "times" }
        else if buttonUnavailable
        then
          { class_ = "execution-button-unavailable"
          ; event = []
          ; title = "Cannot run: some parameters are incomplete"
          ; icon = exeIcon }
        else if buttonNeeded
        then
          { class_ = "execution-button-needed"
          ; event = events
          ; title = "Click to execute function"
          ; icon = exeIcon }
        else
          { class_ = "execution-button-repeat"
          ; event = events
          ; title = "Click to execute function again"
          ; icon = "redo" }
      in
      let executingClass = if showExecuting then " is-executing" else "" in
      let button =
        if not showButton
        then []
        else
          [ Html.div
              ( [ Html.class' ("execution-button " ^ class_ ^ executingClass)
                ; Html.title title ]
              @ event )
              [fontAwesome icon] ]
      in
      let errorIcon =
        if sendToRail = NoRail
        then []
        else
          [ Html.div
              [Html.class' "error-indicator"]
              [Html.div [Html.class' "error-icon"] []] ]
      in
      let fnname parens =
        ViewBlankOr.viewBlankOr
          (fun vs c text -> div vs c [ViewUtils.viewFnName parens text])
          FnCallName
          vs
          []
          nameBo
      in
      let fnDiv parens = n [wc "op"; wc name] (fnname parens :: button) in
      let configs = withROP sendToRail @ all in
      ( match (fn.fnInfix, exprs, fn.fnParameters) with
      | true, [first; second], [p1; p2] ->
          n
            (wc "fncall infix" :: wc (depthString d) :: configs)
            [ n [wc "lhs"] [ve p1 incD first]
            ; fnDiv false
            ; n [wc "rhs"] [ve p2 incD second] ]
      | _ ->
          let args =
            List.map2 ~f:(fun p e_ -> ve p incD e_) fn.fnParameters exprs
          in
          n
            (wc "fncall prefix" :: wc (depthString d) :: configs)
            ((fnDiv fn.fnInfix :: args) @ errorIcon) )
  | Lambda (vars, expr) ->
      n
        (wc "lambdaexpr" :: all)
        [ n [wc "lambdabinding"] (List.map ~f:(viewVarBind vs [atom]) vars)
        ; a [wc "arrow"] "->"
        ; n [wc "lambdabody"] [vExpr 0 expr] ]
  | Thread exprs ->
      let pipe = a [wc "thread pipe"] "|>" in
      let texpr e_ =
        n [wc "threadmember"; ClickSelectAs (B.toID e_)] [pipe; vExpr 0 e_]
      in
      n (wc "threadexpr" :: mo :: config) (List.map ~f:texpr exprs)
  | FieldAccess (obj, field) ->
      n
        (wc "fieldaccessexpr" :: all)
        [ n [wc "fieldobject"] [vExpr 0 obj]
        ; a [wc "fieldaccessop operator"] "."
        ; viewFieldName vs [wc "fieldname"; atom] field ]
  | Constructor (name, exprs) ->
      let exprs = List.map ~f:(fun e -> n [] [viewExpr d vs [] e]) exprs in
      n
        (wc "constructor" :: all)
        (viewConstructorName vs [wc "constructorname"] name :: exprs)
  | ListLiteral exprs ->
      let open_ = a [wc "openbracket"] "[" in
      let close = a [wc "closebracket"] "]" in
      let comma = a [wc "comma"] "," in
      let lexpr e_ =
        n [wc "listelem"; ClickSelectAs (B.toID e_)] [vExpr 0 e_]
      in
      let new_ = List.map ~f:lexpr exprs |> List.intersperse comma in
      n (wc "list" :: mo :: config) ([open_] @ new_ @ [close])
  | ObjectLiteral pairs ->
      let colon = a [wc "colon"] ":" in
      let open_ = a [wc "openbrace"] "{" in
      let close = a [wc "closebrace"] "}" in
      let pexpr (k, v) =
        n [wc "objectpair"] [viewKey vs [wc "objectkey"] k; colon; vExpr 0 v]
      in
      n
        (wc "object" :: mo :: config)
        ([open_] @ List.map ~f:pexpr pairs @ [close])
  | Match (matchExpr, cases) ->
      let separator = a [wc "separator"] "->" in
      let vCase (k, v) =
        n [wc "matchcase"] [viewPattern vs [] k; separator; vExpr 0 v]
      in
      n
        (wc "matchexpr" :: all)
        ( [kw [] "match"; n [wc "mexpr"] [vExpr d matchExpr]]
        @ List.map ~f:vCase cases )
  | FeatureFlag (msg, cond, a_, b_) ->
      let exprLabel msg_ =
        Html.label [Html.class' "expr-label"] [Html.text msg_]
      in
      let isExpanded =
        let mv = StrDict.get ~key:(deID id) vs.featureFlags in
        match mv with Some b -> b | None -> true
      in
      let pickA =
        Html.div
          [ Html.class' "icon pick-a parameter-btn info"
          ; Vdom.attribute "" "data-content" "Use Case A"
          ; Html.title "delete Feature Flag & use Case A"
          ; ViewUtils.eventNoPropagation
              ~key:("effa-" ^ showID id)
              "click"
              (fun _ -> EndFeatureFlag (id, PickA)) ]
          [fontAwesome "check"]
      in
      let pickB =
        Html.div
          [ Html.class' "icon pick-b parameter-btn info"
          ; Vdom.attribute "" "data-content" "Use Case B"
          ; Html.title "delete Feature Flag & use Case B"
          ; ViewUtils.eventNoPropagation
              ~key:("effb-" ^ showID id)
              "click"
              (fun _ -> EndFeatureFlag (id, PickB)) ]
          [fontAwesome "check"]
      in
      let hideModal =
        Html.div
          [ Vdom.attribute "" "data-content" "Hide ff details"
          ; ViewUtils.eventNoPropagation
              ~key:("tfff-" ^ showID id)
              "click"
              (fun _ -> ToggleFeatureFlag (id, false)) ]
          [fontAwesome "minus"]
      in
      let expandModal =
        Html.div
          [ Vdom.attribute "" "data-content" "Show ff details"
          ; ViewUtils.eventNoPropagation
              ~key:("tfft-" ^ showID id)
              "click"
              (fun _ -> ToggleFeatureFlag (id, true)) ]
          [fontAwesome "flag"]
      in
      let titleBar =
        Html.div
          [Html.class' "row title-bar"]
          [ viewText FFMsg vs (wc "flag-name" :: idConfigs) msg
          ; Html.div
              [Html.class' "actions"]
              [(if isExpanded then hideModal else expandModal)] ]
      in
      let condValue =
        ViewBlankOr.getLiveValue vs.currentResults.liveValues (B.toID cond)
      in
      let condResult =
        condValue
        |> Option.map ~f:Runtime.isTrue
        |> Option.withDefault ~default:false
      in
      let blockCondition =
        Html.div
          [Html.class' "row condition"]
          [exprLabel "Condition (run Case B if...)"; vExpr 0 cond]
      in
      let exprBlock lbl act exp =
        Html.div
          [Html.class' "cond-expr"]
          [exprLabel lbl; act; div vs [wc "expr-block"] [vExpr 0 exp]]
      in
      let expressions =
        Html.div
          [Html.class' "row expressions"]
          [exprBlock "Case A" pickA a_; exprBlock "Case B" pickB b_]
      in
      div
        vs
        [wc "flagged shown"]
        [ viewExpr
            0
            {vs with showEntry = false}
            []
            (if condResult then b_ else a_)
        ; fontAwesome "flag"
        ; Html.div
            [ Html.class'
                ("feature-flag" ^ if isExpanded then " expand" else "") ]
            [titleBar; blockCondition; expressions] ]


let externalLink
    (spec : handlerSpec) (canvasName : string) (contentHost : string) =
  match (spec.modifier, spec.name) with
  | F (_, "GET"), F (_, name) ->
      [ Html.a
          [ Html.class' "external"
          ; Html.href
              ("//" ^ Tea.Http.encodeUri canvasName ^ "." ^ contentHost ^ name)
          ; Html.target "_blank" ]
          [fontAwesome "external-link-alt"] ]
  | _ ->
      []


let viewEventSpec (vs : viewState) (spec : handlerSpec) : msg Html.html list =
  let viewEventName =
    let configs = (enterable :: idConfigs) @ [wc "name"] in
    viewText EventName vs configs spec.name
  and viewEventSpace =
    let configs = (enterable :: idConfigs) @ [wc "module"] in
    viewText EventSpace vs configs spec.module_
  and viewEventModifier =
    let configs = (enterable :: idConfigs) @ [wc "modifier"] in
    if SpecHeaders.visibleModifier spec
    then viewText EventModifier vs configs spec.modifier
    else Html.div [] []
  and viewEventActions =
    let testGet = externalLink spec vs.canvasName vs.userContentHost in
    let lock =
      let isLocked = isLocked vs in
      ViewUtils.toggleIconButton
        ~tlid:vs.tlid
        ~name:"handler-lock"
        ~activeIcon:"lock"
        ~inactiveIcon:"lock-open"
        ~msg:(fun _ -> LockHandler (vs.tlid, not isLocked))
        ~active:isLocked
    in
    let expandCollapse =
      let isExpand = isExpanded vs in
      let expandFun _ =
        let state = ViewUtils.getHandlerState vs in
        match state with
        | HandlerExpanding ->
            IgnoreMsg
        | HandlerExpanded ->
            UpdateHandlerState (vs.tlid, HandlerPrepCollapse)
        | HandlerPrepCollapse ->
            IgnoreMsg
        | HandlerCollapsing ->
            IgnoreMsg
        | HandlerCollapsed ->
            UpdateHandlerState (vs.tlid, HandlerExpanding)
      in
      ViewUtils.toggleIconButton
        ~tlid:vs.tlid
        ~name:"handler-expand"
        ~activeIcon:"caret-up"
        ~inactiveIcon:"caret-down"
        ~msg:expandFun
        ~active:isExpand
    in
    Html.div [Html.class' "actions"] (testGet @ [lock; expandCollapse])
  in
  [viewEventName; viewEventSpace; viewEventModifier; viewEventActions]


let handlerAttrs (tlid : tlid) (state : handlerState) : msg Vdom.property list
    =
  let sid = showTLID tlid in
  let codeHeight id =
    let e =
      Native.Ext.querySelector (".toplevel.tl-" ^ id ^ " .handler-body")
    in
    match e with Some el -> Native.Ext.scrollHeight el | None -> 0
  in
  match state with
  | HandlerExpanding ->
      let h = inUnit (codeHeight sid) "px" in
      [ Html.class' "handler-body expand"
      ; Html.style "height" h
      ; ViewUtils.onTransitionEnd ~key:("hdlexp-" ^ sid) ~listener:(fun prop ->
            if prop = "opacity"
            then UpdateHandlerState (tlid, HandlerExpanded)
            else IgnoreMsg ) ]
  | HandlerExpanded ->
      [ Html.class' "handler-body expand"
      ; Html.style "height" "auto"
      ; Vdom.noProp ]
  | HandlerPrepCollapse ->
      let h = inUnit (codeHeight sid) "px" in
      [ Html.class' "handler-body"
      ; Html.style "height" h
      ; ViewUtils.onTransitionEnd
          ~key:("hdlpcol-" ^ sid)
          ~listener:(fun prop ->
            if prop = "opacity"
            then UpdateHandlerState (tlid, HandlerCollapsing)
            else IgnoreMsg ) ]
  | HandlerCollapsing ->
      [ Html.class' "handler-body"
      ; Html.style "height" "0"
      ; ViewUtils.onTransitionEnd
          ~key:("hdlcolng-" ^ sid)
          ~listener:(fun prop ->
            if prop = "height"
            then UpdateHandlerState (tlid, HandlerCollapsed)
            else IgnoreMsg ) ]
  | HandlerCollapsed ->
      [Html.class' "handler-body"; Html.style "height" "0"; Vdom.noProp]


let viewHandler (vs : viewState) (h : handler) : msg Html.html list =
  let showRail = AST.usesRail h.ast in
  let attrs = handlerAttrs vs.tlid (ViewUtils.getHandlerState vs) in
  let ast =
    Html.div
      attrs
      [ Html.div [Html.class' "ast"] [viewExpr 0 vs [] h.ast]
      ; Html.div [Html.classList [("rop-rail", true); ("active", showRail)]] []
      ]
  in
  let header =
    Html.div [Html.class' "spec-header"] (viewEventSpec vs h.spec)
  in
  [header; ast]
