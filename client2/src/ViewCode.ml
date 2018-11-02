open Tea
open! Porting
open Prelude
open Types
module B = Blank
module Attrs = Html.Attributes
module RT = Runtime
module SA = Svg.Attributes
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

let viewNFieldName (vs : viewState) (config : htmlConfig list) (f : string) :
    msg Html.html =
  text vs config f

let viewFieldName (vs : viewState) (c : htmlConfig list) (f : string blankOr) :
    msg Html.html =
  let configs = c @ [ViewBlankOr.ClickSelectAs (B.toID f)] @ ViewBlankOr.withFeatureFlag vs f in
  ViewBlankOr.viewBlankOr viewNFieldName Field vs configs f

let viewNVarBind (vs : viewState) (config : htmlConfig list) (f : string) :
    msg Html.html =
  text vs config f

let viewVarBind (vs : viewState) (c : htmlConfig list) (v : string blankOr) :
    msg Html.html =
  let configs = idConfigs @ c in
  ViewBlankOr.viewBlankOr viewNVarBind VarBind vs configs v

let viewKey (vs : viewState) (c : htmlConfig list) (k : string blankOr) :
    msg Html.html =
  let configs = idConfigs @ c in
  ViewBlankOr.viewBlankOr viewNVarBind Key vs configs k

let viewRopArrow (vs : viewState) : msg Html.html =
  let line =
    Svg.path
      [ SA.stroke "red"
      ; SA.strokeWidth "1.5px"
      ; SA.d "M 0,0 z"
      ; Vdom.attribute "" "opacity" "0.3"
      ; SA.markerEnd "url(#arrow)" ]
      []
  in
  let head =
    Svg.defs []
      [ Svg.marker
          [ SA.id "arrow"
          ; SA.markerWidth "10"
          ; SA.markerHeight "10"
          ; SA.refX "0"
          ; SA.refY "3"
          ; SA.orient "auto"
          ; SA.markerUnits "strokeWidth" ]
          [Svg.path [SA.d "M0,0 L0,6 L9,3 z"; SA.fill "#f00"] []] ]
  in
  let svg =
    Svg.svg
      [ Html.styles
          [ ("position", "absolute")
          ; ("pointer-events", "none")
          ; ("margin-top", "-10px")
          ; ("fill", "none") ] ]
      [line; head]
  in
  Html.node "rop-arrow"
    [ Vdom.attribute "" "update" (Util.random () |> string_of_int)
    ; Vdom.attribute "" "tlid" (string_of_int (deTLID vs.tl.id)) ]
    [svg]

let isExecuting (vs : viewState) (id : id) : bool =
  List.member id vs.executingFunctions

type ('a, 'b, 'c, 'd) x = { class_: 'a ; event: 'b; title: 'c; icon: 'd }

let depthString (n : int) : string = "precedence-" ^ string_of_int n

let rec viewExpr (depth : int) (vs : viewState) (c : htmlConfig list) (e : expr) :
    msg Html.html =
  let width = ViewUtils.approxWidth e in
  let widthClass =
    [wc ("width-" ^ string_of_int width)]
    @ if width > 120 then [wc "too-wide"] else []
  in
  let configs =
    idConfigs @ c @ ViewBlankOr.withFeatureFlag vs e @ ViewBlankOr.withEditFn vs e @ widthClass
  in
  let id = B.toID e in
  ViewBlankOr.viewBlankOr (viewNExpr depth id) Expr vs configs e

and viewNExpr (d : int) (id : id) (vs : viewState) (config : htmlConfig list)
    (e : nExpr) : msg Html.html =
  let vExpr d_ = viewExpr d_ vs [] in
  let vExprTw d_ =
    let vs2 = {vs with tooWide= true} in
    viewExpr d_ vs2 []
  in
  let t = text vs in
  let n c = div vs (nested :: c) in
  let a c = text vs (atom :: c) in
  let kw = keyword vs in
  let all = idConfigs @ config in
  let cs = ViewBlankOr.ClickSelect in
  let mo = ViewBlankOr.Mouseover in
  let incD = d + 1 in
  match e with
  | Value v ->
      let cssClass =
        v |> Decoders.typeOfLiteralString |> toString |> String.toLower
      in
      let value =
        if Decoders.typeOfLiteralString v = TStr then Util.transformToStringEntry v
        else v
      in
      let tooWide = if vs.tooWide then [wc "short-strings"] else [] in
      a (wc cssClass :: wc "value" :: (all @ tooWide)) value
  | Variable name ->
      if List.member id vs.relatedBlankOrs then
        a (wc "variable" :: wc "related-change" :: all) vs.ac.value
      else a (wc "variable" :: all) name
  | Let (lhs, rhs, body) ->
      let bodyID = B.toID body in
      let showRHSInstead =
        B.isBlank body && idOf vs.cursorState = Some bodyID
      in
      let rhsConfig =
        if showRHSInstead then [wc "display-livevalue"] else []
      in
      let bodyViewState =
        if showRHSInstead then {vs with showLivevalue= false} else vs
      in
      n (wc "letexpr" :: all)
        [ kw [] "let"
        ; viewVarBind vs [wc "letvarname"] lhs
        ; a [wc "letbind"] "="
        ; n [wc "letrhs"; cs] [viewExpr d vs rhsConfig rhs]
        ; n [wc "letbody"] [viewExpr d bodyViewState [] body] ]
  | If (cond, ifbody, elsebody) ->
      n (wc "ifexpr" :: all)
        [ kw [] "if"
        ; n [wc "cond"] [vExpr d cond]
        ; n [wc "ifbody"] [vExpr 0 ifbody]
        ; kw [] "else"
        ; n [wc "elsebody"] [vExpr 0 elsebody] ]
  | FnCall (name, exprs, sendToRail) -> (
      let width = ViewUtils.approxNWidth e in
      let viewTooWideArg name_ d_ e_ =
        Html.div [Html.class' "arg-on-new-line"] [vExprTw d_ e_]
      in
      let ve name_ = if width > 120 then viewTooWideArg name_ else vExpr in
      let fnname parens =
        let withP name_ = if parens then "(" ^ name_ ^ ")" else name_ in
        match String.split "::" name with
        | [mod_; justname] ->
            let np = withP justname in
            n [wc "namegroup"; atom]
              [ t [wc "module"] mod_
              ; t [wc "moduleseparator"] "::"
              ; ViewUtils.viewFnName np ["fnname"] ]
        | _ ->
            let np = withP name in
            ViewUtils.viewFnName np ["atom fnname"]
      in
      let fn =
        vs.ac.functions
        |> List.find (fun f -> f.fnName = name)
        |> Option.withDefault
             { fnName= "fnLookupError"
             ; fnParameters= []
             ; fnDescription= "default, fn error"
             ; fnReturnTipe= TError
             ; fnPreviewExecutionSafe= true
             ; fnInfix= false
             ; fnDeprecated= false }
      in
      let previous =
        match vs.tl.data with
        | TLHandler h -> h.ast |> AST.threadPrevious id |> Option.toList
        | TLFunc f -> f.ufAST |> AST.threadPrevious id |> Option.toList
        | TLDB db ->
          match db.activeMigration with
            None -> []
          | Some am -> [am.rollforward; am.rollback]
                       |> List.filterMap (fun m -> AST.threadPrevious id m)
      in
      let _ = "comment" in
      let allExprs = previous @ exprs in
      let isComplete v =
        v
        |> ViewBlankOr.getLiveValue vs.currentResults.liveValues
        |> fun v_ ->
        match v_ with
        | None -> false
        | Some (DError _) -> false
        | Some DIncomplete -> false
        | Some _ -> true
      in
      let ropArrow =
        if sendToRail = NoRail then Html.div [] [] else viewRopArrow vs
      in
      let paramsComplete = List.all (isComplete << B.toID) allExprs in
      let resultHasValue = isComplete id in
      let buttonUnavailable = not paramsComplete in
      let showButton = not fn.fnPreviewExecutionSafe in
      let buttonNeeded = not resultHasValue in
      let showExecuting = isExecuting vs id in
      let exeIcon = "play" in
      let events =
        [ ViewUtils.eventNoPropagation
            ~key:("efb-" ^ (showTLID vs.tl.id) ^ "-" ^ (showID id) ^ "-" ^ name)
            "click"
            (fun _ ->
              ExecuteFunctionButton (vs.tl.id, id, name) )
        ; ViewUtils.nothingMouseEvent "mouseup"
        ; ViewUtils.nothingMouseEvent "mousedown"
        ; ViewUtils.nothingMouseEvent "dblclick" ]
      in
      let {class_; event; title; icon} =
        if buttonUnavailable then
          { class_= "execution-button-unavailable"
          ; event= []
          ; title= "Cannot run: some parameters are incomplete"
          ; icon= exeIcon }
        else if buttonNeeded then
          { class_= "execution-button-needed"
          ; event= events
          ; title= "Click to execute function"
          ; icon= exeIcon }
        else
          { class_= "execution-button-repeat"
          ; event= events
          ; title= "Click to execute function again"
          ; icon= "redo" }
      in
      let executingClass = if showExecuting then " is-executing" else "" in
      let button =
        if not showButton then []
        else
          [ Html.div
              ( [ Html.class' ("execution-button " ^ class_ ^ executingClass)
                ; Html.title title ]
              @ event )
              [fontAwesome icon] ]
      in
      let fnDiv parens =
        n [wc "op"; wc name] (fnname parens :: ropArrow :: button)
      in
      match (fn.fnInfix, exprs, fn.fnParameters) with
      | true, [first; second], [p1; p2] ->
          n
            (wc "fncall infix" :: wc (depthString d) :: all)
            [ n [wc "lhs"] [ve p1.paramName incD first]
            ; fnDiv false
            ; n [wc "rhs"] [ve p2.paramName incD second] ]
      | _ ->
          let args =
            List.map2
              (fun p e_ -> ve p.paramName incD e_)
              fn.fnParameters exprs
          in
          n
            (wc "fncall prefix" :: wc (depthString d) :: all)
            (fnDiv fn.fnInfix :: args) )
  | Lambda (vars, expr) ->
      let varname v = t [wc "lambdavarname"; atom] v in
      n (wc "lambdaexpr" :: all)
        [ n [wc "lambdabinding"] (List.map (viewVarBind vs [atom]) vars)
        ; a [wc "arrow"] "->"
        ; n [wc "lambdabody"] [vExpr 0 expr] ]
  | Thread exprs ->
      let pipe = a [wc "thread pipe"] "|>" in
      let texpr e_ =
        n [wc "threadmember"; ClickSelectAs (B.toID e_)] [pipe; vExpr 0 e_]
      in
      n (wc "threadexpr" :: mo :: config) (List.map texpr exprs)
  | FieldAccess (obj, field) ->
      n
        (wc "fieldaccessexpr" :: all)
        [ n [wc "fieldobject"] [vExpr 0 obj]
        ; a [wc "fieldaccessop operator"] "."
        ; viewFieldName vs [wc "fieldname"; atom] field ]
  | ListLiteral exprs ->
      let open_ = a [wc "openbracket"] "[" in
      let close = a [wc "closebracket"] "]" in
      let comma = a [wc "comma"] "," in
      let lexpr e_ =
        n [wc "listelem"; ClickSelectAs (B.toID e_)] [vExpr 0 e_]
      in
      let new_ = List.map lexpr exprs |> List.intersperse comma in
      n (wc "list" :: mo :: config) ([open_] @ new_ @ [close])
  | ObjectLiteral pairs ->
      let colon = a [wc "colon"] ":" in
      let open_ = a [wc "openbrace"] "{" in
      let close = a [wc "closebrace"] "}" in
      let pexpr (k, v) =
        n [wc "objectpair"] [viewKey vs [] k; colon; vExpr 0 v]
      in
      n (wc "object" :: mo :: config) ([open_] @ List.map pexpr pairs @ [close])
  | FeatureFlag (msg, cond, a_, b_) ->
      let exprLabel msg_ =
        Html.label [Html.class' "expr-label"] [Html.text msg_]
      in
      let isExpanded =
        let mv = IntDict.get (deID id) vs.featureFlags in
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
              (fun _ -> ToggleFeatureFlag (id, false))
          ]
          [fontAwesome "minus"]
      in
      let expandModal =
        Html.div
          [ Vdom.attribute "" "data-content" "Show ff details"
          ; ViewUtils.eventNoPropagation
              ~key:("tfft-" ^ showID id)
              "click"
              (fun _ -> ToggleFeatureFlag (id, true))
          ]
          [fontAwesome "flag"]
      in
      let titleBar =
        Html.div
          [Html.class' "row title-bar"]
          [ viewText FFMsg vs (wc "flag-name" :: idConfigs) msg
          ; Html.div [Html.class' "actions"]
              [(if isExpanded then hideModal else expandModal)] ]
      in
      let condValue =
        ViewBlankOr.getLiveValue vs.currentResults.liveValues (B.toID cond)
      in
      let condResult =
        condValue |> Option.map Runtime.isTrue |> Option.withDefault false
      in
      let blockCondition =
        Html.div
          [Html.class' "row condition"]
          [exprLabel "Condition (run Case B if...)"; vExpr 0 cond]
      in
      let exprBlock lbl act exp =
        Html.div [Html.class' "cond-expr"]
          [exprLabel lbl; act; div vs [wc "expr-block"] [vExpr 0 exp]]
      in
      let expressions =
        Html.div
          [Html.class' "row expressions"]
          [exprBlock "Case A" pickA a_; exprBlock "Case B" pickB b_]
      in
      div vs [wc "flagged shown"]
        [ viewExpr 0 {vs with showEntry= false} []
            (if condResult then b_ else a_)
        ; fontAwesome "flag"
        ; Html.div
            [ Html.class'
                ("feature-flag" ^ if isExpanded then " expand" else "") ]
            [titleBar; blockCondition; expressions] ]

let viewEventName (vs : viewState) (c : htmlConfig list) (v : string blankOr) :
    msg Html.html =
  let configs = idConfigs @ c in
  viewText EventName vs configs v

let viewEventSpace (vs : viewState) (c : htmlConfig list) (v : string blankOr)
    : msg Html.html =
  let configs = idConfigs @ c in
  viewText EventSpace vs configs v

let viewEventModifier (vs : viewState) (c : htmlConfig list)
    (v : string blankOr) : msg Html.html =
  let configs = idConfigs @ c in
  viewText EventModifier vs configs v

let viewHandler (vs : viewState) (h : handler) : msg Html.html list =
  let showRail = AST.usesRail h.ast in
  let ast =
    Html.div [Html.class' "ast"]
      [ Html.div
          [Html.classList [("rop-rail", showRail)]]
          [viewExpr 0 vs [] h.ast] ]
  in
  let externalLink =
    match (h.spec.modifier, h.spec.name) with
    | F (_, "GET"), F (_, name) ->
        [ Html.a
            [ Html.class' "external"
            ; Html.href
                ( "//"
                ^ Http.encodeUri vs.canvasName
                ^ "." ^ vs.userContentHost ^ name )
            ; Html.target "_blank" ]
            [fontAwesome "external-link-alt"] ]
    | _ -> []
  in
  let modifier =
    if SpecHeaders.visibleModifier h.spec then
      viewEventModifier vs [wc "modifier"] h.spec.modifier
    else Html.div [] []
  in
  let lock =
    Html.div
      [ Html.classList
          [("handler-lock", true); ("is-locked", vs.handlerLocked)]
      ; ViewUtils.eventNoPropagation
          ~key:("lh-" ^ (showTLID vs.tlid) ^ "-" ^ (string_of_bool vs.handlerLocked))
          "click"
          (fun _ ->
            LockHandler (vs.tlid, not vs.handlerLocked) ) ]
      [fontAwesome (if vs.handlerLocked then "lock" else "lock-open")]
  in
  let header =
    Html.div
      [Html.class' "spec-header"]
      [ viewEventName vs [wc "name"] h.spec.name
      ; Html.div [] externalLink
      ; viewEventSpace vs [wc "module"] h.spec.module_
      ; modifier
      ; lock ]
  in
  [header; ast]
