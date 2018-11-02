module ViewCode exposing (viewExpr, viewHandler)

-- builtin
import Dict

-- lib
import Http
import Html
import Html.Attributes as Attrs
import List.Extra as LE
import Maybe.Extra as ME
import Svg
import VirtualDom
import Svg.Attributes as SA

-- dark
import DontPort
import Types exposing (..)
import Prelude exposing (..)
import Runtime as RT
import JSON
import AST
import Blank as B
import ViewBlankOr exposing (..)
import ViewUtils exposing (..)
import Runtime
import SpecHeaders
import Util exposing (transformToStringEntry)


viewFieldName : ViewState -> List HtmlConfig -> BlankOr String -> Html.Html Msg
viewFieldName vs c f =
  let configs = c ++ [ClickSelectAs (B.toID f)] ++ withFeatureFlag vs f in
  viewBlankOr viewNFieldName Field vs configs f

viewVarBind : ViewState -> List HtmlConfig -> BlankOr String -> Html.Html Msg
viewVarBind vs c v =
  let configs = idConfigs ++ c in
  viewBlankOr viewNVarBind VarBind vs configs v

viewKey : ViewState -> List HtmlConfig -> BlankOr String -> Html.Html Msg
viewKey vs c k =
  let configs = idConfigs ++ c in
  viewBlankOr viewNVarBind Key vs configs k

viewExpr : Int -> ViewState -> List HtmlConfig -> Expr -> Html.Html Msg
viewExpr depth vs c e =
  let width = approxWidth e
      widthClass = [wc ("width-" ++ DontPort.fromInt width)]
                   ++ (if width > 120 then [wc "too-wide"] else [])
      configs = idConfigs
                ++ c
                ++ withFeatureFlag vs e
                ++ withEditFn vs e
                ++ widthClass
      id = B.toID e
  in
  viewBlankOr (viewNExpr depth id) Expr vs configs e

viewEventName : ViewState -> List HtmlConfig -> BlankOr String -> Html.Html Msg
viewEventName vs c v =
  let configs = idConfigs ++ c in
  viewText EventName vs configs v

viewEventSpace : ViewState -> List HtmlConfig -> BlankOr String -> Html.Html Msg
viewEventSpace vs c v =
  let configs = idConfigs ++ c in
  viewText EventSpace vs configs v

viewEventModifier : ViewState -> List HtmlConfig -> BlankOr String -> Html.Html Msg
viewEventModifier vs c v =
  let configs = idConfigs ++ c in
  viewText EventModifier vs configs v


viewNVarBind : ViewState -> List HtmlConfig -> String -> Html.Html Msg
viewNVarBind vs config f =
  text vs config f

viewNFieldName : ViewState -> List HtmlConfig -> String -> Html.Html Msg
viewNFieldName vs config f =
  text vs config f

depthString : Int -> String
depthString n = "precedence-" ++ (DontPort.fromInt n)

viewRopArrow : ViewState -> Html.Html Msg
viewRopArrow vs =
  let line =
        Svg.path
          [ SA.stroke "red"
          , SA.strokeWidth "1.5px"
          , SA.d "M 0,0 z"
          , VirtualDom.attribute "opacity" "0.3"
          , SA.markerEnd "url(#arrow)"
          ]
          []
      head = Svg.defs
               []
               [ Svg.marker
                 [ SA.id "arrow"
                 , SA.markerWidth "10"
                 , SA.markerHeight "10"
                 , SA.refX "0"
                 , SA.refY "3"
                 , SA.orient "auto"
                 , SA.markerUnits "strokeWidth"
                 ]
                 [ Svg.path [ SA.d "M0,0 L0,6 L9,3 z"
                            , SA.fill "#f00"
                            ]
                            []
                 ]
               ]
      svg =
        Svg.svg
          [ Attrs.style [ ("position", "absolute")
                        , ("pointer-events", "none") -- don't eat clicks
                        , ("margin-top", "-10px")
                        , ("fill", "none")
                        ]
          ]
          [line, head]
  in
  Html.node
    "rop-arrow"
    -- Force the rop-webcomponent to update to fix the size
    [ VirtualDom.attribute "update" (Util.random () |> DontPort.fromInt)
    , VirtualDom.attribute "tlid" (DontPort.fromInt (deTLID vs.tl.id))]
    [svg]


viewNExpr : Int -> ID -> ViewState -> List HtmlConfig -> NExpr -> Html.Html Msg
viewNExpr d id vs config e =
  let vExpr d_ = viewExpr d_ vs []
      vExprTw d_ =
        let vs2 = { vs | tooWide = True } in
        viewExpr d_ vs2 []
      t = text vs
      n c = div vs (nested :: c)
      a c = text vs (atom :: c)
      kw = keyword vs
      all = idConfigs ++ config
      cs = ClickSelect
      mo = Mouseover
      incD = d + 1

  in
  case e of
    Value v ->
      let cssClass = v |> JSON.typeOfLiteralString |> toString |> String.toLower
          value =
            -- TODO: remove
            if JSON.typeOfLiteralString v == TStr
            then transformToStringEntry v
            else v
          tooWide = if vs.tooWide
                    then [wc "short-strings"]
                    else []
      in
      a (wc cssClass :: wc "value" :: all ++ tooWide) value

    Variable name ->
      if List.member id vs.relatedBlankOrs
      then
        a (wc "variable" :: wc "related-change" :: all) (vs.ac.value)
      else
        a (wc "variable" :: all) name

    Let lhs rhs body ->
      let bodyID = B.toID body
          showRHSInstead = B.isBlank body
                           && (idOf vs.cursorState == Just bodyID)
          rhsConfig = if showRHSInstead
                      then [wc "display-livevalue"]
                      else []
          bodyViewState = if showRHSInstead
                          then { vs | showLivevalue = False }
                          else vs

      in
      n (wc "letexpr" :: all)
        [ kw [] "let"
        , viewVarBind vs [wc "letvarname"] lhs
        , a [wc "letbind"] "="
        , n [wc "letrhs", cs] [viewExpr d vs rhsConfig rhs]
        , n [wc "letbody"] [viewExpr d bodyViewState [] body]
        ]

    If cond ifbody elsebody ->
      n (wc "ifexpr" :: all)
      [ kw [] "if"
      , n [wc "cond"] [vExpr d cond]
      , n [wc "ifbody"] [vExpr 0 ifbody]
      , kw [] "else"
      , n [wc "elsebody"] [vExpr 0 elsebody]
      ]

    FnCall name exprs sendToRail ->
      let width = approxNWidth e
          viewTooWideArg name_ d_ e_ =
            Html.div
              [ Attrs.class "arg-on-new-line" ]
              [ vExprTw d_ e_ ]
          ve name_ = if width > 120
                    then viewTooWideArg name_
                    else vExpr
          fnname parens =
            let withP name_ = if parens then "(" ++ name_ ++ ")" else name_ in
            case String.split "::" name of
              [mod, justname] ->
                let np = withP justname in
                n [wc "namegroup", atom]
                [ t [wc "module"] mod
                , t [wc "moduleseparator"] "::"
                , viewFnName np ["fnname"]
                ]
              _ ->
                let np = withP name in
                viewFnName np ["atom fnname"]
          fn = vs.ac.functions
                    |> LE.find (\f -> f.fnName == name)
                    |> Maybe.withDefault
                      { fnName = "fnLookupError"
                      , fnParameters = []
                      , fnDescription = "default, fn error"
                      , fnReturnTipe = TError
                      , fnPreviewExecutionSafe = True
                      , fnInfix = False
                      , fnDeprecated = False
                      }

          previous =
            case vs.tl.data of
              TLHandler h ->
                h.ast
                |> AST.threadPrevious id
                |> ME.toList
              TLFunc f ->
                f.ufAST
                |> AST.threadPrevious id
                |> ME.toList
              TLDB db ->
                case db.activeMigration of
                  Nothing -> []
                  Just am -> [am.rollforward, am.rollback]
                  |> List.filterMap (\m -> AST.threadPrevious id m)

          -- buttons
          allExprs = previous ++ exprs
          isComplete v =
            v
            |> getLiveValue vs.currentResults.liveValues
            |> \v_ ->
                 case v_ of
                   Nothing -> False
                   Just (DError _) -> False
                   Just DIncomplete -> False
                   Just _ -> True

          ropArrow =
            if sendToRail == NoRail
            then Html.div [] []
            else viewRopArrow vs

          paramsComplete = List.all (isComplete << B.toID) allExprs
          resultHasValue = isComplete id

          buttonUnavailable = not paramsComplete
          showButton = not fn.fnPreviewExecutionSafe
          buttonNeeded = not resultHasValue
          showExecuting = isExecuting vs id
          exeIcon = "play"

          events = [ eventNoPropagation "click"
                     (\_ -> ExecuteFunctionButton vs.tl.id id name)
                   , nothingMouseEvent "mouseup"
                   , nothingMouseEvent "mousedown"
                   , nothingMouseEvent "dblclick"
                   ]
          {class, event, title, icon} =
            if buttonUnavailable
            then { class = "execution-button-unavailable"
                 , event = []
                 , title = "Cannot run: some parameters are incomplete"
                 , icon = exeIcon }
            else if buttonNeeded
            then { class = "execution-button-needed"
                 , event = events
                 , title = "Click to execute function"
                 , icon = exeIcon }
            else
              { class = "execution-button-repeat"
              , event = events
              , title = "Click to execute function again"
              , icon = "redo" }
          executingClass = if showExecuting then " is-executing" else ""
          button =
            if not showButton
            then []
            else
              [ Html.div
                ([ Attrs.class ("execution-button " ++ class ++ executingClass)
                 , Attrs.title title
                 ] ++ event)
                [fontAwesome icon]
              ]

          fnDiv parens =
            n
              [wc "op", wc name]
              (fnname parens :: ropArrow :: button)
      in
      case (fn.fnInfix, exprs, fn.fnParameters) of
        (True, [first, second], [p1, p2]) ->
          n (wc "fncall infix" :: wc (depthString d) :: all)
          [ n [wc "lhs"] [ve p1.paramName incD first]
          , fnDiv False
          , n [wc "rhs"] [ve p2.paramName incD second]
          ]
        _ ->
          let args = List.map2
                       (\p e_ -> ve p.paramName incD e_)
                       fn.fnParameters
                       exprs

          in
          n (wc "fncall prefix" :: wc (depthString d) :: all)
            (fnDiv fn.fnInfix :: args)

    Lambda vars expr ->
      let varname v = t [wc "lambdavarname", atom] v in
      n (wc "lambdaexpr" :: all)
        [ n [wc "lambdabinding"] (List.map (viewVarBind vs [atom]) vars)
        , a [wc "arrow"] "->"
        , n [wc "lambdabody"] [vExpr 0 expr]
        ]

    Thread exprs ->
      let pipe = a [wc "thread pipe"] "|>"
          texpr e_ =
            n ([wc "threadmember", ClickSelectAs (B.toID e_)])
              [pipe, vExpr 0 e_]
      in
      n (wc "threadexpr" :: mo :: config)
        (List.map texpr exprs)

    FieldAccess obj field ->
      n (wc "fieldaccessexpr" :: all)
        [ n [wc "fieldobject"] [vExpr 0 obj]
        , a [wc "fieldaccessop operator"] "."
        , viewFieldName vs
            [ wc "fieldname" , atom ]
            field
        ]

    ListLiteral exprs ->
      let open = a [wc "openbracket"] "["
          close = a [wc "closebracket"] "]"
          comma = a [wc "comma"] ","
          lexpr e_ =
            n ([wc "listelem", ClickSelectAs (B.toID e_)])
              [vExpr 0 e_]
          new = List.map lexpr exprs
                |> List.intersperse comma
      in
      n (wc "list" :: mo :: config)
        ([open] ++ new ++ [close])

    ObjectLiteral pairs ->
      let colon = a [wc "colon"] ":"
          open = a [wc "openbrace"] "{"
          close = a [wc "closebrace"] "}"
          pexpr (k,v) =
            n ([wc "objectpair"])
              [viewKey vs [] k, colon, vExpr 0 v]
      in
      n (wc "object" :: mo :: config)
        ([open] ++ List.map pexpr pairs ++ [close])

    FeatureFlag msg cond a_ b_ ->
      let exprLabel msg_ = Html.label [ Attrs.class "expr-label" ] [ Html.text msg_ ]

          isExpanded =
            let mv = Dict.get (deID id) vs.featureFlags
            in case mv of
              Just b -> b
              Nothing -> True

          pickA =
            Html.div
            [ Attrs.class "icon pick-a parameter-btn info"
              , Attrs.attribute "data-content" "Use Case A"
              , Attrs.title "delete Feature Flag & use Case A"
              , eventNoPropagation "click"
                (\_ -> EndFeatureFlag id PickA)
            ] [ fontAwesome "check" ]

          pickB =
            Html.div
            [ Attrs.class "icon pick-b parameter-btn info"
              , Attrs.attribute "data-content" "Use Case B"
              , Attrs.title "delete Feature Flag & use Case B"
              , eventNoPropagation "click"
                (\_ -> EndFeatureFlag id PickB)
            ] [ fontAwesome "check" ]

          hideModal =
            Html.div
            [
              Attrs.attribute "data-content" "Hide ff details"
              , eventNoPropagation "click" (\_ -> ToggleFeatureFlag id False)
            ]
            [ fontAwesome "minus" ]

          expandModal =
            Html.div
            [
              Attrs.attribute "data-content" "Show ff details"
              , eventNoPropagation "click" (\_ -> ToggleFeatureFlag id True)
            ]
            [ fontAwesome "flag" ]

          titleBar = Html.div [ Attrs.class ("row title-bar" )]
            [ viewText FFMsg vs (wc "flag-name" :: idConfigs) msg
              , Html.div [Attrs.class "actions"]
              [ if isExpanded then hideModal else expandModal ]
            ]

          condValue = ViewBlankOr.getLiveValue vs.currentResults.liveValues (B.toID cond)
          condResult = condValue
                       |> Maybe.map Runtime.isTrue
                       |> Maybe.withDefault False

          blockCondition =
            Html.div
            [ Attrs.class "row condition" ]
            [
              exprLabel "Condition (run Case B if...)"
              , vExpr 0 cond
            ]

          exprBlock lbl act exp =
            Html.div
            [ Attrs.class "cond-expr" ]
            [
              exprLabel lbl
              , act
              , div vs [wc "expr-block"] [vExpr 0 exp]
            ]

          expressions =
            Html.div
            [ Attrs.class "row expressions" ]
            [
              exprBlock "Case A" pickA a_
              , exprBlock "Case B" pickB b_
            ]

      in
        div vs
          [ wc "flagged shown"]
          [ viewExpr 0 { vs | showEntry = False } [] (if condResult then b_ else a_)
          , fontAwesome "flag"
          , Html.div
            [
              Attrs.class ("feature-flag" ++ (if isExpanded then " expand" else ""))
            ]
            [
              titleBar
              , blockCondition
              , expressions
            ]
          ]


isExecuting : ViewState -> ID -> Bool
isExecuting vs id =
  List.member id vs.executingFunctions


viewHandler : ViewState -> Handler -> List (Html.Html Msg)
viewHandler vs h =
  let showRail = AST.usesRail h.ast
      ast = Html.div
              [ Attrs.class "ast"]
              [ Html.div
                [ Attrs.classList [ ("rop-rail", showRail)]]
                [ viewExpr 0 vs [] h.ast ]]

      externalLink =
        case (h.spec.modifier, h.spec.name) of
          (F _ "GET", F _ name)  ->
            [Html.a [ Attrs.class "external"
                    , Attrs.href ("//" ++ Http.encodeUri vs.canvasName
                                  ++  "." ++ vs.userContentHost ++ name)
                    , Attrs.target "_blank"
                    ]
                    [fontAwesome "external-link-alt"]]
          _ -> []

      modifier =
        if SpecHeaders.visibleModifier h.spec
        then viewEventModifier vs [wc "modifier"] h.spec.modifier
        else Html.div [] []

      lock =
        Html.div
          [
            Attrs.classList [
              ("handler-lock", True)
              , ("is-locked", vs.handlerLocked)
            ]
            , eventNoPropagation "click"
                (\_ -> LockHandler vs.tlid (not vs.handlerLocked))
          ]
          [ fontAwesome (if vs.handlerLocked then "lock" else "lock-open") ]

      header =
        Html.div
          [Attrs.class "spec-header"]
          [ viewEventName vs [wc "name"] h.spec.name
          , (Html.div [] externalLink)
          , viewEventSpace vs [wc "module"] h.spec.module_
          , modifier
          , lock
          ]
  in [header, ast]
