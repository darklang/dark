module ViewCode exposing (viewExpr, viewDarkType, viewHandler)

-- builtin

-- lib
import Html
import Html.Attributes as Attrs
import String.Extra as SE
import List.Extra as LE

-- dark
import Types exposing (..)
import Util exposing (deMaybe)
import Runtime as RT
import Blank as B
import ViewBlankOr exposing (..)
import ViewUtils exposing (..)


viewFieldName : BlankViewer String
viewFieldName vs c f =
  let configs = idConfigs ++ c ++ withFeatureFlag vs f in
  viewBlankOr (viewNFieldName vs) Field vs configs f

viewVarBind : BlankViewer String
viewVarBind vs c v =
  let configs = idConfigs ++ c in
  viewBlankOr (viewNVarBind vs) VarBind vs configs v

viewDarkType : BlankViewer NDarkType
viewDarkType vs c dt =
  let configs = idConfigs ++ c in
  viewBlankOr (viewNDarkType vs) DarkType vs configs dt

viewExpr : Int -> BlankViewer NExpr
viewExpr depth vs c e =
  let configs = idConfigs ++ c ++ withFeatureFlag vs e ++ withEditFn vs e in
  viewBlankOr (viewNExpr depth vs) Expr vs configs e

viewEventName : BlankViewer String
viewEventName vs c v =
  let configs = idConfigs ++ c in
  viewText EventName vs configs v

viewEventSpace : BlankViewer String
viewEventSpace vs c v =
  let configs = idConfigs ++ c in
  viewText EventSpace vs configs v

viewEventModifier : BlankViewer String
viewEventModifier vs c v =
  let configs = idConfigs ++ c in
  viewText EventModifier vs configs v


viewNDarkType : Viewer NDarkType
viewNDarkType vs c d =
  case d of
    DTEmpty -> text vs c "Empty"
    DTString -> text vs c "String"
    DTAny -> text vs c "Any"
    DTInt -> text vs c "Int"
    DTObj ts ->
      let nested =
            ts
            |> List.map (\(n,dt) ->
                 [ viewText DarkTypeField vs [wc "fieldname"] n
                 , text vs [wc "colon"] ":"
                 , viewDarkType vs [wc "fieldvalue"] dt
                 ])
            |> List.intersperse
                 [text vs [wc "separator"] ","]
            |> List.concat
          open = text vs [wc "open"] "{"
          close = text vs [wc "close"] "}"
      in
      Html.div
        [Attrs.class "type-object"]
        ([open] ++ nested ++ [close])



viewNVarBind : Viewer VarName
viewNVarBind vs config f =
  text vs config f

viewNFieldName : Viewer FieldName
viewNFieldName vs config f =
  text vs config f

depthString : Int -> String
depthString n = "precedence-" ++ (toString n)

viewNExpr : Int -> Viewer NExpr
viewNExpr d vs config e =
  let vExpr d = viewExpr d vs []
      t = text vs
      n c = div vs (nested :: c)
      a c = text vs (atom :: c)
      kw = keyword vs
      all = idConfigs ++ config
      dv = DisplayValue
      cs = ClickSelect
      mo = Mouseover
      incD = d + 1

  in
  case e of
    Value v ->
      let cssClass = v |> RT.tipeOf |> toString |> String.toLower
          valu =
            -- TODO: remove
            if RT.isString v
            then "“" ++ (SE.unquote v) ++ "”"
            else v
          computedValue = if d == 0
            then [ComputedValue]
            else []
      in
      a (wc cssClass :: wc "value" :: all ++ computedValue) valu

    Variable name ->
      a (wc "variable" :: all) name

    Let lhs rhs body ->
      let lhsID = B.toID lhs
          bodyID = B.toID body
      in
      n (wc "letexpr" :: ComputedValueAs lhsID :: all)
        [ kw [] "let"
        , viewVarBind vs [wc "letvarname"] lhs
        , a [wc "letbind"] "="
        , n [wc "letrhs", dv, cs] [vExpr (d+1) rhs]
        , n [wc "letbody", ComputedValueAs bodyID] [vExpr d body]
        ]

    If cond ifbody elsebody ->
      n (wc "ifexpr" :: all)
      [ kw [] "if"
      , n [wc "cond"] [vExpr incD cond]
      , n [wc "ifbody"] [vExpr 0 ifbody]
      , kw [] "else"
      , n [wc "elsebody"] [vExpr 0 elsebody]
      ]

    FnCall name exprs ->
      let fnname parens =
            let withP name = if parens then "(" ++ name ++ ")" else name in
            case String.split "::" name of
              [mod, justname] ->
                n [wc "namegroup", atom]
                [ t [wc "module"] mod
                , t [wc "moduleseparator"] "::"
                , t [wc "fnname"] (withP justname)
                ]
              _ -> a [wc "fnname"] (withP name)
          fnDiv parens = n [wc "op", wc name] [fnname parens]
          isInfix = vs.ac.functions
                    |> LE.find (\f -> f.name == name)
                    |> deMaybe "vExpr fncall"
                    |> .infix
      in
      case (isInfix, exprs) of
        (True, [first, second]) ->
          n (wc "fncall infix" :: wc (depthString d) :: all)
          [ n [wc "lhs"] [vExpr incD first]
          , fnDiv False
          , n [wc "rhs"] [vExpr incD second]
          ]
        _ ->
          n (wc "fncall prefix" :: wc (depthString d) :: all)
            (fnDiv isInfix :: List.map (vExpr incD) exprs)

    Lambda vars expr ->
      let varname v = t [wc "lambdavarname", atom] v in
      n (wc "lambdaexpr" :: all)
        [ n [wc "lambdabinding"] (List.map varname vars)
        , a [wc "arrow"] "->"
        , n [wc "lambdabody"] [vExpr 0 expr]
        ]

    Thread exprs ->
      let pipe = a [wc "thread pipe"] "|>"
          texpr e =
            let id = B.toID e
                dopts = if d == 0
                then [DisplayValueOf id, ClickSelectAs id, ComputedValueAs id]
                else [DisplayValueOf id, ClickSelectAs id]
            in
            n ([wc "threadmember"] ++ dopts)
              [pipe, vExpr 0 e]
      in
      n (wc "threadexpr" :: mo :: dv :: config)
        (List.map texpr exprs)

    FieldAccess obj field ->
      n (wc "fieldaccessexpr" :: all)
        [ n [wc "fieldobject"] [vExpr 0 obj]
        , a [wc "fieldaccessop operator"] "."
        , viewFieldName vs (wc "fieldname" :: atom :: []) field
        ]


viewHandler : ViewState -> Handler -> List (Html.Html Msg)
viewHandler vs h =
  let ast = Html.div
              [ Attrs.class "ast"]
              [viewExpr 0 vs [] h.ast]

      externalLink =
        case (h.spec.modifier, h.spec.name) of
          (F _ "GET", F _ name)  ->
            [Html.a [ Attrs.class "external"
                    , Attrs.href name
                    , Attrs.target "_blank"
                    ]
                    [fontAwesome "external-link"]]
          _ -> []

      input =
        Html.div
          [Attrs.class "spec-type input-type"]
          [ Html.span [Attrs.class "header"] [Html.text "Input:"]
          , viewDarkType vs [] h.spec.types.input]
      output =
        Html.div
          [Attrs.class "spec-type output-type"]
          [ Html.span [Attrs.class "header"] [Html.text "Output:"]
          , viewDarkType vs [] h.spec.types.output]

      header =
        Html.div
          [Attrs.class "spec-header"]
          [ viewEventName vs [wc "name"] h.spec.name
          , (Html.div [] externalLink)
          , viewEventSpace vs [wc "module"] h.spec.module_
          , viewEventModifier vs [wc "modifier"] h.spec.modifier]
  in [header, ast]
