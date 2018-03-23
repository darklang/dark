module ViewCode exposing (viewExpr, viewDarkType)

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




viewFieldName : BlankViewer String
viewFieldName vs c f =
  viewBlankOr (viewNFieldName vs) Field vs c f

viewVarBind : BlankViewer String
viewVarBind vs c v =
  viewBlankOr (viewNVarBind vs) VarBind vs c v

viewDarkType : BlankViewer NDarkType
viewDarkType vs c =
  viewBlankOr (viewNDarkType vs) DarkType vs c

viewExpr : Int -> BlankViewer NExpr
viewExpr depth vs c e =
  viewBlankOr (viewNExpr depth vs) Expr vs c e


viewNDarkType : Viewer NDarkType
viewNDarkType vs c d =
  case d of
    DTEmpty -> Html.text "Empty"
    DTString -> Html.text "String"
    DTAny -> Html.text "Any"
    DTInt -> Html.text "Int"
    DTObj ts ->
      let nested =
            ts
            |> List.map (\(n,dt) ->
                 [ viewText DarkTypeField vs [wc "fieldname"] n
                 , text_ vs [wc "colon"] ":"
                 , viewDarkType vs [wc "fieldvalue"] dt
                 ])
            |> List.intersperse
                 [text_ vs [wc "separator"] ","]
            |> List.concat
          open = text_ vs [wc "open"] "{"
          close = text_ vs [wc "close"] "}"
      in
      Html.div
        [Attrs.class "type-object"]
        ([open] ++ nested ++ [close])



viewNVarBind : Viewer VarName
viewNVarBind vs config f =
  text_ vs config f

viewNFieldName : Viewer FieldName
viewNFieldName vs config f =
  text_ vs config f

depthString : Int -> String
depthString n = "precedence-" ++ (toString n)

viewNExpr : Int -> Viewer NExpr
viewNExpr d vs c e =
  let vExpr d = viewExpr d vs []
      text = text_ vs
      nested = nested_ vs
      keyword = keyword_ vs
      selectable = selectable_ vs
      all = idConfigs
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
      in
      selectable (wc cssClass :: wc "value" :: c) (Html.text valu)

    Variable name ->
      selectable (wc "variable" :: c) (Html.text name)

    Let lhs rhs body ->
      nested (wc "letexpr" :: all ++ c)
        [ keyword [] "let"
        , selectable [wc "letvarname"] (viewVarBind vs [] lhs)
        , text [wc "letbind"] "="
        , nested [wc "letrhs", dv, cs] [vExpr (d+1) rhs]
        , nested [wc "letbody"] [vExpr d body]
        ]

    If cond ifbody elsebody ->
      nested (wc "ifexpr" :: all ++ c)
      [ keyword [] "if"
      , nested [wc "cond"] [vExpr incD cond]
      , nested [wc "ifbody"] [vExpr 0 ifbody]
      , keyword [] "else"
      , nested [wc "elsebody"] [vExpr 0 elsebody]
      ]

    FnCall name exprs ->
      let fnname parens =
            let withP name = if parens then "(" ++ name ++ ")" else name in
            case String.split "::" name of
              [mod, n] ->
                nested [wc "namegroup", atom]
                [ text [wc "module"] mod
                , text [wc "moduleseparator"] "::"
                , text [wc "fnname"] (withP n)
                ]
              _ -> text [atom, wc "fnname"] (withP name)
          fnDiv parens = nested [wc "op", wc name] [fnname parens]
          isInfix = vs.ac.functions
                    |> LE.find (\f -> f.name == name)
                    |> deMaybe "vExpr fncall"
                    |> .infix
      in
      case (isInfix, exprs) of
        (True, [first, second]) ->
          nested (wc "fncall infix" :: wc (depthString d) :: all ++ c)
          [ nested [wc "lhs"] [vExpr incD first]
          , fnDiv False
          , nested [wc "rhs"] [vExpr incD second]
          ]
        _ ->
          nested (wc "fncall prefix" :: wc (depthString d) :: all ++ c)
            (fnDiv isInfix :: List.map (vExpr incD) exprs)

    Lambda vars expr ->
      let varname v = text [wc "lambdavarname", atom] v in
      nested (wc "lambdaexpr" :: all ++ c)
        [ nested [wc "lambdabinding"] (List.map varname vars)
        , text [atom, wc "arrow"] "->"
        , nested [wc "lambdabody"] [vExpr 0 expr]
        ]

    Thread exprs ->
      let pipe = text [atom, wc "thread pipe"] "|>"
          texpr e =
            let id = B.toID e
                p = B.toP Expr e
            in
            nested [wc "threadmember", DisplayValueOf id, ClickSelectAs p]
              [pipe, vExpr 0 e]
      in
      nested (wc "threadexpr" :: mo :: dv :: c)
        (List.map texpr exprs)

    FieldAccess obj field ->
      nested (wc "fieldaccessexpr" :: all ++ c)
        [ nested [wc "fieldobject"] [vExpr 0 obj]
        , text [wc "fieldaccessop operator", atom] "."
        , selectable [wc "fieldname", atom] (viewFieldName vs [] field)
        ]



