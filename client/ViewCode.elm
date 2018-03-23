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

viewExpr : BlankViewer NExpr
viewExpr vs c e =
  viewBlankOr (viewNExpr vs) Expr vs c e


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
                 [ viewBlankOrText DarkTypeField vs [wc "fieldname"] n
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

viewNExpr : Viewer NExpr
viewNExpr vs c e =
  let vExpr = viewExpr vs []
      text = text_ vs
      nesteds = nesteds_ vs
      nested = nested_ vs
      keyword = keyword_ vs
      selectable = selectable_ vs
      all = idConfigs
      dv = DisplayValue
      cs = ClickSelect
      mo = Mouseover
      (m, _) = vs

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
      nesteds (wc "letexpr" :: all ++ c)
        [ keyword [] "let"
        , nesteds [wc "letbinding"]
            [ selectable [wc "letvarname"] (viewVarBind vs [] lhs)
            , text [wc "letbind"] "="
            , nested [wc "letrhs", dv, cs] (vExpr rhs)
            ]
        , nested [wc "letbody"] (vExpr body)
        ]

    If cond ifbody elsebody ->
      nesteds (wc "ifexpr" :: all ++ c)
      [ keyword [] "if"
      , nested [wc "cond"] (vExpr cond)
      , nested [wc "ifbody"] (vExpr ifbody)
      , keyword [] "else"
      , nested [wc "elsebody"] (vExpr elsebody)
      ]

    FnCall name exprs ->
      let fnname parens =
            let withP name = if parens then "(" ++ name ++ ")" else name in
            case String.split "::" name of
              [mod, n] ->
                nesteds [wc "namegroup", atom]
                [ text [wc "module"] mod
                , text [wc "moduleseparator"] "::"
                , text [wc "fnname"] (withP n)
                ]
              _ -> text [atom, wc "fnname"] (withP name)
          fnDiv parens = nested [wc "op", wc name] (fnname parens)
          isInfix = m.complete.functions
                    |> LE.find (\f -> f.name == name)
                    |> deMaybe "vExpr fncall"
                    |> .infix
      in
      case (isInfix, exprs) of
        (True, [first, second]) ->
          nesteds (wc "fncall" :: wc "infix" :: all ++ c)
          [ nested [wc "lhs"] (vExpr first)
          , fnDiv False
          , nested [wc "rhs"] (vExpr second)
          ]
        _ ->
          nesteds (wc "fncall" :: wc "prefix" :: all ++ c)
            (fnDiv isInfix :: List.map vExpr exprs)

    Lambda vars expr ->
      let varname v = text [wc "lambdavarname", atom] v in
      nesteds (wc "lambdaexpr" :: all ++ c)
        [ nesteds [wc "lambdabinding"] (List.map varname vars)
        , text [atom, wc "arrow"] "->"
        , nested [wc "lambdabody"] (vExpr expr)
        ]

    Thread exprs ->
      let pipe = text [atom, wc "thread", wc "pipe"] "|>"
          texpr e =
            let id = B.toID e
                p = B.toP Expr e
            in
            nesteds [wc "threadmember", DisplayValueOf id, ClickSelectAs p]
              [pipe, vExpr e]
      in
      nesteds (wc "threadexpr" :: mo :: dv :: c)
        (List.map texpr exprs)

    FieldAccess obj field ->
      nesteds (wc "fieldaccessexpr" :: all ++ c)
        [ nested [wc "fieldobject"] (vExpr obj)
        , text [wc "fieldaccessop operator", atom] "."
        , selectable [wc "fieldname", atom] (viewFieldName vs [] field)
        ]



