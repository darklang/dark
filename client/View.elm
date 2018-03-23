module View exposing (view)

-- builtin

-- lib
import Html
import Html.Attributes as Attrs
import Html.Events as Events
import String.Extra as SE
import List.Extra as LE

-- dark
import Types exposing (..)
import Util exposing (deMaybe)
import Pointer as P
import Runtime as RT
import Blank as B
import Runtime
import ViewEntry
import ViewUtils exposing (..)
import ViewScaffold
import ViewRoutingTable
import ViewBlankOr exposing (..)


view : Model -> Html.Html Msg
view m =
  let (w, h) = Util.windowSize () in
  Html.div
   [ Attrs.id "grid"
   , Events.onWithOptions
       "mouseup"
       { stopPropagation = False, preventDefault = True}
       (decodeClickEvent GlobalClick)
   ]
   [ ViewScaffold.viewError m.error
   , (viewCanvas m)
   , ViewScaffold.viewButtons m
   ]

viewCanvas : Model -> Html.Html Msg
viewCanvas m =
    let entry = ViewEntry.viewEntry m
        asts = List.map (viewTL m) m.toplevels
        yaxis = axisLine m {x=0, y=1}
        xaxis = axisLine m {x=1, y=0}
        routing = ViewRoutingTable.viewRoutingTable m
        allDivs = xaxis :: yaxis :: routing :: (asts ++ entry)
    in Html.div [Attrs.id "canvas"] allDivs

viewTL : Model -> Toplevel -> Html.Html Msg
viewTL m tl =
  let body =
        case tl.data of
          TLHandler h ->
            viewHandler m tl h
          TLDB db ->
            viewDB m tl db
      events =
        [ eventNoPropagation "mousedown" (ToplevelClickDown tl)
        , eventNoPropagation "mouseup" (ToplevelClickUp tl.id Nothing)
        ]

      selected = if Just tl.id == tlidOf m.state
                 then "selected"
                 else ""
      class = [selected, toString (deTLID tl.id), "toplevel", "cursor-" ++ (toString tl.cursor)]
              |> String.join " "

      html =
        Html.div
          [Attrs.class "sidebar-box"] -- see comment in css
          [ Html.div
              (Attrs.class class :: events)
              body
          ]

  in
      placeHtml m tl.pos html

viewDB : Model -> Toplevel -> DB -> List (Html.Html Msg)
viewDB m tl db =
  let namediv = Html.div
                 [ Attrs.class "dbname"]
                 [ Html.text db.name]
      coldivs =
        List.map (\(n, t) ->
                           Html.div
                             [ Attrs.class "col" ]
                             [ Html.span
                                 [ Attrs.class "name" ]
                                 [ viewBlankOrText DBColName m tl [] n ]
                             , Html.span
                                 [ Attrs.class "type" ]
                                 [ viewBlankOrText DBColType m tl [] t ]
                             ])
                         db.cols
  in
  [
    Html.div
      [ Attrs.class "db"]
      (namediv :: coldivs)
  ]

viewHandler : Model -> Toplevel -> Handler -> List (Html.Html Msg)
viewHandler m tl h =
  let (id, filling) =
        case unwrapState m.state of
          Selecting tlid (Just p) -> (P.toID p, False)
          Entering (Filling tlid p) -> (P.toID p, True)
          _ -> (ID 0, False)

      hovering =
        case m.hovering |> List.head of
          Just hid -> hid
          Nothing -> ID 0

      ast = Html.div
              [ Attrs.class "ast"]
              [ viewExpr m tl [] h.ast ]

      externalLink =
        case (h.spec.modifier, h.spec.name) of
          (F _ "GET", F _ name)  ->
            [Html.a [ Attrs.class "external"
                    , Attrs.href name
                    , Attrs.target "_blank"
                    ]
                    [Html.i [Attrs.class "fa fa-external-link"] []]]
          _ -> []

      input =
        Html.div
          [Attrs.class "spec-type input-type"]
          [ Html.span [Attrs.class "header"] [Html.text "Input:"]
          , viewDarkType m tl [] h.spec.types.input]
      output =
        Html.div
          [Attrs.class "spec-type output-type"]
          [ Html.span [Attrs.class "header"] [Html.text "Output:"]
          , viewDarkType m tl [] h.spec.types.output]

      header =
        Html.div
          [Attrs.class "header"]
          [ Html.div
            [ Attrs.class "name"]
            [ viewBlankOrText EventName m tl [] h.spec.name ]
          , Html.div
            [ Attrs.class "modifier" ]
            ( externalLink ++
              [ Html.div
                [ Attrs.class "module" ]
                [ viewBlankOrText EventSpace m tl [] h.spec.module_ ]
              , viewBlankOrText EventModifier m tl [] h.spec.modifier
              ]
            )
          ]
  in [header, ast]





viewFieldName : BlankViewer String
viewFieldName m tl c f =
  viewBlankOr (viewNFieldName m tl) Field m tl c f

viewVarBind : BlankViewer String
viewVarBind m tl c v =
  viewBlankOr (viewNVarBind m tl) VarBind m tl c v

viewDarkType : BlankViewer NDarkType
viewDarkType m tl c =
  viewBlankOr (viewNDarkType m tl) DarkType m tl c

viewExpr : BlankViewer NExpr
viewExpr m tl c e =
  viewBlankOr (viewNExpr m tl) Expr m tl c e


viewNDarkType : Viewer NDarkType
viewNDarkType m tl c d =
  case d of
    DTEmpty -> Html.text "Empty"
    DTString -> Html.text "String"
    DTAny -> Html.text "Any"
    DTInt -> Html.text "Int"
    DTObj ts ->
      let nested =
            ts
            |> List.map (\(n,dt) ->
                 [ Html.span
                     [Attrs.class "fieldname"]
                     [viewBlankOrText DarkTypeField m tl [] n ]
                 , Html.span [Attrs.class "colon"] [Html.text ":"]
                 , Html.span
                     [Attrs.class "fieldvalue"]
                     [viewDarkType m tl [] dt]
                 ])
            |> List.intersperse
                 [Html.span [Attrs.class "separator"] [Html.text ","]]
            |> List.concat
          open = Html.span [Attrs.class "open"] [Html.text "{"]
          close = Html.span [Attrs.class "close"] [Html.text "}"]
      in
      Html.div
        [Attrs.class "type-object"]
        ([open] ++ nested ++ [close])



viewNVarBind : Viewer VarName
viewNVarBind  m tl config f =
  text_ m tl config f

viewNFieldName : Viewer FieldName
viewNFieldName m tl config f =
  text_ m tl config f

viewNExpr : Viewer NExpr
viewNExpr m tl c e =
  let vExpr = viewExpr m tl []
      text = text_ m tl
      nesteds = nesteds_ m tl
      nested = nested_ m tl
      keyword = keyword_ m tl
      selectable = selectable_ m tl
      all = idConfigs
      dv = DisplayValue
      cs = ClickSelect
      mo = Mouseover

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
            [ selectable [wc "letvarname"] (viewVarBind m tl [] lhs)
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
        , selectable [wc "fieldname", atom] (viewFieldName m tl [] field)
        ]



