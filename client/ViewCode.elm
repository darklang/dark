module ViewCode exposing (viewExpr, viewDarkType, viewHandler)

-- builtin

-- lib
import Html
import Html.Attributes as Attrs
import String.Extra as SE
import List.Extra as LE
import Maybe.Extra as ME

-- dark
import Types exposing (..)
import Prelude exposing (..)
import Runtime as RT
import AST
import Toplevel as TL
import Blank as B
import ViewBlankOr exposing (..)
import ViewUtils exposing (..)
import Runtime


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
  let configs = idConfigs ++ c ++ withFeatureFlag vs e ++ withEditFn vs e
      id = B.toID e
  in
  viewBlankOr (viewNExpr depth id vs) Expr vs configs e

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

viewNExpr : Int -> ID -> Viewer NExpr
viewNExpr d id vs config e =
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
          computedValue =
            if d == 0
            then [ComputedValue]
            else []
      in
      a (wc cssClass :: wc "value" :: all ++ computedValue) valu

    Variable name ->
      if List.member id vs.relatedBlankOrs
      then
        a (wc "variable" :: wc "related-change" :: all) (vs.ac.value)
      else
        a (wc "variable" :: all) name

    Let lhs rhs body ->
      let lhsID = B.toID lhs
          bodyID = B.toID body
      in
      n (wc "letexpr" :: ComputedValueAs lhsID :: all)
        [ kw [] "let"
        , viewVarBind vs [wc "letvarname"] lhs
        , a [wc "letbind"] "="
        , n [wc "letrhs", dv, cs] [vExpr d rhs]
        , n [wc "letbody", ComputedValueAs bodyID] [vExpr d body]
        ]

    If cond ifbody elsebody ->
      n (wc "ifexpr" :: all)
      [ kw [] "if"
      , n [wc "cond"] [vExpr d cond]
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
          fn = vs.ac.functions
                    |> LE.find (\f -> f.name == name)
                    |> Maybe.withDefault
                      { name = "fnLookupError"
                      , parameters = []
                      , description = "default, fn error"
                      , returnTipe = TError
                      , previewExecutionSafe = True
                      , infix = False
                      }

          previous =
            case vs.tl.data of
              TLHandler h ->
                h.ast
                |> AST.threadPrevious id
                |> ME.toList
              TLFunc f ->
                f.ast
                |> AST.threadPrevious id
                |> ME.toList
              TLDB db ->
                impossible db

          -- buttons
          allExprs = previous ++  exprs
          isComplete v =
            v
            |> getLiveValue vs.lvs
            |> \v ->
                 case v of
                   Nothing -> False
                   Just (Err _) -> True
                   Just (Ok val) ->
                     not (Runtime.isIncomplete val
                         || Runtime.isError val)

          paramsComplete = List.all (isComplete << B.toID) allExprs
          resultHasValue = isComplete id

          buttonUnavailable = not paramsComplete
          showButton = not fn.previewExecutionSafe
          buttonNeeded = not resultHasValue

          event = eventNoPropagation "mouseup"
                    (\_ -> ExecuteFunctionButton vs.tl.id id)
          (bClass, bEvent, bTitle, bIcon) =
            if buttonUnavailable
            then ("execution-button-unavailable"
                 , []
                 , "Cannot run: some parameters are incomplete"
                 , "cog")
            else if buttonNeeded
            then
              ( "execution-button-needed"
              , [event]
              , "Click to execute function"
              , "cog")
            else
              ( "execution-button-repeat"
              , [event]
              , "Click to execute function again"
              , "redo")
          button =
            if not showButton
            then []
            else
              [ Html.div
                ([ Attrs.class ("execution-button " ++ bClass)
                 , Attrs.title bTitle
                 ] ++ bEvent)
                [fontAwesome bIcon]
              ]

          fnDiv parens =
            n
              [wc "op", wc name, ComputedValueAs id]
              (fnname parens :: button)
      in
      case (fn.infix, exprs) of
        (True, [first, second]) ->
          n (wc "fncall infix" :: wc (depthString d) :: all)
          [ n [wc "lhs"] [vExpr incD first]
          , fnDiv False
          , n [wc "rhs"] [vExpr incD second]
          ]
        _ ->
          n (wc "fncall prefix" :: wc (depthString d) :: all)
            (fnDiv fn.infix :: List.map (vExpr incD) exprs)

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
                dopts =
                  if d == 0
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
                    [fontAwesome "external-link-alt"]]
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

      modifier =
        if TL.isHTTPHandler vs.tl
        then viewEventModifier vs [wc "modifier"] h.spec.modifier
        else Html.div [] []
      header =
        Html.div
          [Attrs.class "spec-header"]
          [ viewEventName vs [wc "name"] h.spec.name
          , (Html.div [] externalLink)
          , viewEventSpace vs [wc "module"] h.spec.module_
          , modifier]
  in [header, ast]
