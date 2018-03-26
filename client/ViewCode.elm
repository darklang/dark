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
      in
      a (wc cssClass :: wc "value" :: all) valu

    Variable name ->
      a (wc "variable" :: all) name

    Let lhs rhs body ->
      n (wc "letexpr" :: all)
        [ kw [] "let"
        , viewVarBind vs [wc "letvarname"] lhs
        , a [wc "letbind"] "="
        , n [wc "letrhs", dv, cs] [vExpr (d+1) rhs]
        , n [wc "letbody"] [vExpr d body]
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
                p = B.toP Expr e
            in
            n [wc "threadmember", DisplayValueOf id, ClickSelectAs p]
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



