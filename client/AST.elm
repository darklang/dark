module AST exposing (..)

-- builtin
import List
import Html
import Html.Attributes as Attrs

-- lib
-- import String.Extra as SE

-- dark
import Types exposing (..)

ppPrefix : FnName -> List Expr -> Element
ppPrefix name exprs =
  Nested "fncall prefix"
    (Leaf ("op " ++ name, name)
     :: (List.map pp exprs))


ppInfix : FnName -> List Expr -> Element
ppInfix name exprs =
  case exprs of
    [first, second] ->
      Nested "fncall infix"
        [ pp first
        , Leaf ("op " ++ name, name)
        , pp second
        ]
    _ ->
      Debug.crash "using infix with the wrong number of things"

isInfix : FnName -> Bool
isInfix name =
  List.member name ["<", "==", "%"]




type alias Class = String
type Element = Leaf (Class, String)
             | Nested Class (List Element)

ppVarname : VarName -> Element
ppVarname v = Leaf ("varname", v)

pp : Expr -> Element
pp expr =
  case expr of
    Value v -> Leaf ("value", v)

    Let vars expr ->
      Nested "let"
        [ Leaf ("let", "let")
        , Nested "letbindings"
            (List.map
              (\(l, r) ->
                Nested "letbinding"
                  [ ppVarname l
                  , Leaf ("letbind", "=")
                  , pp r
                  ]
              )
              vars
             )
        , Leaf ("in" , "in")
        , pp expr
        ]


    If cond ifblock elseblock ->
      Nested "ifexpr"
        [ Leaf ("if", "if")
        , Nested "cond" [pp cond]
        , Nested "ifblock" [(pp ifblock)]
        , Leaf ("else", "else")
        , Nested "elseblock" [(pp elseblock)]
        ]

    Variable name ->
      ppVarname name

    FnCall name exprs ->
      if isInfix name
      then ppInfix name exprs
      else ppPrefix name exprs

    Lambda vars expr ->
      Nested "lambda"
        [ Nested "lambdabinding" (List.map ppVarname vars)
        , Leaf ("arrow" , "->")
        , pp expr
        ]



elemToHtml : Element -> Html.Html Msg
elemToHtml elem =
  case elem of
    Leaf (class, content) ->
      Html.div [Attrs.class class] [Html.text content]
    Nested class elems ->
      Html.div [Attrs.class class] (List.map elemToHtml elems)

toHtml : Expr -> Html.Html Msg
toHtml expr = expr |> pp |> elemToHtml


