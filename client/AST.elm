module AST exposing (..)

-- builtin
import List
import Html
import Html.Attributes as Attrs

-- lib
import String.Extra as SE

-- dark
import Types exposing (..)
import Runtime as RT

ppFn : FnName -> Element
ppFn name =
  case String.split "::" name of
    [mod, n] ->
      Nested "namegroup atom"
      [ Leaf ("module", mod)
      , Leaf ("moduleseparator", "::")
      , Leaf ("fnname", n)
      ]
    _ -> Leaf ("fnname atom", name)

ppPrefix : FnName -> List Expr -> Element
ppPrefix name exprs =
  Nested "fncall prefix"
    ((Nested ("op " ++ name) [ppFn name])
    :: (List.map pp exprs))


ppInfix : FnName -> List Expr -> Element
ppInfix name exprs =
  case exprs of
    [first, second] ->
      Nested "fncall infix"
        [ pp first
        , Nested ("op " ++ name) [ppFn name]
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
ppVarname v = Leaf ("varname atom", v)

pp : Expr -> Element
pp expr =
  case expr of
    Value v ->
     let cssClass = v |> RT.tipeOf |> toString |> String.toLower
         valu  =
           -- TODO: remove
           if RT.isString v
           then "“" ++ (SE.unquote v) ++ "”"
           else v
     in  Leaf ("atom value " ++ cssClass, valu)

    Let vars expr ->
      Nested "letexpr"
        [ Leaf ("let keyword atom", "let")
        , Nested "letbindings"
            (List.map
              (\(l, r) ->
                Nested "letbinding"
                  [ ppVarname l
                  , Leaf ("letbind atom", "=")
                  , pp r
                  ]
              )
              vars
             )
        , Leaf ("in keyword atom" , "in")
        , Nested "letbody" [pp expr]
        ]


    If cond ifbody elsebody ->
      Nested "ifexpr"
        [ Leaf ("if keyword atom", "if")
        , Nested "cond" [pp cond]
        , Nested "ifbody" [(pp ifbody)]
        , Leaf ("else keyword atom", "else")
        , Nested "elsebody" [(pp elsebody)]
        ]

    Variable name ->
      ppVarname name

    FnCall name exprs ->
      if isInfix name
      then ppInfix name exprs
      else ppPrefix name exprs

    Lambda vars expr ->
      Nested "lambdaexpr"
        [ Nested "lambdabinding" (List.map ppVarname vars)
        , Leaf ("arrow atom" , "->")
        , Nested "lambdabody" [pp expr]
        ]



elemToHtml : Element -> Html.Html Msg
elemToHtml elem =
  case elem of
    Leaf (class, content) ->
      Html.div [Attrs.class <| "leaf " ++ class] [Html.text content]
    Nested class elems ->
      Html.div [Attrs.class <| "nested " ++ class] (List.map elemToHtml elems)

toHtml : Expr -> Html.Html Msg
toHtml expr = expr |> pp |> elemToHtml


