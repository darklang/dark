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

depthString : Int -> String
depthString n = "precedence-" ++ (toString n)

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

ppPrefix : FnName -> List Expr -> Int -> Element
ppPrefix name exprs nest =
  Nested ("fncall prefix " ++ (depthString nest))
    ((Nested ("op " ++ name) [ppFn name])
    :: (List.map (pp (nest + 1)) exprs))


ppInfix : FnName -> List Expr -> Int -> Element
ppInfix name exprs nesting =
  case exprs of
    [first, second] ->
      Nested ("fncall infix " ++ (depthString nesting))
        [ Nested "lhs" [pp (nesting + 1) first]
        , Nested ("op " ++ name) [ppFn name]
        , Nested "rhs" [pp nesting second]
        ]
    _ ->
      Debug.crash "using infix with the wrong number of things"

isInfix : FnName -> Bool
isInfix name =
  List.member name ["<", "==", "%", "+"]


type alias Class = String
type Element = Leaf (Class, String)
             | Nested Class (List Element)

ppVarname : VarName -> Element
ppVarname v = Leaf ("varname atom", v)

pp : Int -> Expr -> Element
pp nest expr =
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
                  , pp nest r
                  ]
              )
              vars
             )
        , Leaf ("in keyword atom" , "in")
        , Nested "letbody" [pp nest expr]
        ]


    If cond ifbody elsebody ->
      Nested "ifexpr"
        [ Leaf ("if keyword atom", "if")
        , Nested "cond" [pp (nest + 1) cond]
        , Nested "ifbody" [(pp 0 ifbody)]
        , Leaf ("else keyword atom", "else")
        , Nested "elsebody" [(pp 0 elsebody)]
        ]

    Variable name ->
      ppVarname name

    FnCall name exprs ->
      if isInfix name
      then ppInfix name exprs nest
      else ppPrefix name exprs nest

    Lambda vars expr ->
      Nested "lambdaexpr"
        [ Nested "lambdabinding" (List.map ppVarname vars)
        , Leaf ("arrow atom" , "->")
        , Nested "lambdabody" [pp 0 expr]
        ]



elemToHtml : Element -> Html.Html Msg
elemToHtml elem =
  case elem of
    Leaf (class, content) ->
      Html.div [Attrs.class <| "leaf " ++ class] [Html.text content]
    Nested class elems ->
      Html.div [Attrs.class <| "nested " ++ class] (List.map elemToHtml elems)

toHtml : Expr -> Html.Html Msg
toHtml expr = expr |> pp 0 |> elemToHtml


