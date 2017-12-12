module ViewAST exposing (..)

-- builtin
import List
import Html
import Html.Attributes as Attrs
import Dict

-- lib
import Maybe.Extra as ME
import String.Extra as SE

-- dark
import Types exposing (..)
import Runtime as RT

type alias HtmlVisitState = { holeID : ID
                            , holeHtml : Html.Html Msg
                            , liveValues : LVDict }

elemToHtml : HtmlVisitState -> Element -> Html.Html Msg
elemToHtml state elem =
  let hover id =
      id
      |> Maybe.andThen (\(ID id) -> Dict.get id state.liveValues)
      |> Maybe.map (\lv -> Attrs.title lv.value)
      |> ME.toList
  in
  case elem of
    Leaf (id, class, content) ->
      let idAttrs =
            case id of
              Just (ID i) -> [Attrs.id (toString i)]
              Nothing -> []

      in if id == Just state.holeID
          then
            Html.div
              ([Attrs.class <| "leaf " ++ class] ++ idAttrs ++ (hover id))
              [ state.holeHtml ]
          else
            Html.div
              ([Attrs.class <| "leaf " ++ class] ++ idAttrs ++ (hover id))
              [Html.text content]

    Nested (id, class) elems ->
      Html.div
        ([Attrs.class <| "nested " ++ class] ++ hover id)
        (List.map (elemToHtml state) elems)

depthString : Int -> String
depthString n = "precedence-" ++ (toString n)

-- v is short for Visit

vFn : FnName -> Element
vFn name =
  case String.split "::" name of
    [mod, n] ->
      Nested (Nothing, "namegroup atom")
      [ Leaf (Nothing, "module", mod)
      , Leaf (Nothing, "moduleseparator", "::")
      , Leaf (Nothing, "fnname", n)
      ]
    _ -> Leaf (Nothing, "fnname atom", name)

vPrefix : ID -> FnName -> List Expr -> Int -> Element
vPrefix id name exprs nest =
  Nested (Just id, "fncall prefix " ++ (depthString nest))
    ((Nested (Nothing, "op " ++ name) [vFn name])
    :: (List.map (vExpr (nest + 1)) exprs))


vInfix : ID -> FnName -> List Expr -> Int -> Element
vInfix id name exprs nesting =
  case exprs of
    [first, second] ->
      Nested (Just id, "fncall infix " ++ (depthString nesting))
        [ Nested (Nothing, "lhs") [vExpr (nesting + 1) first]
        , Nested (Nothing, "op " ++ name) [vFn name]
        , Nested (Nothing, "rhs") [vExpr nesting second]
        ]
    _ -> vPrefix id ("(" ++ name ++ ")") exprs nesting

isInfix : FnName -> Bool
isInfix name =
  List.member name ["<", "==", "%", "+", "-", "^"]

vVarname : Maybe ID -> VarName -> Element
vVarname mId v = Leaf (mId, "varname atom", v)

vVarBind : VarBind -> Element
vVarBind v =
  case v of
    Full s -> Leaf (Nothing, "varname atom", s)
    Empty id -> Leaf (Just id, "hole atom", "＿＿＿＿＿＿")

vExpr : Int -> Expr -> Element
vExpr nest expr =
  case expr of
    Value id v ->
     let cssClass = v |> RT.tipeOf |> toString |> String.toLower
         valu =
           -- TODO: remove
           if RT.isString v
           then "“" ++ (SE.unquote v) ++ "”"
           else v
     in Leaf (Just id, "atom value " ++ cssClass, valu)

    Let id lhs rhs expr ->
      Nested (Just id, "letexpr")
        [ Leaf (Nothing, "let keyword atom", "let")
        , Nested (Nothing, "letbinding")
              [ vVarBind lhs
              , Leaf (Nothing, "letbind atom", "=")
              , vExpr nest rhs ]
        , Leaf (Nothing, "in keyword atom" , "in")
        , Nested (Nothing, "letbody") [vExpr nest expr]
        ]


    If id cond ifbody elsebody ->
      Nested (Just id, "ifexpr")
        [ Leaf (Nothing, "if keyword atom", "if")
        , Nested (Nothing, "cond") [vExpr (nest + 1) cond]
        , Nested (Nothing, "ifbody") [(vExpr 0 ifbody)]
        , Leaf (Nothing, "else keyword atom", "else")
        , Nested (Nothing, "elsebody") [(vExpr 0 elsebody)]
        ]

    Variable id name ->
      vVarname (Just id) name

    FnCall id name exprs ->
      if isInfix name
      then vInfix id name exprs nest
      else vPrefix id name exprs nest

    Lambda id  vars expr ->
      Nested (Just id, "lambdaexpr")
        [ Nested (Nothing, "lambdabinding") (List.map (vVarname Nothing) vars)
        , Leaf (Nothing, "arrow atom" , "->")
        , Nested (Nothing, "lambdabody") [vExpr 0 expr]
        ]

    Hole id -> Leaf (Just id, "hole atom", "＿＿＿＿＿＿")

    Thread id exprs ->
      Nested (Just id, "threadexpr")
      (exprs
       |> List.map (\e -> Nested (Nothing, "threadmember") [vExpr 0 e])
       |> List.intersperse (Leaf (Nothing, "thread atom", "|>")))

    FieldAccess id obj field ->
      Nested (Just id, "fieldaccessexpr")
      [ Nested (Nothing, "fieldobject") [(vExpr 0 obj)]
      , Leaf (Nothing, "fieldaccessop operator atom", ".")
      , vVarBind field
      ]

walk : AST -> Element
walk = vExpr 0

toHtml : HtmlVisitState -> Expr -> Html.Html Msg
toHtml visitState expr =
  expr
  |> walk
  |> elemToHtml visitState

