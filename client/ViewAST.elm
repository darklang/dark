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
import AST

type alias HtmlVisitState = { selectedID : ID
                            , isFilling: Bool
                            , fillingHtml: Html.Html Msg
                            , viewBlankOr: PointerType -> BlankOr String -> Html.Html Msg
                            , liveValues : LVDict }

type alias Class = String
type Element = Text Class String
             | Selectable Class (BlankOr String)
             | Nested (Maybe ID) Class (List Element)



elemToHtml : HtmlVisitState -> Element -> Html.Html Msg
elemToHtml state elem =
  let hover id =
      id
      |> Maybe.andThen (\(ID id) -> Dict.get id state.liveValues)
      |> Maybe.map (\lv -> Attrs.title lv.value)
      |> ME.toList
  in
  case elem of
    Text class str ->
      Html.div
        [Attrs.class <| "leaf " ++ class]
        [Html.text str]

    Selectable class blankOr ->
      let id = blankOrID blankOr
          idAttr = Attrs.id (id |> deID |> toString)
          classes = Attrs.class <| "leaf " ++ class
      in
        Html.div
        ( idAttr :: classes :: hover (Just id) )
        [state.viewBlankOr Expr blankOr]

    Nested id class elems ->
      let newClasses =
            if id == Just state.selectedID
            then Attrs.class <| "nested selected " ++ class
            else Attrs.class <| "nested " ++ class
      in
      Html.div
        ([newClasses] ++ hover id)
        (List.map (elemToHtml state) elems)

depthString : Int -> String
depthString n = "precedence-" ++ (toString n)

-- v is short for Visit

vFn : FnName -> Element
vFn name =
  case String.split "::" name of
    [mod, n] ->
      Nested Nothing "namegroup atom"
      [ Text "module" mod
      , Text "moduleseparator" "::"
      , Text "fnname" n
      ]
    _ -> Text "fnname atom" name

vPrefix : ID -> FnName -> List Expr -> Int -> Element
vPrefix id name exprs nest =
  Nested (Just id) ("fncall prefix " ++ (depthString nest))
    ((Nested Nothing ("op " ++ name) [vFn name])
    :: (List.map (vExpr (nest + 1)) exprs))


vInfix : ID -> FnName -> List Expr -> Int -> Element
vInfix id name exprs nesting =
  case exprs of
    [first, second] ->
      Nested (Just id) ("fncall infix " ++ depthString nesting)
        [ Nested Nothing "lhs" [vExpr (nesting + 1) first]
        , Nested Nothing ("op " ++ name) [vFn name]
        , Nested Nothing "rhs" [vExpr (nesting + 1) second]
        ]
    _ -> vPrefix id ("(" ++ name ++ ")") exprs nesting


vVarname : ID -> VarName -> Element
vVarname id v =
  Selectable "varname atom" (Filled id v)

vVarBind : VarBind -> Element
vVarBind v =
  Selectable "varname atom" v

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
     in Selectable ("atom value " ++ cssClass) (Filled id valu)

    Let id lhs rhs expr ->
      Nested (Just id) "letexpr"
        [ Text "let keyword atom" "let"
        , Nested Nothing "letbinding"
              [ vVarBind lhs
              , Text "letbind atom" "="
              , vExpr nest rhs ]
        , Nested Nothing "letbody" [vExpr nest expr]
        ]


    If id cond ifbody elsebody ->
      Nested (Just id) "ifexpr"
        [ Text "if keyword atom" "if"
        , Nested Nothing "cond" [vExpr (nest + 1) cond]
        , Nested Nothing "ifbody" [vExpr 0 ifbody]
        , Text "else keyword atom" "else"
        , Nested Nothing "elsebody" [vExpr 0 elsebody]
        ]

    Variable id name ->
      vVarname id name

    FnCall id name exprs ->
      if AST.isInfix name
      then vInfix id name exprs nest
      else vPrefix id name exprs nest

    Lambda id vars expr ->
      Nested (Just id) "lambdaexpr"
        [ Nested Nothing "lambdabinding" (List.map (vVarname id) vars)
        , Text "arrow atom" "->"
        , Nested Nothing "lambdabody" [vExpr 0 expr]
        ]

    Hole id -> Selectable "hole atom" (Blank id)

    Thread id exprs ->
      let pipe = Text "thread atom pipe" "|>" in
      Nested (Just id) "threadexpr"
        (List.map (\e -> Nested Nothing "threadmember" [pipe, vExpr 0 e]) exprs)

    FieldAccess id obj field ->
      Nested (Just id) "fieldaccessexpr"
      [ Nested Nothing "fieldobject" [vExpr 0 obj]
      , Text "fieldaccessop operator atom" "."
      , vVarBind field
      ]

walk : AST -> Element
walk = vExpr 0

toHtml : HtmlVisitState -> Expr -> Html.Html Msg
toHtml visitState expr =
  expr
  |> walk
  |> elemToHtml visitState

