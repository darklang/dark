module ViewAST exposing (..)

-- builtin
import List
import Html
import Html.Attributes as Attrs
import Dict

-- lib
import String.Extra as SE
import List.Extra as LE

-- dark
import Types exposing (..)
import Runtime as RT
import AST
import Runtime

type alias HtmlVisitState =
  { selectedID : ID
  , tlid : TLID
  , viewBlankOr: PointerType -> BlankOr String ->
                 Maybe (Result String String) -> Html.Html Msg
  , html4blank: DivSelected -> List Class -> Clickable ->
                Maybe (Result String String) -> List (Html.Html Msg) ->
                Html.Html Msg
  , liveValues : LVDict }

type alias Class = String
type Assoc = ClickSelect PointerType ID
           | HighlightAs ID
           | DisplayValue ID
type Element = Text (List Class) String
             | Selectable (List Class) (BlankOr String) PointerType
             | Nested (List Assoc) (List Class) (List Element)


-- there's a clickable ID and a hoverable ID

elemToHtml : HtmlVisitState -> Element -> Html.Html Msg
elemToHtml state elem =
  let displayVal id =
      id
      |> Maybe.andThen (\(ID id) -> Dict.get id state.liveValues)
      |> Maybe.map .value
      |> Maybe.map (\v -> if Runtime.isError v
                          then Err (Runtime.extractErrorMessage v)
                          else Ok v)
      asClass classNames = classNames |> String.join " " |> Attrs.class
  in
  case elem of
    Text classNames str ->
      Html.div
        [asClass ("leaf" :: classNames)]
        [Html.text str]

    Selectable classNames blankOr pointerType ->
      let id = blankOrID blankOr
          idAttr = Attrs.id (id |> deID |> toString)
          classes = asClass ("leaf" :: classNames)
      in
        Html.div
        ( idAttr :: classes :: [])
        [state.viewBlankOr pointerType blankOr (displayVal (Just id))]

    Nested assocs classNames elems ->
      let selected =
            if LE.find (\a -> a == HighlightAs state.selectedID) assocs == Nothing
            then DivUnselected
            else DivSelected
          click =
            assocs
            |> List.filterMap (\a -> case a of
                                       ClickSelect pt id ->
                                         Just (state.tlid, PFilled pt id)
                                       _ -> Nothing)
            |> List.head
          hovering =
            assocs
            |> List.filterMap (\a -> case a of
                                       DisplayValue id ->
                                         displayVal (Just id)
                                       _ -> Nothing)
            |> List.head

      in
      state.html4blank
        selected
        ("nested" :: classNames)
        click
        hovering
        (List.map (elemToHtml state) elems)


depthString : Int -> String
depthString n = "precedence-" ++ (toString n)

-- v is short for Visit

vFn : ID -> FnName -> Element
vFn id name =
  case String.split "::" name of
    [mod, n] ->
      Nested [] ["namegroup", "atom"]
      [ Text ["module"] mod
      , Text ["moduleseparator"] "::"
      , Text ["fnname"] n
      ]
    _ -> Text ["fnname", "atom"] name

vPrefix : ID -> FnName -> List Expr -> Int -> Element
vPrefix id name exprs nest =
  Nested [DisplayValue id, ClickSelect Expr id, HighlightAs id] ["fncall", "prefix", depthString nest]
    ((Nested [] ["op", name] [vFn id name])
    :: (List.map (vExpr (nest + 1)) exprs))


vInfix : ID -> FnName -> List Expr -> Int -> Element
vInfix id name exprs nesting =
  case exprs of
    [first, second] ->
      Nested [DisplayValue id, ClickSelect Expr id, HighlightAs id] ["fncall", "infix", depthString nesting]
        [ Nested [] ["lhs"] [vExpr (nesting + 1) first]
        , Nested [] ["op", name] [vFn id name]
        , Nested [] ["rhs"] [vExpr (nesting + 1) second]
        ]
    _ -> vPrefix id ("(" ++ name ++ ")") exprs nesting

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
     in Selectable ["atom", "value", cssClass] (Filled id valu) Expr

    Let id lhs rhs expr ->
      let rhsID = AST.toID rhs in
      Nested [DisplayValue id, ClickSelect Expr id, HighlightAs id] ["letexpr"]
        [ Text ["let", "keyword", "atom"] "let"
        , Nested [] ["letbinding"]
              [ Selectable ["letvarname", "atom"] lhs VarBind
              , Text ["letbind", "atom"] "="
              , Nested [DisplayValue rhsID, ClickSelect Expr rhsID] []
                       [vExpr nest rhs]
              ]
        , Nested [] ["letbody"] [vExpr nest expr]
        ]


    If id cond ifbody elsebody ->
      Nested [DisplayValue id, ClickSelect Expr id, HighlightAs id] ["ifexpr"]
        [ Text ["if", "keyword", "atom"] "if"
        , Nested [] ["cond"] [vExpr (nest + 1) cond]
        , Nested [] ["ifbody"] [vExpr 0 ifbody]
        , Text ["else", "keyword", "atom"] "else"
        , Nested [] ["elsebody"] [vExpr 0 elsebody]
        ]

    Variable id name ->
      Selectable ["variable", "atom"] (Filled id name) Expr

    FnCall id name exprs ->
      if AST.isInfix name
      then vInfix id name exprs nest
      else vPrefix id name exprs nest

    Lambda id vars expr ->
      let varname v = Text ["lambdavarname", "atom"] v in
      -- We want to allow you to select the lambda by clicking on the var, but
      -- we don't want you to think the var is selected. But we do that in CSS.
      Nested [ClickSelect Expr id, HighlightAs id, DisplayValue id] ["lambdaexpr"]
        [ Nested [] ["lambdabinding"] (List.map varname vars)
        , Text ["arrow", "atom"] "->"
        , Nested [] ["lambdabody"] [vExpr 0 expr]
        ]

    Hole id -> Selectable ["hole", "atom"] (Blank id) Expr

    Thread id exprs ->
      let pipe = Text ["thread", "atom", "pipe"] "|>" in
      Nested [HighlightAs id, DisplayValue id] ["threadexpr"]
        (List.map (\e ->
          let id = AST.toID e
          in Nested [DisplayValue id, ClickSelect Expr id] ["threadmember"] [pipe, vExpr 0 e]) exprs)

    FieldAccess id obj field ->
      Nested [DisplayValue id, ClickSelect Expr id, HighlightAs id] ["fieldaccessexpr"]
      [ Nested [] ["fieldobject"] [vExpr 0 obj]
      , Text ["fieldaccessop", "operator", "atom"] "."
      , Selectable ["fieldname", "atom"] field Field
      ]

walk : AST -> Element
walk = vExpr 0

toHtml : HtmlVisitState -> Expr -> Html.Html Msg
toHtml visitState expr =
  expr
  |> walk
  |> elemToHtml visitState

