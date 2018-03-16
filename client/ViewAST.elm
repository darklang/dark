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
import Runtime
import Util exposing (deMaybe)
import Blank

type alias HtmlVisitState =
  { selectedID : ID
  , hoveredID : ID
  , tlid : TLID
  , viewBlankOr: PointerType -> BlankOr String ->
                 Maybe (Result String String) -> Html.Html Msg
  , html4blank: DivSelected -> MouseOverDiv -> List Class -> Clickable ->
                Maybe (Result String String) -> List (Html.Html Msg) ->
                Html.Html Msg
  , liveValues : LVDict
  , functions : List Function}

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
      let id = Blank.toID blankOr
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
          mouseOvered =
            if LE.find (\a -> a == HighlightAs state.hoveredID) assocs == Nothing
            then MouseNotOverDiv
            else MouseOverDiv

      in
      state.html4blank
        selected
        mouseOvered
        ("nested" :: classNames)
        click
        hovering
        (List.map (elemToHtml state) elems)


depthString : Int -> String
depthString n = "precedence-" ++ (toString n)

-- v is short for Visit

vFn : HtmlVisitState -> ID -> FnName -> Element
vFn state id name =
  case String.split "::" name of
    [mod, n] ->
      Nested [] ["namegroup", "atom"]
      [ Text ["module"] mod
      , Text ["moduleseparator"] "::"
      , Text ["fnname"] n
      ]
    _ -> Text ["fnname", "atom"] name

vPrefix : HtmlVisitState -> ID -> FnName -> List Expr -> Int -> Element
vPrefix state id name exprs nest =
  Nested [DisplayValue id, ClickSelect Expr id, HighlightAs id] ["fncall", "prefix", depthString nest]
    ((Nested [] ["op", name] [vFn state id name])
    :: (List.map (vExpr state (nest + 1)) exprs))


vInfix : HtmlVisitState -> ID -> FnName -> List Expr -> Int -> Element
vInfix state id name exprs nesting =
  case exprs of
    [first, second] ->
      Nested [DisplayValue id, ClickSelect Expr id, HighlightAs id] ["fncall", "infix", depthString nesting]
        [ Nested [] ["lhs"] [vExpr state (nesting + 1) first]
        , Nested [] ["op", name] [vFn state id name]
        , Nested [] ["rhs"] [vExpr state (nesting + 1) second]
        ]
    _ -> vPrefix state id ("(" ++ name ++ ")") exprs nesting

vExpr : HtmlVisitState -> Int -> Expr -> Element
vExpr state nest bexpr =
  case bexpr of
    Blank id -> Selectable ["atom"] (Blank id) Expr
    F id expr ->
      case expr of
        Value v ->
         let cssClass = v |> RT.tipeOf |> toString |> String.toLower
             valu =
               -- TODO: remove
               if RT.isString v
               then "“" ++ (SE.unquote v) ++ "”"
               else v
         in Selectable ["atom", "value", cssClass] (F id valu) Expr

        Let lhs rhs expr ->
          let rhsID = Blank.toID rhs in
          Nested [DisplayValue id, ClickSelect Expr id, HighlightAs id] ["letexpr"]
            [ Text ["let", "keyword", "atom"] "let"
            , Nested [] ["letbinding"]
                  [ Selectable ["letvarname", "atom"] lhs VarBind
                  , Text ["letbind", "atom"] "="
                  , Nested [DisplayValue rhsID, ClickSelect Expr rhsID] []
                           [vExpr state nest rhs]
                  ]
            , Nested [] ["letbody"] [vExpr state nest expr]
            ]


        If cond ifbody elsebody ->
          Nested [DisplayValue id, ClickSelect Expr id, HighlightAs id] ["ifexpr"]
            [ Text ["if", "keyword", "atom"] "if"
            , Nested [] ["cond"] [vExpr state (nest + 1) cond]
            , Nested [] ["ifbody"] [vExpr state 0 ifbody]
            , Text ["else", "keyword", "atom"] "else"
            , Nested [] ["elsebody"] [vExpr state 0 elsebody]
            ]

        Variable name ->
          Selectable ["variable", "atom"] (F id name) Expr

        FnCall name exprs ->
          if state.functions
             |> LE.find (\f -> f.name == name)
             |> deMaybe "vExpr fncall"
             |> .infix
          then vInfix state id name exprs nest
          else vPrefix state id name exprs nest

        Lambda vars expr ->
          let varname v = Text ["lambdavarname", "atom"] v in
          -- We want to allow you to select the lambda by clicking on the var, but
          -- we don't want you to think the var is selected. But we do that in CSS.
          Nested [ClickSelect Expr id, HighlightAs id, DisplayValue id] ["lambdaexpr"]
            [ Nested [] ["lambdabinding"] (List.map varname vars)
            , Text ["arrow", "atom"] "->"
            , Nested [] ["lambdabody"] [vExpr state 0 expr]
            ]

        Thread exprs ->
          let pipe = Text ["thread", "atom", "pipe"] "|>" in
          Nested [HighlightAs id, DisplayValue id] ["threadexpr"]
            (List.map (\e ->
              let id = Blank.toID e
              in Nested [DisplayValue id, ClickSelect Expr id] ["threadmember"] [pipe, vExpr state 0 e]) exprs)

        FieldAccess obj field ->
          Nested [DisplayValue id, ClickSelect Expr id, HighlightAs id] ["fieldaccessexpr"]
          [ Nested [] ["fieldobject"] [vExpr state 0 obj]
          , Text ["fieldaccessop", "operator", "atom"] "."
          , Selectable ["fieldname", "atom"] field Field
          ]

walk : HtmlVisitState -> Expr -> Element
walk state = vExpr state 0

toHtml : HtmlVisitState -> Expr -> Html.Html Msg
toHtml visitState expr =
  expr
  |> walk visitState
  |> elemToHtml visitState

