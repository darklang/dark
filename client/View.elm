module View exposing (view)

-- builtin
import Json.Decode as JSD
import Json.Decode.Pipeline as JSDP
import Dict

-- lib
import Html
import Html.Attributes as Attrs
import Html.Events as Events
import String.Extra as SE
import List.Extra as LE
import Maybe.Extra as ME

-- dark
import Types exposing (..)
import Util exposing (deMaybe)
import Defaults
import Viewport
import Analysis
import Autocomplete
-- import ViewAST
import Toplevel as TL
import Pointer as P
import AST
import Runtime as RT
import Blank as B
import Runtime
import Toplevel

fontAwesome : String -> Html.Html Msg
fontAwesome name =
  Html.i [Attrs.class ("fa fa-" ++ name)] []

eventNoPropagation : String -> (MouseEvent -> Msg) -> Html.Attribute Msg
eventNoPropagation event constructor =
  Events.onWithOptions
    event
    { stopPropagation = True, preventDefault = False}
    (decodeClickEvent constructor)

nothingMouseEvent : String -> Html.Attribute Msg
nothingMouseEvent name = eventNoPropagation name NothingClick


view : Model -> Html.Html Msg
view m =
  let (w, h) = Util.windowSize ()
      grid = Html.div
               [ Attrs.id "grid"
               , Events.onWithOptions
                   "mouseup"
                   { stopPropagation = False, preventDefault = True}
                   (decodeClickEvent GlobalClick)
               ]
               [ viewError m.error
               , (viewCanvas m)
               , viewButtons m
               ]
 in
    grid

viewButtons : Model -> Html.Html Msg
viewButtons m =
  let integrationTestButton =
    case m.integrationTestState of
      IntegrationTestExpectation _ ->
        [ Html.a
          [ eventNoPropagation "mouseup" (\_ -> FinishIntegrationTest)
          , Attrs.src ""
          , Attrs.id "finishIntegrationTest"
          , Attrs.class "specialButton"]
          [ Html.text "Finish integration tests" ]]
      IntegrationTestFinished (Ok ()) ->
        [ Html.div [ Attrs.id "integrationTestSignal"
                   , Attrs.class "specialButton success"]
                   [ Html.text "success"]]
      IntegrationTestFinished (Err msg) ->
        [ Html.div [ Attrs.id "integrationTestSignal"
                   , Attrs.class "specialButton failure" ]
                   [ Html.text <| "failure: " ++ msg]]
      NoIntegrationTest -> []

  in
  Html.div [Attrs.id "buttons"]
    ([ Html.a
      [ Events.onClick AddRandom
      , Attrs.src ""
      , Attrs.class "specialButton"]
      [ Html.text "Random" ]
    , Html.a
      [ Events.onClick ClearGraph
      , Attrs.src ""
      , Attrs.class "specialButton"]
      [ Html.text "Clear" ]
    , Html.a
      [ Events.onClick SaveTestButton
      , Attrs.src ""
      , Attrs.class "specialButton"]
      [ Html.text "SaveTest" ]
    , Html.a
      [ Events.onClick ToggleSync
      , Attrs.src ""
      , Attrs.class "specialButton"]
      [ Html.text
          (if m.syncEnabled then "DisableSync" else "EnableSync") ]
    , Html.span
      [ Attrs.class "specialButton"]
      [Html.text (toString m.center)]
    , Html.span
      [ Attrs.class "specialButton"]
      [Html.text ("Active tests: " ++ toString m.tests)]
    ] ++ integrationTestButton)

viewError : Maybe String -> Html.Html Msg
viewError mMsg = case mMsg of
    Just msg ->
      Html.div [Attrs.id "status", Attrs.class "error"] [Html.text msg]
    Nothing ->
      Html.div [Attrs.id "status"] [Html.text "Dark"]

viewCanvas : Model -> Html.Html Msg
viewCanvas m =
    let
        entry = viewEntry m
        asts = List.map (viewTL m) m.toplevels
        yaxis = axisLine m {x=0, y=1}
        xaxis = axisLine m {x=1, y=0}
        routing = viewRoutingTable m
        allDivs = xaxis :: yaxis :: routing :: (asts ++ entry)
    in Html.div [Attrs.id "canvas"] allDivs



-- Create a Html.div for this ID, incorporating all ID-related data,
-- such as whether it's selected, appropriate events, mouseover, etc.

type HtmlConfig =
                -- Add this class (can be done multiple times)
                  WithClass String
                -- when you click this node, select this pointer
                | ClickSelectAs Pointer
                | ClickSelect -- use withID
                -- highlight this node as if it were ID
                | HighlightAs ID
                | Highlight
                -- display the value from this ID
                | DisplayValueOf ID
                | DisplayValue
                -- use this as ID for Highlight, ClickSelect and
                -- DisplayValue
                | WithID Pointer

wc = WithClass
atom = wc "atom"
text_ m tl c str =
  div m tl c [Html.text str]
nesteds_ m tl c items =
  div m tl (WithClass "nested" :: c) items
nested_ m tl c item =
  nesteds_ m tl c [item]
keyword_ m tl c name =
  text_ m tl (atom :: wc "keyword" :: wc name :: c) name

div : Model -> Toplevel -> List HtmlConfig -> List (Html.Html Msg) -> Html.Html Msg
div m tl configs content =
  let selectedID = case unwrapState m.state of
                     Selecting _ (Just p) -> Just (P.toID p)
                     _ -> Nothing

      -- Extract config
      thisPointer =
        configs
        |> List.filterMap (\a -> case a of
                                   WithID p -> Just p
                                   _ -> Nothing)
        |> List.head
      thisID = thisPointer |> Maybe.map P.toID

      clickAs =
        configs
        |> List.filterMap (\a -> case a of
                                   ClickSelectAs p -> Just p
                                   ClickSelect -> thisPointer
                                   _ -> Nothing)
        |> List.head
      hoverAs =
        configs
        |> List.filterMap (\a -> case a of
                                   DisplayValueOf id -> Just id
                                   DisplayValue -> thisID
                                   _ -> Nothing)
        |> List.head
      mouseoverAs =
        configs
        |> List.filterMap (\a -> case a of
                                   HighlightAs id -> Just id
                                   Highlight -> thisID
                                   _ -> Nothing)
        |> List.head
      classes =
        configs
        |> List.filterMap (\a -> case a of
                             WithClass c -> Just c
                             _ -> Nothing)


      -- Start using the config
      lvs = Analysis.getLiveValuesDict m tl.id
      hoverdata =
        case hoverAs of
          Just (ID id) ->
            Dict.get id lvs
            |> Maybe.map .value
            |> Maybe.map (\v -> if Runtime.isError v
                                then Err (Runtime.extractErrorMessage v)
                                else Ok v)
          _ -> Nothing

      (valClasses, title) =
        case hoverdata of
          Nothing -> ([], [])
          Just (Ok msg) -> ([], [Attrs.title msg])
          Just (Err err) -> (["value-error"], [Attrs.title err])

      selected = thisID == selectedID
                 && ME.isJust thisID
      mouseover = mouseoverAs == (m.hovering |> List.head)
                                 && ME.isJust mouseoverAs

      idAttr = case thisID of
                 Just id -> ["id-" ++ toString (deID id)]
                 _ -> []
      allClasses = classes
                  ++ idAttr
                  ++ valClasses
                  ++ (if selected then ["selected"] else [])
                  ++ (if mouseover then ["mouseovered"] else [])
      classAttr = Attrs.class (String.join " " allClasses)
      events =
        if selected
        then []
        else
          case clickAs of
            Just p ->
              let id = P.toID p in
              [ eventNoPropagation "mouseup"
                  (ToplevelClickUp tl.id (Just p))
              , eventNoPropagation "mouseenter" (MouseEnter id)
              , eventNoPropagation "mouseleave" (MouseLeave id)
              ]
            _ -> []
      attrs = events ++ title ++ [classAttr]

  in
    Html.div attrs content


type alias Viewer a = Model -> Toplevel -> List HtmlConfig -> a -> Html.Html Msg
type alias BlankViewer a = Viewer (BlankOr a)

viewBlankOrText : PointerType -> BlankViewer String
viewBlankOrText = viewBlankOr (\_ -> Html.text)

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

viewBlankOr : (List HtmlConfig -> a -> Html.Html Msg) -> PointerType ->
  BlankViewer a
viewBlankOr htmlFn pt m tl c bo =
  let pointer = B.toP pt bo
      id = P.toID pointer
      paramPlaceholder =
        tl
        |> TL.asHandler
        |> Maybe.map .ast
        |> Maybe.andThen
            (\ast ->
              case AST.getParamIndex ast id of
                Just (name, index) ->
                  case Autocomplete.findFunction m.complete name of
                    Just {parameters} ->
                      LE.getAt index parameters
                    Nothing -> Nothing
                _ -> Nothing)
        |> Maybe.map (\p -> p.name ++ ": " ++ RT.tipe2str p.tipe ++ "")
        |> Maybe.withDefault ""

      isHTTP = TL.isHTTPHandler tl
      placeholder =
        case pt of
          VarBind -> "varname"
          EventName ->
            if isHTTP
            then
              "route"
            else
              "event name"
          EventModifier ->
            if isHTTP
            then
              "verb"
            else
              "event modifier"
          EventSpace -> "event space"
          Expr -> paramPlaceholder
          Field -> "fieldname"
          DBColName -> "db field name"
          DBColType -> "db type"
          DarkType -> "type"
          DarkTypeField -> "fieldname"

      selected = case unwrapState m.state of
                     Selecting _ (Just p) -> P.toID p == id
                     _ -> False

      featureFlag = if selected
                    then [viewFeatureFlag]
                    else []
      idConfigs = [WithID pointer, ClickSelect, DisplayValue, Highlight]

      thisTextFn bo = case bo of
                        Blank _ ->
                          div m tl
                            (idConfigs ++ c)
                            ([Html.text placeholder] ++ featureFlag)
                        F _ fill ->
                          -- to add FeatureFlag here, we need to pass it
                          -- to the htmlFn maybe?
                          htmlFn (idConfigs ++ c) fill
                        Flagged _ _ _ _ -> Debug.crash "vbo"

      thisText = case bo of
                   Flagged msg setting l r ->
                     Html.div
                       [Attrs.class "flagged"]
                       [ Html.text msg
                       , Html.text (toString setting)
                       , thisTextFn l
                       , thisTextFn r]
                   _ -> thisTextFn bo

      allowStringEntry = pt == Expr

      text = case unwrapState m.state of
               Entering (Filling _ p) ->
                 if pointer == p
                 then entryHtml allowStringEntry placeholder m.complete
                 else thisText
               _ -> thisText
  in
  text


viewFeatureFlag : Html.Html Msg
viewFeatureFlag =
  Html.div
    [ Attrs.class "feature-flag"
    , Events.onMouseDown StartFeatureFlag]
    [ fontAwesome "flag"]



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



viewNDarkType : Model -> Toplevel -> List HtmlConfig -> NDarkType -> Html.Html Msg
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
viewNExpr m tl config e =
  let vExpr = viewExpr m tl []
      text = text_ m tl
      nesteds = nesteds_ m tl
      nested = nested_ m tl
      keyword = keyword_ m tl
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
      text (wc cssClass :: wc "value" :: atom :: config) valu

    Variable name ->
      text (atom :: wc "variable" :: config) name

    Let lhs rhs body ->
      nesteds (wc "letexpr" :: config)
        [ keyword [] "let"
        , nesteds [wc "letbinding"]
            [ nested [atom, wc "letvarname"] (viewVarBind m tl [] lhs)
            , text [wc "letbind"] "="
            , nested [wc "letrhs"] (vExpr rhs)
            ]
        , nested [wc "letbody"] (vExpr body)
        ]

    If cond ifbody elsebody ->
      nesteds (wc "ifexpr" :: config)
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
          nesteds (wc "fncall" :: wc "infix" :: config)
          [ nested [wc "lhs"] (vExpr first)
          , fnDiv False
          , nested [wc "rhs"] (vExpr second)
          ]
        _ ->
          nesteds (wc "fncall" :: wc "prefix" :: config)
            (fnDiv isInfix :: List.map vExpr exprs)

    Lambda vars expr ->
      let varname v = text [wc "lambdavarname", atom] v in
      nesteds (wc "lambdaexpr" :: config)
        [ nesteds [wc "lambdabinding"] (List.map varname vars)
        , text [atom, wc "arrow"] "->"
        , nested [wc "lambdabody"] (vExpr expr)
        ]

    Thread exprs ->
      let pipe = text [atom, wc "thread", wc "pipe"] "|>"
          texpr e =
            let p = B.toP Expr e in
            nesteds [wc "threadmember", WithID p] [pipe, vExpr e]
      in
      nesteds (wc "threadexpr" :: config)
        (List.map texpr exprs)

    FieldAccess obj field ->
      nesteds (wc "fieldaccessexpr" :: config)
        [ nested [wc "fieldobject"] (vExpr obj)
        , text [wc "fieldaccessop operator", atom] "."
        , nested [wc "fieldname", atom] (viewFieldName m tl [] field)
        ]



viewEntry : Model -> List (Html.Html Msg)
viewEntry m =
  case unwrapState m.state of
    Entering (Creating pos) ->
      let html =
            Html.div
              [Attrs.class "omnibox"]
              [entryHtml True "" m.complete]
      in [placeHtml m pos html]
    _ ->
      []


-- The view we see is different from the value representation in a few
-- ways:
-- - the start and end quotes are skipped
-- - all other quotes are escaped
transformToStringEntry : String -> String
transformToStringEntry s_ =
  -- the first time we won't have a closing quote so add it
  let s = if String.endsWith "\"" s_ then s_ else s_ ++ "\"" in
  s
  |> String.dropLeft 1
  |> String.dropRight 1
  |> Util.replace "\\\\\"" "\""

transformFromStringEntry : String -> String
transformFromStringEntry s =
  let s2 = s
           |> Util.replace "\"" "\\\""
  in
  "\"" ++ s2 ++ "\""

stringEntryHtml : Autocomplete -> Html.Html Msg
stringEntryHtml ac =
  let
      -- stick with the overlapping things for now, just ignore the back
      -- one
      value = transformToStringEntry ac.value
      length = value
               |> String.length
               |> max 3
               |> (+) 1


      smallInput =
        Html.input [ Attrs.id Defaults.entryID
                   , Events.onInput (EntryInputMsg << transformFromStringEntry)
                   , nothingMouseEvent "mouseup"
                   , nothingMouseEvent "mouseclick"
                   , nothingMouseEvent "mousedown"
                   , Attrs.value value
                   , widthInCh length
                   , Attrs.spellcheck False
                   , Attrs.autocomplete False
                   ] []


      largeInput =
        Html.textarea [ Attrs.id Defaults.entryID
                      , Events.onInput (EntryInputMsg << transformFromStringEntry)
                      , Attrs.value value
                      , Attrs.spellcheck False
                      -- Stop other events firing
                      , nothingMouseEvent "mouseup"
                      , nothingMouseEvent "mouseclick"
                      , nothingMouseEvent "mousedown"
                      , Attrs.cols 50
                      , Attrs.rows (5 + SE.countOccurrences "\n" value)
                      , Attrs.autocomplete False
                      ] []
    in
    if Autocomplete.isSmallStringEntry ac
    then
      Html.div
      [ Attrs.class "string-entry small-string-entry"
      , widthInCh (length + 3)
      ]
      [
        Html.form
        [ Events.onSubmit (EntrySubmitMsg)
        , Attrs.class "string-container"
        ]
        [ smallInput ]
      ]
    else
       Html.div
      [ Attrs.class "string-entry big-string-entry" ]
      [
        Html.form
        [ Events.onSubmit (EntrySubmitMsg)
        , Attrs.class "string-container"
        ]
        [ largeInput ]
      ]

inCh : Int -> String
inCh w =
  w
  |> toString
  |> \s -> s ++ "ch"

widthInCh : Int -> Html.Attribute Msg
widthInCh w =
  w
  |> inCh
  |> \w -> Attrs.style [("width", w)]



normalEntryHtml : String -> Autocomplete -> Html.Html Msg
normalEntryHtml placeholder ac =
  let autocompleteList =
        (List.indexedMap
           (\i item ->
              let highlighted = ac.index == i
                  hlClass = if highlighted then " highlighted" else ""
                  name = Autocomplete.asName item
              in Html.li
                [ Attrs.class <| "autocomplete-item" ++ hlClass
                , eventNoPropagation "mouseup"
                    (\_ -> AutocompleteClick name)
                ]
                [ Html.text name
                , Html.span
                    [Attrs.class "types"]
                    [Html.text <| Autocomplete.asTypeString item ]
                ])
           (List.concat ac.completions))

      autocomplete = Html.ul
                     [ Attrs.id "autocomplete-holder" ]
                     autocompleteList


      -- two overlapping input boxes, one to provide suggestions, one
      -- to provide the search
      (indent, suggestion, search) =
        Autocomplete.compareSuggestionWithActual ac ac.value

      indentWidth = String.length indent
      searchWidth = search ++ indent
                    |> String.length
                    |> (\l -> if l == 0
                              then max (String.length placeholder) 6
                              else l)
                    |> (+) 1
      searchInput = Html.input [ Attrs.id Defaults.entryID
                               , Events.onInput EntryInputMsg
                               , Attrs.style [("text-indent", inCh indentWidth)]
                               , Attrs.value search
                               , Attrs.placeholder placeholder
                               , Attrs.spellcheck False
                               , Attrs.autocomplete False
                               , widthInCh searchWidth
                               ] []
      suggestionInput = Html.input [ Attrs.id "suggestionBox"
                                   , Attrs.disabled True
                                   , Attrs.value suggestion
                                   , widthInCh searchWidth
                                   ] []

      input = Html.div
              [ Attrs.id "search-container"
              , widthInCh searchWidth
              ]
              [searchInput, suggestionInput]

      viewForm = Html.form
                 [ Events.onSubmit (EntrySubmitMsg) ]
                 [ input, autocomplete ]

      wrapper = Html.div
                [ Attrs.class "entry"
                , widthInCh searchWidth
                ]
                [ viewForm ]
  in wrapper

entryHtml : Bool -> String -> Autocomplete -> Html.Html Msg
entryHtml allowStringEntry placeholder ac =
  if allowStringEntry && Autocomplete.isStringEntry ac
  then stringEntryHtml ac
  else normalEntryHtml placeholder ac













type alias Collapsed = { name: Maybe String
                       , prefix: List String
                       , verbs: List (String, Pos)}

collapseHandlers : List Toplevel -> List Collapsed
collapseHandlers tls =
  let asCollapsed =
        tls
        |> List.filterMap
            (\tl ->
              case TL.asHandler tl of
                Just h -> Just (tl.pos, h)
                Nothing -> Nothing)
        |> List.map
             (\(pos, h) ->
               { name =
                   case B.flattenFF h.spec.name of
                     F _ s -> Just s
                     Blank _ -> Nothing
                     Flagged _ _ _ _ -> Debug.crash "ch - tls"
               , prefix = []
               , verbs =
                   case B.flattenFF h.spec.modifier of
                     F _ s -> [(s, pos)]
                     Blank _ -> []
                     Flagged _ _ _ _ -> Debug.crash "ch - verbs"
               })
        |> List.sortBy (\c -> Maybe.withDefault "ZZZZZZ" c.name)
  in
    prefixify <|
    List.foldr (\curr list ->
      case list of
        [] -> [curr]
        prev :: rest ->
          if prev.name == curr.name
          then
            let new = { prev | verbs = prev.verbs ++ curr.verbs }
            in new :: rest
          else
            curr :: prev :: rest
    ) [] asCollapsed


prefixify : List Collapsed -> List Collapsed
prefixify hs =
  case hs of
    [] -> hs
    [_] -> hs
    h :: rest ->
      case h.name of
        Nothing -> h :: prefixify rest
        Just name ->
          let len = String.length name
              makePrefix : Collapsed -> Collapsed
              makePrefix h2 =
                case h2.name of
                  Just name2 ->
                    let newName = String.dropLeft len name2 in
                    { h2 | name = Just newName
                         , prefix = h.prefix ++ [name]
                    }
                  _ -> h2
              isPrefixOf h2 =
                case h2.name of
                  Nothing -> False
                  Just n2 -> String.startsWith name n2
          in
          -- this should short circuit immediately when not matching, as
          -- first handler will make the fn succeed
          case LE.splitWhen (\h2 -> not (isPrefixOf h2)) rest of
            Nothing ->
              -- never hits, so everything is prefixed
              h :: (rest |> List.map makePrefix |> prefixify)
            Just (matched, unmatched) ->
              h :: (prefixify <| (List.map makePrefix matched) ++ unmatched)



viewRoutingTable : Model -> Html.Html Msg
viewRoutingTable m =
  let span class subs = Html.span [Attrs.class class] subs
      text class msg = span class [Html.text msg]
      div class subs = Html.div [Attrs.class class] subs

      handlers = m.toplevels |> collapseHandlers
      handlerCount = List.length handlers
      def s = Maybe.withDefault "<not entered>" s
      link h =
        if List.member "GET" (List.map Tuple.first h.verbs)
        then
          case h.name of
            Just n ->
              let source = String.join "" (h.prefix ++ [n]) in
              Html.a [ Attrs.class "external"
                     , Attrs.href source
                     , Attrs.target "_blank"
                     ]
                     [fontAwesome "external-link"]
            Nothing ->
              Html.div [] []
        else
          Html.div [] []

      internalLinks h =
        h.verbs
        |> List.map
          (\(verb, pos) ->
            Html.a
            [ eventNoPropagation "mouseup"
                (\_ -> (NavigateTo (Viewport.urlForPos pos)))
            , Attrs.src ""
            , Attrs.class "verb-link as-pointer"
            ]
            [ Html.text verb ])
        |> List.intersperse (Html.text ",")
      handlerHtml h =
        div "handler" [ div "url"
                          (  List.map (text "prefix") h.prefix
                          ++ [text "name" (def h.name)])
                      , link h
                      , span "verbs" (internalLinks h)
                      ]
      header = div "header"
                 [ text "http" "HTTP"
                 , text "parens" "("
                 , text "count" (toString handlerCount)
                 , text "parens" ")"
                 ]
      routes = div "routes" (List.map handlerHtml handlers)
      html = div "routing-table" [header, routes]

  in placeHtml m (Viewport.toAbsolute m {vx=0, vy=0}) html


placeHtml : Model -> Pos -> Html.Html Msg -> Html.Html Msg
placeHtml m pos html =
  let rcpos = Viewport.toViewport m pos
      div class subs = Html.div [Attrs.class class] subs
  in
  Html.div [ Attrs.class "node"
           , Attrs.style [ ("left", (toString rcpos.vx) ++ "px")
                         , ("top", (toString rcpos.vy) ++ "px")
                         ]
           ]
           [ html ]

axisLine : Model -> Pos -> Html.Html Msg
axisLine m p =
  let px = Viewport.toViewport m p
  in
  Html.div [ Attrs.classList [ ("axis", True)
                             , ("horizontal", p.x > p.y)
                             ]
           , Attrs.style [ ("left", (toString px.vx) ++ "px")
                         , ("top", (toString px.vy) ++ "px")
                         ]
           ] []

decodeClickEvent : (MouseEvent -> a) -> JSD.Decoder a
decodeClickEvent fn =
  let toA : Int -> Int -> Int -> a
      toA px py button =
        fn {pos= {vx=px, vy=py}, button = button}
  in JSDP.decode toA
      |> JSDP.required "pageX" JSD.int
      |> JSDP.required "pageY" JSD.int
      |> JSDP.required "button" JSD.int
