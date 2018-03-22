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
-- import Maybe.Extra as ME

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

viewBlankOrText : Model -> Toplevel -> PointerType -> BlankOr String -> Html.Html Msg
viewBlankOrText = viewBlankOr Html.text

viewDarkType : Model -> Toplevel -> DarkType -> Html.Html Msg
viewDarkType m tl b =
  viewBlankOr (viewNDarkType m tl) m tl DarkType b

viewExpr : Model -> Toplevel -> Expr -> Html.Html Msg
viewExpr m tl e =
  viewBlankOr (viewNExpr m tl) m tl Expr e



viewBlankOr : (a -> Html.Html Msg) -> Model -> Toplevel -> PointerType -> BlankOr a -> Html.Html Msg
viewBlankOr htmlFn m tl pt b =
  let pointer = B.toP pt b
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

      thisTextFn bo = case bo of
                        Blank _ ->
                          Html.div
                            [Attrs.class "blank"]
                            [Html.text placeholder]
                        F _ fill -> htmlFn fill
                        Flagged _ _ _ _ -> Debug.crash "vbo"

      thisText = case b of
                   Flagged msg setting l r ->
                     Html.div
                       [Attrs.class "flagged"]
                       [ Html.text msg
                       , Html.text (toString setting)
                       , thisTextFn l
                       , thisTextFn r]
                   _ -> thisTextFn b

      allowStringEntry = pt == Expr

      text = case unwrapState m.state of
               Entering (Filling _ p) ->
                 if pointer == p
                 then entryHtml allowStringEntry placeholder m.complete
                 else thisText
               _ -> thisText

      featureFlag = viewFeatureFlag selected

      lvs = Analysis.getLiveValuesDict m tl.id
      hoverdata =
        id
        |> deID
        |> \id -> Dict.get id lvs
        |> Maybe.map .value
        |> Maybe.map (\v -> if Runtime.isError v
                            then Err (Runtime.extractErrorMessage v)
                            else Ok v)

      (valClass, title) =
        case hoverdata of
          Nothing -> ([], [])
          Just (Ok msg) -> ([], [Attrs.title msg])
          Just (Err err) -> (["value-error"], [Attrs.title err])

      selected = case unwrapState m.state of
                   Selecting _ (Just p) ->
                     if P.toID p == B.toID b
                     then DivSelected
                     else DivUnselected
                   _ -> DivUnselected
      mouseover =
        case m.hovering |> List.head of
          Nothing -> MouseNotOverDiv
          Just hid ->
            if hid == P.toID pointer
            then MouseOverDiv
            else MouseNotOverDiv

      allClasses = valClass
                ++ (if selected == DivSelected
                    then ["selected"]
                    else [])
                ++ (if mouseover == MouseOverDiv
                    then ["mouseovered"]
                    else [])
      events =
        case selected of
          DivUnselected -> []
          DivSelected ->
            -- click so that dragging still works
            -- only the leafiest node should be selected, so
            -- don't let this propagate to ancestors
            [ eventNoPropagation "mouseup"
                (ToplevelClickUp tl.id (Just pointer))
            , eventNoPropagation "mouseenter" (MouseEnter id)
            , eventNoPropagation "mouseleave" (MouseLeave id)
            ]

  in
  Html.div
    []
    (( Html.div
      (events ++ title ++ [Attrs.class (String.join " " allClasses)])
      [text]
      ) :: featureFlag)


viewFeatureFlag : DivSelected -> List (Html.Html Msg)
viewFeatureFlag selected =
  if selected == DivSelected
  then [Html.div
          [ Attrs.class "feature-flag"
          , Events.onMouseDown StartFeatureFlag]
          [fontAwesome "flag"] ]
  else []



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
                                 [ viewBlankOrText m tl DBColName n ]
                             , Html.span
                                 [ Attrs.class "type" ]
                                 [ viewBlankOrText m tl DBColType t ]
                             ])
                         db.cols
  in
  [
    Html.div
      [ Attrs.class "db"]
      (namediv :: coldivs)
  ]


viewNDarkType : Model -> Toplevel -> NDarkType -> Html.Html Msg
viewNDarkType m tl d =
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
                     [viewBlankOrText m tl DarkTypeField n ]
                 , Html.span [Attrs.class "colon"] [Html.text ":"]
                 , Html.span
                     [Attrs.class "fieldvalue"]
                     [viewDarkType m tl dt]
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
              [ viewExpr m tl h.ast ]

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
          , viewDarkType m tl h.spec.types.input]
      output =
        Html.div
          [Attrs.class "spec-type output-type"]
          [ Html.span [Attrs.class "header"] [Html.text "Output:"]
          , viewDarkType m tl h.spec.types.output]

      header =
        Html.div
          [Attrs.class "header"]
          [ Html.div
            [ Attrs.class "name"]
            [ viewBlankOrText m tl EventName h.spec.name ]
          , Html.div
            [ Attrs.class "modifier" ]
            ( externalLink ++
              [ Html.div
                [ Attrs.class "module" ]
                [ viewBlankOrText m tl EventSpace h.spec.module_ ]
              , viewBlankOrText m tl EventModifier h.spec.modifier
              ]
            )
          ]
  in [header, ast]


viewNExpr : Model -> Toplevel -> NExpr -> Html.Html Msg
viewNExpr m tl e =
  let asClass cls = cls |> String.join " " |> Attrs.class
      text cls str =
        Html.div
          [asClass cls]
          [Html.text str]
      wrap cls item =
        Html.div
          [asClass cls]
          [item]
      atom cls str =
        text ("atom" :: cls) str
      keyword str =
        atom [str, "keyword"] str
      nested cls items =
        Html.div
          [asClass ("nested" :: cls)]
          items
      vExpr = viewExpr m tl
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
      atom [cssClass, "value"] valu

    Variable name ->
      atom ["variable"] name

    Let lhs rhs body ->
      let viewLHS = viewBlankOr (Html.text) m tl VarBind lhs in
      nested ["letexpr"]
        [ keyword "let"
        , nested ["letbinding"]
            [ wrap ["atom", "letvarname"] viewLHS
            , atom ["letbind"] "="
            , wrap ["letrhs"] (vExpr rhs)
            ]
        , wrap ["letbody"] (vExpr body)
        ]

    If cond ifbody elsebody ->
      nested ["ifexpr"]
      [ keyword "if"
      , wrap ["cond"] (vExpr cond)
      , wrap ["ifbody"] (vExpr ifbody)
      , keyword "else"
      , wrap ["elsebody"] (vExpr elsebody)
      ]

    FnCall name exprs ->
      let fnname = case String.split "::" name of
                     [mod, n] ->
                       nested ["namegroup", "atom"]
                         [ text ["module"] mod
                         , text ["moduleseparator"] "::"
                         , text ["fnname"] n
                         ]
                     _ -> atom ["fnname"] name
          fnDiv = wrap ["op", name] fnname
          isInfix = m.complete.functions
                    |> LE.find (\f -> f.name == name)
                    |> deMaybe "vExpr fncall"
                    |> .infix
      in
      case (isInfix, exprs) of
        (True, [first, second]) ->
          nested ["fncall", "infix"]
          [ wrap ["lhs"] (vExpr first)
          , fnDiv
          , wrap ["rhs"] (vExpr second)
          ]
        _ ->
          nested ["fncall", "prefix"]
            (fnDiv :: List.map vExpr exprs)

    Lambda vars expr ->
      let varname v = text ["lambdavarname", "atom"] v in
      nested ["lambdaexpr"]
        [ nested ["lambdabinding"] (List.map varname vars)
        , atom ["arrow"] "->"
        , nested ["lambdabody"] [vExpr expr]
        ]

    Thread exprs ->
      let pipe = atom ["thread", "pipe"] "|>"
          texpr e = nested ["threadmember"] [pipe, vExpr e]
      in
      nested ["threadexpr"] (List.map texpr exprs)



    FieldAccess obj field ->
      let viewFieldName  =
            viewBlankOr (Html.text) m tl Field field
      in
      nested ["fieldaccessexpr"]
        [ wrap ["fieldobject"] (vExpr obj)
        , text ["fieldaccessop", "operator", "atom"] "."
        , wrap ["fieldname", "atom"] viewFieldName
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
