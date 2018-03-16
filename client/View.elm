module View exposing (view)

-- builtin
import Json.Decode as JSD
import Json.Decode.Pipeline as JSDP

-- lib
import Svg
import Svg.Attributes as SA
import Html
import Html.Attributes as Attrs
import Html.Events as Events
import VirtualDom
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
import ViewAST
import Toplevel as TL
import Pointer as P
import AST
import Runtime as RT
import Blank

fontAwesome : String -> Html.Html Msg
fontAwesome name =
  Html.i [Attrs.class ("fa fa-" ++ name)] []

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
               , Svg.svg
                 [ SA.width "100%"
                 , SA.height (toString h) ]
                 (viewCanvas m)
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
          [ Events.onWithOptions
              "mouseup"
              { stopPropagation = True, preventDefault = False }
              (decodeClickEvent (\_ -> FinishIntegrationTest))
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
      Html.div [Attrs.id "darkErrors"] [Html.text msg]
    Nothing ->
      Html.text "Dark"

viewCanvas : Model -> List (Svg.Svg Msg)
viewCanvas m =
    let
        entry = viewEntry m
        asts = List.map (viewTL m) m.toplevels
        yaxis = svgLine m {x=0, y=6000} {x=0,y=-6000} "" "" [SA.strokeWidth "1px", SA.stroke "#777"]
        xaxis = svgLine m {x=6000, y=0} {x=-6000,y=0} "" "" [SA.strokeWidth "1px", SA.stroke "#777"]
        routing = viewRoutingTable m
        allSvgs = xaxis :: yaxis :: routing :: (asts ++ entry)
    in allSvgs

viewBlankOrText : Model -> Toplevel -> PointerType -> BlankOr String -> HoverData -> Html.Html Msg
viewBlankOrText = viewBlankOr Html.text


type alias HoverData = Maybe (Result String String)
viewBlankOr : (a -> Html.Html Msg) -> Model -> Toplevel -> PointerType -> BlankOr a -> HoverData -> Html.Html Msg
viewBlankOr htmlFn m tl pt b hoverdata =
  let pointer = Blank.toP pt b
      id = P.idOf pointer
      param =
        tl
        |> TL.asHandler
        |> Maybe.map .ast
        |> Maybe.andThen
            (\ast ->
              let parent = AST.parentOf_ id ast
                  inThread = AST.grandparentIsThread ast parent
              in
              case parent of
                Just (F _ (FnCall name args)) ->
                  let index =
                        args
                        |> LE.findIndex (\a -> Blank.toID a == id)
                        |> Maybe.withDefault -1000
                        |> \i -> if inThread
                                 then i + 1
                                 else i
                  in
                  case Autocomplete.findFunction m.complete name of
                    Just {parameters} ->
                      LE.getAt index parameters
                    Nothing -> Nothing
                _ -> Nothing)
      paramPlaceholder =
        param
        |> Maybe.map (\p -> p.name ++ ": " ++ RT.tipe2str p.tipe ++ "")
        |> Maybe.withDefault ""

      selected = case unwrapState m.state of
                   Selecting _ (Just p) ->
                     if P.idOf p == Blank.toID b
                     then DivSelected
                     else DivUnselected
                   _ -> DivUnselected
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
      thisText = case b of
                   Blank _ ->
                     Html.div
                       [Attrs.class "blank"]
                       [Html.text placeholder]
                   F _ fill -> fill |> htmlFn
      allowStringEntry = pt == Expr
      text = case unwrapState m.state of
               Entering (Filling tlid p) ->
                 if pointer == p
                 then entryHtml allowStringEntry placeholder m
                 else thisText
               _ -> thisText
      onClick = if selected == DivSelected
                then Nothing
                else Just (tl.id, pointer)
      mouseOvered =
        case m.hovering |> List.head of
          Nothing -> MouseNotOverDiv
          Just i ->
            if (P.idOf i) == (P.idOf pointer)
            then MouseOverDiv
            else MouseNotOverDiv
  in html4blank selected mouseOvered [] onClick hoverdata [text]

html4blank : DivSelected -> MouseOverDiv -> List Class -> Clickable -> HoverData -> List (Html.Html Msg) -> Html.Html Msg
html4blank selected mouseover classes clickable hoverdata content =
  let events = case clickable of
                 Nothing -> []
                 Just (tlid, pointer) ->
                   -- click so that dragging still works
                   [Events.onWithOptions "mouseup"
                     -- only the leafiest node should be selected, so
                     -- don't let this propagate to ancestors
                     { stopPropagation = True
                     , preventDefault = False
                     }
                     (decodeClickEvent (ToplevelClickUp tlid (Just pointer)))
                   ,Events.onWithOptions "mouseenter"
                     { stopPropagation = True
                     , preventDefault = False
                     }
                     (decodeClickEvent (MouseEnter pointer))
                   ,Events.onWithOptions "mouseleave"
                     { stopPropagation = True
                     , preventDefault = False
                     }
                     (decodeClickEvent (MouseLeave pointer))
                   ]
      (valClass, title) =
        case hoverdata of
          Nothing -> ([], [])
          Just (Ok msg) -> ([], [Attrs.title msg])
          Just (Err err) -> (["value-error"], [Attrs.title err])

      allClasses = classes
                ++ valClass
                ++ (if selected == DivSelected
                    then ["selected"]
                    else [])
                ++ (if mouseover == MouseOverDiv
                    then ["mouseovered"]
                    else [])

  in
  Html.div
    (events ++ title ++ [Attrs.class (String.join " " allClasses)])
    content

viewTL : Model -> Toplevel -> Svg.Svg Msg
viewTL m tl =
  let body =
        case tl.data of
          TLHandler h ->
            viewHandler m tl h
          TLDB db ->
            viewDB m tl db
      events = [ Events.onWithOptions
                   "mousedown"
                   { stopPropagation = True, preventDefault = False }
                   (decodeClickEvent (ToplevelClickDown tl))
               , Events.onWithOptions
                   "mouseup"
                   { stopPropagation = True, preventDefault = False }
                   (decodeClickEvent (ToplevelClickUp tl.id Nothing))
               ]

      selected = if Just tl.id == tlidOf m.state
                 then "selected"
                 else ""
      class = [selected, toString (deTLID tl.id), "toplevel", "cursor-" ++ (toString tl.cursor)]
              |> String.join " "

      html = Html.div
        (Attrs.class class :: events)
        body

  in
      placeHtml m tl.pos html

viewDB : Model -> Toplevel -> DB -> List (Html.Html Msg)
viewDB m tl db =
  let namediv = Html.div
                 [ Attrs.class "dbname"]
                 [ Html.text db.name]
      coldivs = List.map (\(n, t) ->
                           Html.div
                             [ Attrs.class "col" ]
                             [ Html.span
                                 [ Attrs.class "name" ]
                                 [ viewBlankOrText m tl DBColName n Nothing ]
                             , Html.span
                                 [ Attrs.class "type" ]
                                 [ viewBlankOrText m tl DBColType t Nothing ]
                             ])
                         db.cols
  in
  [
    Html.div
      [ Attrs.class "db"]
      (namediv :: coldivs)
  ]

viewBlankOrDarkType : Model -> Toplevel -> BlankOr DarkType -> Html.Html Msg
viewBlankOrDarkType m tl b =
  viewBlankOr (viewDarkType m tl) m tl DarkType b Nothing

viewDarkType : Model -> Toplevel -> DarkType -> Html.Html Msg
viewDarkType m tl d =
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
                     [viewBlankOrText m tl DarkTypeField n Nothing]
                 , Html.span [Attrs.class "colon"] [Html.text ":"]
                 , Html.span
                     [Attrs.class "fieldvalue"]
                     [viewBlankOrDarkType m tl dt]
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
          Selecting tlid (Just p) -> (P.idOf p, False)
          Entering (Filling tlid p) -> (P.idOf p, True)
          _ -> (ID 0, False)

      hovering =
        case m.hovering |> List.head of
          Just p -> P.idOf p
          Nothing -> ID 0

      lvs = Analysis.getLiveValuesDict m tl.id
      ast = Html.div
              [ Attrs.class "ast"]
              [ ViewAST.toHtml
                { selectedID = id
                , tlid = tl.id
                , hoveredID = hovering
                , viewBlankOr = viewBlankOrText m tl
                , html4blank = html4blank
                , liveValues = lvs
                , functions = m.complete.functions}
                h.ast]

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
          , viewBlankOrDarkType m tl h.spec.types.input]
      output =
        Html.div
          [Attrs.class "spec-type output-type"]
          [ Html.span [Attrs.class "header"] [Html.text "Output:"]
          , viewBlankOrDarkType m tl h.spec.types.output]

      header =
        Html.div
          [Attrs.class "header"]
          [ Html.div
            [ Attrs.class "name"]
            [ viewBlankOrText m tl EventName h.spec.name Nothing ]
          , Html.div
            [ Attrs.class "modifier" ]
            ( externalLink ++
              [ Html.div
                [ Attrs.class "module" ]
                [ viewBlankOrText m tl EventSpace h.spec.module_ Nothing ]
              , viewBlankOrText m tl EventModifier h.spec.modifier Nothing
              ]
            )
          ]
  in [header, input, ast, output]


viewEntry : Model -> List (Svg.Svg Msg)
viewEntry m =
  case unwrapState m.state of
    Entering (Creating pos) ->
      let html = Html.div [Attrs.class "omnibox"] [entryHtml True "" m]
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

nothingMouseEvent : String -> Html.Attribute Msg
nothingMouseEvent name =
  Events.onWithOptions
    name
    { stopPropagation = True, preventDefault = False}
    (decodeClickEvent NothingClick)

stringEntryHtml : Model -> Html.Html Msg
stringEntryHtml m =
  let
      -- stick with the overlapping things for now, just ignore the back
      -- one
      value = transformToStringEntry m.complete.value
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
    if Autocomplete.isSmallStringEntry m.complete
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



normalEntryHtml : String -> Model -> Html.Html Msg
normalEntryHtml placeholder m =
  let autocompleteList =
        (List.indexedMap
           (\i item ->
              let highlighted = m.complete.index == i
                  hlClass = if highlighted then " highlighted" else ""
                  name = Autocomplete.asName item
              in Html.li
                [ Attrs.class <| "autocomplete-item" ++ hlClass
                , Events.onWithOptions
                    "mouseup"
                    { stopPropagation = True, preventDefault = False }
                    (decodeClickEvent (\_ -> AutocompleteClick name))
                ]
                [ Html.text name
                , Html.span
                    [Attrs.class "types"]
                    [Html.text <| Autocomplete.asTypeString item ]
                ])
           (List.concat m.complete.completions))

      autocomplete = Html.ul
                     [ Attrs.id "autocomplete-holder" ]
                     autocompleteList


      -- two overlapping input boxes, one to provide suggestions, one
      -- to provide the search
      (indent, suggestion, search) =
        Autocomplete.compareSuggestionWithActual m.complete m.complete.value

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

entryHtml : Bool -> String -> Model -> Html.Html Msg
entryHtml allowStringEntry placeholder m =
  if allowStringEntry && Autocomplete.isStringEntry m.complete
  then stringEntryHtml m
  else normalEntryHtml placeholder m

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
        |> List.map (\(pos, h) -> { name = case h.spec.name of
                                      F _ s -> Just s
                                      Blank _ -> Nothing
                                  , prefix = []
                                  , verbs = case h.spec.modifier of
                                             F _ s -> [(s, pos)]
                                             Blank _ -> []
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



viewRoutingTable : Model -> Svg.Svg Msg
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
            [ Events.onWithOptions
              "mouseup"
              { stopPropagation = True, preventDefault = False }
              (decodeClickEvent (\_ -> (NavigateTo (Viewport.urlForPos pos))))
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


placeHtml : Model -> Pos -> Html.Html Msg -> Svg.Svg Msg
placeHtml m pos html =
  let rcpos = Viewport.toViewport m pos in
  Svg.foreignObject
    [ SA.x (toString rcpos.vx)
    , SA.y (toString rcpos.vy)
    ]
    [ html ]

svgLine : Model -> Pos -> Pos -> String -> String -> List (Svg.Attribute Msg) -> Svg.Svg Msg
svgLine m p1a p2a sourcedebug targetdebug attrs =
  let p1v = Viewport.toViewport m p1a
      p2v = Viewport.toViewport m p2a
  in
  Svg.line
    ([ SA.x1 (toString p1v.vx)
     , SA.y1 (toString p1v.vy)
     , SA.x2 (toString p2v.vx)
     , SA.y2 (toString p2v.vy)
     , VirtualDom.attribute "source" sourcedebug
     , VirtualDom.attribute "target" targetdebug
     ] ++ attrs)
    []

decodeClickEvent : (MouseEvent -> a) -> JSD.Decoder a
decodeClickEvent fn =
  let toA : Int -> Int -> Int -> a
      toA px py button =
        fn {pos= {vx=px, vy=py}, button = button}
  in JSDP.decode toA
      |> JSDP.required "pageX" JSD.int
      |> JSDP.required "pageY" JSD.int
      |> JSDP.required "button" JSD.int

