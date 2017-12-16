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

-- dark
import Types exposing (..)
import Util exposing (deMaybe)
import Defaults
import Viewport
import Analysis
import Autocomplete
import ViewAST
import Toplevel as TL

view : Model -> Html.Html Msg
view m =
  let (w, h) = Util.windowSize ()
      grid = Html.div
               [ Attrs.id "grid"
               , Events.on "mouseup" (decodeClickEvent GlobalClick)
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
viewButtons m = Html.div [Attrs.id "buttons"]
    [ Html.a
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
    , Html.span
      [ Attrs.class "specialButton"]
      [Html.text (toString m.center)]
    , Html.span
      [ Attrs.class "specialButton"]
      [Html.text ("Active tests: " ++ toString m.tests)]
    ]

viewError : Maybe String -> Html.Html Msg
viewError mMsg = case mMsg of
    Just msg ->
      Html.div [Attrs.id "darkErrors"] [Html.text msg]
    Nothing ->
      Html.div [Attrs.id "darkErrors"] [Html.text "Dark"]

viewCanvas : Model -> List (Svg.Svg Msg)
viewCanvas m =
    let
        entry = viewEntry m
        asts = List.map (viewTL m) m.toplevels
        yaxis = svgLine m {x=0, y=2000} {x=0,y=-2000} "" "" [SA.strokeWidth "1px", SA.stroke "#777"]
        xaxis = svgLine m {x=2000, y=0} {x=-2000,y=0} "" "" [SA.strokeWidth "1px", SA.stroke "#777"]
        routing = viewRoutingTable m
        allSvgs = xaxis :: yaxis :: routing :: (asts ++ entry)
    in allSvgs


viewHoleOrText : Model -> HoleOr String -> Html.Html Msg
viewHoleOrText m h =
  case h of
    Empty hid ->
      case unwrapState m.state of
        Selecting _ (Just id) ->
          if hid == id
          then selectedHoleHtml
          else unselectedHoleHtml
        Entering (Filling _ id) ->
          if hid == id
          then entryHtml m
          else unselectedHoleHtml
        _ -> unselectedHoleHtml
    Full id s -> Html.text s -- TODO(ian): add selecting

selectedHoleHtml : Html.Html Msg
selectedHoleHtml =
  Html.div [Attrs.class "hole selected"] [Html.text "＿＿＿＿＿＿"]

unselectedHoleHtml : Html.Html Msg
unselectedHoleHtml =
  Html.div [Attrs.class "hole"] [Html.text "＿＿＿＿＿＿"]

viewTL : Model -> Toplevel -> Svg.Svg Msg
viewTL m tl =
  let body =
        case tl.data of
          TLHandler h ->
            viewHandler m tl h
          TLDB db ->
            viewDB m tl db
      events = [ Events.on "mousedown" (decodeClickEvent (ToplevelClickDown tl))
               , Events.onWithOptions
                   "mouseup"
                   { stopPropagation = True, preventDefault = False }
                   (decodeClickEvent (ToplevelClickUp tl.id))
               ]

      class = case unwrapState m.state of
          Selecting tlid _ ->
            if tlid == tl.id then "selected" else ""
          Entering (Filling tlid _) ->
            if tlid == tl.id then "selected" else ""
          _ -> ""

      html = Html.div
        (Attrs.class ("toplevel " ++ class) :: events)
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
                                 [ viewHoleOrText m n ]
                             , Html.span
                                 [ Attrs.class "type" ]
                                 [ viewHoleOrText m t ]
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
          Selecting tlid (Just id) -> (id, False)
          Entering (Filling tlid id) -> (id, True)
          _ -> (ID 0, False)

      lvs = Analysis.getLiveValuesDict m tl.id
      ast = Html.div
              [ Attrs.class "ast"]
              [ ViewAST.toHtml
                { selectedID = id
                , isFilling = filling
                , fillingHtml = entryHtml m
                , liveValues = lvs }
                h.ast]
      header =
        Html.div
          [Attrs.class "header"]
          [ Html.div
            [ Attrs.class "module"]
            [ viewHoleOrText m h.spec.module_]
          , Html.div
            [ Attrs.class "name"]
            [ viewHoleOrText m h.spec.name]
          , Html.div
            [Attrs.class "modifier"]
            [ viewHoleOrText m h.spec.modifier]]
  in
      [ast, header]


viewEntry : Model -> List (Svg.Svg Msg)
viewEntry m =
  case m.state of
    Entering (Creating pos) ->
      [placeHtml m pos (entryHtml m)]
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
  |> Debug.log "toStringEntry"

transformFromStringEntry : String -> String
transformFromStringEntry s =
  let s2 = s
           |> Util.replace "\"" "\\\""
  in
  "\"" ++ s2 ++ "\""
  |> Debug.log "fromStringEntry"

stringEntryHtml : Model -> Html.Html Msg
stringEntryHtml m =
  let
      -- stick with the overlapping things for now, just ignore the back
      -- one
      value = transformToStringEntry m.complete.value

      smallInput =
        Html.input [ Attrs.id Defaults.entryID
                   , Events.onInput (EntryInputMsg << transformFromStringEntry)
                   , Attrs.value value
                   , Attrs.spellcheck False
                   , Attrs.autocomplete False
                   ] []


      largeInput =
        Html.textarea [ Attrs.id Defaults.entryID
                      , Events.onInput (EntryInputMsg << transformFromStringEntry)
                      , Attrs.value value
                      , Attrs.spellcheck False
                      , Attrs.cols 50
                      , Attrs.rows (5 + SE.countOccurrences "\n" value)
                      , Attrs.autocomplete False
                      ] []

      stringInput = if Autocomplete.isSmallStringEntry m.complete
                    then smallInput
                    else largeInput

      input = Html.div
              [ Attrs.class "string-container"]
              [ stringInput ]

      viewForm = Html.form
                 [ Events.onSubmit (EntrySubmitMsg) ]
                 [ input ]

      -- outer node wrapper
      classes = "string-entry"

      wrapper = Html.div
                [ Attrs.class classes
                , Attrs.width 100]
                [ viewForm ]
  in wrapper


normalEntryHtml : Model -> Html.Html Msg
normalEntryHtml m =
  let autocompleteList =
        (List.indexedMap
           (\i item ->
              let highlighted = m.complete.index == i
                  hlClass = if highlighted then " highlighted" else ""
                  class = "autocomplete-item" ++ hlClass
                  str = Autocomplete.asName item
                  name = Html.span [] [Html.text str]
                  types = Html.span
                    [Attrs.class "types"]
                    [Html.text <| Autocomplete.asTypeString item ]
              in Html.li
                [ Attrs.class class ]
                [name, types])
           m.complete.completions)

      autocompletions = case (m.state, m.complete.index) of
                          -- (Entering (Filling _ (ParamHole _ _ _)), -1) ->
                          --   [ Html.li
                          --     [ Attrs.class "autocomplete-item greyed" ]
                          --     [ Html.text "Press down to autocomplete…" ]
                          --   ]
                          _ -> autocompleteList


      autocomplete = Html.ul
                     [ Attrs.id "autocomplete-holder" ]
                     autocompletions


      -- two overlapping input boxes, one to provide suggestions, one
      -- to provide the search
      (indent, suggestion, search) =
        Autocomplete.compareSuggestionWithActual m.complete m.complete.value

      indentHtml = "<span style=\"font-family:sans-serif; font-size:14px;\">" ++ indent ++ "</span>"
      (width, _) = Util.htmlSize indentHtml
      w = toString width ++ "px"
      searchInput = Html.input [ Attrs.id Defaults.entryID
                               , Events.onInput EntryInputMsg
                               , Attrs.style [("text-indent", w)]
                               , Attrs.value search
                               , Attrs.spellcheck False
                               , Attrs.autocomplete False
                               ] []
      suggestionInput = Html.input [ Attrs.id "suggestionBox"
                                   , Attrs.disabled True
                                   , Attrs.value suggestion
                                   ] []

      input = Html.div
              [Attrs.id "search-container"]
              [searchInput, suggestionInput]

      viewForm = Html.form
                 [ Events.onSubmit (EntrySubmitMsg) ]
                 [ input, autocomplete ]

      wrapper = Html.div
                [ Attrs.class "entry"
                , Attrs.width 100]
                [ viewForm ]
  in wrapper

entryHtml : Model -> Html.Html Msg
entryHtml m =
  if Autocomplete.isStringEntry m.complete
  then stringEntryHtml m
  else normalEntryHtml m

type alias Collapsed = { name: Maybe String
                       , prefix: List String
                       , verbs: List String}

collapseHandlers : List Handler -> List Collapsed
collapseHandlers handlers =
  let asCollapsed =
        handlers
        |> List.map (\h -> { name = case h.spec.name of
                                      Full _ s -> Just s
                                      Empty _ -> Nothing
                           , prefix = []
                           , verbs = case h.spec.modifier of
                                       Full _ s -> [s]
                                       Empty _ -> []
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

      handlers = TL.handlers m.toplevels |> collapseHandlers
      handlerCount = List.length handlers
      missing = text "no-handlers" "No HTTP handlers yet"
      def s = Maybe.withDefault "<not entered>" s
      link h =
        if List.member "GET" h.verbs
        then
          case h.name of
            Just n ->
              let source = String.join "" (h.prefix ++ [n]) in
              Html.a [ Attrs.class "external"
                     , Attrs.href source
                     , Attrs.target "_blank"
                     ]
                     [Html.i [Attrs.class "fa fa-external-link"] []]
            Nothing ->
              Html.div [] []
        else
          Html.div [] []
      handlerHtml h =
        div "handler" [ div "name"
                          (  List.map (text "p") h.prefix
                          ++ [text "n" (def h.name)])
                      , link h
                      , span "verbs"
                          (List.map (text "verb") h.verbs)
                      ]
      header = div "header"
                 [ text "http" "HTTP"
                 , text "parens" "("
                 , text "count" (toString handlerCount)
                 , text "parens" ")"
                 ]
      routes = div "routes" (List.map handlerHtml handlers)
      html = div "routing-table" [header, routes]

  in placeHtml m {x=0, y=0}
       (if handlers == []
       then missing
       else html)


escapeCSSName : String -> String
escapeCSSName s =
  Util.replace "[^0-9a-zA-Z_-]" "_" s


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

