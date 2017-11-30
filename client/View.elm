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

-- dark
import Types exposing (..)
import Util exposing (deMaybe)
import Defaults
import Viewport
import Analysis
import Autocomplete
import ViewAST

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
        allSvgs = xaxis :: yaxis :: (asts ++ entry)
    in allSvgs

viewHoleOrText : Model -> HoleOr String -> Html.Html Msg
viewHoleOrText m h =
  case h of
    Empty hid ->
      case m.state of
        Selecting _ id _ ->
          if hid == id
          then selectedHoleHtml
          else unselectedHoleHtml
        Entering _ (Filling _ id) _ ->
          if hid == id
          then identifierEntryHtml m
          else unselectedHoleHtml
        _ -> unselectedHoleHtml
    Full s -> Html.text s

selectedHoleHtml : Html.Html Msg
selectedHoleHtml =
  Html.div [Attrs.class "selectedHole"] [Html.text "＿＿＿＿＿＿"]

unselectedHoleHtml : Html.Html Msg
unselectedHoleHtml =
  Html.div [] [Html.text "＿＿＿＿＿＿"]

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

      class = case m.state of
          Selecting tlid _ _ ->
            if tlid == tl.id then "selected" else ""
          Entering _ (Filling tlid _) _ ->
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
                 [ Attrs.class "name"]
                 [ Html.text db.name]
      rowdivs = List.map (\(n, t) ->
                           Html.div
                             [ Attrs.class "row" ]
                             [ viewHoleOrText m n, viewHoleOrText m t ])
                         db.rows
  in
  [
    Html.div
      [ Attrs.class "db"]
      (namediv :: rowdivs)
  ]


viewHandler : Model -> Toplevel -> Handler -> List (Html.Html Msg)
viewHandler m tl h =
  let (id, bindHoleHtml, exprHoleHtml) =
        case m.state of
          Selecting tlid id _ ->
            ( id
            , selectedHoleHtml
            , selectedHoleHtml)
          Entering _ (Filling tlid id) _ ->
            ( id
            , identifierEntryHtml m
            , normalEntryHtml m)
          _ -> (ID 0, Html.div [] [], Html.div [] [])

      lvs = Analysis.getLiveValues m tl.id
      ast = Html.div
              [ Attrs.class "ast"]
              [ ViewAST.toHtml
                { holeID = id
                , bindHoleHtml = bindHoleHtml
                , exprHoleHtml = exprHoleHtml
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
      [header, ast]

placeHtml : Model -> Pos -> Html.Html Msg -> Svg.Svg Msg
placeHtml m pos html =
  let rcpos = Viewport.toViewport m pos in
  Svg.foreignObject
    [ SA.x (toString rcpos.vx)
    , SA.y (toString rcpos.vy)
    ]
    [ html ]


viewEntry : Model -> List (Svg.Svg Msg)
viewEntry m =
  if Autocomplete.isStringEntry m.complete
  then viewStringEntry m
  else viewNormalEntry m


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

viewStringEntry : Model -> List (Svg.Svg Msg)
viewStringEntry m =
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
              [ Attrs.id "string-container"
              , Attrs.class "string-container"]
              [ stringInput ]

      viewForm = Html.form
                 [ Events.onSubmit (EntrySubmitMsg) ]
                 [ input ]

      -- outer node wrapper
      classes = "function node string-entry"

      wrapper = Html.div
                [ Attrs.class classes
                , Attrs.width 100]
                [ viewForm ]
      html pos = placeHtml m pos wrapper
  in
    case m.state of
      -- Entering _ (Filling) -> [html m.center]
      Entering _ (Creating pos) _ -> [html pos]
      _ -> []


viewNormalEntry : Model -> List (Svg.Svg Msg)
viewNormalEntry m =
  case m.state of
    Entering _ (Creating pos) _ ->
      [placeHtml m pos (normalEntryHtml m)]
    _ -> []


identifierEntryHtml : Model -> Html.Html Msg
identifierEntryHtml m =
  let
      inputBox =
        Html.input [ Attrs.id Defaults.entryID
                   , Events.onInput EntryInputMsg
                   , Attrs.value m.complete.value
                   , Attrs.spellcheck False
                   , Attrs.autocomplete False
                   ] []

      input = Html.div
              [ Attrs.id "identifier-container"
              , Attrs.class "identifier-container"]
              [ inputBox ]

      viewForm = Html.form
                 [ Events.onSubmit (EntrySubmitMsg) ]
                 [ input ]

      classes = "identifier-entry"
  in Html.div
      [ Attrs.class classes
      , Attrs.width 100]
      [ viewForm ]


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
                          -- (Entering _ (Filling _ (ParamHole _ _ _)), -1) ->
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

      paramInfo =
        case m.state of
          -- Entering _ (Filling _ (ParamHole _ param _)) ->
          --   Html.div [] [ Html.text (param.name ++ " : " ++ RT.tipe2str param.tipe)
          --               , Html.br [] []
          --               , Html.text param.description
          --               ]
          _ -> Html.div [] []

      wrapper = Html.div
                [ Attrs.class "entry"
                , Attrs.width 100]
                [ paramInfo, viewForm ]
  in
      wrapper


escapeCSSName : String -> String
escapeCSSName s =
  Util.replace "[^0-9a-zA-Z_-]" "_" s

-- placeNode : Model -> Node -> Int -> List (Html.Attribute Msg) -> List String -> List (Html.Html Msg) -> List (Html.Html Msg) -> Html.Html Msg
-- placeNode m n width attrs classes header body =
--   let width_attr = Attrs.style [("width", (toString width) ++ "px")]
--       class = String.toLower (toString n.tipe)
--       classStr = String.join " "
--         (["node", class] ++ classes)
--       node = Html.div
--                 (width_attr :: (Attrs.class classStr) :: attrs)
--                 body
--       header_wrapper = Html.div [Attrs.class "header", width_attr ] header
--       events = [ Events.on "mousedown" (decodeClickEvent (NodeClickDown n))
--                , Events.onWithOptions
--                    "mouseup"
--                    { stopPropagation = True, preventDefault = False }
--                    (decodeClickEvent (NodeClickUp n.id))
--                ]
--       wrapper = Html.div events [ node, header_wrapper ]
--   in
--     placeHtml m (G.pos m n) wrapper
--
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

