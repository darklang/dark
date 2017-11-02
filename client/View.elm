module View exposing (view)

-- builtin
import Json.Decode as JSD
import Json.Decode.Pipeline as JSDP
import Regex exposing (regex)

-- lib
import Svg
import Svg.Attributes as SA
import Html
import Html.Attributes as Attrs
import Html.Events as Events
import VirtualDom

-- dark
import Types exposing (..)
import Util exposing (deMaybe)
import Entry
import Graph as G
import Node as N
import Defaults
import Viewport
import Runtime as RT
import Selection
import Autocomplete
import VariantTesting as VT

view : Model -> Html.Html Msg
view m =
  let (w, h) = Util.windowSize ()
      grid = Html.div
               ([ Attrs.id "grid"
               , Events.on "mouseup" (decodeClickEvent GlobalClick)
               ] ++ List.map (\x -> (Attrs.class << VT.toCSSClass) x) m.tests)
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
    let ordered = G.orderedNodes m
        nodes = List.indexedMap (\i n -> viewNode m n i) ordered
        values = ordered |> List.map (viewValue m) |> List.concat
        (edges, edgeMaybeLabels) = ordered |> List.map (viewNodeEdges m) |> List.concat |> List.unzip
        edgeLabels = edgeMaybeLabels |> List.filterMap identity
        entry = viewEntry m
        yaxis = svgLine m {x=0, y=2000} {x=0,y=-2000} "" "" [SA.strokeWidth "1px", SA.stroke "#777"]
        xaxis = svgLine m {x=2000, y=0} {x=-2000,y=0} "" "" [SA.strokeWidth "1px", SA.stroke "#777"]

        allSvgs = xaxis :: yaxis :: (edges ++ values ++ nodes ++ entry ++ edgeLabels)
    in allSvgs

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
                          (Entering _ (Filling _ (ParamHole _ _ _)), -1) ->
                            [ Html.li
                              [ Attrs.class "autocomplete-item greyed" ]
                              [ Html.text "Press down to autocomplete…" ]
                            ]
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
      w = width |> toString
      searchInput = Html.input [ Attrs.id Defaults.entryID
                               , Events.onInput EntryInputMsg
                               , Attrs.style [("text-indent", w ++ "px")]
                               , Attrs.value search
                               , Attrs.spellcheck False
                               , Attrs.autocomplete False
                               ] []
      suggestionInput = Html.input [ Attrs.id "suggestion"
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
          Entering _ (Filling _ (ParamHole _ param _)) ->
            Html.div [] [ Html.text (param.name ++ " : " ++ RT.tipe2str param.tipe)
                        , Html.br [] []
                        , Html.text param.description
                        ]
          _ -> Html.div [] []

      -- outer node wrapper
      classes = "selection function node entry"

      wrapper = Html.div
                [ Attrs.class classes
                , Attrs.width 100]
                [ paramInfo, viewForm ]
      html pos = placeHtml m pos wrapper
  in
    case m.state of
      Entering _ (Filling n h) ->
        let holePos = holeDisplayPos m h
            edgePos = { x = holePos.x + 10
                      , y = holePos.y + 10}
            nodePos = { x = G.posx m n + 10
                      , y = G.posy m n + 10}
        in
        [svgLine m nodePos edgePos "" "" edgeStyle, html holePos]
      Entering _ (Creating pos) -> [html pos]
      _ -> []


valueDisplayPos : Model -> Node -> Pos
valueDisplayPos m n =
  if (G.outgoingNodes m n |> List.length |> (==) 1) && n.isBlockParent
  then Entry.holeCreatePos m (ResultHole n)
  else
    let xpad = max (N.nodeWidth n + 50) 400
    in {x=(G.posx m n)+xpad, y=G.posy m n}

holeDisplayPos : Model -> Hole -> Pos
holeDisplayPos m hole =
  case hole of
    ResultHole _ -> let {x,y} = Entry.holeCreatePos m hole
                    in {x=x, y=y + 50}
    ParamHole n _ _ -> {x=(G.posx m n)-350, y=(G.posy m n)-100}



viewValue : Model -> Node -> List (Html.Html Msg)
viewValue m n =
  let valueStr val tipeStr =
        val
          |> String.trim
          |> String.left 120
          |> Util.replace "\n" ""
          |> Util.replace "\r" ""
          |> Util.replace "\\s+" " "
          |> (\s -> if String.length s > 54
                    then String.left 54 s ++ "…" ++ String.right 1 (String.trim val)
                    else s)
          |> \v -> v ++ " :: " ++ tipeStr
          |> Html.text
      -- lv = case Dict.get (n.id |> deID) m.phantoms of
      --   Nothing -> n.liveValue
      --   Just pn -> pn.liveValue
      lv = n.liveValue
      isPhantom = lv /= n.liveValue
      class = if isPhantom then "phantom" else "preview"
      newPos = valueDisplayPos m n
      displayedBelow = newPos.y /= G.posy m n
      edge =
        if displayedBelow
        then [svgLine m {x=G.posx m n + 10, y=G.posy m n +10} {x=newPos.x+10,y=newPos.y+10} "" "" edgeStyle]
        else []
      allOutputs = edge ++
                    [placeHtml m newPos
                        (case lv.exc of
                          Nothing -> Html.pre
                                      [Attrs.class class, Attrs.title lv.value]
                                      [valueStr lv.value (RT.tipe2str lv.tipe)]
                          Just exc -> Html.span
                                        [ Attrs.class <| "unexpected " ++ class
                                        , Attrs.title
                                            ( "Problem: " ++ exc.short
                                            ++ "\n\nActual value: " ++ exc.actual
                                            ++ "\n\nExpected: " ++ exc.expected
                                            ++ "\n\nMore info: " ++ exc.long
                                          ) ]
                                        [ Html.pre
                                          [ ]
                                          [ valueStr exc.result exc.resultType ]
                                        , Html.span
                                            [Attrs.class "info" ]
                                            [Html.text "ⓘ "]
                                        , Html.span
                                            [Attrs.class "explanation" ]
                                            [Html.text exc.short ]])]
  in if G.hasRelativePos n
     then []
     else allOutputs

getClass : String -> String
getClass func = case String.slice 0 6 func of
                "if"   -> "conditional"
                "else" -> "conditional"
                "then" -> "conditional"
                "val" -> "iter"
                "char" -> "iter"
                "List::" -> if Regex.contains (regex "foreach|filter|fold|find_first") func then "iter" else "name"
                "String" -> if Regex.contains (regex "foreach|filter|fold|find_first") func then "iter" else "name"
                _ -> "name"

viewNode : Model -> Node -> Int -> Html.Html Msg
viewNode m n i =
  let
      -- header
      header = [ Html.span
                   [Attrs.class "letter"]
                   [Html.text (Util.int2letter i)]
                   --  [Html.text (toString <| deID <| n.id)]
               ]

      -- heading
      paramtext = if N.hasFace n
                  then [("arg_const", n.face)]
                  else
                    n.arguments
                            |> List.map
                                (\(p, a) ->
                                  if p.tipe == TBlock
                                  then ("", "")
                                  else
                                    case (a, p) of
                                      (Const c, _) -> ("arg_const", if c == "null" then "∅" else c)
                                      (NoArg, _) -> ("arg_none", "◉")
                                      (Edge _ _, _) -> ("arg_edge", "◉"))
      params = List.map (\(class, val) ->
        Html.span
        [ Attrs.class class]
        [ Html.text <| " " ++ val]) paramtext

      heading = Html.span
                [ Attrs.class "title"]
                ((Html.span [Attrs.class <| getClass n.name] [Html.text n.name]) :: params)

   in
    placeNode
      m
      n
      (N.nodeWidth n)
      []
      []
      header
      [heading]

placeNode : Model -> Node -> Int -> List (Html.Attribute Msg) -> List String -> List (Html.Html Msg) -> List (Html.Html Msg) -> Html.Html Msg
placeNode m n width attrs classes header body =
  let width_attr = Attrs.style [("width", (toString width) ++ "px")]
      selectedCl = if Selection.isSelected m n then ["selected"] else []
      class = String.toLower (toString n.tipe)
      classStr = String.join " " (["node", class] ++ [n.name] ++ selectedCl ++ classes)
      node = Html.div
                (width_attr :: (Attrs.class classStr) :: attrs)
                body
      header_wrapper = Html.div [Attrs.class "header", width_attr ] header
      events = [ Events.on "mousedown" (decodeClickEvent (NodeClickDown n))
               , Events.onWithOptions
                   "mouseup"
                   { stopPropagation = True, preventDefault = False }
                   (decodeClickEvent (NodeClickUp n.id))
               ]
      wrapper = Html.div events [ node, header_wrapper ]
  in
    placeHtml m (G.pos m n) wrapper

edgeStyle : List (Svg.Attribute Msg)
edgeStyle =
  [ SA.strokeWidth Defaults.edgeSize
  , SA.stroke Defaults.edgeStrokeColor
  ]

viewNodeEdges : Model -> Node -> List (Svg.Svg Msg, Maybe (Svg.Svg Msg))
viewNodeEdges m n = n.arguments
                    |> List.map Tuple.second
                    |> List.filter N.isParentEdge
                    |> (List.map <| viewEdge m n)

viewEdge : Model -> Node -> Argument -> (Svg.Svg Msg, Maybe (Svg.Svg Msg))
viewEdge m target edge =
    let source = edge |> N.getParentID |> deMaybe |> G.getNodeExn m
        targetPos = target.pos
        (sourceW, sourceH) = N.nodeSize source
        (targetW, targetH) = N.nodeSize target
        spos = { x = G.posx m source + 10
               , y = G.posy m source + (sourceH // 2)}
        tpos = { x = G.posx m target + 10
               , y = G.posy m target + (targetH // 2)}
        edgePos = { x = spos.x + ((tpos.x - spos.x) // 4)
                  , y = spos.y + 4}
        label = case (target.tipe, edge) of
                  (Arg, _) -> Nothing
                  (_, Edge _ (BlockEdge l)) -> Just l
                  _ -> Nothing
        edgeLabel = Maybe.map (\l ->
                      placeHtml m edgePos
                        (Html.div
                          [Attrs.class <| "edgelabel " ++ getClass l]
                          [Html.text l])) label
    in ( svgLine
         m
         spos
         tpos
         (toString source.id)
         (toString target.id)
         edgeStyle
       , edgeLabel)

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

