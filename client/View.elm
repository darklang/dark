module View exposing (view)

-- builtin
import Set
import Json.Decode as JSD
import Json.Decode.Pipeline as JSDP
import Char

-- lib
import Svg
import Svg.Attributes as SA
import Html
import Html.Attributes as Attrs
import Html.Events as Events

-- dark
import Types exposing (..)
import Util exposing (deMaybe)
import Graph as G
import Entry
import Defaults
import Viewport
import Selection
import Autocomplete

view : Model -> Html.Html Msg
view m =
  let (w, h) = Util.windowSize ()
      grid = Html.div
               [ Attrs.id "grid"
               , Events.on "mousedown" (decodeClickEvent RecordClick)
               ]
               [ viewError m.error
               , Svg.svg
                 [ SA.width "100%"
                 , SA.height (toString h) ]
                 (viewCanvas m)
               ]
 in
    grid

viewError : Maybe String -> Html.Html msg
viewError mMsg =
  case mMsg of
    Just msg -> Html.div [Attrs.id "darkErrors"] [Html.text msg]
    Nothing -> Html.div [] []


viewCanvas : Model -> List (Svg.Svg Msg)
viewCanvas m =
    let visible = List.filter .visible (G.orderedNodes m)
        nodes = List.indexedMap (\i n -> viewNode m n i) visible
        values = visible |> List.map (viewValue m)
        edges = visible |> List.map (viewNodeEdges m) |> List.concat
        entry = viewEntry m
        yaxis = svgLine m {x=0, y=2000} {x=0,y=-2000} [SA.strokeWidth "1px", SA.stroke "#777"]
        xaxis = svgLine m {x=2000, y=0} {x=-2000,y=0} [SA.strokeWidth "1px", SA.stroke "#777"]
        allSvgs = xaxis :: yaxis :: (edges ++ values ++ nodes ++ entry)
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
  let html pos =
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
                            (Entering (Filling _ (ParamHole _ _ _)), -1) ->
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
        searchinput = Html.input [ Attrs.id Defaults.entryID
                                 , Events.onInput EntryInputMsg
                                 , Attrs.value m.complete.value
                                 , Attrs.autocomplete False
                                 ] []
        prefix_ = Autocomplete.sharedPrefix m.complete
        prefix = Autocomplete.joinPrefix m.complete.value prefix_

        suggestioninput = Html.input [ Attrs.id "suggestion"
                                     , Attrs.disabled True
                                     , Attrs.value prefix
                                     ] []

        input = Html.div
                [Attrs.id "search-container"]
                [searchinput, suggestioninput]

        viewForm = Html.form
                   [ Events.onSubmit (EntrySubmitMsg) ]
                   [ input, autocomplete ]

        paramInfo =
          case m.state of
            Entering (Filling _ (ParamHole _ param _)) ->
              Html.div [] [ Html.text (param.name ++ " : " ++ param.tipe)
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
      in
        placeHtml m pos wrapper
  in
    case m.state of
      Entering (Filling n h) ->
        let holePos = Entry.holeDisplayPos m h
            edgePos = { x = holePos.x + 10
                      , y = holePos.y + 10}
            nodePos = { x = n.pos.x + 10
                      , y = n.pos.y + 10}
        in
        [svgLine m nodePos edgePos edgeStyle, html holePos]
      Entering (Creating pos) -> [html pos]
      _ -> []


nodeWidth : Node -> Int
nodeWidth n =
  let
    space = 3.5
    fours = Set.fromList ['i', 'l', '[', ',', ']', 'l', ':', '/', '.', ' ', ',', '{', '}']
    fives = Set.fromList ['I', 't', Char.fromCode 34 ] -- '"'
    len name = name
             |> String.toList
             |> List.map (\c -> if c == ' '
                                then 3.5
                                else if Set.member c fours
                                     then 4.0
                                     else if Set.member c fives
                                          then 5.0
                                          else 8.0)
             |> List.sum
    paramLen = G.args n
                |> List.map (\(p, a) ->
                  case a of
                    Const c -> if c == "null" then 8 else (len c)
                    _ -> 14)
                |> List.sum
    -- nameMultiple = case n.tipe of
    --                  Datastore -> 2
    --                  Page -> 2.2
    --                  _ -> 1
    width = 6.0 + len n.name + paramLen + (G.args n |> List.length |> toFloat |> (+) 1.0 |> (*) space)
  in
    round(width)

nodeHeight : Node -> Int
nodeHeight n =
  case n.tipe of
    Datastore -> Defaults.nodeHeight * ( 1 + (List.length n.fields))
    _ -> Defaults.nodeHeight

nodeSize : Node -> (Int, Int)
nodeSize node =
  (nodeWidth node, nodeHeight node)

viewValue : Model -> Node -> Html.Html Msg
viewValue m n =
  let width = nodeWidth n
      xpad = max (width+50) 250
      valueStr val tipe = val
                          |> String.left 120
                          |> Util.replace "\n" ""
                          |> Util.replace "\r" ""
                          |> Util.replace "\\s+" " "
                          |> (\s -> if String.length s > 55
                                    then (String.left 55 s) ++ "..."
                                    else s)
                          |> \v -> v ++ " :: " ++ tipe
                          |> Html.text
  in
  placeHtml m {x=n.pos.x+xpad, y=n.pos.y}
      (case n.liveValue.exc of
        Nothing -> Html.pre
                    [Attrs.class "preview", Attrs.title n.liveValue.value]
                    [valueStr n.liveValue.value n.liveValue.tipe]
        Just exc -> Html.span
                      [ Attrs.class "unexpected preview"
                      , Attrs.title
                          ( "Problem: " ++ exc.short
                          ++ "\n\nActual value: " ++ exc.actual
                          ++ "\n\nExpected: " ++ exc.expected
                          ++ "\n\nMore info: " ++ exc.long
                        ) ]
                      [ Html.pre
                        [ ]
                        [ valueStr exc.actual exc.actualType ]
                      , Html.span
                          [Attrs.class "info" ]
                          [Html.text "ⓘ "]
                      , Html.span
                          [Attrs.class "explanation" ]
                          [Html.text exc.short ]])


viewNode : Model -> Node -> Int -> Html.Html Msg
viewNode m n i =
  case n.tipe of
    Arg -> viewNormalNode m n i
    FunctionDef -> Html.div [] []
    _ -> viewNormalNode m n i

-- TODO: If there are default parameters, show them inline in
-- the node body
viewNormalNode : Model -> Node -> Int -> Html.Html Msg
viewNormalNode m n i =
  let
      -- header
      header = [ Html.span
                   [Attrs.class "letter"]
                   [Html.text (G.int2letter i)]
               ]

      -- heading
      params = n.arguments
              |> List.map
                    (\a ->
                      case a of
                        Const c -> ("arg_const", if c == "null" then "∅" else c)
                        NoArg -> ("arg_none", "◉")
                        Edge _ -> ("arg_edge", "◉"))
               |> List.map (\(class, val) ->
                              Html.span
                                [ Attrs.class class]
                                [ Html.text <| " " ++ val])

      heading = Html.span
                [ Attrs.class "title"]
                ((Html.span [Attrs.class "name"] [Html.text n.name]) :: params)


      -- fields (in list)
      viewField (name, tipe) = [ Html.text (name ++ " : " ++ tipe)
                               , Html.br [] []]
      viewFields = List.map viewField n.fields

      -- list
      list = if viewFields /= []
             then
               [Html.span
                 [Attrs.class "list"]
                 (List.concat viewFields)]
             else []

  in
    placeNode
      m
      n
      (nodeWidth n)
      []
      []
      header
      (heading :: list)

placeNode : Model -> Node -> Int -> List (Html.Attribute Msg) -> List String -> List (Html.Html Msg) -> List (Html.Html Msg) -> Html.Html Msg
placeNode m n width attrs classes header body =
  let width_attr = Attrs.style [("width", (toString width) ++ "px")]
      selectedCl = if Selection.isSelected m n then ["selected"] else []
      class = String.toLower (toString n.tipe)
      classStr = String.join " " (["node", class] ++ selectedCl ++ classes)
      node = Html.div
                (width_attr :: (Attrs.class classStr) :: attrs)
                body
      header_wrapper = Html.div [Attrs.class "header", width_attr ] header
      wrapper = Html.div [] [ node, header_wrapper ]
  in
    placeHtml m n.pos wrapper

edgeStyle : List (Svg.Attribute msg)
edgeStyle =
  [ SA.strokeWidth Defaults.edgeSize
  , SA.stroke Defaults.edgeStrokeColor
  ]

viewNodeEdges : Model -> Node -> List (Svg.Svg Msg)
viewNodeEdges m n =
  n
    |> G.incomingNodePairs m
    |> List.map (\(n2, p) -> viewEdge m n2 n p)

viewEdge : Model -> Node -> Node -> ParamName -> Svg.Svg Msg
viewEdge m source target param =
    let targetPos = target.pos
        (sourceW, sourceH) = nodeSize source
        (targetW, targetH) = nodeSize target
        spos = { x = source.pos.x + 10
               , y = source.pos.y + (sourceH // 2)}
        tpos = { x = target.pos.x + 10
               , y = target.pos.y + (targetH // 2)}
    in svgLine
      m
      spos
      tpos
      edgeStyle

svgLine : Model -> Pos -> Pos -> List (Svg.Attribute Msg) -> Svg.Svg Msg
svgLine m p1a p2a attrs =
  let p1v = Viewport.toViewport m p1a
      p2v = Viewport.toViewport m p2a
  in
  Svg.line
    ([ SA.x1 (toString p1v.vx)
     , SA.y1 (toString p1v.vy)
     , SA.x2 (toString p2v.vx)
     , SA.y2 (toString p2v.vy)
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

