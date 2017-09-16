module View exposing (view)

-- builtin
import Dict exposing (Dict)
import Set
import Json.Decode as JSD
import Json.Decode.Pipeline as JSDP

-- lib
import Svg
import Svg.Attributes as SA
import Html
import Html.Attributes as Attrs
import Html.Events as Events
import List.Extra as LE

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
               [ Svg.svg
                    [ SA.width (toString (w - 400))
                    , SA.height (toString h)
                    ]
                    (viewCanvas m)
               ]
      docs = Html.div
              [ Attrs.id "docs"]
              [ viewError m.error
              , viewLive m m.state
              , viewDescription m m.complete
              ]
  in
     Html.div [Attrs.id "container"] [grid, docs]

viewError : ( String, a ) -> Html.Html msg
viewError (msg, ts) =
  Html.div
    [Attrs.id "darkErrors"]
    (case (toString ts) of
       "0" -> [Html.text <| "Err: " ++ msg]
       ts -> [Html.text <| "Err: " ++ msg ++ " (" ++ ts ++ ")"])

viewCanvas : Model -> List (Svg.Svg Msg)
viewCanvas m =
    let allNodes = List.indexedMap (\i n -> viewNode m n i) (G.orderedNodes m)
        edges = m.nodes |> Dict.values |> List.map (viewNodeEdges m) |> List.concat
        entry = viewEntry m
        allSvgs = svgDefs :: svgArrowHead :: (entry ++ allNodes ++ edges)
    in allSvgs

placeHtml : Model -> Pos -> Html.Html Msg -> Svg.Svg Msg
placeHtml m pos html =
  let rcpos = Viewport.toViewport m pos in
  Svg.foreignObject
    [ SA.x (toString rcpos.vx)
    , SA.y (toString rcpos.vy)
    ]
    [ html ]

viewClick : VPos -> Svg.Svg msg
viewClick pos =
  Svg.circle [ SA.r "10"
             , SA.cx (toString pos.vx)
             , SA.cy (toString pos.vy)
             , SA.fill "#333"] []

viewEntry : Model -> List (Svg.Svg Msg)
viewEntry m =
  let html pos =
    let autocompleteList =
          (List.indexedMap
             (\i item ->
                let highlighted = m.complete.index == i
                    hlClass = if highlighted then " highlighted" else ""
                    class = "autocomplete-item" ++ hlClass
                    str = Autocomplete.asString item
                in Html.li
                  [ Attrs.value str
                  , Attrs.class class
                  , Attrs.id ("autocomplete-item-" ++ (toString i))
                  ]
                  [Html.text str])
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


        -- inner node
        inner = Html.div
                [ Attrs.width 100
                , Attrs.class "inner"]
                [ paramInfo, viewForm ]


        -- outer node wrapper
        classes = "selection function node entry"

        wrapper = Html.span
                  [ Attrs.class classes
                  , Attrs.width 100]
                  [ inner ]
      in
        placeHtml m pos wrapper
  in
    case m.state of
      Entering (Filling n hole) ->
        let pos = Entry.holePos m hole in
        [html pos, svgLine m n.pos pos dragEdgeStyle]
      Entering (Creating pos) -> [html pos]
      _ -> []


nodeWidth : Node -> Int
nodeWidth n =
  let
    slimChars = Set.fromList Defaults.narrowChars
    len name = name
             |> String.toList
             |> List.map (\c -> if Set.member c slimChars then 0.5 else 1)
             |> List.sum
    nameMultiple = case n.tipe of
                     Datastore -> 2
                     Page -> 2.2
                     _ -> 1
    lp = List.length n.parameters
    ln = [nameMultiple * len (nodeName n) + 1.3 * (toFloat lp)]
    lf = List.map (\(n,t) -> len n + len t + 3) n.fields
    charWidth = List.foldl max 2 (ln ++ lf)
    width = charWidth * 8.5
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

nodeName : Node -> String
nodeName n =
  let defaultParam = "◉"
      parameterTexts = List.map
                       (\(p, a) -> case a of
                                     Const c -> if c == "null" then "∅" else c
                                     _ -> defaultParam)
                         (G.args n)

  in
    String.join " " (n.name :: parameterTexts)

viewNode : Model -> Node -> Int -> Html.Html Msg
viewNode m n i =
  case n.tipe of
    Arg -> viewArg m n i
    Return -> viewReturn m n i
    FunctionDef -> viewAnon m n i
    _ -> viewNormalNode m n i

-- TODO: If there are default parameters, show them inline in
-- the node body
viewNormalNode : Model -> Node -> Int -> Html.Html Msg
viewNormalNode m n i =
  let
      -- params
      connected name = if G.slotIsConnected m n.id name
                       then "connected"
                       else "disconnected"
      desc desc = if desc == ""
                  then ""
                  else "\n\n" ++ desc
      viewParam {name, tipe, description} =
        Html.span
          [ Attrs.title <| name ++ ": " ++ tipe ++ (desc description)
          , Attrs.class (connected name)]
        [Html.text "◉"]

      -- header
      viewHeader = Html.div
                   [Attrs.class "header"]
                     [ Html.span
                         [Attrs.class "parameters"]
                         (List.map viewParam n.parameters)
                     , Html.span
                         [Attrs.class "letter"]
                         [Html.text (G.int2letter i)]
                     ]

      -- heading
      heading = Html.span
                [ Attrs.class "name"]
                [ Html.text (nodeName n) ]

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
      (viewHeader :: heading :: list)

placeNode : Model -> Node -> Int -> List (Html.Attribute Msg) -> List String -> List (Html.Html Msg) -> Html.Html Msg
placeNode m n width attrs classes body =
  let width_attr = Attrs.style [("width", (toString width) ++ "px")]
      selectedCl = if Selection.isSelected m n then ["selected"] else []
      class = String.toLower (toString n.tipe)
      classStr = String.join " " (["node", class] ++ selectedCl ++ classes)
      inner = Html.div
              (width_attr :: (Attrs.class "inner") :: attrs)
              body
      wrapper = Html.span
                [ Attrs.class classStr, width_attr]
                [ inner ]
  in
    placeHtml m n.pos wrapper

viewAnon : Model -> Node -> Int -> Html.Html Msg
viewAnon m n i =
  let rid = deMaybe n.returnID
      returnNode = G.getNodeExn m rid
      height = 40 + returnNode.pos.y - n.pos.y
      width = 25 + returnNode.pos.x - n.pos.x
      height_attr = Attrs.style [("height", (toString height) ++ "px")]
      viewHeader = Html.div
                   [Attrs.class "header"]
                   [ Html.span
                       [Attrs.class "letter"]
                       [Html.text (G.int2letter i)]
                   ]
  in
    placeNode m n width [height_attr] [] [viewHeader]


viewNodeIcon : String -> Model -> Node -> Int -> Html.Html Msg
viewNodeIcon name m n i =
  let viewHeader = Html.div
                   [Attrs.class "header"]
                   [ Html.span
                       [Attrs.class "letter"]
                       [Html.text (G.int2letter i)]
                   ]
      selectedCl = if Selection.isSelected m n then " selected" else ""
  in
    placeHtml m n.pos
    (Html.div
       [Attrs.class <| name ++ selectedCl]
       [ viewHeader
       , Html.span
         [ Attrs.class "name" ]
         [ Html.text "◉" ]])

viewReturn : Model -> Node -> Int -> Html.Html Msg
viewReturn = viewNodeIcon "return"
viewArg : Model -> Node -> Int -> Html.Html Msg
viewArg = viewNodeIcon "arg"


viewLive : Model -> State -> Html.Html Msg
viewLive m cursor =
  let live =
        cursor
          |> Selection.getCursorID
          |> Maybe.andThen (G.getNode m)
          |> Maybe.map .liveValue
  in
    Html.div
      [ Attrs.id "darkLive"]
      [ case live of
          Just (_, tipe, val) -> Html.pre [] [Html.text <| "LiveValue: " ++ val ++ " (" ++ tipe ++ ")"]
          Nothing -> Html.text "n/a"
      ]

viewDescription : Model -> Autocomplete -> Html.Html Msg
viewDescription m complete =
  Html.div
    [ Attrs.id "darkDesc" ]
    [ Html.text <|
      case Autocomplete.highlighted complete of
        Just (ACFunction {description}) -> description
        _ -> "n/a"
    ]


-- Our edges should be a lineargradient from "darker" to "arrowColor".
-- SVG gradients are weird, they don't allow you specify based on the
-- line direction, but only on the absolute direction. So we define 8
-- linear gradients, one for each 45 degree angle/direction. We define
-- this in terms of "rise over run" (eg like you'd calculate a slope).
-- Then we translate the x,y source/target positions into (rise,run) in
-- the integer range [-1,0,1].
svgDefs : Svg.Svg a
svgDefs =
  Svg.defs []
    [ linearGradient 0 1
    , linearGradient 1 1
    , linearGradient 1 0
    , linearGradient 1 -1
    , linearGradient 0 -1
    , linearGradient -1 -1
    , linearGradient -1 0
    , linearGradient -1 1
    ]

coord2id : Int -> Int -> String
coord2id rise run =
  "linear-rise" ++ toString rise ++ "-run" ++ toString run


linearGradient : Int -> Int -> Svg.Svg a
linearGradient rise run =
  -- edge case, linearGradients use positive integers
  let (x1, x2) = if run == -1 then (1,0) else (0, run)
      (y1, y2) = if rise == -1 then (1,0) else (0, rise)
  in
    Svg.linearGradient
      [ SA.id (coord2id rise run)
      , SA.x1 (toString x1)
      , SA.y1 (toString y1)
      , SA.x2 (toString x2)
      , SA.y2 (toString y2)]
    [ Svg.stop [ SA.offset "0%"
               , SA.stopColor Defaults.edgeGradColor] []
    , Svg.stop [ SA.offset "100%"
               , SA.stopColor Defaults.edgeColor] []]

dragEdgeStyle : List (Svg.Attribute msg)
dragEdgeStyle =
  [ SA.strokeWidth Defaults.dragEdgeSize
  , SA.stroke Defaults.dragEdgeStrokeColor
  ]

edgeStyle : Int -> Int -> Int -> Int -> List (Svg.Attribute msg)
edgeStyle x1 y1 x2 y2 =
  -- edge case: We don't want to use a vertical gradient for really tiny rises,
  -- or it'll just be one color (same for the run). 20 seems enough to avoid
  -- this, empirically.
  let rise = if y2 - y1 > 20 then 1 else if y2 - y1 < -20 then -1 else 0
      run = if x2 - x1 > 20 then 1 else if x2 - x1 < -20 then -1 else 0
      -- edge case: (0,0) is nothing; go in range.
      amendedRise = if (rise,run) == (0,0)
                    then if y2 - y1 > 0 then 1 else -1
                    else rise
  in [ SA.strokeWidth Defaults.edgeSize
     , SA.stroke ("url(#" ++ coord2id amendedRise run ++ ")")
     , SA.markerEnd "url(#triangle)"
     ]

svgLine : Model -> Pos -> Pos -> List (Svg.Attribute Msg) -> Svg.Svg Msg
svgLine m p1a p2a attrs =
  -- edge case: avoid zero width/height lines, or they won't appear
  let p1v = Viewport.toViewport m p1a
      p2v = Viewport.toViewport m p2a
      ( x1, y1, x2_, y2_ ) = (p1v.vx, p1v.vy, p2v.vx, p2v.vy)
      x2 = if x1 == x2_ then x2_ + 1 else x2_
      y2 = if y1 == y2_ then y2_ + 1 else y2_
  in
  Svg.line
    ([ SA.x1 (toString x1)
     , SA.y1 (toString y1)
     , SA.x2 (toString x2)
     , SA.y2 (toString y2)
     ] ++ attrs)
    []

viewNodeEdges : Model -> Node -> List (Svg.Svg Msg)
viewNodeEdges m n =
  n
    |> G.incomingNodePairs m
    |> List.map (\(n2, p) -> viewEdge m n2 n p)

paramOffset : Node -> String -> Pos
paramOffset node param =
  let
    index = deMaybe (LE.findIndex (\p -> p.name == param) node.parameters)
  in
    {x=index*10, y=-2}


viewEdge : Model -> Node -> Node -> ParamName -> Svg.Svg Msg
viewEdge m source target param =
    let targetPos = target.pos
        (sourceW, sourceH) = nodeSize source

        pOffset = paramOffset target param
        (tnx, tny) = (target.pos.x + pOffset.x, target.pos.y + pOffset.y)

        -- find the shortest line and link to there
        joins = [ (tnx, tny) -- topleft
                , (tnx + 5, tny) -- topright
                , (tnx, tny + 5) -- bottomleft
                , (tnx + 5, tny + 5) -- bottomright
                ]
        sq x = toFloat (x*x)
        -- ideally to source pos would be at the bottom of the node.
        -- But, the positioning of the node is a little bit off because
        -- css, and nodes with parameters are in different relative
        -- offsets than nodes without parameters. This makes it hard to
        -- line things up exactly.
        spos = { x = source.pos.x + (sourceW // 2)
               , y = source.pos.y + (sourceH // 2)}

        join = List.head
               (List.sortBy (\(x,y) -> sqrt ((sq (spos.x - x)) + (sq (spos.y - y))))
                  joins)
        (tx, ty) = deMaybe join
    in svgLine
      m
      spos
      {x=tx,y=ty}
      (edgeStyle spos.x spos.y tx ty)

svgArrowHead : Svg.Svg msg
svgArrowHead =
  Svg.marker [ SA.id "triangle"
             , SA.viewBox "0 0 10 10"
             , SA.refX "4"
             , SA.refY "5"
             , SA.markerUnits "strokeWidth"
             , SA.markerWidth "4"
             , SA.markerHeight "4"
             , SA.orient "auto"
             , SA.fill Defaults.edgeColor
             ]
    [Svg.path [SA.d "M 0 0 L 5 5 L 0 10 z"] []]

decodeClickEvent : (MouseEvent -> a) -> JSD.Decoder a
decodeClickEvent fn =
  let toA : Int -> Int -> Int -> a
      toA px py button =
        fn {pos= {vx=px, vy=py}, button = button}
  in JSDP.decode toA
      |> JSDP.required "pageX" JSD.int
      |> JSDP.required "pageY" JSD.int
      |> JSDP.required "button" JSD.int

