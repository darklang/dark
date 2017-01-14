module View exposing (view)

import Dict exposing (Dict)
import Json.Decode as JSD
import Json.Decode.Pipeline as JSDP
import Set

import Svg
import Svg.Attributes as SA

import Html
import Html.Attributes as Attrs
import Html.Events as Events


import Consts
import Types exposing (..)
import Util

view : Model -> Html.Html Msg
view model =
  -- TODO: recalculate this using Tasks
  let (w, h) = Util.windowSize ()
  in
    Html.div
      [Attrs.id "grid"]
      [ (Svg.svg
           [ SA.width (toString w) , SA.height (toString <| h - 30)]
           (viewCanvas model))
      , viewInput model.inputValue
      , viewState model.state
      , viewErrors model.errors
      ]

viewInput value = Html.form [
                   Events.onSubmit (SubmitMsg)
                  ] [
                   Html.input [ Attrs.id Consts.inputID
                              , Events.onInput InputMsg
                              , Attrs.value value
                              ] []
                  ]

-- TODO: CSS this onto the bottom
viewState state = Html.text ("state: " ++ toString state)
viewErrors errors = Html.span [] <| (Html.text " -----> errors: ") :: (List.map Html.text errors)

viewCanvas : Model -> List (Svg.Svg Msg)
viewCanvas m =
    let (w, h) = Util.windowSize ()
        allNodes = List.map (viewNode m) (Dict.values m.nodes)
        edges = List.map (viewEdge m) m.edges
        mDragEdge = viewDragEdge m.drag m.lastPos
        dragEdge = case mDragEdge of
                     Just de -> [de]
                     Nothing -> []
        click = viewClick m.lastPos
    in svgDefs :: svgArrowHead :: click :: (allNodes ++ dragEdge ++ edges)

placeHtml : Pos -> Html.Html Msg -> Svg.Svg Msg
placeHtml pos html =
  Svg.foreignObject
    [ SA.x (toString pos.x)
    , SA.y (toString pos.y)
    ]
    [ html ]

viewClick pos =
  Svg.circle [ SA.r "10"
             , SA.cx (toString pos.x)
             , SA.cy (toString pos.y)
             , SA.fill "#333"] []

nodeWidth : Node -> Bool -> Int
nodeWidth n selected =
  let
    slimChars = Set.fromList Consts.narrowChars
    len name =
      name
        |> synonym
        |> String.toList
        |> List.map (\c -> if Set.member c slimChars then 0.5 else 1)
        |> List.sum
    nameMultiple = case n.tipe of
                     Datastore -> 2
                     Page -> 2.2
                     _ -> 1
    ln = [nameMultiple * len n.name]
    lp = if selected
         then
           List.map (\p -> len p + 3) n.parameters
         else []
    lf = List.map (\(n,t) -> len n + len t + 3) n.fields
    charWidth = List.foldl max 2 (ln ++ lp ++ lf)
    width = charWidth * 10
  in
    round(width)

nodeHeight : Node -> Bool -> Int
nodeHeight n selected =
  case n.tipe of
    Datastore -> Consts.nodeHeight * ( 1 + (List.length n.fields))
    _ -> Consts.nodeHeight

nodeSize node selected =
  (nodeWidth node selected, nodeHeight node selected)

synonym x =
  case x of
    "get_field" -> " ."
    "wrap" -> " :"
    _ -> x

viewNode : Model -> Node -> Html.Html Msg
viewNode m n =
  let name = synonym n.name

      -- css classes
      selected = case m.cursor of
                       Just id -> id == n.id
                       _ -> False
      selectedCl = if selected then ["selected"] else []
      class = String.toLower (toString n.tipe)
      classes = String.join " " (["node", class] ++ selectedCl)

      -- css attrs
      width = Attrs.style [("width",
                            (toString (nodeWidth n selected)) ++ "px")]
      events =
        [ Events.onClick (NodeClick n)
        , Events.on "mousedown" (decodeClickEvent (DragNodeStart n))
        , Events.onMouseUp (DragSlotEnd n)
        ]
      attrs = [Attrs.class classes, width ] ++ events

      -- heading params
      slotHandler name = (decodeClickEvent (DragSlotStart n name))
      viewHeadingParam name = Html.span
                       [Events.on "mousedown" (slotHandler name)]
                       [Html.text "◉"]
      headingParams = List.map viewHeadingParam n.parameters
      headingParamHolder = Html.div [Attrs.class "parameter"] headingParams

      -- heading
      heading = Html.span
                [ Attrs.class "name"]
                [ Html.text name ]

      -- fields (in list)
      viewField (name, tipe) = [ Html.text (name ++ " : " ++ tipe)
                               , Html.br [] []]
      -- params (in list)
      viewParam name = Html.span [] [Html.text name]
      viewParams = if n.tipe == Datastore
                   then []
                   else [Html.span
                           [Attrs.class "list"]
                           (List.map viewParam n.parameters)]

      -- list
      includeList = n.tipe == Datastore ||
                    (selected && (List.length n.parameters > 0))
      list = if includeList
             then
               [Html.span
                 [ Attrs.class "list"]
                 (List.concat
                    (List.map viewField n.fields)
                    ++ viewParams)]
             else []

  in
    placeHtml
      n.pos <|
      Html.span [] [headingParamHolder, Html.div attrs (heading :: list)]

-- Our edges should be a lineargradient from "darker" to "arrowColor". SVG
-- gradients are weird, they don't allow you specify based on the line
-- direction, but only on the absolute direction. So we define 8 linear
-- gradients, one for each 45 degree angle/direction. We define this in terms of
-- "rise over run" (eg like you'd calculate a slope). Then we translate the x,y
-- source/target positions into (rise,run) in the integer range [-1,0,1].
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
               , SA.stopColor Consts.edgeGradColor] []
    , Svg.stop [ SA.offset "100%"
               , SA.stopColor Consts.edgeColor] []]

dragEdgeStyle =
  [ SA.strokeWidth Consts.dragEdgeSize
  , SA.stroke Consts.dragEdgeStrokeColor
  ]

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
  in [ SA.strokeWidth Consts.edgeSize
     , SA.stroke ("url(#" ++ coord2id amendedRise run ++ ")")
     , SA.markerEnd "url(#triangle)"
     ]

svgLine : Pos -> Pos -> List (Svg.Attribute Msg) -> Svg.Svg Msg
svgLine p1 p2 attrs =
  -- edge case: avoid zero width/height lines, or they won't appear
  let ( x1, y1, x2_, y2_ ) = (p1.x, p1.y, p2.x, p2.y)
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

viewDragEdge : Drag -> Pos -> Maybe (Svg.Svg Msg)
viewDragEdge drag currentPos =
  case drag of
    DragNode _ _ -> Nothing
    NoDrag -> Nothing
    DragSlot id param mStartPos ->
      Just <|
        svgLine mStartPos
                currentPos
                dragEdgeStyle

deID (ID x) = x
viewEdge : Model -> Edge -> Svg.Svg Msg
viewEdge m {source, target, targetParam} =
    let mSourceN = Dict.get (deID source) m.nodes
        mTargetN = Dict.get (deID target) m.nodes
        (sourceN, targetN) = case (mSourceN, mTargetN) of
                             (Just s, Just t) -> (s, t)
                             _ -> Debug.crash "Can't happen"
        targetPos = targetN.pos
        (sourceW, sourceH) =
          nodeSize sourceN (m.cursor == Just sourceN.id)
        (targetW, targetH) =
          nodeSize targetN (m.cursor == Just targetN.id)

        -- find the shortest line and link to there
        joins = [ (targetN.pos.x, targetN.pos.y + targetH // 2) -- left
                , (targetN.pos.x + targetW // 2, targetN.pos.y) -- top
                , (targetN.pos.x + targetW, targetN.pos.y + targetH // 2) -- right
                , (targetN.pos.x + targetW // 2, targetN.pos.y + targetH) -- bottom
                ]
        sq x = toFloat (x*x)
        spos = { x = sourceN.pos.x + (sourceW // 2)
               , y = sourceN.pos.y + (sourceH // 2)}

        join = List.head
               (List.sortBy (\(x,y) -> sqrt ((sq (spos.x - x)) + (sq (spos.y - y))))
                  joins)
        (tx, ty) = case join of
                     Nothing -> Debug.crash "not possible"
                     Just j -> j
    in svgLine
      spos
      {x=tx,y=ty}
      (edgeStyle spos.x spos.y tx ty)

svgArrowHead =
  Svg.marker [ SA.id "triangle"
             , SA.viewBox "0 0 10 10"
             , SA.refX "4"
             , SA.refY "5"
             , SA.markerUnits "strokeWidth"
             , SA.markerWidth "4"
             , SA.markerHeight "4"
             , SA.orient "auto"
             , SA.fill Consts.edgeColor
             ]
    [Svg.path [SA.d "M 0 0 L 5 5 L 0 10 z"] []]

decodeClickEvent : (MouseEvent -> a) -> JSD.Decoder a
decodeClickEvent fn =
  let toA : Int -> Int -> Int -> a
      toA px py button =
        fn {pos= {x=px, y=py}, button = button}
  in JSDP.decode toA
      |> JSDP.required "pageX" JSD.int
      |> JSDP.required "pageY" JSD.int
      |> JSDP.required "button" JSD.int

