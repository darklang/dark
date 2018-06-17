module View exposing (view)

-- builtin

-- lib
import Html
import Html.Attributes as Attrs
import Html.Events as Events
import List.Extra as LE

-- dark
import Analysis
import Types exposing (..)
import Prelude exposing (..)
import Util
import Toplevel as TL
import ViewEntry
import ViewUtils exposing (..)
import ViewScaffold
import ViewRoutingTable
import ViewCode
import ViewDB
import ViewData
import ViewFunction
import Autocomplete


view : Model -> Html.Html Msg
view m =
  let (w, h) = Util.windowSize ()
      attributes =
        [ Attrs.id "grid"
        , Events.onWithOptions
        "mouseup"
        { stopPropagation = False, preventDefault = True }
        (decodeClickEvent GlobalClick)
        ]
      header =
        ViewScaffold.viewError m.error
      footer =
        ViewScaffold.viewButtons m
      body =
        viewCanvas m
      content =
        [ header
        , body
        , footer
        ]
  in
      Html.div attributes content

viewCanvas : Model -> Html.Html Msg
viewCanvas m =
    let entry = ViewEntry.viewEntry m
        asts =
          case m.currentPage of
            Toplevels _ -> List.map (viewTL m) m.toplevels
            Fn tlid ->
              case LE.find (\f -> f.tlid == tlid) m.userFunctions of
                Just func -> [viewTL m (TL.ufToTL m func)]
                Nothing -> List.map (viewTL m) m.toplevels -- TODO(ian): change to crash
        yaxis = axisLine m {x=0, y=1}
        xaxis = axisLine m {x=1, y=0}
        axes =
          case m.currentPage of
            Toplevels _ -> [xaxis, yaxis]
            Fn _ -> []
        routing =
          case m.currentPage of
            Toplevels _ -> [ViewRoutingTable.viewRoutingTable m]
            Fn _ -> []
        allDivs = axes ++ routing ++ asts ++ entry
    in
        Html.div [Attrs.id "canvas"] allDivs

viewTL : Model -> Toplevel -> Html.Html Msg
viewTL m tl =
  let recalc = \_ -> viewTL_ m tl.id
      html =
        if Just tl.id == tlidOf m.cursorState
        then recalc ()
        else
          case Util.cacheGet (deTLID tl.id) of
            Just html -> html
            Nothing ->
              let result = recalc ()
                  _ = Util.cacheSet (deTLID tl.id) result in
              result
   in
   placeHtml m tl.pos html

viewTL_ : Model -> TLID -> Html.Html Msg
viewTL_ m tlid =
  let tl = TL.getTL m tlid
      vs = createVS m tl
      (body, data) =
        case tl.data of
          TLHandler h ->
            ( ViewCode.viewHandler vs h
            , ViewData.viewHandler vs h
            )
          TLDB db ->
            ( ViewDB.viewDB vs db
            , []
            )
          TLFunc f ->
            ( [ViewFunction.viewFunction vs f]
            , []
            )
      events =
        [ eventNoPropagation "mousedown" (ToplevelMouseDown tl.id)
        , eventNoPropagation "mouseup" (ToplevelMouseUp tl.id)
        , eventNoPropagation "click" (ToplevelClick tl.id)
        ]
      selected =
        if Just tl.id == tlidOf m.cursorState
        then "selected"
        else ""
      boxClasses =
        case m.cursorState of
          Dragging tlid _ _ _ ->
            if tlid == tl.id then ["dragging"] else []
          _ -> []
      class =
        [ selected
        , toString (deTLID tl.id)
        , "toplevel"
        , "cursor-" ++ (toString (Analysis.cursor m tl.id))
        ]
        |> String.join " "

      documentation =
        if Just tl.id == tlidOf m.cursorState
        then
          m.complete
          |> Autocomplete.highlighted
          |> Maybe.andThen
            Autocomplete.documentationForItem
          |> Maybe.map
            (\desc ->
              [Html.div
                [Attrs.class "documentation-box"]
                [Html.p [] [Html.text (desc)]]
              ])
        else
          Nothing

      top =
        case documentation of
          Just doc -> doc
          _ -> data

      html =
        Html.div
          [Attrs.class <| String.join " " (boxClasses ++ ["sidebar-box", selected])] -- see comment in css
          [Html.div
            (Attrs.class class :: events)
            (body ++ top)
          ]

  in
      html
