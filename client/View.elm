module View exposing (view)

-- builtin

-- lib
import Browser
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
import Defaults


view : Model -> Browser.Document Msg
view m =
  let attributes =
        [ Attrs.id "grid"
        , eventNoPropagation "mouseup" GlobalClick
        ]
      footer =
        [ ViewScaffold.viewError m.error, ViewScaffold.viewButtons m ]
      routing =
        ViewRoutingTable.viewRoutingTable m
      body =
        viewCanvas m
      content =
        [ routing
        , body
        ] ++ footer
  in
      { title = "Dark"
      , body = [ Html.div attributes content ]
      }

viewCanvas : Model -> Html.Html Msg
viewCanvas m =
    let entry = ViewEntry.viewEntry m
        asts =
          case m.currentPage of
            Toplevels _ -> List.map (viewTL m) m.toplevels
            Fn tlid _ ->
              case LE.find (\f -> f.tlid == tlid) m.userFunctions of
                Just func -> [viewTL m (TL.ufToTL m func)]
                Nothing -> List.map (viewTL m) m.toplevels -- TODO(ian): change to crash

        canvasTransform =
          let offset =
                case m.currentPage of
                  Toplevels _ -> m.canvas.offset
                  Fn _ _ -> m.canvas.fnOffset
              x = String.fromInt (-offset.x)
              y = String.fromInt (-offset.y)
          in
          "translate(" ++ x ++ "px, " ++ y ++ "px)"

        allDivs = asts ++ entry
    in
        Html.div
        [ Attrs.id "canvas"
        , Attrs.style "transform" canvasTransform
        ]
        allDivs

viewTL : Model -> Toplevel -> Html.Html Msg
viewTL m tl =
  let id = deTLID tl.id
      pos =
        case m.currentPage of
          Toplevels _ -> tl.pos
          Fn tLID _ -> Defaults.centerPos
   in
   placeHtml m pos (viewTL_ m tl.id)

viewTL_ : Model -> TLID -> Html.Html Msg
viewTL_ m tlid =
  let tl = TL.getTL m tlid
      vs = createVS m tl
      (body, data) =
        case tl.data of
          TLHandler h ->
            ( ViewCode.viewHandler vs h
            , ViewData.viewData vs h.ast
            )
          TLDB db ->
            ( ViewDB.viewDB vs db
            , []
            )
          TLFunc f ->
            ( [ViewFunction.viewFunction vs f]
            , ViewData.viewData vs f.ast
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
          Dragging tlid_ _ _ _ ->
            if tlid_ == tl.id then ["dragging"] else []
          _ -> []
      class =
        [ selected
        , "tl-" ++ String.fromInt (deTLID tl.id)
        , "toplevel"
        , "cursor-" ++ (String.fromInt (Analysis.cursor m tl.id))
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
