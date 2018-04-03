module View exposing (view)

-- builtin

-- lib
import Html
import Html.Attributes as Attrs
import Html.Events as Events

-- dark
import Types exposing (..)
import Util exposing (deMaybe)
import ViewEntry
import ViewUtils exposing (..)
import ViewScaffold
import ViewRoutingTable
import ViewCode
import ViewDB


view : Model -> Html.Html Msg
view m =
  let (w, h) = Util.windowSize () in
  Html.div
   [ Attrs.id "grid"
   , Events.onWithOptions
       "mouseup"
       { stopPropagation = False, preventDefault = True}
       (decodeClickEvent GlobalClick)
   ]
   [ ViewScaffold.viewError m.error
   , (viewCanvas m)
   , ViewScaffold.viewButtons m
   ]

viewCanvas : Model -> Html.Html Msg
viewCanvas m =
    let entry = ViewEntry.viewEntry m
        asts = List.map (viewTL m) m.toplevels
        yaxis = axisLine m {x=0, y=1}
        xaxis = axisLine m {x=1, y=0}
        routing = ViewRoutingTable.viewRoutingTable m
        allDivs = xaxis :: yaxis :: routing :: (asts ++ entry)
    in Html.div [Attrs.id "canvas"] allDivs

viewTL : Model -> Toplevel -> Html.Html Msg
viewTL m tl =
  let vs = createVS m tl
      body =
        case tl.data of
          TLHandler h ->
            ViewCode.viewHandler vs h
          TLDB db ->
            ViewDB.viewDB vs db
      events =
        [ eventNoPropagation "mousedown" (ToplevelMouseDown tl.id)
        , eventNoPropagation "mouseup" (ToplevelMouseUp tl.id)
        , eventNoPropagation "click" (ToplevelClick tl.id)
        ]

      selected = if Just tl.id == tlidOf m.cursorState
                 then "selected"
                 else ""
      class = [ selected
              , toString (deTLID tl.id)
              , "toplevel"
              , "cursor-" ++ (toString tl.cursor)
              ]
              |> String.join " "

      html =
        Html.div
          [Attrs.class "sidebar-box"] -- see comment in css
          [ Html.div
              (Attrs.class class :: events)
              body
          ]

  in placeHtml m tl.pos html


