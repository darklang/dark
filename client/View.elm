module View exposing (view)

-- builtin

-- lib
import Html
import Html.Attributes as Attrs
import Html.Events as Events
import List.Extra as LE

-- dark
import Types exposing (..)
import Util exposing (deMaybe)
import Toplevel as TL
import ViewEntry
import ViewUtils exposing (..)
import ViewScaffold
import ViewRoutingTable
import ViewCode
import ViewDB


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
            Toplevels -> List.map (viewTL m) m.toplevels
            Fn tlid ->
              case LE.find (\f -> f.tlid == tlid) m.userFunctions of
                Just func -> [viewTL m (TL.ufToTL m func)]
                Nothing -> List.map (viewTL m) m.toplevels -- TODO(ian): change to crash
        yaxis = axisLine m {x=0, y=1}
        xaxis = axisLine m {x=1, y=0}
        routing =
          case m.currentPage of
            Toplevels -> [ViewRoutingTable.viewRoutingTable m]
            Fn _ -> []
        allDivs = [xaxis, yaxis] ++ routing ++ asts ++ entry
    in
        Html.div [Attrs.id "canvas"] allDivs

viewFunction : ViewState -> UserFunction -> Html.Html Msg
viewFunction vs fn =
  Html.div
  [ Attrs.class "ast"]
  [viewExpr 0 vs [] fn.ast]


viewTL : Model -> Toplevel -> Html.Html Msg
viewTL m tl =
  let vs = createVS m tl
      body =
        case tl.data of
          TLHandler h ->
            ViewCode.viewHandler vs h
          TLDB db ->
            viewDB (createVS m tl) db
          TLFunc f ->
            [viewFunction (createVS m tl) f]
      events =
        [ eventNoPropagation "mousedown" (ToplevelMouseDown tl.id)
        , eventNoPropagation "mouseup" (ToplevelMouseUp tl.id)
        , eventNoPropagation "click" (ToplevelClick tl.id)
        ]
      selected =
        if Just tl.id == tlidOf m.cursorState
        then "selected"
        else ""
      class =
        [ selected
        , toString (deTLID tl.id)
        , "toplevel"
        , "cursor-" ++ (toString tl.cursor)
        ]
        |> String.join " "
      html =
        Html.div
          [Attrs.class "sidebar-box"] -- see comment in css
          [Html.div
            (Attrs.class class :: events)
            body
          ]
  in
      placeHtml m tl.pos html


