module ViewData exposing (..)

-- builtin
import Dict

-- lib
import Html
import Html.Attributes as Attrs

-- dark
import Types exposing (..)
import Prelude exposing (..)
import ViewUtils exposing (..)
import Analysis
import Runtime as RT

viewRequest : TLID -> Int -> String -> Bool -> Bool -> Tipe -> Html.Html Msg
viewRequest tlid idx value isActive isHover tipe =
  let activeClass = if isActive then [Attrs.class "active"] else []
      hoverClass = if isHover then [Attrs.class "mouseovered"] else []
      tipeClassName = "tipe-" ++ RT.tipe2str tipe
      tipeClass = [Attrs.class tipeClassName]
      classes = activeClass ++ hoverClass ++ tipeClass
      events = [ eventNoPropagation "click" (DataClick tlid idx)
               , eventNoPropagation "mouseenter" (DataMouseEnter tlid idx)
               , eventNoPropagation "mouseleave" (DataMouseLeave tlid idx)
               ]
  in
  Html.li ([(Attrs.attribute "data-content" value)] ++ classes ++ events)
          [Html.text "•"]

viewRequests : ViewState -> Handler -> List (Html.Html Msg)
viewRequests vs h =
  let resultToHtml idx result =
    let key = if vs.handlerSpace == HSHTTP
              then "request"
              else "event"
        value = Dict.get key result.inputValues
                |> Maybe.map .value
                |> Maybe.withDefault ""
        -- Note: the following tlCursors are very different things.
        isActive = (Analysis.cursor_ vs.tlCursors vs.tl.id) == idx
        -- Note: this is not the same tlCursor as above
        hoverID = tlCursorID vs.tl.id idx
        isHover = vs.hovering == Just hoverID
    in
    viewRequest vs.tl.id idx value isActive isHover result.astValue.tipe
  in
  List.indexedMap resultToHtml <| List.reverse vs.results


viewHandler : ViewState -> Handler -> List (Html.Html Msg)
viewHandler vs h =
  let requestEls = viewRequests vs h
  in [ Html.div
        [Attrs.class "view-data"]
        [ Html.ul
            [Attrs.class "request-cursor"]
            requestEls
        ]
    ]
