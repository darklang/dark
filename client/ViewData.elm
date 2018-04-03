module ViewData exposing (..)

-- builtin
import Dict

-- lib
import Html
import Html.Attributes as Attrs

-- dark
import Types exposing (..)
import ViewUtils exposing (..)

viewRequest : TLID -> Int -> String -> Bool -> Bool -> Html.Html Msg
viewRequest tlid idx value isActive isHover =
  let activeClass = if isActive then [Attrs.class "active"] else []
      hoverClass = if isHover then [Attrs.class "mouseovered"] else []
      classes = activeClass ++ hoverClass
      events = [ eventNoPropagation "click" (DataClick tlid idx)
               , eventNoPropagation "mouseenter" (DataMouseEnter tlid idx)
               , eventNoPropagation "mouseleave" (DataMouseLeave tlid idx)
               ]
  in
  Html.li ([(Attrs.attribute "data-content" value)] ++ classes ++ events)
          [Html.text "📡"]

viewRequests : ViewState -> Handler -> List (Html.Html Msg)
viewRequests vs h =
  let resultToHtml =
    \idx result ->
      let value = if vs.isHTTP
          then
            case (Dict.get "request" result.inputValues) of
              Just v -> v.value
              _ -> ""
          else
            case (Dict.get "event" result.inputValues) of
              Just v -> v.value
              _ -> ""
          isActive = vs.tl.cursor == idx
          -- we need to make up a unique'ish ID from tlid + idx
          stringID = (toString (deTLID vs.tl.id)) ++ (toString idx)
          intID = Result.withDefault 0 (String.toInt stringID)
          hoverID = (ID intID)
          isHover = vs.hovering == Just hoverID
      in
      viewRequest vs.tl.id idx value isActive isHover
  in
  List.indexedMap resultToHtml vs.results


viewHandler : ViewState -> Handler -> List (Html.Html Msg)
viewHandler vs h =
  let requestEls = viewRequests vs h
  in
  [ Html.div [Attrs.class "view-data"] [(Html.ul [Attrs.class "request-cursor"] requestEls)] ]
