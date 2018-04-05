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
          [Html.text "•"]

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
          hoverID = tlCursorID vs.tl.id idx
          isHover = vs.hovering == Just hoverID
      in
      viewRequest vs.tl.id idx value isActive isHover
  in
  List.indexedMap resultToHtml <| List.reverse vs.results


viewHandler : ViewState -> Handler -> List (Html.Html Msg)
viewHandler vs h =
  let requestEls = viewRequests vs h
      isSelecting = case vs.cursorState of
        Selecting tlid id ->
          case id of
            Just id ->
              let lv = vs.lvs |> Dict.get (deID id) in
              case lv of
                Just lv -> tlid == vs.tl.id
                _ -> False
            _ -> False
        _ -> False
      classes = if isSelecting then [Attrs.class "view-data live-view-selection-active"] else [Attrs.class "view-data"]
  in
  [ Html.div classes [(Html.ul [Attrs.class "request-cursor"] requestEls)] ]
