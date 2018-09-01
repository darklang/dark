module ViewData exposing (..)

-- builtin
import Dict

-- lib
import Html
import Html.Attributes as Attrs

-- dark
import Blank as B
import Types exposing (..)
import Prelude exposing (..)
import ViewUtils exposing (..)
import Analysis
import Runtime as RT
import Util

viewInput : TLID -> Int -> String -> Bool -> Bool -> Tipe -> Html.Html Msg
viewInput tlid idx value isActive isHover tipe =
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

asValue : InputValueDict -> String
asValue inputValue =
  inputValue
  |> Dict.toList
  |> List.filter (\(k,dv) -> RT.typeOf dv /= TDB)
  |> List.map (\(k,dv) -> k
                         ++ ":\n  "
                         ++ Util.replace "\n" "\n  " (RT.toString dv))
  |> String.join "\n"

viewInputs : ViewState -> ID -> List (Html.Html Msg)
viewInputs vs (ID astID) =
  let traceToHtml idx trace =
    let value = asValue trace.input
        -- Note: the following tlCursors are very different things.
        isActive = (Analysis.cursor_ vs.tlCursors vs.tl.id) == idx
        -- Note: this is not the same tlCursor as above
        hoverID = tlCursorID vs.tl.id idx
        isHover = vs.hovering == Just hoverID
        astTipe = Dict.get trace.id vs.analyses
                  |> Maybe.map .liveValues
                  |> Maybe.andThen (Dict.get astID)
                  |> Maybe.map RT.typeOf
                  |> Maybe.withDefault TIncomplete
    in
    viewInput vs.tl.id idx value isActive isHover astTipe
  in
  List.indexedMap traceToHtml vs.traces


viewData : ViewState -> Expr -> List (Html.Html Msg)
viewData vs ast =
  let astID = B.toID ast
      requestEls = viewInputs vs astID
      selectedValue =
        case vs.cursorState of
          Selecting tlid (Just (ID id)) ->
            Dict.get id vs.currentResults.liveValues
          _ -> Nothing
  in
  case selectedValue of
    Just selectedValue ->
      [ Html.div
          [Attrs.class "view-data live-view-selection-active"]
          [ Html.ul
              [Attrs.class "request-cursor"]
              requestEls
          ]
      ]
    Nothing ->
      [ Html.div
          [Attrs.class "view-data"]
          [ Html.ul
              [Attrs.class "request-cursor"]
              requestEls
          ]
      ]

