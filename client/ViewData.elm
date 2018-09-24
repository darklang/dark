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
-- import Util

viewInput : TLID -> Int -> String -> Bool -> Bool -> Tipe -> Html.Html Msg
viewInput tlid idx value isActive isHover tipe =
  let classes =
        [ Attrs.classList
            [ ("active", isActive)
            , ("mouseovered", isHover)
            , ("tipe-" ++ RT.tipe2str tipe, True)
            ]
        ]
      events = [ eventNoPropagation "click" (DataClick tlid idx)
               , eventNoPropagation "mouseenter" (DataMouseEnter tlid idx)
               , eventNoPropagation "mouseleave" (DataMouseLeave tlid idx)
               ]
  in
  Html.li ((Attrs.style [("order", toString (idx))]) :: classes ++ events)
          [ Html.text " "
          , Html.div [Attrs.class "input-details"] [Html.text value]
          ]

asValue : InputValueDict -> String
asValue inputValue =
  RT.inputValueAsString inputValue

viewInputs : ViewState -> ID -> List (Html.Html Msg)
viewInputs vs (ID astID) =
  let traceLength = List.length vs.traces 
      cursorId = (Analysis.cursor_ vs.tlCursors vs.tl.id)
      traceToHtml idx trace =
        let value = asValue trace.input
            rid = traceLength - idx - 1
            -- Note: the following tlCursors are very different things.
            isActive = cursorId == rid
            -- Note: this is not the same tlCursor as above
            hoverID = tlCursorID vs.tl.id rid
            isHover = vs.hovering == Just hoverID
            astTipe = Dict.get trace.id vs.analyses
                      |> Maybe.map .liveValues
                      |> Maybe.andThen (Dict.get astID)
                      |> Maybe.map RT.typeOf
                      |> Maybe.withDefault TIncomplete
        in
        viewInput vs.tl.id rid value isActive isHover astTipe
  in
  vs.traces
  |> List.reverse
  |> List.indexedMap traceToHtml

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
  [ Html.div
      [ Attrs.classList
          [ ("view-data", True)
          , ("live-view-selection-active", selectedValue /= Nothing )
          ]
      ]
      [ Html.ul
          [Attrs.class "request-cursor"]
          requestEls
      ]
  ]
