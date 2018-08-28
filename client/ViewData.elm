module ViewData exposing
    ( asValue
    , viewData
    , viewInput
    , viewInputs
    )

import Analysis
import Dict
import Html
import Html.Attributes as Attrs
import Prelude exposing (..)
import Runtime as RT
import Types exposing (..)
import Util
import ViewUtils exposing (..)


viewInput : TLID -> Int -> String -> Bool -> Bool -> Tipe -> Html.Html Msg
viewInput tlid idx value isActive isHover tipe =
    let
        activeClass =
            if isActive then
                [ Attrs.class "active" ]

            else
                []

        hoverClass =
            if isHover then
                [ Attrs.class "mouseovered" ]

            else
                []

        tipeClassName =
            "tipe-" ++ RT.tipe2str tipe

        tipeClass =
            [ Attrs.class tipeClassName ]

        classes =
            activeClass ++ hoverClass ++ tipeClass

        events =
            [ eventNoPropagation "click" (DataClick tlid idx)
            , eventNoPropagation "mouseenter" (DataMouseEnter tlid idx)
            , eventNoPropagation "mouseleave" (DataMouseLeave tlid idx)
            ]
    in
    Html.li ([ Attrs.attribute "data-content" value ] ++ classes ++ events)
        [ Html.text "•" ]


asValue : InputDict -> String
asValue inputValue =
    inputValue
        |> Dict.toList
        |> List.filter (\( k, v ) -> v.tipe /= TDB)
        |> List.map
            (\( k, v ) ->
                k
                    ++ ":\n  "
                    ++ Util.replace "\n" "\n  " v.value
            )
        |> String.join "\n"


viewInputs : ViewState -> List (Html.Html Msg)
viewInputs vs =
    let
        resultToHtml idx result =
            let
                value =
                    asValue result.inputValues

                -- Note: the following tlCursors are very different things.
                isActive =
                    Analysis.cursor_ vs.tlCursors vs.tl.id == idx

                -- Note: this is not the same tlCursor as above
                hoverID =
                    tlCursorID vs.tl.id idx

                isHover =
                    vs.hovering == Just hoverID
            in
            viewInput vs.tl.id idx value isActive isHover result.astValue.tipe
    in
    List.indexedMap resultToHtml vs.results


viewData : ViewState -> List (Html.Html Msg)
viewData vs =
    let
        requestEls =
            viewInputs vs

        selectedValue =
            case vs.cursorState of
                Selecting tlid (Just (ID id)) ->
                    Dict.get id vs.lvs

                _ ->
                    Nothing
    in
    case selectedValue of
        Just selectedValue ->
            [ Html.div
                [ Attrs.class "view-data live-view-selection-active" ]
                [ Html.ul
                    [ Attrs.class "request-cursor" ]
                    requestEls
                ]
            ]

        Nothing ->
            [ Html.div
                [ Attrs.class "view-data" ]
                [ Html.ul
                    [ Attrs.class "request-cursor" ]
                    requestEls
                ]
            ]
