module ViewFunction exposing
    ( viewFnName
    , viewFunction
    , viewKillParameterBtn
    , viewMetadata
    , viewParamName
    , viewParamTipe
    )

import AST
import Html
import Html.Attributes as Attrs
import Types exposing (..)
import ViewBlankOr exposing (..)
import ViewCode
import ViewUtils exposing (..)


viewFunction : ViewState -> UserFunction -> Html.Html Msg
viewFunction vs fn =
    Html.div
        [ Attrs.class "user-fn-toplevel" ]
        [ Html.div
            [ Attrs.class "metadata" ]
            [ viewMetadata vs fn ]
        , Html.div
            [ Attrs.class "ast" ]
            [ ViewCode.viewExpr 0 vs [] fn.ast ]
        ]


viewFnName : BlankViewer String
viewFnName vs c v =
    viewText FnName vs (idConfigs ++ c) v


viewParamName : BlankViewer String
viewParamName vs c v =
    viewText ParamName vs (idConfigs ++ c) v


viewParamTipe : BlankViewer Tipe
viewParamTipe vs c v =
    viewTipe ParamTipe vs (idConfigs ++ c) v


viewKillParameterBtn : ViewState -> UserFunction -> UserFunctionParameter -> Html.Html Msg
viewKillParameterBtn vs uf p =
    let
        freeVariables =
            AST.freeVariables uf.ast |> List.map Tuple.second

        canDeleteParameter pname =
            List.member pname freeVariables
                |> not

        buttonContent allowed =
            if allowed then
                Html.div
                    [ Attrs.class "parameter-btn allowed"
                    , eventNoPropagation "click" (\_ -> DeleteUserFunctionParameter uf p)
                    ]
                    [ fontAwesome "times-circle" ]

            else
                Html.div
                    [ Attrs.class "parameter-btn disallowed"
                    , Attrs.title "Can't delete parameter because it is used in the function body"
                    ]
                    [ fontAwesome "times-circle" ]
    in
    case p.name of
        F _ pname ->
            buttonContent (canDeleteParameter pname)

        _ ->
            buttonContent True


viewMetadata : ViewState -> UserFunction -> Html.Html Msg
viewMetadata vs fn =
    let
        namediv =
            Html.div
                [ Attrs.class "ufn-name" ]
                [ viewFnName vs [ wc "fn-name-content" ] fn.metadata.name ]

        coldivs =
            fn.metadata.parameters
                |> List.map
                    (\p ->
                        Html.div
                            [ Attrs.class "col" ]
                            [ viewParamName vs [ wc "name" ] p.name
                            , viewParamTipe vs [ wc "type" ] p.tipe
                            , viewKillParameterBtn vs fn p
                            ]
                    )
    in
    Html.div
        [ Attrs.class "user-fn" ]
        (namediv :: coldivs)
