module ViewFunction exposing (..)

-- lib
import Html
import Html.Attributes as Attrs

-- dark
import Types exposing (..)
import ViewCode
import ViewUtils exposing (..)
import ViewBlankOr exposing (..)

viewFunction : ViewState -> UserFunction -> Html.Html Msg
viewFunction vs fn =
  Html.div
  [ Attrs.class "user-fn-toplevel" ]
  [
    Html.div
    [ Attrs.class "metadata"]
    [ viewMetadata vs fn ]
    ,  Html.div
    [ Attrs.class "ast" ]
    [ ViewCode.viewExpr 0 vs [] fn.ast ]
  ]

viewFnName : BlankViewer String
viewFnName vs c v =
  viewText FnName vs (idConfigs ++ c) v

viewParamName : BlankViewer String
viewParamName vs c v =
  viewText ParamName vs (idConfigs ++ c)  v

viewParamTipe : BlankViewer Tipe
viewParamTipe vs c v =
  viewTipe ParamTipe vs (idConfigs ++ c) v

viewMetadata : ViewState -> UserFunction -> Html.Html Msg
viewMetadata vs fn =
  let namediv = Html.div
                 [ Attrs.class "ufn-name"]
                 [ viewFnName vs [wc "fn-name-content"] fn.metadata.name ]
      coldivs =
        fn.metadata.parameters
        |> List.map (\p ->
             Html.div
               [ Attrs.class "col" ]
               [ viewParamName vs [wc "name"] p.name
               , viewParamTipe vs [wc "type"] p.tipe
               ])
  in
      Html.div
      [ Attrs.class "user-fn" ]
      (namediv :: coldivs)


