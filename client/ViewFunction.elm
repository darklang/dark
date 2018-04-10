module ViewFunction exposing (..)

-- lib
import Html
import Html.Attributes as Attrs

-- dark
import Types exposing (..)
import ViewCode
import ViewUtils exposing (..)

viewFunction : ViewState -> UserFunction -> Html.Html Msg
viewFunction vs fn =
  Html.div
  [ Attrs.class "ast" ]
  [ ViewCode.viewExpr 0 vs [] fn.ast ]
