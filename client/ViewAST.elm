module ViewAST exposing (..)

-- builtin
import List
import Html
import Html.Attributes as Attrs

-- lib

-- dark
import Types exposing (..)
import AST


elemToHtml : Element -> Html.Html Msg
elemToHtml elem =
  case elem of
    Leaf (id, class, content) ->
      let idAttrs =
        case id of
          Just i -> [Attrs.id (toString (deID i))]
          Nothing -> []
      in
      Html.div ([Attrs.class <| "leaf " ++ class] ++ idAttrs) [Html.text content]
    Nested class elems ->
      Html.div [Attrs.class <| "nested " ++ class] (List.map elemToHtml elems)

toHtml : Expr -> Html.Html Msg
toHtml expr = expr |> AST.walk |> elemToHtml


