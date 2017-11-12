module ViewAST exposing (..)

-- builtin
import List
import Html
import Html.Attributes as Attrs

-- lib

-- dark
import Types exposing (..)
import AST

type alias HtmlVisitState = { holeID : HID }

elemToHtml : HtmlVisitState -> Element -> Html.Html Msg
elemToHtml state elem =
  case elem of
    Leaf (id, class, content) ->
      let idAttrs =
        case id of
          Just (HID i) -> [Attrs.id (toString i)]
          Nothing -> []
      in if id == Just state.holeID
          then
            Html.div
              ([Attrs.class <| "leaf " ++ class] ++ idAttrs)
              [Html.input [] [Html.text content]]
          else
            Html.div
              ([Attrs.class <| "leaf " ++ class] ++ idAttrs)
              [Html.text content]

    Nested class elems ->
      Html.div
        [Attrs.class <| "nested " ++ class]
        (List.map (elemToHtml state) elems)

toHtml : HID -> Expr -> Html.Html Msg
toHtml holeID expr = expr |> AST.walk |> elemToHtml {holeID = holeID}


