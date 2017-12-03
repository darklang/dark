module ViewAST exposing (..)

-- builtin
import List
import Html
import Html.Attributes as Attrs
import Dict

-- lib
import Maybe.Extra as ME

-- dark
import Types exposing (..)
import AST

type alias HtmlVisitState = { holeID : ID
                            , holeHtml : Html.Html Msg
                            , liveValues : LVDict }

elemToHtml : HtmlVisitState -> Element -> Html.Html Msg
elemToHtml state elem =
  let hover id =
      id
      |> Maybe.andThen (\(ID id) -> Dict.get id state.liveValues)
      |> Maybe.map (\lv -> Attrs.title lv.value)
      |> ME.toList
  in
  case elem of
    Leaf (id, class, content) ->
      let idAttrs =
            case id of
              Just (ID i) -> [Attrs.id (toString i)]
              Nothing -> []

      in if id == Just state.holeID
          then
            Html.div
              ([Attrs.class <| "leaf " ++ class] ++ idAttrs ++ (hover id))
              [ state.holeHtml ]
          else
            Html.div
              ([Attrs.class <| "leaf " ++ class] ++ idAttrs ++ (hover id))
              [Html.text content]

    Nested (id, class) elems ->
      Html.div
        ([Attrs.class <| "nested " ++ class] ++ hover id)
        (List.map (elemToHtml state) elems)

toHtml : HtmlVisitState -> Expr -> Html.Html Msg
toHtml visitState expr =
  expr
  |> AST.walk
  |> elemToHtml visitState

