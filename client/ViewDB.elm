module ViewDB exposing (viewDB)

-- builtin

-- lib
import Html
import Html.Attributes as Attrs
-- import String.Extra as SE
-- import List.Extra as LE

-- dark
import Blank as B
import Types exposing (..)
import ViewBlankOr exposing (..)
import ViewUtils exposing (..)

viewDBColName : BlankViewer String
viewDBColName vs c v =
  let configs = if B.isBlank v || not vs.dbLocked
                then idConfigs ++ c
                else c
  in
  viewText DBColName vs configs v

viewDBColType : BlankViewer String
viewDBColType vs c v =
  let configs = if B.isBlank v || not vs.dbLocked
                then idConfigs ++ c
                else c
  in
  viewText DBColType vs configs v



viewDB : ViewState -> DB -> List (Html.Html Msg)
viewDB vs db =
  let locked = if vs.dbLocked
               then fontAwesome "lock"
               else fontAwesome "unlock"
      namediv = Html.div
                 [ Attrs.class "dbname"]
                 [ Html.text db.name]
      coldivs =
        db.cols
        |> List.map (\(n, t) ->
             Html.div
               [ Attrs.class "col" ]
               [ viewDBColName vs [wc "name"] n
               , viewDBColType vs [wc "type"] t
               ])
  in
  [
    Html.div
      [ Attrs.class "db"]
      (locked :: namediv :: coldivs)
  ]


