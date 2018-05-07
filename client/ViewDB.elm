module ViewDB exposing (viewDB)

-- builtin

-- lib
import Html
import Html.Attributes as Attrs
import Maybe.Extra as ME
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
  let configs = idConfigs ++ c in
  viewText DBColType vs configs v

viewDBMigration : ViewState -> DBMigration -> Html.Html Msg
viewDBMigration vs m =
  Html.div
    [ Attrs.class "migration-view" ]
    [
      Html.text ("new version: " ++ toString (m.startingVersion + 1))
    -- , viewBlankOrExpr rollforward
    -- , viewBlankOrExpr rollback
    ]


viewDB : ViewState -> DB -> List (Html.Html Msg)
viewDB vs db =
  let locked = if vs.dbLocked
               then fontAwesome "lock"
               else fontAwesome "unlock"
      namediv = Html.div
                 [ Attrs.class "dbname"]
                 [ Html.text (db.name ++ ".v" ++ (toString db.version)) ]
      coldivs =
        db.cols
        |> List.map (\(n, t) ->
          let migration =
              db.activeMigration
              |> Maybe.andThen
                   (\am ->
                     if am.target == B.toID n
                     || am.target == B.toID t
                     then Just (viewDBMigration vs am)
                     else Nothing)
              |> ME.toList
          in
          Html.div
            [ Attrs.class "col" ]
            ([ viewDBColName vs [wc "name"] n
            , viewDBColType vs [wc "type"] t
            ] ++ migration ))
  in
  [
    Html.div
      [ Attrs.class "db"]
      (locked :: namediv :: coldivs)
  ]

