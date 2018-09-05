module ViewDB exposing (viewDB)

-- builtin

-- lib
import Html
import Html.Attributes as Attrs
import Maybe.Extra as ME
import Nineteen.String as String

-- dark
import Blank as B
import Types exposing (..)
import ViewBlankOr exposing (..)
import ViewUtils exposing (..)

viewDBName : String -> Int -> Html.Html Msg
viewDBName name version =
  Html.div
    [ Attrs.class "dbname" ]
    [ Html.span [ Attrs.class "name"] [ Html.text name]
      , Html.span
          [ Attrs.class "version" ]
          [ Html.text (".v" ++ (toString version)) ]
    ]

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

viewDBCol : ViewState -> (BlankOr DBColName, BlankOr DBColType) -> Html.Html Msg
viewDBCol vs (n, t) =
  Html.div
    [ Attrs.class "col" ]
    [ viewDBColName vs [wc "name"] n
    , viewDBColType vs [wc "type"] t
    ]

viewDBMigration : DBSchemaMigration -> DBName -> ViewState -> Html.Html Msg
viewDBMigration migra dbname vs =
  let name = viewDBName dbname (migra.version)
      cols = List.map (viewDBCol vs) migra.cols
  in Html.div
    [ Attrs.class "db migration-view" ]
    (name :: cols)


viewDB : ViewState -> DB -> List (Html.Html Msg)
viewDB vs db =
  let locked =
          if vs.dbLocked && (vs.dbMigration == Nothing)
          then fontAwesome "lock" (Just (StartMigration db))
          else fontAwesome "unlock" Nothing
      namediv = viewDBName db.name (db.version)
      coldivs = List.map (viewDBCol vs) db.cols
      migrations =
        case vs.dbMigration of
          Just migra -> [viewDBMigration migra db.name vs]
          Nothing -> []
  in
  [
    Html.div
      [ Attrs.class "db"]
      (locked :: namediv :: coldivs)
  ] ++ migrations
