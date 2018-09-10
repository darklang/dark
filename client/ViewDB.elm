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
import ViewCode exposing (viewExpr)

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

viewDBCol : ViewState -> Bool -> TLID -> DBColumn -> Html.Html Msg
viewDBCol vs isMigra tlid (n, t) =
  let deleteButton =
        if isMigra
        then
          if (B.isF n) || (B.isF t)
          then
            [ Html.div
              [ Attrs.class "delete-col"
              , eventNoPropagation "click" (\_ -> DeleteColInDB (n, t) tlid) ]
              [ fontAwesome "minus-circle" ]
            ]
          else []
        else []
      row =
        [ viewDBColName vs [wc "name"] n
        , viewDBColType vs [wc "type"] t ]
  in Html.div
    [ Attrs.class "col" ]
    (row ++ deleteButton)

viewMigraFuncs : ViewState -> Expr -> FnName -> VarName -> Html.Html Msg
viewMigraFuncs vs expr fnName varName =
  Html.div
    [ Attrs.class "col roll-fn" ]
    [ Html.div [ Attrs.class "fn-title" ]
      [ Html.span [] [Html.text (fnName ++ " : ")]
      , Html.span [Attrs.class "varname"] [Html.text varName]
      ]
    , viewExpr 0 vs [] expr ]

viewDBMigration : DBSchemaMigration -> DB -> ViewState -> Html.Html Msg
viewDBMigration migra db vs =
  let name = viewDBName db.name (migra.version)
      cols = (List.map (viewDBCol vs True db.tlid) migra.cols)
      funcs = 
        [ viewMigraFuncs vs migra.rollforward "Rollforward" "oldObj"
        , viewMigraFuncs vs migra.rollback "Rollback"  "newObj" ]
      cancelBtn =
        Html.button
          [ Attrs.disabled False
          , eventNoPropagation "click" (\_ -> CancelMigration db) ]
          [ Html.text "cancel"]
      migrateBtn =
        Html.button [Attrs.disabled True] [ Html.text "migration"]
      actions =
        [ Html.div
          [Attrs.class "col actions"]
          [ cancelBtn, migrateBtn ]
        ]
  in Html.div
    [ Attrs.class "db migration-view" ]
    (name :: cols ++ funcs ++ actions)

viewDB : ViewState -> DB -> List (Html.Html Msg)
viewDB vs db =
  let locked =
          if vs.dbLocked && (db.newMigration == Nothing)
          then
            Html.div
              [ eventNoPropagation "click" (\_ -> StartMigration db) ]
              [ fontAwesome "lock" ]
          else fontAwesome "unlock"
      namediv = viewDBName db.name (db.version)
      cols =
        if vs.dbLocked
        then List.filter (\(n, t) -> (B.isF n) && (B.isF t)) db.cols 
        else db.cols
      coldivs = List.map (viewDBCol vs False db.tlid) cols
      migrationView =
        case db.newMigration of
          Just migra -> [viewDBMigration migra db vs]
          Nothing -> []
  in
  [
    Html.div
      [ Attrs.class "db"]
      (locked :: namediv :: coldivs)
  ] ++ migrationView

