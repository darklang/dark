open Belt
open Porting
module B = Blank
module Attrs = Html.Attributes
open Types
open ViewBlankOr
open ViewUtils

let viewDBName name version =
  Html.div [Attrs.class_ "dbname"]
    [ Html.span [Attrs.class_ "name"] [Html.text name]
    ; Html.span [Attrs.class_ "version"] [Html.text (".v" ^ toString version)]
    ]

let viewDBColName vs c v =
  let configs = if B.isBlank v || not vs.dbLocked then idConfigs ^ c else c in
  viewText DBColName vs configs v

let viewDBColType vs c v =
  let configs = idConfigs ^ c in
  viewText DBColType vs configs v

let viewDBCol vs isMigra tlid (n, t) =
  let deleteButton =
    if isMigra then
      if B.isF n || B.isF t then
        [ Html.div
            [ Attrs.class_ "delete-col"
            ; eventNoPropagation "click" (fun _ ->
                  DeleteColInDB (tlid, B.toID n) ) ]
            [fontAwesome "minus-circle"] ]
      else []
    else []
  in
  let row = [viewDBColName vs [wc "name"] n; viewDBColType vs [wc "type"] t] in
  Html.div [Attrs.class_ "col"] (row ^ deleteButton)

let viewMigraFuncs vs expr fnName varName =
  Html.div
    [Attrs.class_ "col roll-fn"]
    [ Html.div [Attrs.class_ "fn-title"]
        [ Html.span [] [Html.text (fnName ^ " : ")]
        ; Html.span [Attrs.class_ "varname"] [Html.text varName] ]
    ; viewExpr 0 vs [] expr ]

let viewDBMigration migra db vs =
  let name = viewDBName db.name migra.version in
  let cols = List.map (viewDBCol vs true db.tlid) migra.cols in
  let funcs =
    [ viewMigraFuncs vs migra.rollforward "Rollforward" "oldObj"
    ; viewMigraFuncs vs migra.rollback "Rollback" "newObj" ]
  in
  let lockReady = isMigrationLockReady migra in
  let errorMsg =
    if not lockReady then
      [ Html.div [Attrs.class_ "col err"]
          [ Html.text
              "Fill in rollback and rollforward functions to activate your \
               migration" ] ]
    else []
  in
  let cancelBtn =
    Html.button
      [ Attrs.disabled false
      ; eventNoPropagation "click" (fun _ -> AbandonMigration db.tlid) ]
      [Html.text "cancel"]
  in
  let migrateBtn =
    Html.button [Attrs.disabled (not lockReady)] [Html.text "activate"]
  in
  let actions =
    [Html.div [Attrs.class_ "col actions"] [cancelBtn; migrateBtn]]
  in
  Html.div
    [Attrs.class_ "db migration-view"]
    ((((name :: cols) ^ funcs) ^ errorMsg) ^ actions)

let viewDB vs db =
  let locked =
    if vs.dbLocked && db.activeMigration = None then
      Html.div
        [eventNoPropagation "click" (fun _ -> StartMigration db.tlid)]
        [fontAwesome "lock"]
    else fontAwesome "unlock"
  in
  let namediv = viewDBName db.name db.version in
  let cols =
    if vs.dbLocked then List.filter (fun (n, t) -> B.isF n && B.isF t) db.cols
    else db.cols
  in
  let coldivs = List.map (viewDBCol vs false db.tlid) cols in
  let migrationView =
    match db.activeMigration with
    | Some migra ->
        if migra.state <> DBMigrationAbandoned then
          [viewDBMigration migra db vs]
        else []
    | None -> []
  in
  [Html.div [Attrs.class_ "db"] ((locked :: namediv) :: coldivs)]
  ^ migrationView
