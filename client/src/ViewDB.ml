open! Porting
open Types
open Prelude

(* Dark *)
module B = Blank

type viewState = ViewUtils.viewState

type htmlConfig = ViewBlankOr.htmlConfig

let idConfigs = ViewBlankOr.idConfigs

let fontAwesome = ViewUtils.fontAwesome

let wc = ViewBlankOr.wc

let enterable = ViewBlankOr.Enterable

let viewDBName (name : string) (version : int) : msg Html.html =
  Html.div
    [Html.class' "dbname"]
    [ Html.span [Html.class' "name"] [Html.text name]
    ; Html.span
        [Html.class' "version"]
        [Html.text (".v" ^ string_of_int version)] ]


let viewDBColName (vs : viewState) (c : htmlConfig list) (v : string blankOr) :
    msg Html.html =
  let configs =
    if B.isBlank v || not vs.dbLocked then (enterable :: idConfigs) @ c else c
  in
  ViewBlankOr.viewText DBColName vs configs v


let viewDBColType (vs : viewState) (c : htmlConfig list) (v : string blankOr) :
    msg Html.html =
  let configs = (enterable :: idConfigs) @ c in
  ViewBlankOr.viewText DBColType vs configs v


let viewDBCol
    (vs : viewState) (isMigra : bool) (tlid : tlid) ((n, t) : dBColumn) :
    msg Html.html =
  let deleteButton =
    if isMigra
    then
      if B.isF n || B.isF t
      then
        [ Html.div
            [ Html.class' "delete-col"
            ; ViewUtils.eventNoPropagation
                ~key:("dcidb-" ^ showTLID tlid ^ "-" ^ (n |> B.toID |> showID))
                "click"
                (fun _ -> DeleteColInDB (tlid, B.toID n)) ]
            [fontAwesome "minus-circle"] ]
      else []
    else []
  in
  let row = [viewDBColName vs [wc "name"] n; viewDBColType vs [wc "type"] t] in
  Html.div [Html.class' "col"] (row @ deleteButton)


let viewMigraFuncs
    (vs : viewState) (expr : expr) (fnName : fnName) (varName : varName) :
    msg Html.html =
  Html.div
    [Html.class' "col roll-fn"]
    [ Html.div
        [Html.class' "fn-title"]
        [ Html.span [] [Html.text (fnName ^ " : ")]
        ; Html.span [Html.class' "varname"] [Html.text varName] ]
    ; ViewCode.viewExpr 0 vs [] expr ]


let viewDBMigration (migra : dBMigration) (db : dB) (vs : viewState) :
    msg Html.html =
  let name = viewDBName db.dbName migra.version in
  let cols = List.map (viewDBCol vs true db.dbTLID) migra.cols in
  let funcs =
    [ viewMigraFuncs vs migra.rollforward "Rollforward" "oldObj"
    ; viewMigraFuncs vs migra.rollback "Rollback" "newObj" ]
  in
  let lockReady = DB.isMigrationLockReady migra in
  let errorMsg =
    if not lockReady
    then
      [ Html.div
          [Html.class' "col err"]
          [ Html.text
              "Fill in rollback and rollforward functions to activate your migration"
          ] ]
    else []
  in
  let cancelBtn =
    Html.button
      [ Html.Attributes.disabled false
      ; ViewUtils.eventNoPropagation
          ~key:("am-" ^ showTLID db.dbTLID)
          "click"
          (fun _ -> AbandonMigration db.dbTLID) ]
      [Html.text "cancel"]
  in
  let migrateBtn =
    Html.button
      [Html.Attributes.disabled (not lockReady)]
      [Html.text "activate"]
  in
  let actions =
    [Html.div [Html.class' "col actions"] [cancelBtn; migrateBtn]]
  in
  Html.div
    [Html.class' "db migration-view"]
    (name :: (cols @ funcs @ errorMsg @ actions))


let viewDB (vs : viewState) (db : dB) : msg Html.html list =
  let locked =
    if vs.dbLocked && db.activeMigration = None
    then
      Html.div
        [ ViewUtils.eventNoPropagation
            ~key:("sm-" ^ showTLID db.dbTLID)
            "click"
            (fun _ -> StartMigration db.dbTLID) ]
        [fontAwesome "lock"]
    else fontAwesome "unlock"
  in
  let namediv = viewDBName db.dbName db.version in
  let cols =
    if vs.dbLocked
    then List.filter (fun (n, t) -> B.isF n && B.isF t) db.cols
    else db.cols
  in
  let coldivs = List.map (viewDBCol vs false db.dbTLID) cols in
  let migrationView =
    match db.activeMigration with
    | Some migra ->
        if migra.state <> DBMigrationAbandoned
        then [viewDBMigration migra db vs]
        else []
    | None ->
        []
  in
  [Html.div [Html.class' "db"] (locked :: namediv :: coldivs)] @ migrationView
