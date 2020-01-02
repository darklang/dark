open Prelude

(* Dark *)
module B = BlankOr

type viewState = ViewUtils.viewState

type domEventList = ViewUtils.domEventList

type htmlConfig = ViewBlankOr.htmlConfig

let idConfigs = ViewBlankOr.idConfigs

let fontAwesome = ViewUtils.fontAwesome

let wc = ViewBlankOr.wc

let enterable = ViewBlankOr.Enterable

let dbName2String (name : dbName blankOr) : dbName = B.valueWithDefault "" name

let viewDbCount (stats : dbStats) : msg Html.html =
  Html.div
    [Html.class' "db db-count"]
    [ Html.span
        [Html.class' "dbcount-txt"]
        [Html.text ("# of Entries: " ^ string_of_int stats.count)] ]


let viewDbLatestEntry (stats : dbStats) : msg Html.html =
  let title =
    Html.div
      [Html.class' "title"]
      [ Html.span
          [ Html.classList
              [("label", true); ("show", Option.isSome stats.example)] ]
          [Html.text "Latest Entry:"] ]
  in
  let exampleHtml =
    match stats.example with
    | Some (example, key) ->
        Html.div
          [Html.class' "dbexample"]
          [ Html.div [Html.class' "key"] [Html.text (key ^ ":")]
          ; Html.div [Html.class' "value"] [Html.text (Runtime.toRepr example)]
          ]
    | None ->
        Vdom.noNode
  in
  Html.div [Html.class' "db db-liveVal"] [title; exampleHtml]


let viewDBData (vs : viewState) (db : db) : msg Html.html =
  match StrDict.get ~key:(deTLID db.dbTLID) vs.dbStats with
  | Some stats when tlidOf vs.cursorState = Some db.dbTLID ->
      let liveVal = viewDbLatestEntry stats in
      let count = viewDbCount stats in
      Html.div [Html.class' "dbdata"] [count; liveVal]
  | _ ->
      Vdom.noNode


let viewDBName (vs : viewState) (db : db) : msg Html.html =
  let nameField =
    if vs.dbLocked
    then Html.span [Html.class' "name"] [Html.text (dbName2String db.dbName)]
    else
      let c = (enterable :: idConfigs) @ [wc "dbname"] in
      ViewBlankOr.viewText DBName vs c db.dbName
  in
  Html.div
    [Html.class' "dbtitle"]
    [ nameField
    ; Html.span
        [Html.class' "version"]
        [Html.text (".v" ^ string_of_int db.version)] ]


let viewDBColName (vs : viewState) (c : htmlConfig list) (v : string blankOr) :
    msg Html.html =
  let configs =
    if B.isBlank v || not vs.dbLocked then (enterable :: idConfigs) @ c else c
  in
  ViewBlankOr.viewText DBColName vs configs v


let viewDBColType (vs : viewState) (c : htmlConfig list) (v : string blankOr) :
    msg Html.html =
  let configs =
    if B.isBlank v || not vs.dbLocked then (enterable :: idConfigs) @ c else c
  in
  ViewBlankOr.viewText DBColType vs configs v


let viewDBCol
    (vs : viewState) (isMigra : bool) (tlid : tlid) ((n, t) : dbColumn) :
    msg Html.html =
  let deleteButton =
    if isMigra || not vs.dbLocked
    then
      if B.isF n || B.isF t
      then
        Html.div
          [ Html.class' "delete-col"
          ; ViewUtils.eventNoPropagation
              ~key:("dcidb-" ^ showTLID tlid ^ "-" ^ (n |> B.toID |> showID))
              "click"
              (fun _ -> DeleteColInDB (tlid, B.toID n)) ]
          [fontAwesome "minus-circle"]
      else Vdom.noNode
    else Vdom.noNode
  in
  let row = [viewDBColName vs [wc "name"] n; viewDBColType vs [wc "type"] t] in
  Html.div
    [Html.class' "col"]
    ((if vs.permission = Some ReadWrite then [deleteButton] else []) @ row)


let viewMigraFuncs
    (vs : viewState) (expr : fluidExpr) (desc : string) (varName : string) :
    msg Html.html =
  Html.div
    [Html.class' "col roll-fn"]
    ( [ Html.div
          [Html.class' "fn-title"]
          [ Html.span [] [Html.text (desc ^ " : ")]
          ; Html.span [Html.class' "varname"] [Html.text varName] ] ]
    @ FluidView.view vs expr )


let viewDBMigration (migra : dbMigration) (db : db) (vs : viewState) :
    msg Html.html =
  let name = Html.text (dbName2String db.dbName) in
  let cols = List.map ~f:(viewDBCol vs true db.dbTLID) migra.cols in
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
    Html.button [Html.Attributes.disabled (not lockReady)] [Html.text "activate"]
  in
  let actions =
    [Html.div [Html.class' "col actions"] [cancelBtn; migrateBtn]]
  in
  Html.div
    [Html.class' "db migration-view"]
    (name :: (cols @ funcs @ errorMsg @ actions))


let viewDB (vs : viewState) (db : db) (dragEvents : domEventList) :
    msg Html.html list =
  let locked =
    if vs.dbLocked && db.activeMigration = None
    then
      Html.div
        (* DB migrations dont work yet, and they just confuse users who open
         * them. *)
        (* [ ViewUtils.eventNoPropagation *)
        (*     ~key:("sm-" ^ showTLID db.dbTLID) *)
        (*     "click" *)
        (*     (fun _ -> StartMigration db.dbTLID) ] *)
        []
        [fontAwesome "lock"]
    else fontAwesome "unlock"
  in
  let namediv = viewDBName vs db in
  let cols =
    if (not (vs.permission = Some ReadWrite)) || vs.dbLocked
    then List.filter ~f:(fun (n, t) -> B.isF n && B.isF t) db.cols
    else db.cols
  in
  let keyView = Html.div [Html.class' "col key"] [Html.text "key : String"] in
  let coldivs = List.map ~f:(viewDBCol vs false db.dbTLID) cols in
  let data = viewDBData vs db in
  let migrationView =
    match db.activeMigration with
    | Some migra ->
        if migra.state <> DBMigrationAbandoned
        then [viewDBMigration migra db vs]
        else []
    | None ->
        []
  in
  [ Html.div
      (Html.class' "db" :: dragEvents)
      (locked :: namediv :: keyView :: coldivs) ]
  @ migrationView
  @ [data]
