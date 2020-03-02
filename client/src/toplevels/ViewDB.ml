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
  | Some stats when CursorState.tlidOf vs.cursorState = Some db.dbTLID ->
      let liveVal = viewDbLatestEntry stats in
      let count = viewDbCount stats in
      Html.div [Html.class' "dbdata"] [count; liveVal]
  | _ ->
      Vdom.noNode


let viewDBHeader (vs : viewState) (db : db) : msg Html.html list =
  let typeView =
    Html.span
      [Html.class' "toplevel-type"]
      [fontAwesome "database"; Html.text "DB"]
  in
  let titleView =
    let nameField =
      if vs.dbLocked
      then Html.text (dbName2String db.dbName)
      else
        let c = (enterable :: idConfigs) @ [wc "dbname"] in
        ViewBlankOr.viewText DBName vs c db.dbName
    in
    Html.span
      [Html.class' "toplevel-name"]
      [ nameField
      ; Html.span
          [Html.class' "version"]
          [Html.text (".v" ^ string_of_int db.version)] ]
  in
  let menuView =
    let delAct : TLMenu.menuItem =
      let disableMsg =
        if vs.dbLocked
        then Some "Cannot delete due to data inside"
        else if not (List.isEmpty vs.usedInRefs)
        then Some "Cannot delete because your code refers to this DB"
        else None
      in
      { title = "Delete"
      ; key = "del-db-"
      ; icon = Some "times"
      ; action = (fun _ -> ToplevelDelete vs.tlid)
      ; disableMsg }
    in
    Html.div [Html.class' "menu"] [TLMenu.viewMenu vs.menuState vs.tlid [delAct]]
  in
  [typeView; titleView; menuView]


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
    if vs.permission = Some ReadWrite
       && (isMigra || not vs.dbLocked)
       && (B.isF n || B.isF t)
    then
      Html.div
        [ Html.class' "delete-col"
        ; ViewUtils.eventNoPropagation
            ~key:("dcidb-" ^ showTLID tlid ^ "-" ^ (n |> B.toID |> showID))
            "click"
            (fun _ -> DeleteColInDB (tlid, B.toID n)) ]
        [fontAwesome "minus-circle"]
    else Vdom.noNode
  in
  let row = [viewDBColName vs [wc "name"] n; viewDBColType vs [wc "type"] t] in
  Html.div
    [Html.classList [("col", true); ("has-empty", deleteButton = Vdom.noNode)]]
    (deleteButton :: row)


let viewMigraFuncs (vs : viewState) (desc : string) (varName : string) :
    msg Html.html =
  Html.div
    [Html.class' "col roll-fn"]
    ( [ Html.div
          [Html.class' "fn-title"]
          [ Html.span [] [Html.text (desc ^ " : ")]
          ; Html.span [Html.class' "varname"] [Html.text varName] ] ]
    @ FluidView.view vs )


let viewDBMigration (migra : dbMigration) (db : db) (vs : viewState) :
    msg Html.html =
  let name = Html.text (dbName2String db.dbName) in
  let cols = List.map ~f:(viewDBCol vs true db.dbTLID) migra.cols in
  let funcs =
    (* this AST expr stuff is kind of a hack but until we reintroduce migration
     * fields I don't know what else to do with it -- @dstrelau 2020-02-25 *)
    [ viewMigraFuncs
        {vs with ast = FluidAST.ofExpr migra.rollforward}
        "Rollforward"
        "oldObj"
    ; viewMigraFuncs
        {vs with ast = FluidAST.ofExpr migra.rollback}
        "Rollback"
        "newObj" ]
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
  let lockClass =
    if vs.dbLocked && db.activeMigration = None then "lock" else "unlock"
  in
  let cols =
    if (not (vs.permission = Some ReadWrite)) || vs.dbLocked
    then List.filter ~f:(fun (n, t) -> B.isF n && B.isF t) db.cols
    else db.cols
  in
  let keyView =
    Html.div
      [Html.class' "col key"]
      [Html.text "All entries are identified by a unique string `key`."]
  in
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
  let headerView =
    Html.div [Html.class' ("spec-header " ^ lockClass)] (viewDBHeader vs db)
  in
  [Html.div (Html.class' "db" :: dragEvents) (headerView :: keyView :: coldivs)]
  @ migrationView
  @ [data]
