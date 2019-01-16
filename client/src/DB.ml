open! Porting
open Types

(* Dark *)
module B = Blank

let validateName (s : string) = Util.reExactly "[A-Z][a-zA-Z0-9_-]+" s

let astsFor (db : dB) : expr list =
  match db.activeMigration with
  | None ->
      []
  | Some am ->
      [am.rollforward; am.rollback]


let allData (db : dB) : pointerData list =
  let cols, rolls =
    match db.activeMigration with
    | Some migra ->
        ( db.cols @ migra.cols
        , List.concat
            [AST.allData migra.rollforward; AST.allData migra.rollback] )
    | None ->
        (db.cols, [])
  in
  let colpointers =
    cols
    |> List.map (fun (lhs, rhs) -> [PDBColName lhs; PDBColType rhs])
    |> List.concat
  in
  colpointers @ rolls


let siblings (_ : pointerData) (db : dB) : pointerData list = allData db

let hasCol (db : dB) (name : string) : bool =
  db.cols
  |> List.any (fun (colname, _) ->
         match colname with Blank _ -> false | F (_, n) -> name = n )


let isLocked (m : model) (tlid : tlid) : bool =
  not (List.member tlid m.unlockedDBs)


let isMigrating (db : dB) : bool =
  match db.activeMigration with Some _ -> true | None -> false


let isMigrationCol (db : dB) (id : id) : bool =
  match db.activeMigration with
  | Some schema ->
      let inCols =
        schema.cols
        |> List.filter (fun (n, t) -> B.toID n = id || B.toID t = id)
      in
      not (List.isEmpty inCols)
  | None ->
      false


let isMigrationLockReady (m : dBMigration) : bool =
  B.isF m.rollforward && B.isF m.rollback


let startMigration (tlid : tlid) (cols : dBColumn list) : modification =
  let newCols =
    cols |> List.map (fun (n, t) -> (B.clone identity n, B.clone identity t))
  in
  let rb = B.new_ () in
  let rf = B.new_ () in
  RPC ([CreateDBMigration (tlid, B.toID rb, B.toID rf, newCols)], FocusSame)


let createUnnamedDB (m : model) (tlid : tlid) (center : pos) : modification =
  let udb = m.unnamedDBs
  and dbs =
    {udbId = tlid; udbName = ""; udbPos = center; udbError = None}
    :: m.unnamedDBs.dbs
  in
  TweakModel (fun m -> {m with unnamedDBs = {udb with dbs}})


let focusOnUnnamedDB (m : model) (id : tlid) : modification =
  let udb = m.unnamedDBs in
  TweakModel (fun m -> {m with unnamedDBs = {udb with focusedDB = Some id}})


let updateOnUnnamedDB (m : model) (name : string) : modification =
  match m.unnamedDBs.focusedDB with
  | Some id ->
      let l =
        List.replace
          (fun d -> d.udbId = id)
          (fun d -> {d with udbName = name})
          m.unnamedDBs.dbs
      and tdb = m.unnamedDBs in
      TweakModel (fun m -> {m with unnamedDBs = {tdb with dbs = l}})
  | None ->
      NoChange


let errorOnUnnamedDB (m : model) (error : string) : model =
  match m.unnamedDBs.focusedDB with
  | Some id ->
      let l =
        List.replace
          (fun d -> d.udbId = id)
          (fun d -> {d with udbError = Some error})
          m.unnamedDBs.dbs
      and tdb = m.unnamedDBs in
      {m with unnamedDBs = {tdb with dbs = l}}
  | None ->
      m


let allDBNames (toplevels : toplevel list) : string list =
  toplevels
  |> List.filterMap (fun tl ->
         match tl.data with TLDB db -> Some db.dbName | _ -> None )


let validateNewDBName (m : model) (db : udb) : modification =
  let name = db.udbName in
  if not (validateName name)
  then
    DBNameError
      "Database names must start with a capitial letter and be alphanumeric"
  else if List.member name (allDBNames m.toplevels)
  then DBNameError ("There is already a database named " ^ name)
  else
    let next = Prelude.gid () in
    Many
      [ RemoveUnnamedDB db
      ; RPC
          ( [ CreateDB (db.udbId, db.udbPos, db.udbName)
            ; AddDBCol (db.udbId, next, Prelude.gid ()) ]
          , FocusExact (db.udbId, next) ) ]


let blurOnUnnamedDB (m : model) : modification =
  match m.unnamedDBs.focusedDB with
  | Some id ->
      let db = List.find (fun d -> d.udbId = id) m.unnamedDBs.dbs in
      (match db with Some d -> validateNewDBName m d | None -> NoChange)
  | None ->
      NoChange


let removeUnnamedDB (m : model) (db : udb) : model =
  let udbs = List.filter (fun d -> d.udbId <> db.udbId) m.unnamedDBs.dbs in
  {m with unnamedDBs = {dbs = udbs; focusedDB = None}}
