open! Porting
open Types

(* Dark *)
module B = Blank

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
  let tdb = m.unnamedDBs
  and dbs = { udbId = tlid ; udbName = "" ; udbPos = center } :: m.unnamedDBs.dbs
  in
  TweakModel (fun m -> { m with unnamedDBs = { tdb with dbs = dbs } })

let focusOnUnnamedDB (m : model) (id : tlid) : modification =
  let tdb = m.unnamedDBs in 
  TweakModel (fun m -> {m with unnamedDBs = { tdb with focused_db = Some id } })

let updateOnUnnamedDB (m: model) (name : string) : modification =
  match m.unnamedDBs.focused_db with
  | Some id ->
    let l =
      List.replace
      (fun d -> d.udbId = id)
      (fun d -> {d with udbName = name})
      m.unnamedDBs.dbs
    in
    let tdb = m.unnamedDBs in 
    TweakModel (fun m -> { m with unnamedDBs = { tdb with dbs = l } })
  | None -> NoChange

let blurOnUnnamedDB (m : model) : modification =
  match m.unnamedDBs.focused_db with
  | Some id ->
    let db = List.find (fun d -> d.udbId = id) m.unnamedDBs.dbs in
    (match db with
    | Some d -> (* Validate: regex and unique name, make RPC call to createdb, unselect this db from focused_db, and remove it from list of unnamed db*)
      Debug.loG "created db named " d.udbName; NoChange
    | None -> NoChange
    )
  | None -> NoChange
