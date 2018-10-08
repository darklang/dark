module B = Blank
open Types

let astsFor db =
  match db.activeMigration with
  | Nothing -> []
  | Just am -> [am.rollforward; am.rollback]

let allData db =
  let cols, rolls =
    match db.activeMigration with
    | Just migra ->
        ( db.cols ++ migra.cols
        , List.concat
            [AST.allData migra.rollforward; AST.allData migra.rollback] )
    | Nothing -> (db.cols, [])
  in
  let colpointers =
    cols
    |> List.map (fun (lhs, rhs) -> [PDBColName lhs; PDBColType rhs])
    |> List.concat
  in
  colpointers ++ rolls

let siblings _ db = allData db

let hasCol db name =
  db.cols
  |> List.any (fun (colname, _) ->
         match colname with Blank _ -> false | F (_, n) -> name == n )

let isLocked m tlid = not (List.member tlid m.unlockedDBs)

let isMigrating db =
  match db.activeMigration with Just _ -> true | Nothing -> false

let isMigrationCol db id =
  match db.activeMigration with
  | Just schema ->
      let inCols =
        schema.cols
        |> List.filter (fun (n, t) -> (B.toID n == id || B.toID t) == id)
      in
      not (List.isEmpty inCols)
  | Nothing -> false

let isMigrationLockReady m = B.isF m.rollforward && B.isF m.rollback

let startMigration tlid cols =
  let newCols =
    cols |> List.map (fun (n, t) -> (B.clone identity n, B.clone identity t))
  in
  let rb = B.new_ () in
  let rf = B.new_ () in
  RPC ([CreateDBMigration (tlid, B.toID rb, B.toID rf, newCols)], FocusSame)
