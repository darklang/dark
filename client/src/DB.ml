open! Porting
open Types

(* Dark *)
module B = Blank

let astsFor (db : dB) : expr list =
  match db.activeMigration with
  | None -> []
  | Some am -> [am.rollforward; am.rollback]

let allData (db : dB) : pointerData list =
  let cols, rolls =
    match db.activeMigration with
    | Some migra ->
        ( db.cols @ migra.cols
        , List.concat
            [AST.allData migra.rollforward; AST.allData migra.rollback] )
    | None -> (db.cols, [])
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
  | None -> false

let isMigrationLockReady (m : dBMigration) : bool =
  B.isF m.rollforward && B.isF m.rollback

let startMigration (tlid : tlid) (cols : dBColumn list) : modification =
  let newCols =
    cols |> List.map (fun (n, t) -> (B.clone identity n, B.clone identity t))
  in
  let rb = B.new_ () in
  let rf = B.new_ () in
  RPC ([CreateDBMigration (tlid, B.toID rb, B.toID rf, newCols)], FocusSame)
