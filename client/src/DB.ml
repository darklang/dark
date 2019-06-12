open Tc
open Types
open Prelude

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
    |> List.map ~f:(fun (lhs, rhs) -> [PDBColName lhs; PDBColType rhs])
    |> List.concat
  in
  let name = PDBName db.dbName in
  (name :: colpointers) @ rolls


let hasCol (db : dB) (name : string) : bool =
  db.cols
  |> List.any ~f:(fun (colname, _) ->
         match colname with Blank _ -> false | F (_, n) -> name = n )


let isLocked (m : model) (TLID tlid : tlid) : bool =
  not (StrSet.has ~value:tlid m.unlockedDBs)


let isMigrationCol (db : dB) (id : id) : bool =
  match db.activeMigration with
  | Some schema ->
      let inCols =
        schema.cols
        |> List.filter ~f:(fun (n, t) -> B.toID n = id || B.toID t = id)
      in
      not (List.isEmpty inCols)
  | None ->
      false


let isMigrationLockReady (m : dBMigration) : bool =
  B.isF m.rollforward && B.isF m.rollback


let startMigration (tlid : tlid) (cols : dBColumn list) : modification =
  let newCols =
    cols |> List.map ~f:(fun (n, t) -> (B.clone identity n, B.clone identity t))
  in
  let rb = B.new_ () in
  let rf = B.new_ () in
  RPC ([CreateDBMigration (tlid, B.toID rb, B.toID rf, newCols)], FocusSame)


let createDB (name : string) (pos : pos) : modification =
  let next = Prelude.gid () in
  let tlid = Prelude.gtlid () in
  (* This is not _strictly_ correct, as there's no guarantee that the new DB
   * doesn't share a name with an old DB in a weird state that still has
   * data in the user_data table. But it's 99.999% correct, which of course
   * is the best type of correct *)
  Many
    [ AppendUnlockedDBs (StrSet.fromList [deTLID tlid])
    ; RPC
        ( [ CreateDBWithBlankOr (tlid, pos, Prelude.gid (), name)
          ; AddDBCol (tlid, next, Prelude.gid ()) ]
        , FocusExact (tlid, next) ) ]


let generateDBName (_ : unit) : string =
  "Db" ^ (() |> Util.random |> string_of_int)
