open Prelude

// Dark
module B = BlankOr
module TD = TLIDDict

let toID = (db: db): TLID.t => db.dbTLID

let upsert = (m: model, db: db): model => {...m, dbs: Map.add(~key=db.dbTLID, ~value=db, m.dbs)}

let update = (m: model, ~tlid: TLID.t, ~f: db => db): model => {
  ...m,
  dbs: Map.updateIfPresent(~key=tlid, ~f, m.dbs),
}

let remove = (m: model, db: db): model => {...m, dbs: Map.remove(~key=db.dbTLID, m.dbs)}

let fromList = (dbs: list<db>): TLIDDict.t<db> =>
  dbs |> List.map(~f=db => (db.dbTLID, db)) |> TLIDDict.fromList

let blankOrData = (db: db): list<blankOrData> => {
  let cols = switch db.activeMigration {
  | Some(migra) => Belt.List.concat(db.cols, migra.cols)
  | None => db.cols
  }

  let colpointers =
    cols |> List.map(~f=((lhs, rhs)) => list{PDBColName(lhs), PDBColType(rhs)}) |> List.flatten

  list{PDBName(db.dbName), ...colpointers}
}

let hasCol = (db: db, name: string): bool =>
  db.cols |> List.any(~f=((colname, _)) =>
    switch colname {
    | Blank(_) => false
    | F(_, n) => name == n
    }
  )

let isLocked = (m: model, tlid: TLID.t): bool =>
  !Set.member(~value=tlid, m.unlockedDBs)

let isMigrationCol = (db: db, id: id): bool =>
  switch db.activeMigration {
  | Some(schema) =>
    let inCols = schema.cols |> List.filter(~f=((n, t)) => B.toID(n) == id || B.toID(t) == id)

    !List.isEmpty(inCols)
  | None => false
  }

let isMigrationLockReady = (m: dbMigration): bool =>
  !(FluidExpression.isBlank(m.rollforward) || FluidExpression.isBlank(m.rollback))

let startMigration = (tlid: TLID.t, cols: list<dbColumn>): modification => {
  let newCols = cols |> List.map(~f=((n, t)) => (B.clone(identity, n), B.clone(identity, t)))

  let rb = B.new_()
  let rf = B.new_()
  AddOps(list{CreateDBMigration(tlid, B.toID(rb), B.toID(rf), newCols)}, FocusSame)
}

let generateDBName = (_: unit): string => "Db" ++ (() |> Util.random |> string_of_int)
