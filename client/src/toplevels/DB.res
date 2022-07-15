open Prelude

// Dark
module B = BlankOr
module TD = TLID.Dict

let toID = (db: db): TLID.t => db.dbTLID

let upsert = (m: model, db: db): model => {...m, dbs: Map.add(~key=db.dbTLID, ~value=db, m.dbs)}

let update = (m: model, ~tlid: TLID.t, ~f: db => db): model => {
  ...m,
  dbs: Map.updateIfPresent(~key=tlid, ~f, m.dbs),
}

let remove = (m: model, db: db): model => {...m, dbs: Map.remove(~key=db.dbTLID, m.dbs)}

let fromList = (dbs: list<db>): TLID.Dict.t<db> =>
  dbs |> List.map(~f=db => (db.dbTLID, db)) |> TLID.Dict.fromList

let blankOrData = (db: db): list<blankOrData> => {
  let colpointers =
    db.cols |> List.map(~f=((lhs, rhs)) => list{PDBColName(lhs), PDBColType(rhs)}) |> List.flatten

  list{PDBName(db.dbName), ...colpointers}
}

let hasCol = (db: db, name: string): bool =>
  db.cols |> List.any(~f=((colname, _)) =>
    switch colname {
    | Blank(_) => false
    | F(_, n) => name == n
    }
  )

let isLocked = (m: model, tlid: TLID.t): bool => !Set.member(~value=tlid, m.unlockedDBs)

let generateDBName = (_: unit): string => "Db" ++ (() |> Util.random |> string_of_int)
