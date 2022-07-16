open Prelude

// Dark
module B = BlankOr
module TD = TLID.Dict

let toID = (db: PT.DB.t): TLID.t => db.tlid

let upsert = (m: model, db: PT.DB.t): model => {
  ...m,
  dbs: Map.add(~key=db.tlid, ~value=db, m.dbs),
}

let update = (m: model, ~tlid: TLID.t, ~f: PT.DB.t => PT.DB.t): model => {
  ...m,
  dbs: Map.updateIfPresent(~key=tlid, ~f, m.dbs),
}

let remove = (m: model, db: PT.DB.t): model => {...m, dbs: Map.remove(~key=db.tlid, m.dbs)}

let fromList = (dbs: list<PT.DB.t>): TLID.Dict.t<PT.DB.t> =>
  dbs |> List.map(~f=(db: PT.DB.t) => (db.tlid, db)) |> TLID.Dict.fromList

let blankOrData = (db: PT.DB.t): list<blankOrData> => {
  let colpointers =
    db.cols |> List.map(~f=((lhs, rhs)) => list{PDBColName(lhs), PDBColType(rhs)}) |> List.flatten

  list{PDBName(db.name), ...colpointers}
}

let hasCol = (db: PT.DB.t, name: string): bool =>
  db.cols |> List.any(~f=((colname, _)) =>
    switch colname {
    | Blank(_) => false
    | F(_, n) => name == n
    }
  )

let isLocked = (m: model, tlid: TLID.t): bool => !Set.member(~value=tlid, m.unlockedDBs)

let generateDBName = (_: unit): string => "Db" ++ (() |> Util.random |> string_of_int)
