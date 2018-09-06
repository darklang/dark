module DB exposing (..)

-- lib
import Dict exposing (Dict)

-- dark
import Types exposing (..)
import Blank as B
import AST

astsFor : DB -> List Expr
astsFor db =
  case db.activeMigration of
    Nothing -> []
    Just am -> [am.rollforward, am.rollback]

allData : DB -> List PointerData
allData db =
  let migrationData m =
        [AST.allData m.rollforward, AST.allData m.rollback]
        |> List.concat
      migrationBlanks  =
        db.activeMigration
        |> Maybe.map migrationData
        |> Maybe.withDefault []
  in
  db.cols
  |> List.map (\(lhs,rhs) -> [PDBColName lhs, PDBColType rhs])
  |> List.concat
  |> (++) migrationBlanks


findMigraExpr : DBSchemaMigration -> ID -> PointerData
findMigraExpr migra id =
  -- TODO find roll/back forward functions too
  let selectCols =
        migra.cols
        |> List.filterMap
          (\(cn, ct) ->
            if (B.toID cn) == id then Just (PDBColName cn)
            else if (B.toID ct) == id then Just (PDBColType ct)
            else Nothing
          )
      foundFirst = List.head selectCols
  in deMaybe "findMigraExpr" foundFirst


siblings : PointerData -> DB -> List PointerData
siblings _ db = allData db

hasCol : DB -> String -> Bool
hasCol db name =
  db.cols
  |> List.any
        (\(colname,_) ->
          case colname of
            Blank _ -> False
            F _ n -> name == n)

isLocked : Model -> TLID -> Bool
isLocked m tlid =
  not (List.member tlid m.unlockedDBs)

initFieldTypeMigration : Model -> Toplevel -> (BlankOr String) -> Modification
initFieldTypeMigration m tl tipe =
  RPC ([InitDBMigration tl.id (B.toID tipe) (gid ()) (gid ()) ChangeColType]
      , FocusSame)

isMigrating : Model -> DBName -> Bool
isMigrating m name = Dict.member name m.dbMigrations

isMigrationCol : ID -> DBSchemaMigration -> Bool
isMigrationCol id schema =
  let inCols = schema.cols
               |> List.filter (\(n, t) -> (B.toID n) == id || (B.toID t) == id )
  in not (List.isEmpty inCols)

startMigration : DB -> Modification
startMigration db =
  let newCols = db.cols
                |> List.map (\(n, t) -> (B.clone identity n, B.clone identity t))
      migra = DBSchemaMigration newCols (db.version + 1) (B.new ()) (B.new ())
  in AddDBMigration db.name migra
