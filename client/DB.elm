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
  let (cols, rolls) =
        case db.newMigration of
          Just migra ->
            ( db.cols ++ migra.cols
            , List.concat [AST.allData migra.rollforward, AST.allData migra.rollback])
          Nothing -> (db.cols, [])
      colpointers =
        cols
          |> List.map (\(lhs,rhs) -> [PDBColName lhs, PDBColType rhs] )
          |> List.concat
  in colpointers ++ rolls

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

isMigrating : DB -> Bool
isMigrating db =
  case db.newMigration of
    Just _ -> True
    Nothing -> False

isMigrationCol : ID -> DB -> Bool
isMigrationCol id db =
  case db.newMigration of
    Just schema ->
      let inCols = schema.cols
          |> List.filter (\(n, t) -> (B.toID n) == id || (B.toID t) == id )
      in not (List.isEmpty inCols)
    Nothing -> False

startMigration : DB -> Modification
startMigration db =
  let newCols = db.cols
                |> List.map (\(n, t) -> (B.clone identity n, B.clone identity t))
      migra = DBSchemaMigration newCols (B.new ()) (B.new ()) (db.version + 1)
      newDB = { db | newMigration = Just migra }
  in UpdateDB newDB

updateMigrationCol : DB -> ID -> Maybe String -> Modification
updateMigrationCol db id val =
  case db.newMigration of
    Just migra ->
      let value =
            case val of
              Just str -> B.newF str
              Nothing -> B.new ()
          replacer = B.replace id value
          newCols = migra.cols
                    |> List.map (\(n, t) -> (replacer n, replacer t))
      in UpdateDB { db | newMigration = Just ({ migra | cols = newCols }) }
    _ -> NoChange
