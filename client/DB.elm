module DB exposing (..)

-- lib

-- dark
import Types exposing (..)
import Blank as B
import AST
import DontPort exposing ((@))

astsFor : DB -> List Expr
astsFor db =
  case db.activeMigration of
    Nothing -> []
    Just am -> [am.rollforward, am.rollback]

allData : DB -> List PointerData
allData db =
  let (cols, rolls) =
        case db.activeMigration of
          Just migra ->
            ( db.cols @ migra.cols
            , List.concat [AST.allData migra.rollforward, AST.allData migra.rollback])
          Nothing -> (db.cols, [])
      colpointers =
        cols
          |> List.map (\(lhs,rhs) -> [PDBColName lhs, PDBColType rhs] )
          |> List.concat
  in colpointers @ rolls

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

isMigrating : DB -> Bool
isMigrating db =
  case db.activeMigration of
    Just _ -> True
    Nothing -> False

isMigrationCol : DB -> ID -> Bool
isMigrationCol db id =
  case db.activeMigration of
    Just schema ->
      let inCols = schema.cols
        |> List.filter (\(n, t) -> (B.toID n) == id || (B.toID t) == id )
      in not (List.isEmpty inCols)
    Nothing -> False

isMigrationLockReady : DBMigration -> Bool
isMigrationLockReady m = B.isF m.rollforward && B.isF m.rollback

startMigration : TLID -> List DBColumn -> Modification
startMigration tlid cols =
  let newCols = cols
                |> List.map (\(n, t) -> (B.clone identity n, B.clone identity t))
      rb = B.new ()
      rf = B.new ()
  in RPC ([ CreateDBMigration tlid (B.toID rb) (B.toID rf) newCols ], FocusSame)
