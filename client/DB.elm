module DB exposing (..)

-- lib
import String

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

isMigrating : DB -> Bool
isMigrating db =
  case db.newMigration of
    Just _ -> True
    Nothing -> False

isMigrationCol : DB -> ID -> Bool
isMigrationCol db id =
  case db.newMigration of
    Just schema ->
      let inCols = schema.cols
        |> List.filter (\(n, t) -> (B.toID n) == id || (B.toID t) == id )
      in not (List.isEmpty inCols)
    Nothing -> False

maybeAddBlankField : List DBColumn -> List DBColumn
maybeAddBlankField cols =
  if List.any (\(n, t) -> (B.isBlank n) || (B.isBlank t) ) cols
  then cols
  else cols ++ [(B.new (), B.new ())]

startMigration : DB -> Modification
startMigration db =
  let newCols = db.cols
    |> List.map (\(n, t) -> (B.clone identity n, B.clone identity t))
      migra = DBSchemaMigration newCols (B.new ()) (B.new ()) (db.version + 1)
      newDB = { db | newMigration = Just migra }
  in UpdateDB newDB

updateMigrationCol : DB -> ID -> String -> Modification
updateMigrationCol db id val =
  case db.newMigration of
    Just migra ->
      let value = if (String.isEmpty val) then B.new () else B.newF val
          replacer = B.replace id value
          newCols = migra.cols
            |> List.map (\(n, t) -> (replacer n, replacer t))
            |> maybeAddBlankField
      in UpdateDB { db | newMigration = Just ({ migra | cols = newCols }) }
    _ -> NoChange

deleteCol : DB -> DBColumn -> Modification
deleteCol db (n, t) =
  case db.newMigration of
    Just migra ->
      let nid = B.toID n
          tid = B.toID t
          cols = migra.cols
            |> List.filter (\(cn, ct) -> (B.toID cn) /= nid && (B.toID ct) /= tid )
      in UpdateDB { db | newMigration = Just ({ migra | cols = cols }) }
    _ -> NoChange
