module DB exposing (..)

-- lib
-- import List.Extra as LE

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
