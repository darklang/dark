module DB exposing (..)

-- lib
-- import List.Extra as LE

-- dark
import Types exposing (..)
import Prelude exposing (..)
import Blank as B

allData : DB -> List PointerData
allData db =
  db.cols
  |> List.map (\(lhs,rhs) -> [PDBColName lhs, PDBColType rhs])
  |> List.concat


siblings : PointerData -> DB -> List PointerData
siblings _ db = allData db

hasCol : DB -> String -> Bool
hasCol db name =
  db.cols
  |> List.any
        (\(colname,_) ->
          case colname of
            Blank _ -> False
            F _ n -> name == n
            Flagged _ _ _ _ _ ->
              impossible ("no flags allowed in DBs", colname))

isLocked : Model -> TLID -> Bool
isLocked m tlid =
  not (List.member tlid m.unlockedDBs)

initFieldTypeMigration : Model -> Toplevel -> (BlankOr String) -> Modification
initFieldTypeMigration m tl tipe =
  RPC ([InitDBMigration tl.id (B.toID tipe) ChangeColType]
      , FocusSame)
