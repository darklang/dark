module DB exposing (..)

-- lib
-- import List.Extra as LE

-- dark
import Types exposing (..)
import Pointer as P

allData : DB -> List PointerData
allData db =
  db.cols
  |> List.map (\(lhs,rhs) -> [PDBColName lhs, PDBColType rhs])
  |> List.concat


allPointers : DB -> List Pointer
allPointers db =
  allData db
  |> List.map P.pdToP

siblings : Pointer -> DB -> List Pointer
siblings _ db = allPointers db

