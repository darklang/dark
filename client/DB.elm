module DB exposing (..)

-- lib
-- import List.Extra as LE

-- dark
import Types exposing (..)
import Blank

allPointers : DB -> List Pointer
allPointers db =
  let colToList col =
        case col of
          (lhs, rhs) -> [Blank.toP DBColName lhs, Blank.toP DBColType rhs]
  in
      db.cols
      |> List.map colToList
      |> List.concat

siblings : Pointer -> DB -> List Pointer
siblings p db = allPointers db

