module DB exposing (..)

-- lib
-- import List.Extra as LE

-- dark
import Types exposing (..)
import Pointer as P

listPointers : DB -> List Pointer
listPointers db =
  let colToList col =
        case col of
          (lhs, rhs) -> [P.blankTo DBColName lhs, P.blankTo DBColType rhs]
  in
      db.cols
      |> List.map colToList
      |> List.concat


