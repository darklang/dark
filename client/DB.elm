module DB exposing (..)

-- lib
import List.Extra as LE

-- dark
import Types exposing (..)
import Pointer as P


listBlanks : DB -> List Pointer
listBlanks db =
  LE.interweave (listColNameBlanks db) (listColTypeBlanks db)

listColNameBlanks : DB -> List Pointer
listColNameBlanks db =
  let r2h col =
        case col of
          (Blank n, _) -> [PBlank DBColName n]
          _ -> []
  in
  db.cols
  |> List.map r2h
  |> List.concat

listColTypeBlanks : DB -> List Pointer
listColTypeBlanks db =
  let r2h col =
        case col of
          (_, Blank t) -> [PBlank DBColType t]
          _ -> []
  in
  db.cols
  |> List.map r2h
  |> List.concat

listPointers : DB -> List Pointer
listPointers db =
  let colToList col =
        case col of
          (lhs, rhs) -> [P.blankTo DBColName lhs, P.blankTo DBColType rhs]
  in
      db.cols
      |> List.map colToList
      |> List.concat


