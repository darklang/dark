module DB exposing (..)

-- lib
import List.Extra as LE

-- dark
import Types exposing (..)


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


