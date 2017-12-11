module DB exposing (..)

-- lib
import List.Extra as LE

-- dark
import Types exposing (..)


listHoles : DB -> List ID
listHoles db =
  LE.interweave (listColNameHoles db) (listColTypeHoles db)

listColNameHoles : DB -> List ID
listColNameHoles db =
  let r2h col =
        case col of
          (Empty n, _) -> [n]
          _ -> []
  in
  db.cols
  |> List.map r2h
  |> List.concat

listColTypeHoles : DB -> List ID
listColTypeHoles db =
  let r2h col =
        case col of
          (_, Empty t) -> [t]
          _ -> []
  in
  db.cols
  |> List.map r2h
  |> List.concat


