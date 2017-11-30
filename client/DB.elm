module DB exposing (..)

-- lib
import List.Extra as LE

-- dark
import Types exposing (..)


listHoles : DB -> List ID
listHoles db =
  LE.interweave (listRowNameHoles db) (listRowTypeHoles db)

listRowNameHoles : DB -> List ID
listRowNameHoles db =
  let r2h row =
        case row of
          (Empty n, _) -> [n]
          _ -> []
  in
  db.rows
  |> List.map r2h
  |> List.concat

listRowTypeHoles : DB -> List ID
listRowTypeHoles db =
  let r2h row =
        case row of
          (_, Empty t) -> [t]
          _ -> []
  in
  db.rows
  |> List.map r2h
  |> List.concat


