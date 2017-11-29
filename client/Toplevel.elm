module Toplevel exposing (..)

-- builtin

-- lib
import List.Extra as LE

-- dark
import Types exposing (..)
import Util exposing (deMaybe)
import AST

getTL : Model -> TLID -> Toplevel
getTL m id =
  LE.find (\tl -> tl.id == id) m.toplevels
  |> deMaybe

replace : Model -> Toplevel -> Model
replace m tl =
  update m tl.id (\_ -> tl)

firstHole : Model -> TLID -> ID
firstHole m id =
  getTL m id |> allHoles |> List.head |> Maybe.withDefault (ID 3)

isBindHole : Handler -> ID -> Bool
isBindHole h id =
  h.ast |> AST.listBindHoles |> List.member id

isSpecHole : Handler -> ID -> Bool
isSpecHole h id =
  h |> specHoles |> List.member id

specHoles : Handler -> List ID
specHoles h =
  let e2l a =
        case a of
          Empty hid -> [hid]
          _ -> []
  in e2l h.spec.module_ ++ e2l h.spec.name ++ e2l h.spec.modifier

replaceSpecHole : ID -> String -> HandlerSpec -> HandlerSpec
replaceSpecHole id value hs =
  let rh a =
        case a of
          Empty hid ->
            if id == hid
            then Full value
            else a
          _ -> a
  in { name = rh hs.name
     , module_ = rh hs.module_
     , modifier = rh hs.modifier
     }

allHoles : Toplevel -> List ID
allHoles tl =
  case tl.data of
    TLHandler h ->
      AST.listHoles h.ast ++ specHoles h
    TLDB _ -> []

findNextHole : Toplevel -> ID -> ID
findNextHole tl cur =
  let holes = allHoles tl in
  case (LE.dropWhile (\x -> x /= cur) holes) of
     cur :: next :: _ -> next
     [cur] -> holes |> List.head |> deMaybe
     [] -> ID 237


update : Model -> TLID -> (Toplevel -> Toplevel) -> Model
update m tlid f =
  let mapped = List.map (\t ->
        if t.id /= tlid
        then t
        else f t) m.toplevels
  in
      { m | toplevels = mapped }

move : TLID -> Int -> Int -> Model -> Model
move tlid xOffset yOffset m = update m tlid (moveTL xOffset yOffset)

moveTL : Int -> Int -> Toplevel -> Toplevel
moveTL xOffset yOffset tl =
  let newPos = { x = tl.pos.x + xOffset, y = tl.pos.y + yOffset }
  in
      { tl | pos = newPos }

