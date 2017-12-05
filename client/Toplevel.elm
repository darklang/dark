module Toplevel exposing (..)

-- builtin

-- lib
import List.Extra as LE

-- dark
import DB
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

isThreadHole : Handler -> ID -> Bool
isThreadHole h id =
  h.ast |> AST.listThreadHoles |> List.member id

isBindHole : Handler -> ID -> Bool
isBindHole h id =
  h.ast |> AST.listBindHoles |> List.member id

isSpecHole : Handler -> ID -> Bool
isSpecHole h id =
  h |> specHoles |> List.member id

isDBRowNameHole : DB -> ID -> Bool
isDBRowNameHole db id =
  db |> DB.listRowNameHoles |> List.member id

isDBRowTypeHole : DB -> ID -> Bool
isDBRowTypeHole db id =
  db |> DB.listRowTypeHoles |> List.member id

holeType : Toplevel -> ID -> HoleType
holeType tl id =
  case tl.data of
    TLHandler h ->
      if isBindHole h id
      then BindHole h
      else if isSpecHole h id
      then SpecHole h
      else ExprHole h -- threadholes included here
    TLDB db ->
      if isDBRowNameHole db id
      then DBRowNameHole db
      else DBRowTypeHole db



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
    TLDB db ->
      DB.listHoles db

getNextHole : Toplevel -> Predecessor -> ID
getNextHole tl pred =
  case pred of
    Just pred ->
      let holes = allHoles tl in
      holes
      |> LE.elemIndex pred
      |> Maybe.map ((+) 1)
      |> Maybe.andThen (\i -> LE.getAt i holes)
      |> Maybe.withDefault (firstHole tl)
    Nothing ->
      firstHole tl

getPrevHole : Toplevel -> ID -> Predecessor
getPrevHole tl next =
  let holes = allHoles tl in
  holes
  |> LE.elemIndex next
  |> Maybe.map (\i -> i - 1)
  |> Maybe.andThen (\i -> LE.getAt i holes)

firstHole : Toplevel -> ID
firstHole tl =
  tl |> allHoles |> List.head |> Maybe.withDefault (ID 3)

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


asHandler : Toplevel -> Maybe Handler
asHandler tl =
  case tl.data of
    TLHandler h -> Just h
    _ -> Nothing

asDB : Toplevel -> Maybe DB
asDB tl =
  case tl.data of
    TLDB h -> Just h
    _ -> Nothing

handlers : List Toplevel -> List Handler
handlers tls =
  List.filterMap asHandler tls

dbs : List Toplevel -> List DB
dbs tls =
  List.filterMap asDB tls
