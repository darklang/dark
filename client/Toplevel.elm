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

isExprHole : Handler -> ID -> Bool
isExprHole h id =
  let bhs = AST.listBindHoles h.ast in
  h.ast
  |> AST.listHoles
  |> List.filter (\hl -> not <| List.member hl bhs)
  |> List.member id

isFieldHole : Handler -> ID -> Bool
isFieldHole h id =
  h.ast |> AST.listFieldHoles |> List.member id

isDBColNameHole : DB -> ID -> Bool
isDBColNameHole db id =
  db |> DB.listColNameHoles |> List.member id

isDBColTypeHole : DB -> ID -> Bool
isDBColTypeHole db id =
  db |> DB.listColTypeHoles |> List.member id

holeType : Toplevel -> ID -> HoleType
holeType tl id =
  case tl.data of
    TLHandler h ->
      if isBindHole h id
      then BindHole h
      else if isSpecHole h id
      then SpecHole h
      else if isFieldHole h id
      then FieldHole h
      else if isExprHole h id
      then ExprHole h
      else NotAHole
    TLDB db ->
      if isDBColNameHole db id
      then DBColNameHole db
      else if isDBColTypeHole db id
      then DBColTypeHole db
      else NotAHole

isHole : Toplevel -> ID -> Bool
isHole tl id =
  case (holeType tl id) of
    NotAHole -> False
    _ -> True

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

getNextSibling : Toplevel -> ID -> ID
getNextSibling tl id =
  case tl.data of
    TLHandler h ->
      let siblings = AST.siblings id h.ast in
      siblings
      |> LE.elemIndex id
      |> Maybe.map ((+) 1)
      |> Maybe.map (\i -> i % List.length siblings)
      |> Maybe.andThen (\i -> LE.getAt i siblings)
      |> Maybe.withDefault id
    _ -> id

getPrevSibling : Toplevel -> ID -> ID
getPrevSibling tl id =
  case tl.data of
    TLHandler h ->
      let siblings = AST.siblings id h.ast
          -- 'safe' to deMaybe as there's always at least
          -- one member in the array
          last = deMaybe <| LE.last siblings
      in
      siblings
      |> LE.elemIndex id
      |> Maybe.map (\i -> i - 1)
      |> Maybe.andThen (\i -> LE.getAt i siblings)
      |> Maybe.withDefault last
    _ -> id

getNextHole : Toplevel -> Predecessor -> Maybe ID
getNextHole tl pred =
  case pred of
    Just pred ->
      let holes = allHoles tl in
      holes
      |> LE.elemIndex pred
      |> Maybe.map ((+) 1)
      |> Maybe.andThen (\i -> LE.getAt i holes)
    Nothing -> Nothing

getPrevHole : Toplevel -> ID -> Predecessor
getPrevHole tl next =
  let holes = allHoles tl in
  holes
  |> LE.elemIndex next
  |> Maybe.map (\i -> i - 1)
  |> Maybe.andThen (\i -> LE.getAt i holes)

firstHole : Toplevel -> Maybe ID
firstHole tl =
  tl |> allHoles |> List.head

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

getParentOf : Toplevel -> ID -> Maybe ID
getParentOf tl id =
  case tl.data of
    TLHandler h ->
      AST.parentOf_ id h.ast |> Maybe.map AST.toID
    _ -> Nothing

getChildrenOf : Toplevel -> ID -> List ID
getChildrenOf tl id =
  case tl.data of
    TLHandler h ->
      AST.childrenOf id h.ast
    _ -> []

firstChild : Toplevel -> ID -> Maybe ID
firstChild tl id = getChildrenOf tl id
                 |> List.head

rootOf : Toplevel -> Maybe ID
rootOf tl =
  case tl.data of
    TLHandler h ->
      Just <| AST.toID h.ast
    _ -> Nothing


