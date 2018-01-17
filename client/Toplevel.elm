module Toplevel exposing (..)

-- builtin

-- lib
import List.Extra as LE

-- dark
import DB
import Types exposing (..)
import Util exposing (deMaybe)
import AST
import Pointer as P

getTL : Model -> TLID -> Toplevel
getTL m id =
  LE.find (\tl -> tl.id == id) m.toplevels
  |> deMaybe "getTL"

replace : Model -> Toplevel -> Model
replace m tl =
  update m tl.id (\_ -> tl)

isThread : Handler -> Pointer -> Bool
isThread h p =
  h.ast |> AST.listThreadHoles |> List.member (P.idOf p)

specBlanks : Handler -> List Pointer
specBlanks h =
  let e2l a =
        case a of
          Blank hid -> [PBlank Spec hid]
          _ -> []
  in e2l h.spec.module_ ++ e2l h.spec.name ++ e2l h.spec.modifier

getSpec : Handler -> Pointer -> Maybe (BlankOr String)
getSpec h p =
  [h.spec.module_, h.spec.name, h.spec.modifier]
  |> List.filter (\spec -> blankOrID spec == P.idOf p)
        -- TODO: opportunity to check pointer types
  |> List.head

replaceSpecBlank : ID -> String -> HandlerSpec -> HandlerSpec
replaceSpecBlank id value hs =
  let rh a = if blankOrID a == id
             then Filled (blankOrID a) value
             else a
  in { name = rh hs.name
     , module_ = rh hs.module_
     , modifier = rh hs.modifier
     }

allBlanks : Toplevel -> List Pointer
allBlanks tl =
  case tl.data of
    TLHandler h ->
      AST.listBlanks h.ast ++ specBlanks h
    TLDB db ->
      DB.listBlanks db

specs : Handler -> List Pointer
specs h =
  [h.spec.module_, h.spec.name, h.spec.modifier]
  |> List.map (P.blankTo Spec)

siblings : Toplevel -> Pointer -> List Pointer
siblings tl p =
  case tl.data of
    TLHandler h ->
      case getParentOf tl p of
       Just _ ->
         AST.siblings p h.ast
       Nothing ->
         specs h ++ [AST.toP h.ast]
    _ -> []

getNextSibling : Toplevel -> Pointer -> Pointer
getNextSibling tl p =
  case tl.data of
    TLHandler h ->
      let sibs = siblings tl p in
      sibs
      |> LE.elemIndex p
      |> Maybe.map ((+) 1)
      |> Maybe.map (\i -> i % List.length sibs)
      |> Maybe.andThen (\i -> LE.getAt i sibs)
      |> Maybe.withDefault p
    _ -> p

getPrevSibling : Toplevel -> Pointer -> Pointer
getPrevSibling tl p =
  case tl.data of
    TLHandler h ->
      let sibs = siblings tl p
          -- 'safe' to deMaybe as there's always at least
          -- one member in the array
          last = deMaybe "sibling" <| LE.last sibs
      in
      sibs
      |> LE.elemIndex p
      |> Maybe.map (\i -> i - 1)
      |> Maybe.andThen (\i -> LE.getAt i sibs)
      |> Maybe.withDefault last
    _ -> p

getNextBlank : Toplevel -> Predecessor -> Successor
getNextBlank tl pred =
  case pred of
    Just pred ->
      let holes = allBlanks tl in
      holes
      |> LE.elemIndex pred
      |> Maybe.map ((+) 1)
      |> Maybe.andThen (\i -> LE.getAt i holes)
    Nothing -> firstBlank tl

getPrevBlank : Toplevel -> Pointer -> Predecessor
getPrevBlank tl next =
  let holes = allBlanks tl in
  holes
  |> LE.elemIndex next
  |> Maybe.map (\i -> i - 1)
  |> Maybe.andThen (\i -> LE.getAt i holes)

firstBlank : Toplevel -> Successor
firstBlank tl =
  tl |> allBlanks |> List.head

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

getParentOf : Toplevel -> Pointer -> Maybe Pointer
getParentOf tl p =
  case tl.data of
    TLHandler h ->
      AST.parentOf_ (P.idOf p) h.ast
      |> Maybe.map AST.toP
    _ -> Nothing

getChildrenOf : Toplevel -> Pointer -> List Pointer
getChildrenOf tl p =
  case tl.data of
    TLHandler h ->
      h.ast
      |> AST.childrenOf (P.idOf p)
    _ -> []

firstChild : Toplevel -> Pointer -> Maybe Pointer
firstChild tl id = getChildrenOf tl id
                   |> List.head

rootOf : Toplevel -> Maybe Pointer
rootOf tl =
  case tl.data of
    TLHandler h ->
      Just <| AST.toP h.ast
    _ -> Nothing

isValidPointer : Toplevel -> Pointer -> Bool
isValidPointer tl p =
  case P.ownerOf p of
    POSpec ->
      let handler = asHandler tl in
      case handler of
        Nothing ->
          False
        Just h ->
          if List.member p (specs h) then
            True
          else
            False
    POAst ->
      let handler = asHandler tl in
      case handler of
        Nothing ->
          False
        Just h ->
          if List.member p (AST.listPointers (h.ast)) then
            True
          else
            False
    PODb ->
      let db = asDB tl in
      case db of
        Nothing ->
          False
        Just d ->
          if List.member p (DB.listPointers d) then
            True
          else
            False
