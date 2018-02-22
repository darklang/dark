module Toplevel exposing (..)

-- builtin

-- lib
import List.Extra as LE
import Maybe.Extra as ME

-- dark
import DB
import Types exposing (..)
import Util exposing (deMaybe)
import AST
import Pointer as P
import SpecTypes
import SpecHeaders
import DB

-------------------------
-- Toplevel manipulation
-------------------------
getTL : Model -> TLID -> Toplevel
getTL m id =
  LE.find (\tl -> tl.id == id) m.toplevels
  |> deMaybe "getTL"

replace : Model -> Toplevel -> Model
replace m tl =
  update m tl.id (\_ -> tl)

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
  in { tl | pos = newPos }


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


-------------------------
-- Generic
-------------------------
allPointers : Toplevel -> List Pointer
allPointers tl =
  case tl.data of
    TLHandler h ->
      SpecHeaders.allPointers h.spec
      ++ SpecTypes.allPointers h.spec.types.input
      ++ AST.allPointers h.ast
      ++ SpecTypes.allPointers h.spec.types.output
    TLDB db ->
      DB.allPointers db

isValidPointer : Toplevel -> Pointer -> Bool
isValidPointer tl p =
  List.member p (allPointers tl)

clonePointerData : PointerData -> PointerData
clonePointerData pd =
  let replaceBlankOr nid bo =
      case bo of
        Blank _ -> Blank nid
        Filled _ a -> Filled nid a
  in
  case pd of
    PVarBind id vb ->
      let nid = gid ()
      in PVarBind nid (replaceBlankOr nid vb)
    PHTTPVerb id sp ->
      let nid = gid ()
      in PHTTPVerb nid (replaceBlankOr nid sp)
    PHTTPRoute id sp ->
      let nid = gid ()
      in PHTTPRoute nid (replaceBlankOr nid sp)
    PExpr id expr ->
      let (nid, ast) = AST.clone expr
      in PExpr nid ast
    PField id f ->
      let nid = gid ()
      in PField nid (replaceBlankOr nid f)
    PDBColName id cn -> pd
    PDBColType id ct -> pd
    PDarkType id dt -> Debug.crash "TODO clonePointerdata"
    PDarkTypeField id dt -> Debug.crash "TODO clonePointerdata"

-------------------------
-- Blanks
-------------------------
allBlanks : Toplevel -> List Pointer
allBlanks tl =
  tl
  |> allPointers
  |> List.filter P.isBlank

blanksWhere : (Pointer -> Bool) -> Toplevel -> List Pointer
blanksWhere fn tl =
  tl
  |> allBlanks
  |> List.filter fn

getNextBlank : Toplevel -> Predecessor -> Successor
getNextBlank tl pred =
  case pred of
    Just pred ->
      let ps = allPointers tl |> Debug.log "ps"
          index = LE.elemIndex pred ps |> Maybe.withDefault (-1) |> Debug.log "index"
          remaining = List.drop (index+1) ps |> Debug.log "remaining"
          blanks = List.filter P.isBlank remaining |> Debug.log "blanks" in
      blanks
      |> List.head
      |> ME.orElse (firstBlank tl)
    Nothing -> firstBlank tl |> Debug.log "first"

getPrevBlank : Toplevel -> Successor -> Predecessor
getPrevBlank tl next =
  case next of
    Just next ->
      let ps = allPointers tl |> Debug.log "prevAllPs"
          index = LE.elemIndex next ps |> Maybe.withDefault (List.length ps) |> Debug.log "prevIndex"
          remaining = List.take index ps  |> Debug.log "prevRemaining"
          blanks = List.filter P.isBlank remaining |> Debug.log "blanks" in
      blanks
      |> LE.last
      |> ME.orElse (lastBlank tl)
    Nothing -> lastBlank tl |> Debug.log "last"


firstBlank : Toplevel -> Successor
firstBlank tl =
  tl |> allBlanks |> List.head

lastBlank : Toplevel -> Successor
lastBlank tl =
  tl |> allBlanks |> LE.last

getFirstASTBlank : Toplevel -> Successor
getFirstASTBlank tl =
  tl
  |> blanksWhere (\p -> P.ownerOf p == POAst)
  |> List.head


-------------------------
-- Siblings
-------------------------
siblings : Toplevel -> Pointer -> List Pointer
siblings tl p =
  case tl.data of
    TLHandler h ->
      let toplevels =
            SpecHeaders.allPointers h.spec
            ++ [ P.blankTo DarkType h.spec.types.input
               , AST.toP h.ast
               , P.blankTo DarkType h.spec.types.output] in

      if List.member p toplevels
      then toplevels
      else
         AST.siblings p h.ast
         ++ SpecTypes.siblings p h.spec.types.input
         ++ SpecTypes.siblings p h.spec.types.output
    TLDB db -> DB.siblings p db

getNextSibling : Toplevel -> Pointer -> Pointer
getNextSibling tl p =
  let sibs = siblings tl p in
  sibs
  |> LE.elemIndex p
  |> Maybe.map ((+) 1)
  |> Maybe.map (\i -> i % List.length sibs)
  |> Maybe.andThen (\i -> LE.getAt i sibs)
  |> Maybe.withDefault p

getPrevSibling : Toplevel -> Pointer -> Pointer
getPrevSibling tl p =
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


-------------------------
-- Up/Down the tree
-------------------------
getParentOf : Toplevel -> Pointer -> Maybe Pointer
getParentOf tl p =
  -- TODO SpecTypePointerRefactor
  case tl.data of
    TLHandler h ->
      AST.parentOf_ (P.idOf p) h.ast
      |> Maybe.map AST.toP
    _ -> Nothing

getChildrenOf : Toplevel -> Pointer -> List Pointer
getChildrenOf tl p =
  case P.ownerOf p of
    POSpecHeader -> []
    POAst ->
      let h = asHandler tl |> deMaybe "getChildrenOf" in
      AST.childrenOf (P.idOf p) h.ast
    PODb -> []
    POSpecType ->
      let h = asHandler tl |> deMaybe "getChildrenOf" in
      SpecTypes.childrenOf (P.idOf p) h.spec.types.input
      ++ SpecTypes.childrenOf (P.idOf p) h.spec.types.output


firstChild : Toplevel -> Pointer -> Maybe Pointer
firstChild tl id = getChildrenOf tl id
                   |> List.head

rootOf : Toplevel -> Maybe Pointer
rootOf tl =
  -- TODO SpecTypePointerRefactor
  case tl.data of
    TLHandler h ->
      Just <| AST.toP h.ast
    _ -> Nothing


