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
import Blank as B
import Pointer as P
import SpecTypes
import SpecHeaders
import DB

-------------------------
-- Toplevel manipulation
-------------------------
getTL : Model -> TLID -> Toplevel
getTL m id =
  get m id
  |> deMaybe "getTL"

get : Model -> TLID -> Maybe Toplevel
get m id =
  LE.find (\tl -> tl.id == id) m.toplevels



upsert : Model -> Toplevel -> Model
upsert m tl =
  let updated = update m tl.id (\_ -> tl) in
  if m.toplevels == updated.toplevels
  then { m | toplevels = m.toplevels ++ [tl] }
  else updated


update : Model -> TLID -> (Toplevel -> Toplevel) -> Model
update m tlid f =
  let mapped = m.toplevels
               |> List.map (\t -> if t.id /= tlid
                                  then t
                                  else f t)
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

isHTTPHandler : Toplevel -> Bool
isHTTPHandler tl =
  case asHandler tl of
    Nothing -> False
    Just h ->
      case h.spec.module_ of
        Blank _ -> True
        Flagged _ _ _ _ as ff ->
          case B.flattenFF ff of
            F _ s -> String.toLower s == "http"
            _ -> False

        F _ s ->
          String.toLower s == "http"

-------------------------
-- Generic
-------------------------
allPointers : Toplevel -> List Pointer
allPointers tl =
  allData tl
  |> List.map P.pdToP

isValidPointer : Toplevel -> Pointer -> Bool
isValidPointer tl p =
  List.member p (allPointers tl)

clonePointerData : PointerData -> PointerData
clonePointerData pd =
  case pd of
    PVarBind vb ->
      PVarBind (B.clone identity vb)
    PEventModifier sp ->
      PEventModifier (B.clone identity sp)
    PEventName sp ->
      PEventName (B.clone identity sp)
    PEventSpace sp ->
      PEventSpace (B.clone identity sp)
    PExpr expr ->
      PExpr (AST.clone expr)
    PField f ->
      PField (B.clone identity f)
    PDBColName cn -> pd
    PDBColType ct -> pd
    PDarkType dt -> Debug.crash "TODO clonePointerdata"
    PDarkTypeField dt -> Debug.crash "TODO clonePointerdata"

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
      let ps = allPointers tl
          index = LE.elemIndex pred ps |> Maybe.withDefault (-1)
          remaining = List.drop (index+1) ps
          blanks = List.filter P.isBlank remaining in
      blanks
      |> List.head
      |> ME.orElse (firstBlank tl)
    Nothing -> firstBlank tl

getPrevBlank : Toplevel -> Successor -> Predecessor
getPrevBlank tl next =
  case next of
    Just next ->
      let ps = allPointers tl
          index = LE.elemIndex next ps |> Maybe.withDefault (List.length ps)
          remaining = List.take index ps
          blanks = List.filter P.isBlank remaining in
      blanks
      |> LE.last
      |> ME.orElse (lastBlank tl)
    Nothing -> lastBlank tl


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
            ++ [ B.toP DarkType h.spec.types.input
               , B.toP Expr h.ast
               , B.toP DarkType h.spec.types.output] in

      if List.member p toplevels
      then toplevels
      else
         AST.siblings p h.ast
         ++ SpecTypes.siblings p h.spec.types.input
         ++ SpecTypes.siblings p h.spec.types.output
    TLDB db -> DB.siblings p db

getNextSibling : Toplevel -> Pointer -> Pointer
getNextSibling tl p =
  siblings tl p
  |> Util.listNextWrap p
  -- 'safe' to deMaybe as there's always at least one member in the array
  |> deMaybe "nextSibling"

getPrevSibling : Toplevel -> Pointer -> Pointer
getPrevSibling tl p =
  siblings tl p
  |> Util.listPreviousWrap p
  -- 'safe' to deMaybe as there's always at least one member in the array
  |> deMaybe "prevSibling"


-------------------------
-- Up/Down the tree
-------------------------
getParentOf : Toplevel -> Pointer -> Maybe Pointer
getParentOf tl p =
  -- TODO SpecTypePointerRefactor
  case tl.data of
    TLHandler h ->
      AST.parentOf_ (P.toID p) h.ast
      |> Maybe.map AST.toP
    _ -> Nothing

getChildrenOf : Toplevel -> Pointer -> List Pointer
getChildrenOf tl p =
  case P.ownerOf p of
    POSpecHeader -> []
    POAst ->
      let h = asHandler tl |> deMaybe "getChildrenOf" in
      AST.childrenOf (P.toID p) h.ast
    PODb -> []
    POSpecType ->
      let h = asHandler tl |> deMaybe "getChildrenOf" in
      SpecTypes.childrenOf (P.toID p) h.spec.types.input
      ++ SpecTypes.childrenOf (P.toID p) h.spec.types.output


firstChild : Toplevel -> Pointer -> Maybe Pointer
firstChild tl id = getChildrenOf tl id
                   |> List.head

rootOf : Toplevel -> Maybe Pointer
rootOf tl =
  -- TODO SpecTypePointerRefactor
  case tl.data of
    TLHandler h ->
      Just <| B.toP Expr h.ast
    _ -> Nothing


-------------------------
-- Generic
-------------------------
replace : Toplevel -> Pointer -> PointerData -> Toplevel
replace tl p pd =
  let ha () = tl |> asHandler |> deMaybe "TL.replace"
      id = P.toID p
  in
  case pd of
    PVarBind vb ->
      let h = ha ()
          replacement = AST.replace p pd h.ast
      in { tl | data = TLHandler { h | ast = replacement } }
    PEventName en ->
      let h = ha ()
          replacement = SpecHeaders.replaceEventName id en h.spec
      in { tl | data = TLHandler { h | spec = replacement } }
    PEventModifier em ->
      let h = ha ()
          replacement = SpecHeaders.replaceEventModifier id em h.spec
      in { tl | data = TLHandler { h | spec = replacement } }
    PEventSpace es ->
      let h = ha ()
          replacement = SpecHeaders.replaceEventSpace id es h.spec
      in { tl | data = TLHandler { h | spec = replacement } }
    PField _ ->
      let h = ha ()
          ast = AST.replace p pd h.ast
      in { tl | data = TLHandler { h | ast = ast } }
    PExpr _ ->
      let h = ha ()
          ast = AST.replace p pd h.ast
      in { tl | data = TLHandler { h | ast = ast } }
    PDarkType _ ->
      let h = ha ()
          replacement = SpecTypes.replace p pd h.spec
      in { tl | data = TLHandler { h | spec = replacement } }
    PDarkTypeField _ ->
      let h = ha ()
          replacement = SpecTypes.replace p pd h.spec
      in { tl | data = TLHandler { h | spec = replacement } }

    PDBColType tipe ->
      tl
      -- SetDBColType tl.id id (tipe |> B.toMaybe |> deMaybe "replace - tipe")
    PDBColName name ->
      tl
      -- SetDBColName tl.id id (name |> B.toMaybe |> deMaybe "replace - name")

delete : Toplevel -> Pointer -> ID -> Toplevel
delete tl p newID =
  let replacement = P.emptyD_ newID (P.typeOf p)
  in replace tl p replacement


allData : Toplevel -> List PointerData
allData tl =
  case tl.data of
    TLHandler h ->
      SpecHeaders.allData h.spec
      ++ SpecTypes.allData h.spec.types.input
      ++ AST.allData h.ast
      ++ SpecTypes.allData h.spec.types.output
    TLDB db ->
      DB.allData db

findExn : Toplevel -> ID -> PointerData
findExn tl id =
  find tl id
  |> deMaybe "findExn"

find : Toplevel -> ID -> Maybe PointerData
find tl id =
  allData tl
  |> List.filter (\d -> id == P.dToID d)
  |> Util.assert (List.length >> ((==) 1))
  |> List.head


