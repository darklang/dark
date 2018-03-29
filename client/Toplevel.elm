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
        Flagged _ _ _ _ _ as ff ->
          case B.flattenFF ff of
            F _ s -> String.toLower s == "http"
            _ -> False

        F _ s ->
          String.toLower s == "http"

-------------------------
-- Generic
-------------------------
isValidID : Toplevel -> ID -> Bool
isValidID tl id =
  List.member id (tl |> allData |> List.map P.toID)

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
    PDarkType dt -> Debug.crash "TODO clonePointerDatadata"
    PDarkTypeField dt -> Debug.crash "TODO clonePointerDatadata"
    PFFMsg msg -> PFFMsg (B.clone identity msg)

-------------------------
-- Blanks
-------------------------
allBlanks : Toplevel -> List PointerData
allBlanks tl =
  tl
  |> allData
  |> List.filter P.isBlank

blanksWhere : (PointerData -> Bool) -> Toplevel -> List PointerData
blanksWhere fn tl =
  tl
  |> allBlanks
  |> List.filter fn

getNextBlank : Toplevel -> Predecessor -> Successor
getNextBlank tl pred =
  case pred of
    Just pred ->
      let ps = allData tl
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
      let ps = allData tl
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


-------------------------
-- Siblings
-------------------------
siblings : Toplevel -> PointerData -> List PointerData
siblings tl p =
  case tl.data of
    TLHandler h ->
      let toplevels =
            SpecHeaders.allData h.spec
            -- types are disabled for now
            ++ [ --PDarkType h.spec.types.input
                PExpr h.ast
               --, PDarkType h.spec.types.output
               ] in

      if List.member p toplevels
      then toplevels
      else
         AST.siblings p h.ast
         ++ SpecTypes.siblings p h.spec.types.input
         ++ SpecTypes.siblings p h.spec.types.output
    TLDB db -> DB.siblings p db

getNextSibling : Toplevel -> PointerData -> PointerData
getNextSibling tl p =
  siblings tl p
  |> Util.listNextWrap p
  -- 'safe' to deMaybe as there's always at least one member in the array
  |> deMaybe "nextSibling"

getPrevSibling : Toplevel -> PointerData -> PointerData
getPrevSibling tl p =
  siblings tl p
  |> Util.listPreviousWrap p
  -- 'safe' to deMaybe as there's always at least one member in the array
  |> deMaybe "prevSibling"


-------------------------
-- Up/Down the tree
-------------------------
getParentOf : Toplevel -> PointerData -> Maybe PointerData
getParentOf tl p =
  -- TODO SpecTypePointerDataRefactor
  case tl.data of
    TLHandler h ->
      AST.parentOf_ (P.toID p) h.ast
      |> Maybe.map PExpr
    _ -> Nothing

getChildrenOf : Toplevel -> PointerData -> List PointerData
getChildrenOf tl pd =
  let astChildren () =
        let h = asHandler tl |> deMaybe "getChildrenOf - ast" in
        AST.childrenOf (P.toID pd) h.ast
      specChildren () =
        let h = asHandler tl |> deMaybe "getChildrenOf - spec" in
        SpecTypes.childrenOf (P.toID pd) h.spec.types.input
        ++ SpecTypes.childrenOf (P.toID pd) h.spec.types.output
  in
  case pd of
    PVarBind _ -> []
    PField d -> []
    PExpr _ -> astChildren ()
    PEventModifier d -> []
    PEventName d -> []
    PEventSpace d -> []
    PDBColName d -> []
    PDBColType d -> []
    PDarkType _ -> specChildren ()
    PDarkTypeField d -> []
    PFFMsg _ -> []


firstChild : Toplevel -> PointerData -> Maybe PointerData
firstChild tl id = getChildrenOf tl id
                   |> List.head

rootOf : Toplevel -> Maybe PointerData
rootOf tl =
  -- TODO SpecTypePointerDataRefactor
  case tl.data of
    TLHandler h ->
      Just <| PExpr h.ast
    _ -> Nothing


-------------------------
-- Generic
-------------------------
replace : Toplevel -> PointerData -> PointerData -> Toplevel
replace tl p pd =
  let ha () = tl |> asHandler |> deMaybe "TL.replace"
      id = P.toID p
      astReplace () =
        let h = ha ()
            replacement = AST.replace p pd h.ast
        in { tl | data = TLHandler { h | ast = replacement } }
      specTypeReplace () =
        let h = ha ()
            replacement = SpecTypes.replace p pd h.spec
        in { tl | data = TLHandler { h | spec = replacement } }
      specHeaderReplace bo =
        let h = ha ()
            replacement = SpecHeaders.replace id bo h.spec
        in { tl | data = TLHandler { h | spec = replacement } }
  in
  case pd of
    PVarBind vb -> astReplace ()
    PField _ -> astReplace ()
    PExpr _ -> astReplace ()
    PEventName en -> specHeaderReplace en
    PEventModifier em -> specHeaderReplace em
    PEventSpace es -> specHeaderReplace es
    PDarkType _ -> specTypeReplace ()
    PDarkTypeField _ -> specTypeReplace ()
    PDBColType tipe ->
      tl
      -- SetDBColType tl.id id (tipe |> B.toMaybe |> deMaybe "replace - tipe")
    PDBColName name ->
      tl
      -- SetDBColName tl.id id (name |> B.toMaybe |> deMaybe "replace - name")
    PFFMsg bo ->
      let h = ha ()
          -- replace everywhere
          spec = SpecTypes.replace p pd h.spec
          spec2 = SpecHeaders.replace id bo spec
          ast = AST.replace p pd h.ast
      in { tl | data = TLHandler { h | spec = spec2, ast = ast } }

delete : Toplevel -> PointerData -> ID -> Toplevel
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
  |> List.filter (\d -> id == P.toID d)
  |> Util.assert (\r -> List.length r <= 1) -- guard against dups
  |> List.head


