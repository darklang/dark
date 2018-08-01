module Toplevel exposing (..)

-- builtin

-- lib
import List.Extra as LE
import Maybe.Extra as ME

-- dark
import DB
import Types exposing (..)
import Util
import Prelude exposing (..)
import AST
import Blank as B
import Pointer as P
import Functions as Fns
import SpecTypes
import SpecHeaders
import DB
import Defaults

all : Model -> List Toplevel
all m =
  m.toplevels ++ List.map (ufToTL m) m.userFunctions

-------------------------
-- Toplevel manipulation
-------------------------
getTL : Model -> TLID -> Toplevel
getTL m id =
  get m id
  |> deMaybe "getTL"

get : Model -> TLID -> Maybe Toplevel
get m id =
  let tls = all m in
  LE.find (\tl -> tl.id == id) tls

name : Toplevel -> String
name tl =
  case tl.data of
    TLHandler h ->
      "H: "
      ++ (h.spec.name |> B.toMaybe |> Maybe.withDefault "")
    TLDB db ->
      "DB: "
      ++ db.name
    TLFunc f ->
      "Func: "
      ++ (f.metadata.name |> B.toMaybe |> Maybe.withDefault "")


upsert : Model -> Toplevel -> Model
upsert m tl =
  let updated = update m tl.id (\_ -> tl) in
  if Nothing == LE.find (\thisTl -> thisTl.id == tl.id) updated.toplevels
  then { m | toplevels = m.toplevels ++ [tl] }
  else updated

upsertAll : Model -> List Toplevel -> Model
upsertAll m tls =
  List.foldl (flip upsert) m tls

remove : Model -> Toplevel -> Model
remove m tl =
  { m | toplevels = List.filter ((/=) tl) m.toplevels }

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

ufToTL : Model -> UserFunction -> Toplevel
ufToTL m uf = { id = uf.tlid
              , pos = Defaults.initialPos
              , data = TLFunc uf
              }

asUserFunction : Toplevel -> Maybe UserFunction
asUserFunction tl =
  case tl.data of
    TLFunc f -> Just f
    _ -> Nothing

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

spaceOfHandler : Handler -> HandlerSpace
spaceOfHandler h =
  SpecHeaders.spaceOf h.spec

spaceOf : Toplevel -> Maybe HandlerSpace
spaceOf tl =
  tl
  |> asHandler
  |> Maybe.map spaceOfHandler

isHTTPHandler : Toplevel -> Bool
isHTTPHandler tl =
  tl
  |> spaceOf
  |> (==) (Just HSHTTP)

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
    PKey k ->
      PKey (B.clone identity k)
    PDBColName cn -> pd
    PDBColType ct -> pd
    PDarkType dt -> todo ("clonePointerData", pd)
    PDarkTypeField dt -> todo ("clonePointerData", pd)
    PFFMsg msg -> PFFMsg (B.clone identity msg)
    PFnName name -> PFnName (B.clone identity name)
    PParamName name -> PParamName (B.clone identity name)
    PParamTipe tipe -> PParamTipe (B.clone identity tipe)

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
    TLFunc f -> AST.siblings p f.ast

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
    TLFunc f ->
      AST.parentOf_ (P.toID p) f.ast
      |> Maybe.map PExpr
    TLDB db ->
      db
      |> DB.astsFor
      |> List.map (AST.parentOf_ (P.toID p))
      |> ME.values
      |> List.head
      |> Maybe.map PExpr

getChildrenOf : Toplevel -> PointerData -> List PointerData
getChildrenOf tl pd =
  let pid = P.toID pd
      astChildren () =
        case tl.data of
          TLHandler h ->
            AST.childrenOf pid h.ast
          TLFunc f ->
            AST.childrenOf pid f.ast
          TLDB db ->
            db
            |> DB.astsFor
            |> List.map (AST.childrenOf pid)
            |> List.concat
      specChildren () =
        let h = asHandler tl |> deMaybe "getChildrenOf - spec" in
        SpecTypes.childrenOf (P.toID pd) h.spec.types.input
        ++ SpecTypes.childrenOf (P.toID pd) h.spec.types.output
  in
  case pd of
    PVarBind _ -> []
    PField d -> []
    PKey d -> []
    PExpr _ -> astChildren ()
    PEventModifier d -> []
    PEventName d -> []
    PEventSpace d -> []
    PDBColName d -> []
    PDBColType d -> []
    PDarkType _ -> specChildren ()
    PDarkTypeField d -> []
    PFFMsg _ -> []
    PFnName _ -> []
    PParamName _ -> []
    PParamTipe _ -> []


firstChild : Toplevel -> PointerData -> Maybe PointerData
firstChild tl id = getChildrenOf tl id
                   |> List.head

rootOf : Toplevel -> Maybe PointerData
rootOf tl =
  -- TODO SpecTypePointerDataRefactor
  case tl.data of
    TLHandler h ->
      Just <| PExpr h.ast
    TLFunc f ->
      Just <| PExpr f.ast
    _ -> Nothing


-------------------------
-- Generic
-------------------------
replace : PointerData -> PointerData -> Toplevel -> Toplevel
replace p replacement tl =
  let ha () = tl |> asHandler |> deMaybe "TL.replace"
      fn () = tl |> asUserFunction |> deMaybe "TL.replace"
      id = P.toID p
      astReplace () =
        case tl.data of
          TLHandler h ->
            let newAST = AST.replace p replacement h.ast
            in { tl | data = TLHandler { h | ast = newAST } }
          TLFunc f ->
            let newAST = AST.replace p replacement f.ast
            in { tl | data = TLFunc { f | ast = newAST } }
          _ -> impossible ("no AST here", tl.data)
      specTypeReplace () =
        let h = ha ()
            newSpec = SpecTypes.replace p replacement h.spec
        in { tl | data = TLHandler { h | spec = newSpec } }
      specHeaderReplace bo =
        let h = ha ()
            newSpec = SpecHeaders.replace id bo h.spec
        in { tl | data = TLHandler { h | spec = newSpec } }
      fnMetadataReplace () =
        let f = fn ()
            newF =
              Fns.replaceMetadataField p replacement f
        in
           { tl | data = TLFunc newF }

  in
  case replacement of
    PVarBind vb -> astReplace ()
    PField _ -> astReplace ()
    PKey _ -> astReplace ()
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
          -- spec = SpecTypes.replace p replacement h.spec
          spec2 = SpecHeaders.replace id bo h.spec
          ast = AST.replace p replacement h.ast
      in { tl | data = TLHandler { h | spec = spec2, ast = ast } }
    PFnName _ -> fnMetadataReplace ()
    PParamName _ -> fnMetadataReplace ()
    PParamTipe _ -> fnMetadataReplace ()

delete : Toplevel -> PointerData -> ID -> Toplevel
delete tl p newID =
  let replacement = P.emptyD_ newID (P.typeOf p)
  in replace p replacement tl


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
    TLFunc f ->
      Fns.allData f

findExn : Toplevel -> ID -> PointerData
findExn tl id =
  find tl id
  |> deMaybe "findExn"

find : Toplevel -> ID -> Maybe PointerData
find tl id =
  allData tl
  |> List.filter (\d -> id == P.toID d)
  |> assert (\r -> List.length r <= 1) -- guard against dups
  |> List.head


