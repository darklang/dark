module Toplevel exposing (..)

-- builtin

-- lib
import List.Extra as LE
import Maybe.Extra as ME

-- dark
import DontPort exposing ((@))
import DB
import Types exposing (..)
import Util
import Prelude exposing (..)
import AST
import Blank as B
import Pointer as P
import Functions as Fns
import SpecHeaders
import DB
import Defaults

-------------------------
-- Toplevel manipulation
-------------------------
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
      ++ (f.ufMetadata.ufmName |> B.toMaybe |> Maybe.withDefault "")

containsByTLID : List Toplevel -> Toplevel -> Bool
containsByTLID tls elem =
  LE.find (\tl -> tl.id == elem.id) tls /= Nothing

removeByTLID : List Toplevel -> List Toplevel -> List Toplevel
removeByTLID origTls toBeRemoved =
  List.filter
    (\origTl -> not (containsByTLID toBeRemoved origTl))
    origTls


upsertByTLID : List Toplevel -> Toplevel -> List Toplevel
upsertByTLID tls tl =
  (removeByTLID tls [tl]) ++ [tl]

upsert : Model -> Toplevel -> Model
upsert m tl =
  { m | toplevels = upsertByTLID m.toplevels tl }

upsertAllByTLID : List Toplevel -> List Toplevel -> List Toplevel
upsertAllByTLID tls new =
  List.foldl (\a b -> upsertByTLID b a) tls new

upsertAll : Model -> List Toplevel -> Model
upsertAll m tls =
  List.foldl (\a b -> upsert b a) m tls

remove : Model -> Toplevel -> Model
remove m tl =
  { m | toplevels = removeByTLID m.toplevels [tl] }

updateByTLID : List Toplevel -> TLID -> (Toplevel -> Toplevel) -> List Toplevel
updateByTLID tls tlid f =
  tls
  |> List.map (\t -> if t.id /= tlid
                     then t
                     else f t)

update : Model -> TLID -> (Toplevel -> Toplevel) -> Model
update m tlid f =
  { m | toplevels = updateByTLID m.toplevels tlid f }

move : TLID -> Int -> Int -> Model -> Model
move tlid xOffset yOffset m = update m tlid (moveTL xOffset yOffset)

moveTL : Int -> Int -> Toplevel -> Toplevel
moveTL xOffset yOffset tl =
  let newPos = { x = tl.pos.x + xOffset, y = tl.pos.y + yOffset }
  in { tl | pos = newPos }

ufToTL : Model -> UserFunction -> Toplevel
ufToTL m uf = { id = uf.ufTLID
              , pos = Defaults.centerPos
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

toOp : Toplevel -> List Op
toOp tl =
  case tl.data of
    TLHandler h ->
      [SetHandler tl.id tl.pos h]
    TLFunc fn ->
      [SetFunction fn]
    _ -> impossible "This isn't how database ops work"

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
    PFFMsg msg -> PFFMsg (B.clone identity msg)
    PFnName name_ -> PFnName (B.clone identity name_)
    PParamName name_ -> PParamName (B.clone identity name_)
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
    Just pred_ ->
      let ps = allData tl
          index = LE.elemIndex pred_ ps |> Maybe.withDefault (-1)
          remaining = List.drop (index+1) ps
          blanks = List.filter P.isBlank remaining in
      blanks
      |> List.head
      |> ME.orElse (firstBlank tl)
    Nothing -> firstBlank tl

getPrevBlank : Toplevel -> Successor -> Predecessor
getPrevBlank tl next =
  case next of
    Just next_ ->
      let ps = allData tl
          index = LE.elemIndex next_ ps |> Maybe.withDefault (List.length ps)
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
            ++ [ PExpr h.ast ]
      in
      if List.member p toplevels
      then toplevels
      else AST.siblings p h.ast
    TLDB db -> DB.siblings p db
    TLFunc f -> AST.siblings p f.ufAST

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
      AST.parentOf_ (P.toID p) f.ufAST
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
            AST.childrenOf pid f.ufAST
          TLDB db ->
            db
            |> DB.astsFor
            |> List.map (AST.childrenOf pid)
            |> List.concat
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
      Just <| PExpr f.ufAST
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
            let newAST = AST.replace p replacement f.ufAST
            in { tl | data = TLFunc { f | ufAST = newAST } }
          _ -> impossible ("no AST here", tl.data)
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
    PDBColType tipe ->
      tl
      -- SetDBColType tl.id id (tipe |> B.toMaybe |> deMaybe "replace - tipe")
    PDBColName _ ->
      tl
      -- SetDBColName tl.id id (name |> B.toMaybe |> deMaybe "replace - name")
    PFFMsg bo ->
      case tl.data of
        TLHandler h ->
          let spec2 = SpecHeaders.replace id bo h.spec
              ast = AST.replace p replacement h.ast
          in { tl | data = TLHandler { h | spec = spec2, ast = ast } }
        TLFunc f ->
          let ast = AST.replace p replacement f.ufAST
          in { tl | data = TLFunc { f | ufAST = ast } }
        _ -> tl
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
      ++ AST.allData h.ast
    TLDB db ->
      DB.allData db
    TLFunc f ->
      Fns.allData f

all : Model -> List Toplevel
all m =
  m.toplevels @ List.map (ufToTL m) m.userFunctions

get : Model -> TLID -> Maybe Toplevel
get m id =
  let tls = all m in
  LE.find (\tl -> tl.id == id) tls


getTL : Model -> TLID -> Toplevel
getTL m id =
  get m id
  |> deMaybe "getTL"


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
