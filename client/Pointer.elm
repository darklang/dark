module Pointer exposing (..)

-- dark
import Types exposing (..)


idOf : Pointer -> ID
idOf p =
  case p of
    PBlank _ id -> id
    PFilled _ id -> id

blankTo : PointerType -> BlankOr a -> Pointer
blankTo t b =
  case b of
    Blank id -> PBlank t id
    Filled id _ -> PFilled t id

typeOf : Pointer -> PointerType
typeOf p =
  case p of
    PBlank t _ -> t
    PFilled t _ -> t

isBlank : Pointer -> Bool
isBlank p =
  case p of
    PBlank _ _ -> True
    PFilled _ _ -> False

isFilled : Pointer -> Bool
isFilled p =
  case p of
    PBlank _ _ -> False
    PFilled _ _ -> True

------------------------
-- PointerData
------------------------
pdToP : PointerData -> Pointer
pdToP pd =
  case pd of
    PVarBind id _ -> PFilled VarBind id
    PHTTPVerb id _ -> PFilled HTTPVerb id
    PHTTPRoute id _ -> PFilled HTTPRoute id
    PExpr id _ -> PFilled Expr id
    PField id _ -> PFilled Field id
    PDBColName id _ -> PFilled DBColName id
    PDBColType id _ -> PFilled DBColType id
    PDarkType id _ -> PFilled DarkType id
    PDarkTypeField id _ -> PFilled DarkTypeField id


emptyD : PointerType -> ID -> PointerData
emptyD pt id =
  case pt of
    VarBind -> PVarBind id (Blank id)
    HTTPVerb -> PHTTPVerb id (Blank id)
    HTTPRoute -> PHTTPRoute id (Blank id)
    Expr -> PExpr id (Hole id)
    Field -> PField id (Blank id)
    DBColName -> PDBColName id (Blank id)
    DBColType -> PDBColType id (Blank id)
    DarkType -> PDarkType id (Blank id)
    DarkTypeField -> PDarkTypeField id (Blank id)


idOfD : PointerData -> ID
idOfD = pdToP >> idOf

typeOfD : PointerData -> PointerType
typeOfD = pdToP >> typeOf

------------------------
-- PointerOwner
------------------------
ownerOf : Pointer -> PointerOwner
ownerOf p =
  case (typeOf p) of
    VarBind -> POAst
    HTTPRoute -> POSpecHeader
    HTTPVerb -> POSpecHeader
    Expr -> POAst
    Field -> POAst
    DBColName -> PODb
    DBColType -> PODb
    DarkTypeField -> POSpecType
    DarkType -> POSpecType

pdOwnerOf : PointerData -> PointerOwner
pdOwnerOf pd =
  pd
  |> pdToP
  |> ownerOf

