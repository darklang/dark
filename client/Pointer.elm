module Pointer exposing (..)

-- dark
import Types exposing (..)


idOf : Pointer -> ID
idOf p =
  case p of
    PBlank _ id -> id
    PFilled _ id -> id

idOfD : PointerData -> ID
idOfD p =
  case p of
    PVarBind id _ -> id
    PSpec id -> id
    PExpr id _ -> id
    PField id _ -> id
    PDBColName id -> id
    PDBColType id -> id


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

pdToP : PointerData -> Pointer
pdToP pd =
  case pd of
    PVarBind id _ -> PFilled VarBind id
    PSpec id -> PFilled Spec id
    PExpr id _ -> PFilled Expr id
    PField id _ -> PFilled Field id
    PDBColName id -> PFilled DBColName id
    PDBColType id -> PFilled DBColType id

ownerOf : Pointer -> PointerOwner
ownerOf p =
  case (typeOf p) of
    VarBind -> POAst
    Spec -> POSpec
    Expr -> POAst
    Field -> POAst
    DBColName -> PODb
    DBColType -> PODb

pdOwnerOf : PointerData -> PointerOwner
pdOwnerOf pd =
  case pd of
    PVarBind _ _ -> POAst
    PSpec _ -> POSpec
    PExpr _  _ -> POAst
    PField _ _ -> POAst
    PDBColName _ -> PODb
    PDBColType _ -> PODb


