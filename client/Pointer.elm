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


blankOfD : PointerData -> (Maybe String, Maybe Expr)
blankOfD pd =
  Debug.crash "TODO"
  -- case pd of
  --   PVarBind _ val -> val
  --   PSpec _ val -> val
  --   PExpr _ (Hole id) -> Blank id
  --   PExpr id expr -> Filled id "TODO"
  --   PField _ val -> val
  --   PDBColName _ val -> val
  --   PDBColType _ val -> val
  --


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
    HTTPRoute -> POSpec
    HTTPVerb -> POSpec
    Expr -> POAst
    Field -> POAst
    DBColName -> PODb
    DBColType -> PODb

pdOwnerOf : PointerData -> PointerOwner
pdOwnerOf pd =
  case pd of
    PVarBind _ _ -> POAst
    PHTTPRoute _ _ -> POSpec
    PHTTPVerb _ _ -> POSpec
    PExpr _  _ -> POAst
    PField _ _ -> POAst
    PDBColName _ _ -> PODb
    PDBColType _ _ -> PODb


