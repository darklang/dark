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
