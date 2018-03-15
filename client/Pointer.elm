module Pointer exposing (..)

-- dark
import Types exposing (..)
import Blank

idOf : Pointer -> ID
idOf p =
  case p of
    PBlank _ id -> id
    PFilled _ id -> id

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
    PVarBind d -> PFilled VarBind (Blank.toID d)
    PEventModifier d -> PFilled EventModifier (Blank.toID d)
    PEventName d -> PFilled EventName (Blank.toID d)
    PEventSpace d -> PFilled EventSpace (Blank.toID d)
    PExpr d -> PFilled Expr (Blank.toID d)
    PField d -> PFilled Field (Blank.toID d)
    PDBColName d -> PFilled DBColName (Blank.toID d)
    PDBColType d -> PFilled DBColType (Blank.toID d)
    PDarkType d -> PFilled DarkType (Blank.toID d)
    PDarkTypeField d -> PFilled DarkTypeField (Blank.toID d)

emptyD_ : ID -> PointerType -> PointerData
emptyD_ id pt =
  case pt of
    VarBind -> PVarBind (Blank id)
    EventModifier -> PEventModifier (Blank id)
    EventName -> PEventName (Blank id)
    EventSpace -> PEventSpace (Blank id)
    Expr -> PExpr (Blank id)
    Field -> PField (Blank id)
    DBColName -> PDBColName (Blank id)
    DBColType -> PDBColType (Blank id)
    DarkType -> PDarkType (Blank id)
    DarkTypeField -> PDarkTypeField (Blank id)


emptyD : PointerType -> PointerData
emptyD pt =
  emptyD_ (gid()) pt

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
    EventName -> POSpecHeader
    EventModifier -> POSpecHeader
    EventSpace -> POSpecHeader
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

