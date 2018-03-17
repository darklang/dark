module Pointer exposing (..)

-- dark
import Types exposing (..)
import Blank as B

toID : Pointer -> ID
toID p =
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
    _ -> False

isFilled : Pointer -> Bool
isFilled p =
  case p of
    PFilled _ _ -> True
    _ -> False

------------------------
-- PointerData
------------------------
pdToP : PointerData -> Pointer
pdToP pd =
  case pd of
    PVarBind d -> PFilled VarBind (B.toID d)
    PEventModifier d -> PFilled EventModifier (B.toID d)
    PEventName d -> PFilled EventName (B.toID d)
    PEventSpace d -> PFilled EventSpace (B.toID d)
    PExpr d -> PFilled Expr (B.toID d)
    PField d -> PFilled Field (B.toID d)
    PDBColName d -> PFilled DBColName (B.toID d)
    PDBColType d -> PFilled DBColType (B.toID d)
    PDarkType d -> PFilled DarkType (B.toID d)
    PDarkTypeField d -> PFilled DarkTypeField (B.toID d)

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

dToID : PointerData -> ID
dToID = pdToP >> toID

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

