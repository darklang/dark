module Pointer exposing (..)

-- builtin
import Maybe

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
  let (filled, tipe, id) =
        case pd of
          PVarBind d -> (B.isF d, VarBind, B.toID d)
          PEventModifier d -> (B.isF d, EventModifier, B.toID d)
          PEventName d -> (B.isF d, EventName, B.toID d)
          PEventSpace d -> (B.isF d, EventSpace, B.toID d)
          PExpr d -> (B.isF d, Expr, B.toID d)
          PField d -> (B.isF d, Field, B.toID d)
          PDBColName d -> (B.isF d, DBColName, B.toID d)
          PDBColType d -> (B.isF d, DBColType, B.toID d)
          PDarkType d -> (B.isF d, DarkType, B.toID d)
          PDarkTypeField d -> (B.isF d, DarkTypeField, B.toID d)
  in
  if filled
  then PFilled tipe id
  else PBlank tipe id

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

toContent : PointerData -> String
toContent pd =
  let bs2s s = s |> B.toMaybe |> Maybe.withDefault "" in
  case pd of
    PVarBind v -> bs2s v
    PField f -> bs2s f
    PExpr e ->
      case e of
        F _ (Value s) -> s
        F _ (Variable v) -> v
        _ -> ""
    PEventModifier d -> bs2s d
    PEventName d -> bs2s d
    PEventSpace d -> bs2s d
    PDBColName d -> bs2s d
    PDBColType d -> bs2s d
    PDarkType _ -> ""
    PDarkTypeField d -> bs2s d

dtmap : (DarkType -> DarkType) -> PointerData -> PointerData
dtmap fn pd =
  case pd of
    PDarkType d -> PDarkType (fn d)
    _ -> pd

exprmap : (Expr -> Expr) -> PointerData -> PointerData
exprmap fn pd =
  case pd of
    PExpr d -> PExpr (fn d)
    _ -> pd

strmap : (PointerType -> BlankOr String -> BlankOr String) -> PointerData -> PointerData
strmap fn pd =
  let pt = pd |> pdToP |> typeOf in
  case pd of
    PVarBind d -> PVarBind (fn pt d)
    PField d -> PField (fn pt d)
    PExpr _ -> pd
    PEventModifier d -> PEventModifier (fn pt d)
    PEventName d -> PEventName (fn pt d)
    PEventSpace d -> PEventSpace (fn pt d)
    PDBColName d -> PDBColName (fn pt d)
    PDBColType d -> PDBColType (fn pt d)
    PDarkType _ -> pd
    PDarkTypeField d -> PDarkTypeField (fn pt d)


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

