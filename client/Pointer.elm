module Pointer exposing (..)

-- builtin
import Maybe

-- dark
import Types exposing (..)
import Blank as B
import Runtime

------------------------
-- PointerData
------------------------
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
    FFMsg -> PFFMsg (Blank id)
    FnName -> PFnName (Blank id)
    ParamName -> PParamName (Blank id)
    ParamTipe -> PParamTipe (Blank id)

typeOf : PointerData -> PointerType
typeOf pd =
  case pd of
    PVarBind _ -> VarBind
    PEventModifier _ -> EventModifier
    PEventName _ -> EventName
    PEventSpace _ -> EventSpace
    PExpr _ -> Expr
    PField _ -> Field
    PDBColName _ -> DBColName
    PDBColType _ -> DBColType
    PDarkType _ -> DarkType
    PDarkTypeField _ -> DarkTypeField
    PFFMsg _ -> FFMsg
    PFnName _ -> FnName
    PParamName _ -> ParamName
    PParamTipe _ -> ParamTipe


emptyD : PointerType -> PointerData
emptyD pt =
  emptyD_ (gid()) pt

toID : PointerData -> ID
toID pd =
  case pd of
    PVarBind d -> B.toID d
    PField d -> B.toID d
    PExpr d -> B.toID d
    PEventModifier d -> B.toID d
    PEventName d -> B.toID d
    PEventSpace d -> B.toID d
    PDBColName d -> B.toID d
    PDBColType d -> B.toID d
    PDarkType d -> B.toID d
    PDarkTypeField d -> B.toID d
    PFFMsg d -> B.toID d
    PFnName d -> B.toID d
    PParamName d -> B.toID d
    PParamTipe d -> B.toID d


isBlank : PointerData -> Bool
isBlank pd =
  case pd of
    PVarBind d -> B.isBlank d
    PField d -> B.isBlank d
    PExpr d -> B.isBlank d
    PEventModifier d -> B.isBlank d
    PEventName d -> B.isBlank d
    PEventSpace d -> B.isBlank d
    PDBColName d -> B.isBlank d
    PDBColType d -> B.isBlank d
    PDarkType d -> B.isBlank d
    PDarkTypeField d -> B.isBlank d
    PFFMsg d -> B.isBlank d
    PFnName d -> B.isBlank d
    PParamName d -> B.isBlank d
    PParamTipe d -> B.isBlank d


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
    PFFMsg d -> bs2s d
    PFnName d -> bs2s d
    PParamName d -> bs2s d
    PParamTipe d ->
      d |> B.toMaybe |> Maybe.map Runtime.tipe2str |> Maybe.withDefault ""

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

tmap : (BlankOr Tipe -> BlankOr Tipe) -> PointerData -> PointerData
tmap fn pd =
  case pd of
    PParamTipe d -> PParamTipe (fn d)
    _ -> pd

strmap : (PointerType -> BlankOr String -> BlankOr String) -> PointerData -> PointerData
strmap fn pd =
  case pd of
    PVarBind d -> PVarBind (fn VarBind d)
    PField d -> PField (fn Field d)
    PExpr _ -> pd
    PEventModifier d -> PEventModifier (fn EventModifier d)
    PEventName d -> PEventName (fn EventName d)
    PEventSpace d -> PEventSpace (fn EventSpace d)
    PDBColName d -> PDBColName (fn DBColName d)
    PDBColType d -> PDBColType (fn DBColType d)
    PDarkType _ -> pd
    PDarkTypeField d -> PDarkTypeField (fn DarkTypeField d)
    PFFMsg d -> PFFMsg (fn FFMsg d)
    PFnName d -> PFnName (fn FnName d)
    PParamName d -> PParamName (fn ParamName d)
    PParamTipe _ -> pd

