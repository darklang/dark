open Tea
open! Porting
module B = Blank
open Prelude
open Types

let emptyD_ (id : id) (pt : pointerType) : pointerData =
  match pt with
  | VarBind -> PVarBind (Blank id)
  | EventModifier -> PEventModifier (Blank id)
  | EventName -> PEventName (Blank id)
  | EventSpace -> PEventSpace (Blank id)
  | Expr -> PExpr (Blank id)
  | Key -> PKey (Blank id)
  | Field -> PField (Blank id)
  | DBColName -> PDBColName (Blank id)
  | DBColType -> PDBColType (Blank id)
  | DarkType -> PDarkType (Blank id)
  | DarkTypeField -> PDarkTypeField (Blank id)
  | FFMsg -> PFFMsg (Blank id)
  | FnName -> PFnName (Blank id)
  | ParamName -> PParamName (Blank id)
  | ParamTipe -> PParamTipe (Blank id)

let typeOf (pd : pointerData) : pointerType =
  match pd with
  | PVarBind _ -> VarBind
  | PEventModifier _ -> EventModifier
  | PEventName _ -> EventName
  | PEventSpace _ -> EventSpace
  | PExpr _ -> Expr
  | PField _ -> Field
  | PKey _ -> Key
  | PDBColName _ -> DBColName
  | PDBColType _ -> DBColType
  | PDarkType _ -> DarkType
  | PDarkTypeField _ -> DarkTypeField
  | PFFMsg _ -> FFMsg
  | PFnName _ -> FnName
  | PParamName _ -> ParamName
  | PParamTipe _ -> ParamTipe

let emptyD (pt : pointerType) : pointerData = emptyD_ (gid ()) pt

let toID (pd : pointerData) : id =
  match pd with
  | PVarBind d -> B.toID d
  | PField d -> B.toID d
  | PKey d -> B.toID d
  | PExpr d -> B.toID d
  | PEventModifier d -> B.toID d
  | PEventName d -> B.toID d
  | PEventSpace d -> B.toID d
  | PDBColName d -> B.toID d
  | PDBColType d -> B.toID d
  | PDarkType d -> B.toID d
  | PDarkTypeField d -> B.toID d
  | PFFMsg d -> B.toID d
  | PFnName d -> B.toID d
  | PParamName d -> B.toID d
  | PParamTipe d -> B.toID d

let isBlank (pd : pointerData) : bool =
  match pd with
  | PVarBind d -> B.isBlank d
  | PField d -> B.isBlank d
  | PKey d -> B.isBlank d
  | PExpr d -> B.isBlank d
  | PEventModifier d -> B.isBlank d
  | PEventName d -> B.isBlank d
  | PEventSpace d -> B.isBlank d
  | PDBColName d -> B.isBlank d
  | PDBColType d -> B.isBlank d
  | PDarkType d -> B.isBlank d
  | PDarkTypeField d -> B.isBlank d
  | PFFMsg d -> B.isBlank d
  | PFnName d -> B.isBlank d
  | PParamName d -> B.isBlank d
  | PParamTipe d -> B.isBlank d

let toContent (pd : pointerData) : string option =
  let bs2s s = s |> B.toMaybe |> Option.withDefault "" |> Some in
  match pd with
  | PVarBind v -> bs2s v
  | PField f -> bs2s f
  | PKey f -> bs2s f
  | PExpr e -> (
    match e with
    | F (_, Value s) -> Some s
    | F (_, Variable v) -> Some v
    | _ -> None )
  | PEventModifier d -> bs2s d
  | PEventName d -> bs2s d
  | PEventSpace d -> bs2s d
  | PDBColName d -> bs2s d
  | PDBColType d -> bs2s d
  | PDarkType _ -> None
  | PDarkTypeField d -> bs2s d
  | PFFMsg d -> bs2s d
  | PFnName d -> bs2s d
  | PParamName d -> bs2s d
  | PParamTipe d ->
      d |> B.toMaybe
      |> Option.map Runtime.tipe2str
      |> Option.withDefault "" |> Some

let dtmap (fn : darkType -> darkType) (pd : pointerData) : pointerData =
  match pd with PDarkType d -> PDarkType (fn d) | _ -> pd

let exprmap (fn : expr -> expr) (pd : pointerData) : pointerData =
  match pd with PExpr d -> PExpr (fn d) | _ -> pd

let tmap (fn : tipe blankOr -> tipe blankOr) (pd : pointerData) : pointerData =
  match pd with PParamTipe d -> PParamTipe (fn d) | _ -> pd

let strmap (fn : (pointerType -> string blankOr) -> string blankOr)
    (pd : pointerData) : pointerData =
  match pd with
  | PVarBind d -> PVarBind (fn VarBind d)
  | PField d -> PField (fn Field d)
  | PKey d -> PKey (fn Key d)
  | PExpr _ -> pd
  | PEventModifier d -> PEventModifier (fn EventModifier d)
  | PEventName d -> PEventName (fn EventName d)
  | PEventSpace d -> PEventSpace (fn EventSpace d)
  | PDBColName d -> PDBColName (fn DBColName d)
  | PDBColType d -> PDBColType (fn DBColType d)
  | PDarkType _ -> pd
  | PDarkTypeField d -> PDarkTypeField (fn DarkTypeField d)
  | PFFMsg d -> PFFMsg (fn FFMsg d)
  | PFnName d -> PFnName (fn FnName d)
  | PParamName d -> PParamName (fn ParamName d)
  | PParamTipe _ -> pd
