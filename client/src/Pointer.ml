open! Porting
open Prelude
open Types

(* Dark *)
module B = Blank

(* ------------------------ *)
(* PointerData *)
(* ------------------------ *)
let emptyD_ (id : id) (pt : pointerType) : pointerData =
  match pt with
  | VarBind ->
      PVarBind (Blank id)
  | EventModifier ->
      PEventModifier (Blank id)
  | EventName ->
      PEventName (Blank id)
  | EventSpace ->
      PEventSpace (Blank id)
  | Expr ->
      PExpr (Blank id)
  | Key ->
      PKey (Blank id)
  | Field ->
      PField (Blank id)
  | DBColName ->
      PDBColName (Blank id)
  | DBColType ->
      PDBColType (Blank id)
  | FFMsg ->
      PFFMsg (Blank id)
  | FnName ->
      PFnName (Blank id)
  | FnCallName ->
      PFnCallName (Blank id)
  | ParamName ->
      PParamName (Blank id)
  | ParamTipe ->
      PParamTipe (Blank id)
  | Pattern ->
      PPattern (Blank id)
  | ConstructorName ->
      PConstructorName (Blank id)


let typeOf (pd : pointerData) : pointerType =
  match pd with
  | PVarBind _ ->
      VarBind
  | PEventModifier _ ->
      EventModifier
  | PEventName _ ->
      EventName
  | PEventSpace _ ->
      EventSpace
  | PExpr _ ->
      Expr
  | PField _ ->
      Field
  | PKey _ ->
      Key
  | PDBColName _ ->
      DBColName
  | PDBColType _ ->
      DBColType
  | PFFMsg _ ->
      FFMsg
  | PFnName _ ->
      FnName
  | PFnCallName _ ->
      FnCallName
  | PConstructorName _ ->
      ConstructorName
  | PParamName _ ->
      ParamName
  | PParamTipe _ ->
      ParamTipe
  | PPattern _ ->
      Pattern


let emptyD (pt : pointerType) : pointerData = emptyD_ (gid ()) pt

let toID (pd : pointerData) : id =
  match pd with
  | PVarBind d ->
      B.toID d
  | PField d ->
      B.toID d
  | PKey d ->
      B.toID d
  | PExpr d ->
      B.toID d
  | PEventModifier d ->
      B.toID d
  | PEventName d ->
      B.toID d
  | PEventSpace d ->
      B.toID d
  | PDBColName d ->
      B.toID d
  | PDBColType d ->
      B.toID d
  | PFFMsg d ->
      B.toID d
  | PFnName d ->
      B.toID d
  | PFnCallName d ->
      B.toID d
  | PParamName d ->
      B.toID d
  | PParamTipe d ->
      B.toID d
  | PPattern d ->
      B.toID d
  | PConstructorName d ->
      B.toID d


let isBlank (pd : pointerData) : bool =
  match pd with
  | PVarBind d ->
      B.isBlank d
  | PField d ->
      B.isBlank d
  | PKey d ->
      B.isBlank d
  | PExpr d ->
      B.isBlank d
  | PEventModifier d ->
      B.isBlank d
  | PEventName d ->
      B.isBlank d
  | PEventSpace d ->
      B.isBlank d
  | PDBColName d ->
      B.isBlank d
  | PDBColType d ->
      B.isBlank d
  | PFFMsg d ->
      B.isBlank d
  | PFnName d ->
      B.isBlank d
  | PFnCallName d ->
      B.isBlank d
  | PConstructorName d ->
      B.isBlank d
  | PParamName d ->
      B.isBlank d
  | PParamTipe d ->
      B.isBlank d
  | PPattern d ->
      B.isBlank d


let toContent (pd : pointerData) : string option =
  let bs2s s = s |> B.toMaybe |> Option.withDefault "" |> fun x -> Some x in
  match pd with
  | PVarBind v ->
      bs2s v
  | PField f ->
      bs2s f
  | PKey f ->
      bs2s f
  | PExpr e ->
    ( match e with
    | F (_, Value s) ->
        Some s
    | F (_, Variable v) ->
        Some v
    | F (_, FnCall (F (_, name), [], _)) ->
        Some name
    (* feature flags are ignored because you want to enter the *)
    (* feature flag and this is how this is used. *)
    | _ ->
        None )
  | PEventModifier d ->
      bs2s d
  | PEventName d ->
      bs2s d
  | PEventSpace d ->
      bs2s d
  | PDBColName d ->
      bs2s d
  | PDBColType d ->
      bs2s d
  | PFFMsg d ->
      bs2s d
  | PFnName d ->
      bs2s d
  | PFnCallName d ->
      bs2s d
  | PConstructorName d ->
      bs2s d
  | PParamName d ->
      bs2s d
  | PParamTipe d ->
      d
      |> B.toMaybe
      |> Option.map Runtime.tipe2str
      |> Option.withDefault ""
      |> fun x -> Some x
  | PPattern d ->
    ( match d with
    | F (_, PLiteral l) ->
        Some l
    | F (_, PVariable v) ->
        Some v
    | _ ->
        None )


let exprmap (fn : expr -> expr) (pd : pointerData) : pointerData =
  match pd with PExpr d -> PExpr (fn d) | _ -> pd
