open Tc
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
  | DBName ->
      PDBName (Blank id)
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
  | TypeName ->
      PTypeName (Blank id)
  | TypeFieldName ->
      PTypeFieldName (Blank id)
  | TypeFieldTipe ->
      PTypeFieldTipe (Blank id)


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
  | PDBName _ ->
      DBName
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
  | PTypeName _ ->
      TypeName
  | PTypeFieldName _ ->
      TypeFieldName
  | PTypeFieldTipe _ ->
      TypeFieldTipe


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
  | PDBName d ->
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
  | PTypeName d ->
      B.toID d
  | PTypeFieldName d ->
      B.toID d
  | PTypeFieldTipe d ->
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
  | PDBName d ->
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
  | PTypeName d ->
      B.isBlank d
  | PTypeFieldName d ->
      B.isBlank d
  | PTypeFieldTipe d ->
      B.isBlank d


let strMap (pd : pointerData) ~(f : string -> string) : pointerData =
  let bf s =
    match s with
    | Blank _ ->
      (match f "" with "" -> s | other -> Blank.newF other)
    | F (id, str) ->
        F (id, f str)
  in
  match pd with
  | PVarBind v ->
      PVarBind (bf v)
  | PField f ->
      PField (bf f)
  | PKey f ->
      PKey (bf f)
  | PExpr e ->
    ( match e with
    | F (id, Value v) ->
        PExpr (F (id, Value (f v)))
    | F (id, Variable v) ->
        PExpr (F (id, Variable (f v)))
    | _ ->
        PExpr e )
  | PEventModifier d ->
      PEventModifier (bf d)
  | PEventName d ->
      PEventName (bf d)
  | PEventSpace d ->
      PEventSpace (bf d)
  | PDBName d ->
      PDBName (bf d)
  | PDBColName d ->
      PDBColName (bf d)
  | PDBColType d ->
      PDBColType (bf d)
  | PFFMsg d ->
      PFFMsg (bf d)
  | PFnName d ->
      PFnName (bf d)
  | PFnCallName d ->
      PFnCallName (bf d)
  | PConstructorName d ->
      PConstructorName (bf d)
  | PParamName d ->
      PParamName (bf d)
  | PParamTipe d ->
      PParamTipe d
  | PPattern d ->
    ( match d with
    | F (id, PVariable v) ->
        PPattern (F (id, PVariable (f v)))
    | F (id, PLiteral v) ->
        PPattern (F (id, PLiteral (f v)))
    | _ ->
        PPattern d )
  | PTypeName d ->
      PTypeName (bf d)
  | PTypeFieldName d ->
      PTypeFieldName (bf d)
  | PTypeFieldTipe d ->
      PTypeFieldTipe d


let toContent (pd : pointerData) : string option =
  let bs2s s =
    s |> B.toMaybe |> Option.withDefault ~default:"" |> fun x -> Some x
  in
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
        if Runtime.isStringLiteral s
        then Some (Runtime.convertLiteralToDisplayString s)
        else Some s
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
  | PDBName d ->
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
      |> Option.map ~f:Runtime.tipe2str
      |> Option.withDefault ~default:""
      |> fun x -> Some x
  | PPattern d ->
    ( match d with
    | F (_, PLiteral l) ->
        Some l
    | F (_, PVariable v) ->
        Some v
    | _ ->
        None )
  | PTypeName d ->
      bs2s d
  | PTypeFieldName d ->
      bs2s d
  | PTypeFieldTipe d ->
      d
      |> B.toMaybe
      |> Option.map ~f:Runtime.tipe2str
      |> Option.withDefault ~default:""
      |> fun x -> Some x


let exprmap (fn : expr -> expr) (pd : pointerData) : pointerData =
  match pd with PExpr d -> PExpr (fn d) | _ -> pd
