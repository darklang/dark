open Tc
open Prelude
open Types

(* Dark *)
module B = Blank

let astOwned pt =
  match pt with
  | Expr | VarBind | Key | Field | FnCallName | Pattern | ConstructorName ->
      true
  | EventModifier
  | EventName
  | EventSpace
  | DBName
  | DBColName
  | DBColType
  | FFMsg
  | FnName
  | ParamName
  | ParamTipe
  | TypeName
  | TypeFieldName
  | TypeFieldTipe
  | GroupName ->
      false


(* ------------------------ *)
(* PointerData *)
(* ------------------------ *)
let emptyD_ (id : id) (pt : blankOrType) : blankOrData =
  match pt with
  | VarBind ->
      PVarBind (id, "")
  | EventModifier ->
      PEventModifier (Blank id)
  | EventName ->
      PEventName (Blank id)
  | EventSpace ->
      PEventSpace (Blank id)
  | Expr ->
      PExpr (EBlank id)
  | Key ->
      PKey (id, "")
  | Field ->
      PField (id, "")
  | DBName ->
      PDBName (Blank id)
  | DBColName ->
      PDBColName (Blank id)
  | DBColType ->
      PDBColType (Blank id)
  | FFMsg ->
      PFFMsg (id, "")
  | FnName ->
      PFnName (Blank id)
  | FnCallName ->
      PFnCallName (id, "")
  | ParamName ->
      PParamName (Blank id)
  | ParamTipe ->
      PParamTipe (Blank id)
  | Pattern ->
      PPattern (FPBlank (id, gid ()))
  | ConstructorName ->
      PConstructorName (id, "")
  | TypeName ->
      PTypeName (Blank id)
  | TypeFieldName ->
      PTypeFieldName (Blank id)
  | TypeFieldTipe ->
      PTypeFieldTipe (Blank id)
  | GroupName ->
      PGroupName (Blank id)


let typeOf (pd : blankOrData) : blankOrType =
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
  | PGroupName _ ->
      GroupName


let emptyD (pt : blankOrType) : blankOrData = emptyD_ (gid ()) pt

let toID (pd : blankOrData) : id =
  match pd with
  | PVarBind (id, _)
  | PField (id, _)
  | PKey (id, _)
  | PConstructorName (id, _)
  | PFnCallName (id, _)
  | PFFMsg (id, _) ->
      id
  | PExpr d ->
      FluidExpression.id d
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
  | PFnName d ->
      B.toID d
  | PParamName d ->
      B.toID d
  | PParamTipe d ->
      B.toID d
  | PPattern d ->
      FluidPattern.id d
  | PTypeName d ->
      B.toID d
  | PTypeFieldName d ->
      B.toID d
  | PTypeFieldTipe d ->
      B.toID d
  | PGroupName d ->
      B.toID d


let isBlank (pd : blankOrData) : bool =
  match pd with
  | PEventModifier d
  | PEventName d
  | PEventSpace d
  | PDBName d
  | PDBColName d
  | PDBColType d
  | PFnName d
  | PParamName d
  | PTypeName d
  | PTypeFieldName d
  | PGroupName d ->
      B.isBlank d
  | PTypeFieldTipe d | PParamTipe d ->
      B.isBlank d
  | PVarBind (_, "")
  | PField (_, "")
  | PKey (_, "")
  | PFFMsg (_, "")
  | PFnCallName (_, "")
  | PConstructorName (_, "")
  | PExpr (EBlank _)
  | PPattern (FPBlank _) ->
      true
  | PVarBind _
  | PField _
  | PKey _
  | PFFMsg _
  | PFnCallName _
  | PConstructorName _
  | PExpr _
  | PPattern _ ->
      false


let strMap (pd : blankOrData) ~(f : string -> string) : blankOrData =
  let bf s =
    match s with
    | Blank _ ->
      (match f "" with "" -> s | other -> Blank.newF other)
    | F (id, str) ->
        F (id, f str)
  in
  match pd with
  | PVarBind (id, s) ->
      PVarBind (id, f s)
  | PField (id, s) ->
      PField (id, f s)
  | PKey (id, s) ->
      PKey (id, f s)
  | PExpr e ->
    ( match e with
    | EVariable (id, v) ->
        PExpr (EVariable (id, f v))
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
  | PFFMsg (id, s) ->
      PFFMsg (id, f s)
  | PFnName d ->
      PFnName (bf d)
  | PFnCallName (id, s) ->
      PFnCallName (id, f s)
  | PConstructorName (id, s) ->
      PConstructorName (id, f s)
  | PParamName d ->
      PParamName (bf d)
  | PParamTipe d ->
      PParamTipe d
  | PPattern d ->
    ( match d with
    | FPVariable (id, mid, v) ->
        PPattern (FPVariable (id, mid, f v))
    | _ ->
        PPattern d )
  | PTypeName d ->
      PTypeName (bf d)
  | PTypeFieldName d ->
      PTypeFieldName (bf d)
  | PTypeFieldTipe d ->
      PTypeFieldTipe d
  | PGroupName g ->
      PGroupName (bf g)


let toContent (pd : blankOrData) : string =
  let bs2s s = s |> B.toMaybe |> Option.withDefault ~default:"" in
  match pd with
  | PVarBind (_, s) ->
      s
  | PField (_, s) ->
      s
  | PKey (_, s) ->
      s
  | PExpr e ->
      FluidPrinter.eToString e
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
  | PFFMsg (_, s) ->
      s
  | PFnName d ->
      bs2s d
  | PFnCallName (_, s) ->
      s
  | PConstructorName (_, s) ->
      s
  | PParamName d ->
      bs2s d
  | PParamTipe d ->
      d
      |> B.toMaybe
      |> Option.map ~f:Runtime.tipe2str
      |> Option.withDefault ~default:""
  | PPattern d ->
      FluidPrinter.pToString d
  | PTypeName d ->
      bs2s d
  | PTypeFieldName d ->
      bs2s d
  | PTypeFieldTipe d ->
      d
      |> B.toMaybe
      |> Option.map ~f:Runtime.tipe2str
      |> Option.withDefault ~default:""
  | PGroupName g ->
      bs2s g
