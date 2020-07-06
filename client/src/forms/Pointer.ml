open Prelude

(* Dark *)
module B = BlankOr

(* ------------------------ *)
(* PointerData *)
(* ------------------------ *)

let typeOf (pd : blankOrData) : blankOrType =
  match pd with
  | PEventModifier _ ->
      EventModifier
  | PEventName _ ->
      EventName
  | PEventSpace _ ->
      EventSpace
  | PDBName _ ->
      DBName
  | PDBColName _ ->
      DBColName
  | PDBColType _ ->
      DBColType
  | PFnName _ ->
      FnName
  | PFnReturnTipe _ ->
      FnReturnTipe
  | PParamName _ ->
      ParamName
  | PParamTipe _ ->
      ParamTipe
  | PTypeName _ ->
      TypeName
  | PTypeFieldName _ ->
      TypeFieldName
  | PTypeFieldTipe _ ->
      TypeFieldTipe
  | PGroupName _ ->
      GroupName


let toID (pd : blankOrData) : ID.t =
  match pd with
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
  | PFnReturnTipe d ->
      B.toID d
  | PParamName d ->
      B.toID d
  | PParamTipe d ->
      B.toID d
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
  | PEventModifier str
  | PEventName str
  | PEventSpace str
  | PDBName str
  | PDBColName str
  | PDBColType str
  | PFnName str
  | PParamName str
  | PTypeName str
  | PTypeFieldName str
  | PGroupName str ->
      B.isBlank str
  | PFnReturnTipe t | PTypeFieldTipe t | PParamTipe t ->
      B.isBlank t


let strMap (pd : blankOrData) ~(f : string -> string) : blankOrData =
  let bf s =
    match s with
    | Blank _ ->
      (match f "" with "" -> s | other -> B.newF other)
    | F (id, str) ->
        F (id, f str)
  in
  match pd with
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
  | PFnName d ->
      PFnName (bf d)
  | PFnReturnTipe d ->
      PFnReturnTipe d
  | PParamName d ->
      PParamName (bf d)
  | PParamTipe d ->
      PParamTipe d
  | PTypeName d ->
      PTypeName (bf d)
  | PTypeFieldName d ->
      PTypeFieldName (bf d)
  | PTypeFieldTipe d ->
      PTypeFieldTipe d
  | PGroupName g ->
      PGroupName (bf g)


let toContent (pd : blankOrData) : string =
  let bs2s s = s |> B.toOption |> Option.withDefault ~default:"" in
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
      bs2s d
  | PFnReturnTipe d | PParamTipe d | PTypeFieldTipe d ->
      d
      |> B.toOption
      |> Option.map ~f:Prelude.tipe2str
      |> Option.withDefault ~default:""
