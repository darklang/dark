open! Porting
open Types

let isCompatible (t1 : tipe) (t2 : tipe) : bool =
  t1 = TAny || t2 = TAny || t1 = t2


let rec tipe2str (t : tipe) : string =
  match t with
  | TAny ->
      "Any"
  | TInt ->
      "Int"
  | TFloat ->
      "Float"
  | TBool ->
      "Bool"
  | TNull ->
      "Null"
  | TChar ->
      "Char"
  | TCharacter ->
      "Character"
  | TStr ->
      "String"
  | TList ->
      "List"
  | TObj ->
      "Obj"
  | TBlock ->
      "Block"
  | TIncomplete ->
      "Incomplete"
  | TError ->
      "Error"
  | TResp ->
      "Response"
  | TDB ->
      "Datastore"
  | TID ->
      "ID"
  | TDate ->
      "Date"
  | TTitle ->
      "Title"
  | TUrl ->
      "Url"
  | TOption ->
      "Option"
  | TPassword ->
      "Password"
  | TUuid ->
      "UUID"
  | TErrorRail ->
      "ErrorRail"
  | TBelongsTo s ->
      s
  | THasMany s ->
      "[" ^ s ^ "]"
  | TDbList a ->
      "[" ^ tipe2str a ^ "]"


let str2tipe (t : string) : tipe =
  let parseListTipe lt =
    match lt with
    | "str" ->
        TStr
    | "string" ->
        TStr
    | "int" ->
        TInt
    | "integer" ->
        TInt
    | "float" ->
        TFloat
    | "bool" ->
        TBool
    | "boolean" ->
        TBool
    | "password" ->
        TPassword
    | "id" ->
        TID
    | "uuid" ->
        TUuid
    | "option" ->
        TOption
    | "null" ->
        TNull
    | "any" ->
        TAny
    | "list" ->
        TList
    | "obj" ->
        TObj
    | "block" ->
        TBlock
    | "incomplete" ->
        TIncomplete
    | "response" ->
        TResp
    | "datastore" ->
        TDB
    | "date" ->
        TDate
    | "error" ->
        TError
    | table ->
        THasMany table
  in
  match String.toLower t with
  | "any" ->
      TAny
  | "int" ->
      TInt
  | "float" ->
      TFloat
  | "bool" ->
      TBool
  | "boolean" ->
      TBool
  | "null" ->
      TNull
  | "char" ->
      TChar
  | "str" ->
      TStr
  | "string" ->
      TStr
  | "list" ->
      TList
  | "obj" ->
      TObj
  | "block" ->
      TBlock
  | "incomplete" ->
      TIncomplete
  | "response" ->
      TResp
  | "datastore" ->
      TDB
  | "date" ->
      TDate
  | "error" ->
      TError
  | "option" ->
      TOption
  | "password" ->
      TPassword
  | "id" ->
      TID
  | "uuid" ->
      TUuid
  | other ->
      if String.startsWith "[" other && String.endsWith "]" other
      then other |> String.dropLeft 1 |> String.dropRight 1 |> parseListTipe
      else TBelongsTo other


let typeOf (dv : dval) : tipe =
  match dv with
  | DInt _ ->
      TInt
  | DFloat _ ->
      TFloat
  | DBool _ ->
      TBool
  | DNull ->
      TNull
  | DChar _ ->
      TChar
  | DCharacter _ ->
      TCharacter
  | DStr _ ->
      TStr
  | DList _ ->
      TList
  | DObj _ ->
      TObj
  | DBlock ->
      TBlock
  | DIncomplete ->
      TIncomplete
  | DError _ ->
      TError
  | DResp (_, _) ->
      TResp
  | DDB _ ->
      TDB
  | DID _ ->
      TID
  | DDate _ ->
      TDate
  | DTitle _ ->
      TTitle
  | DUrl _ ->
      TUrl
  | DOption _ ->
      TOption
  | DErrorRail _ ->
      TErrorRail
  | DPassword _ ->
      TPassword
  | DUuid _ ->
      TUuid


let isLiteral (dv : dval) : bool =
  match dv with
  | DInt _ ->
      true
  | DFloat _ ->
      true
  | DBool _ ->
      true
  | DNull ->
      true
  | DChar _ ->
      true
  | DCharacter _ ->
      true
  | DStr _ ->
      true
  | _ ->
      false


let isComplete (dv : dval) : bool =
  match dv with DError _ -> false | DIncomplete -> false | _ -> true


let isTrue (dv : dval) : bool = dv = DBool true

(* Copied from Dval.to_repr in backend code *)
let rec toRepr_ (oldIndent : int) (dv : dval) : string =
  let wrap value = "<" ^ (dv |> typeOf |> tipe2str) ^ ": " ^ value ^ ">" in
  let asType = "<" ^ (dv |> typeOf |> tipe2str) ^ ">" in
  let nl = "\n" ^ String.repeat oldIndent " " in
  let inl = "\n" ^ String.repeat (oldIndent + 2) " " in
  let indent = oldIndent + 2 in
  let objToString l =
    l
    |> List.map (fun (k, v) -> k ^ ": " ^ toRepr_ indent v)
    |> String.join ("," ^ inl)
    |> fun s -> "{" ^ inl ^ s ^ nl ^ "}"
  in
  match dv with
  | DInt i ->
      string_of_int i
  | DFloat f ->
      string_of_float f
  | DStr s ->
      "\"" ^ s ^ "\""
  | DBool true ->
      "true"
  | DBool false ->
      "false"
  | DChar c ->
      "'" ^ String.fromList [c] ^ "'"
  | DCharacter c ->
      "'" ^ c ^ "'"
  | DNull ->
      "null"
  | DID s ->
      wrap s
  | DDate s ->
      wrap s
  | DTitle s ->
      wrap s
  | DUrl s ->
      wrap s
  | DDB s ->
      wrap s
  | DUuid s ->
      wrap s
  | DError s ->
      wrap s
  | DPassword s ->
      wrap s
  | DBlock ->
      asType
  | DIncomplete ->
      asType
  | DResp (Redirect url, dv_) ->
      "302 " ^ url ^ nl ^ toRepr_ indent dv_
  | DResp (Response (code, hs), dv_) ->
      let headers =
        objToString (List.map (Tuple.mapSecond (fun s -> DStr s)) hs)
      in
      string_of_int code ^ " " ^ headers ^ nl ^ toRepr dv_
  | DOption OptNothing ->
      "Nothing"
  | DOption (OptJust dv_) ->
      "Just " ^ toRepr dv_
  | DErrorRail dv_ ->
      wrap (toRepr dv_)
  (* TODO: newlines and indentation *)
  | DList l ->
    ( match l with
    | [] ->
        "[]"
    | DObj _ :: _ ->
        "["
        ^ inl
        ^ String.join (inl ^ ", ") (List.map (toRepr_ indent) l)
        ^ nl
        ^ "]"
    | l ->
        "[ " ^ String.join ", " (List.map (toRepr_ indent) l) ^ " ]" )
  | DObj o ->
      objToString (StrDict.toList o)


and toRepr (dv : dval) : string = toRepr_ 0 dv

let inputValueAsString (iv : inputValueDict) : string =
  iv
  |> fun i ->
  DObj i
  |> toRepr
  |> String.split ~on:"\n"
  |> List.drop 1
  |> List.init
  |> Option.withDefault []
  |> List.map (String.dropLeft 2)
  |> String.join "\n"
