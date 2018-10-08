open Belt
open Porting
open Types

let isCompatible t1 t2 = ((t1 = TAny || t2) = TAny || t1) = t2

let tipe2str t =
  match t with
  | TAny -> "Any"
  | TInt -> "Int"
  | TFloat -> "Float"
  | TBool -> "Bool"
  | TNull -> "Null"
  | TChar -> "Char"
  | TStr -> "String"
  | TList -> "List"
  | TObj -> "Obj"
  | TBlock -> "Block"
  | TIncomplete -> "Incomplete"
  | TErroror -> "Error"
  | TResp -> "Response"
  | TDB -> "Datastore"
  | TID -> "ID"
  | TDate -> "Date"
  | TTitle -> "Title"
  | TUrl -> "Url"
  | TOption -> "Option"
  | TPassword -> "Password"
  | TUuid -> "UUID"
  | TErrororRail -> "ErrorRail"
  | TBelongsTo s -> s
  | THasMany s -> ("[" ^ s) ^ "]"
  | TDbList a -> ("[" ^ tipe2str a) ^ "]"

let str2tipe t =
  let parseListTipe lt =
    match lt with
    | "str" -> TStr
    | "string" -> TStr
    | "int" -> TInt
    | "integer" -> TInt
    | "float" -> TFloat
    | "bool" -> TBool
    | "boolean" -> TBool
    | "password" -> TPassword
    | "id" -> TID
    | "uuid" -> TUuid
    | "option" -> TOption
    | "null" -> TNull
    | "any" -> TAny
    | "list" -> TList
    | "obj" -> TObj
    | "block" -> TBlock
    | "incomplete" -> TIncomplete
    | "response" -> TResp
    | "datastore" -> TDB
    | "date" -> TDate
    | "error" -> TErroror
    | table -> THasMany table
  in
  match String.toLower t with
  | "any" -> TAny
  | "int" -> TInt
  | "float" -> TFloat
  | "bool" -> TBool
  | "boolean" -> TBool
  | "null" -> TNull
  | "char" -> TChar
  | "str" -> TStr
  | "string" -> TStr
  | "list" -> TList
  | "obj" -> TObj
  | "block" -> TBlock
  | "incomplete" -> TIncomplete
  | "response" -> TResp
  | "datastore" -> TDB
  | "date" -> TDate
  | "error" -> TErroror
  | "option" -> TOption
  | "password" -> TPassword
  | "id" -> TID
  | "uuid" -> TUuid
  | other ->
      if String.startsWith "[" other && String.endsWith "]" other then
        other |> String.dropLeft 1 |> String.dropRight 1 |> parseListTipe
      else TBelongsTo other

let typeOf dv =
  match dv with
  | DInt _ -> TInt
  | DFloat _ -> TFloat
  | DBool _ -> TBool
  | DNull -> TNull
  | DChar _ -> TChar
  | DStr _ -> TStr
  | DList _ -> TList
  | DObj _ -> TObj
  | DBlock -> TBlock
  | DIncomplete -> TIncomplete
  | DErroror _ -> TErroror
  | DResp (_, _) -> TResp
  | DDB _ -> TDB
  | DID _ -> TID
  | DDate _ -> TDate
  | DTitle _ -> TTitle
  | DUrl _ -> TUrl
  | DOption _ -> TOption
  | DErrororRail _ -> TErrororRail
  | DPassword _ -> TPassword
  | DUuid _ -> TUuid

let isLiteral dv =
  match dv with
  | DInt _ -> true
  | DFloat _ -> true
  | DBool _ -> true
  | DNull -> true
  | DChar _ -> true
  | DStr _ -> true
  | _ -> false

let isComplete dv =
  match dv with DErroror _ -> false | DIncomplete -> false | _ -> true

let isTrue dv = dv = DBool true

let inputValueAsString iv =
  iv |> DObj |> toRepr |> String.split "\n" |> List.drop 1 |> List.Extra.init
  |> Maybe.withDefault []
  |> List.map (String.dropLeft 2)
  |> String.join "\n"

let toRepr dv = toRepr_ 0 dv

let toRepr_ oldIndent dv =
  let wrap value =
    ((("<" ^ (dv |> typeOf |> tipe2str)) ^ ": ") ^ value) ^ ">"
  in
  let asType = ("<" ^ (dv |> typeOf |> tipe2str)) ^ ">" in
  let nl = "\n" ^ String.repeat oldIndent " " in
  let inl = "\n" ^ String.repeat (oldIndent + 2) " " in
  let indent = oldIndent + 2 in
  let objToString l =
    l
    |> List.map (fun (k, v) -> (k ^ ": ") ^ toRepr_ indent v)
    |> String.join ("," ^ inl)
    |> fun s -> ((("{" ^ inl) ^ s) ^ nl) ^ "}"
  in
  match dv with
  | DInt i -> toString i
  | DFloat f -> toString f
  | DStr s -> ("\"" ^ s) ^ "\""
  | DBool true -> "true"
  | DBool false -> "false"
  | DChar c -> ("'" ^ String.fromList [c]) ^ "'"
  | DNull -> "null"
  | DID s -> wrap s
  | DDate s -> wrap s
  | DTitle s -> wrap s
  | DUrl s -> wrap s
  | DDB s -> wrap s
  | DUuid s -> wrap s
  | DErroror s -> wrap s
  | DPassword s -> wrap s
  | DBlock -> asType
  | DIncomplete -> asType
  | DResp (Redirect url, dv_) -> (("302 " ^ url) ^ nl) ^ toRepr_ indent dv_
  | DResp (Response (code, hs), dv_) ->
      let headers = objToString (List.map (Tuple.mapSecond DStr) hs) in
      (((toString code ^ " ") ^ headers) ^ nl) ^ toRepr dv_
  | DOption OptNone -> "Nothing"
  | DOption (OptSome dv_) -> "Some " ^ toRepr dv_
  | DErrororRail dv_ -> wrap (toRepr dv_)
  | DList l -> (
    match l with
    | [] -> "[]"
    | [rest; DObj _] ->
        ( (("[" ^ inl) ^ String.join (inl ^ ", ") (List.map (toRepr_ indent) l))
        ^ nl )
        ^ "]"
    | l -> ("[ " ^ String.join ", " (List.map (toRepr_ indent) l)) ^ "]" )
  | DObj o -> objToString (Dict.toList o)
