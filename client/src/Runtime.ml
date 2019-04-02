open Tc
open Types

let isCompatible (t1 : tipe) (t2 : tipe) : bool =
  t1 = TAny || t2 = TAny || t1 = t2


let errorRailTypes : tipe list = [TOption; TResult]

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
  | TOption ->
      "Option"
  | TPassword ->
      "Password"
  | TUuid ->
      "UUID"
  | TErrorRail ->
      "ErrorRail"
  | TResult ->
      "Result"
  | TBelongsTo s ->
      s
  | THasMany s ->
      "[" ^ s ^ "]"
  | TDbList a ->
      "[" ^ tipe2str a ^ "]"
  | TUserType (name, _) ->
      name
  | TDeprecated1 | TDeprecated2 | TDeprecated3 ->
      raise (Js.Exn.raiseError "Deprecated type")


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
  | "character" | "char" ->
      TCharacter
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
  | "result" ->
      TResult
  | "password" ->
      TPassword
  | "id" ->
      TID
  | "uuid" ->
      TUuid
  | other ->
      if String.startsWith ~prefix:"[" other
         && String.endsWith ~suffix:"]" other
      then
        other
        |> String.dropLeft ~count:1
        |> String.dropRight ~count:1
        |> parseListTipe
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
  | DOption _ ->
      TOption
  | DErrorRail _ ->
      TErrorRail
  | DPassword _ ->
      TPassword
  | DUuid _ ->
      TUuid
  | DResult _ ->
      TResult


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
  | DCharacter _ ->
      true
  | DStr _ ->
      true
  | _ ->
      false


let isComplete (dv : dval) : bool =
  match dv with DError _ -> false | DIncomplete -> false | _ -> true


let isTrue (dv : dval) : bool = dv = DBool true

(* Copied from Dval.to_repr in backend code, but that's terrible and it should
 * be recopied from to_developer_repr *)
let rec toRepr_ (oldIndent : int) (dv : dval) : string =
  let wrap value = "<" ^ (dv |> typeOf |> tipe2str) ^ ": " ^ value ^ ">" in
  let asType = "<" ^ (dv |> typeOf |> tipe2str) ^ ">" in
  let nl = "\n" ^ String.repeat ~count:oldIndent " " in
  let inl = "\n" ^ String.repeat ~count:(oldIndent + 2) " " in
  let indent = oldIndent + 2 in
  let objToString l =
    l
    |> List.map ~f:(fun (k, v) -> k ^ ": " ^ toRepr_ indent v)
    |> String.join ~sep:("," ^ inl)
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
  | DCharacter c ->
      "'" ^ c ^ "'"
  | DNull ->
      "null"
  | DID s ->
      wrap s
  | DDate s ->
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
        objToString (List.map ~f:(Tuple2.mapSecond (fun s -> DStr s)) hs)
      in
      string_of_int code ^ " " ^ headers ^ nl ^ toRepr dv_
  | DOption OptNothing ->
      "Nothing"
  | DOption (OptJust dv_) ->
      "Just " ^ toRepr dv_
  | DResult (ResOk dv_) ->
      "Ok " ^ toRepr dv_
  | DResult (ResError dv_) ->
      "Error " ^ toRepr dv_
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
        ^ String.join ~sep:(inl ^ ", ") (List.map ~f:(toRepr_ indent) l)
        ^ nl
        ^ "]"
    | l ->
        "[ " ^ String.join ~sep:", " (List.map ~f:(toRepr_ indent) l) ^ " ]" )
  | DObj o ->
      objToString (StrDict.toList o)


and toRepr (dv : dval) : string = toRepr_ 0 dv

let inputValueAsString (iv : inputValueDict) : string =
  DObj iv
  |> toRepr
  |> String.split ~on:"\n"
  |> List.drop ~count:1
  |> List.init
  |> Option.withDefault ~default:[]
  |> List.map ~f:(String.dropLeft ~count:2)
  |> String.join ~sep:"\n"
