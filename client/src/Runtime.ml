open Tc
open Types
open Prelude

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
  | TDbList a ->
      "[" ^ tipe2str a ^ "]"
  | TUserType (name, _) ->
      name
  | TBytes ->
      "Bytes"


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
    | _ ->
        impossible "invalid type in str2tipe"
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
  | "bytes" ->
      TBytes
  | "password" ->
      TPassword
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
      else impossible "invalid list type in str2tipe"


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
  | DBytes _ ->
      TBytes


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


let isValidDisplayString (str : string) : bool =
  str
  |> String.toList
  |> List.foldl ~init:(false, true) ~f:(fun c (sawSlash, valid) ->
         (* Bucklescript actually stores chars are strings, and comparisons
          * against literal chars don't work. wtf. *)
         if not valid
         then (false, false)
         else if sawSlash
         then
           (false, List.member ~value:c (Obj.magic ["t"; "r"; "n"; "\\"; "\""]))
         else (c = Obj.magic "\\", true) )
  |> fun (lastCharSlash, valid) -> valid && not lastCharSlash


let isStringLiteral (s : string) : bool =
  String.length s >= 2
  && String.endsWith ~suffix:"\"" s
  && String.startsWith ~prefix:"\"" s


let stripQuotes (s : string) : string =
  s |> String.dropLeft ~count:1 |> String.dropRight ~count:1


let addQuotes (s : string) : string = "\"" ^ s ^ "\""

module Regex = Util.Regex

let convertLiteralToDisplayString (s : string) : string =
  let conversion str =
    (* Convert special chars to use two backslashes (literal strings hold the
   * correct bytes for \n, etc), while the view/display version holds a
   * backslash and a character *)
    str
    (* 4 re slashes, become 2 in the js source, and 1 in the js runtime *)
    |> Regex.replace ~re:[%re "/\\\\/g"] ~repl:"\\\\"
    |> Regex.replace ~re:[%re "/\\n/g"] ~repl:"\\n"
    |> Regex.replace ~re:[%re "/\\r/g"] ~repl:"\\r"
    |> Regex.replace ~re:[%re "/\\t/g"] ~repl:"\\t"
    |> Regex.replace ~re:[%re "/\\\"/g"] ~repl:"\\\""
  in
  if isStringLiteral s
  then s |> stripQuotes |> conversion |> addQuotes
  else conversion s


let convertDisplayStringToLiteral (s : string) : string =
  let conversion str =
    (* Convert escaped version into special chars *)
    str
    (* 8 re slashes, become 4 in the js source, and 2 in the js runtime *)
    |> Regex.replace ~re:[%re "/\\\\\\\\/g"] ~repl:"\\"
    |> Regex.replace ~re:[%re "/\\\\n/g"] ~repl:"\n"
    |> Regex.replace ~re:[%re "/\\\\r/g"] ~repl:"\r"
    |> Regex.replace ~re:[%re "/\\\\t/g"] ~repl:"\t"
    |> Regex.replace ~re:[%re "/\\\\\"/g"] ~repl:"\""
  in
  if isStringLiteral s
  then s |> stripQuotes |> conversion |> addQuotes |> Debug.log "done"
  else conversion s


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
      Js.Float.toString f
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
        objToString (List.map ~f:(Tuple2.mapSecond ~f:(fun s -> DStr s)) hs)
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
  | DBytes s ->
      "<"
      ^ (dv |> typeOf |> tipe2str)
      ^ ": length="
      ^ (Bytes.length s |> string_of_int)
      ^ ">"


and toRepr (dv : dval) : string = toRepr_ 0 dv

(* TODO: copied from Libexecution/http.ml *)
let route_variables (route : string) : string list =
  let split_uri_path (path : string) : string list =
    let subs = String.split ~on:"/" path in
    List.filter ~f:(fun x -> String.length x > 0) subs
  in
  route
  |> split_uri_path
  |> List.filter ~f:(String.startsWith ~prefix:":")
  |> List.map ~f:(String.dropLeft ~count:1 (* ":" *))


let inputVariables (tl : toplevel) : varName list =
  match tl with
  | TLHandler h ->
    ( match h.spec.space with
    | F (_, m) when String.toLower m = "http" ->
        let fromRoute =
          h.spec.name
          |> Blank.toMaybe
          |> Option.map ~f:route_variables
          |> Option.withDefault ~default:[]
        in
        ["request"] @ fromRoute
    | F (_, m) when String.toLower m = "cron" ->
        []
    | F (_, m) when String.toLower m = "repl" ->
        []
    | F (_, m) when String.toLower m = "worker" ->
        ["event"]
    | F (_, _) ->
        (* workers, including old names *)
        ["event"]
    | Blank _ ->
        (* we used to be allowed unknown *)
        ["request"; "event"] )
  | TLFunc f ->
      f.ufMetadata.ufmParameters
      |> List.filterMap ~f:(fun p -> Blank.toMaybe p.ufpName)
  | TLTipe _ | TLDB _ ->
      []


let sampleInputValue (tl : toplevel) : inputValueDict =
  tl
  |> inputVariables
  |> List.map ~f:(fun v -> (v, DIncomplete))
  |> StrDict.fromList


let inputValueAsString (tl : toplevel) (iv : inputValueDict) : string =
  let dval =
    (* Merge sample + trace, preferring trace.
     *
     * This ensures newly added parameters show as incomplete.
     * *)
    StrDict.merge
      ~f:(fun _key sampleVal traceVal ->
        match (sampleVal, traceVal) with
        | None, None ->
            None
        | Some v, None ->
            Some v
        | None, Some v ->
            Some v
        | Some _sample, Some trace ->
            Some trace )
      (sampleInputValue tl)
      iv
    |> fun dict -> DObj dict
  in
  dval
  |> toRepr
  |> String.split ~on:"\n"
  |> List.drop ~count:1
  |> List.init
  |> Option.withDefault ~default:[]
  |> List.map ~f:(String.dropLeft ~count:2)
  |> String.join ~sep:"\n"


let pathFromInputVars (iv : inputValueDict) : string option =
  StrDict.get ~key:"request" iv
  |> Option.andThen ~f:(fun obj ->
         match obj with DObj r -> r |> StrDict.get ~key:"url" | _ -> None )
  |> Option.andThen ~f:(fun dv -> match dv with DStr s -> Some s | _ -> None)
  |> Option.andThen ~f:WebURL.make
  |> Option.map ~f:(fun url -> url##pathname ^ url##search)


let setHandlerExeState
    (tlid : tlid) (state : exeState) (hp : handlerProp TLIDDict.t) :
    handlerProp TLIDDict.t =
  hp
  |> TLIDDict.update ~tlid ~f:(fun old ->
         let p =
           old |> Option.withDefault ~default:Defaults.defaultHandlerProp
         in
         Some {p with execution = state} )
