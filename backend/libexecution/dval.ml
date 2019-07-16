open Core_kernel
open Types
open Types.RuntimeT

(* ------------------------- *)
(* Strings *)
(* ------------------------- *)
let dstr_of_string (s : string) : dval option =
  s |> Unicode_string.of_string |> Option.map ~f:(fun s -> DStr s)


let dstr_of_string_exn (s : string) : dval =
  s |> dstr_of_string |> Option.value_exn ~message:("Invalid UTF-8 string:" ^ s)


(* ------------------------- *)
(* Types *)
(* ------------------------- *)
let rec tipe_to_string t : string =
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
      "Nothing"
  | TCharacter ->
      "Character"
  | TStr ->
      "Str"
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
  | TDbList tipe ->
      "[" ^ tipe_to_string tipe ^ "]"
  | TPassword ->
      "Password"
  | TUuid ->
      "UUID"
  | TOption ->
      "Option"
  | TErrorRail ->
      "ErrorRail"
  | TResult ->
      "Result"
  | TUserType (name, _) ->
      name
  | TBytes ->
      "Bytes"
  | TDeprecated1
  | TDeprecated2
  | TDeprecated3
  | TDeprecated4 _
  | TDeprecated5 _
  | TDeprecated6 ->
      Exception.internal "Deprecated type"


let rec tipe_of_string str : tipe =
  match String.lowercase str with
  | "any" ->
      TAny
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
  | "nothing" ->
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
  | "error" ->
      TError
  | "response" ->
      TResp
  | "datastore" ->
      TDB
  | "date" ->
      TDate
  | "title" ->
      Exception.internal "Deprecated type"
  | "url" ->
      Exception.internal "Deprecated type"
  | "password" ->
      TPassword
  | "uuid" ->
      TUuid
  | "option" ->
      TOption
  | "errorrail" ->
      TErrorRail
  | "result" ->
      TResult
  | _ ->
      (* otherwise *)
      if String.is_prefix str "[" && String.is_suffix str "]"
      then
        str
        |> fun s ->
        String.drop_prefix s 1
        |> fun s -> String.drop_suffix s 1 |> parse_list_tipe
      else Exception.internal ("Unhandled tipe_of_string: " ^ str)


and parse_list_tipe (list_tipe : string) : tipe =
  match String.lowercase list_tipe with
  | "str" ->
      TDbList TStr
  | "string" ->
      TDbList TStr
  | "int" ->
      TDbList TInt
  | "integer" ->
      TDbList TInt
  | "float" ->
      TDbList TFloat
  | "bool" ->
      TDbList TBool
  | "boolean" ->
      TDbList TBool
  | "password" ->
      TDbList TPassword
  | "uuid" ->
      TDbList TUuid
  | "obj" ->
      Exception.internal "todo"
  | "block" ->
      Exception.internal "todo"
  | "incomplete" ->
      Exception.internal "todo"
  | "error" ->
      Exception.internal "todo"
  | "response" ->
      Exception.internal "todo"
  | "datastore" ->
      Exception.internal "todo"
  | "date" ->
      TDbList TDate
  | "title" ->
      TDbList TStr
  | "url" ->
      TDbList TStr
  | _ ->
      Exception.internal ("Unhandled parse_list_tipe: " ^ list_tipe)


let rec tipe_of (dv : dval) : tipe =
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
  | DBlock _ ->
      TBlock
  | DError _ ->
      TError
  | DIncomplete ->
      TIncomplete
  | DResp _ ->
      TResp
  | DDB _ ->
      TDB
  | DDate _ ->
      TDate
  | DPassword _ ->
      TPassword
  | DUuid _ ->
      TUuid
  | DOption _ ->
      TOption
  | DErrorRail _ ->
      TErrorRail
  | DResult _ ->
      TResult
  | DBytes _ ->
      TBytes


(* Users should not be aware of this *)
let tipename (dv : dval) : string =
  dv |> tipe_of |> tipe_to_string |> String.lowercase


let pretty_tipename (dv : dval) : string = dv |> tipe_of |> tipe_to_string

let unsafe_tipe_to_yojson (t : tipe) : Yojson.Safe.t =
  `String (t |> tipe_to_string |> String.lowercase)


let unsafe_tipe_of_yojson (json : Yojson.Safe.t) =
  match json with
  | `String s ->
      Ok (s |> String.lowercase |> tipe_of_string)
  | _ ->
      Exception.code "Invalid tipe"


(* ------------------------- *)
(* ErrorRail Functions *)
(* ------------------------- *)

let is_errorrail e = match e with DErrorRail _ -> true | _ -> false

let unwrap_from_errorrail (dv : dval) =
  match dv with DErrorRail dv -> dv | other -> other


let exception_to_dval exc = DError (Exception.to_string exc)

(* A dval is a `fake marker` if it is a {DErrorRail, DIncomplete, DError}.
 * These types of values are stand-ins/markers for some static property about the
 * code. They should be propagated through the programs execution whenever they are
 * found.*)
let is_fake_marker_dval (dv : dval) =
  match dv with DErrorRail _ | DIncomplete | DError _ -> true | _ -> false


(* Anytime we create a dlist with the except of list literals,
 * we need to make sure that there are no incomplete, error rail
 * values within the list *)
let to_list (l : dval list) : dval =
  let found = List.find l ~f:is_fake_marker_dval in
  match found with Some v -> v | None -> DList l


(* ------------------------- *)
(* Obj Functions *)
(* ------------------------- *)
let obj_merge (l : dval) (r : dval) : dval =
  match (l, r) with
  | DObj l, DObj r ->
      DObj (Util.merge_left l r)
  | DNull, DObj r ->
      DObj r
  | DObj l, DNull ->
      DObj l
  | _ ->
      Exception.code "was expecting objs"


let empty_dobj : dval = DObj DvalMap.empty

(* ------------------------- *)
(* Json *)
(* ------------------------- *)

(* The "unsafe" variations here are bad. They encode data ambiguously, and
 * though we mostly have the decoding right, it's brittle and unsafe.  This
 * should be considered append only. There's a ton of dangerous things in this,
 * and we really need to move off it, but for now we're here. Do not change
 * existing encodings - this will break everything.
 *)
let rec unsafe_dval_of_yojson_v0 (json : Yojson.Safe.t) : dval =
  (* sort so this isn't key-order-dependent. *)
  let json = Yojson.Safe.sort json in
  match json with
  | `Int i ->
      DInt (Dint.of_int i)
  | `Intlit i ->
      DInt (Dint.of_string_exn i)
  | `Float f ->
      DFloat f
  | `Bool b ->
      DBool b
  | `Null ->
      DNull
  | `String s ->
      dstr_of_string_exn s
  | `List l ->
      (* We shouldnt have saved dlist that have incompletes or error rails but we might have *)
      to_list (List.map ~f:unsafe_dval_of_yojson_v0 l)
  | `Variant v ->
      Exception.internal "We dont use variants"
  | `Tuple v ->
      Exception.internal "We dont use tuples"
  | `Assoc [("type", `String "response"); ("value", `List [a; b])] ->
      DResp
        (Result.ok_or_failwith (dhttp_of_yojson a), unsafe_dval_of_yojson_v0 b)
  | `Assoc
      [ ("constructor", `String constructor)
      ; ("type", `String tipe)
      ; ("values", `List vs) ] ->
      let expectOne ~f vs =
        match vs with
        | [v] ->
            f v
        | _ ->
            DObj (unsafe_dvalmap_of_yojson_v0 json)
      in
      ( match (tipe, constructor) with
      | "result", "Ok" ->
          vs
          |> expectOne ~f:(fun v ->
                 DResult (ResOk (unsafe_dval_of_yojson_v0 v)) )
      | "result", "Error" ->
          vs
          |> expectOne ~f:(fun v ->
                 DResult (ResError (unsafe_dval_of_yojson_v0 v)) )
      | _ ->
          DObj (unsafe_dvalmap_of_yojson_v0 json) )
  | `Assoc [("type", `String tipe); ("value", `Null)] ->
    ( match tipe with
    | "incomplete" ->
        DIncomplete
    | "option" ->
        DOption OptNothing
    | "block" ->
        DBlock (fun _ -> DError "Can't deserialize blocks")
    | "errorrail" ->
        DErrorRail DNull
    | _ ->
        DObj (unsafe_dvalmap_of_yojson_v0 json) )
  | `Assoc [("type", `String tipe); ("value", `String v)] ->
    ( match tipe with
    | "date" ->
        DDate (Util.date_of_isostring v)
    | "title" ->
        Exception.internal "Deprecated type"
    | "url" ->
        Exception.internal "Deprecated type"
    | "error" ->
        DError v
    | "password" ->
        v |> B64.decode |> Bytes.of_string |> DPassword
    | "datastore" ->
        DDB v
    | "uuid" ->
        DUuid (Uuidm.of_string v |> Option.value_exn)
    | "char" | "character" ->
        DCharacter (Unicode_string.Character.unsafe_of_string v)
    | "bytes" ->
        DBytes (v |> B64.decode |> RawBytes.of_string)
    | _ ->
        DObj (unsafe_dvalmap_of_yojson_v0 json) )
  | `Assoc [("type", `String "option"); ("value", dv)] ->
      DOption (OptJust (unsafe_dval_of_yojson_v0 dv))
  | `Assoc [("type", `String "errorrail"); ("value", dv)] ->
      DErrorRail (unsafe_dval_of_yojson_v0 dv)
  | `Assoc _ ->
      DObj (unsafe_dvalmap_of_yojson_v0 json)


and unsafe_dvalmap_of_yojson_v0 (json : Yojson.Safe.t) : dval_map =
  match json with
  | `Assoc alist ->
      List.fold_left
        alist
        ~f:(fun m (k, v) ->
          DvalMap.insert m ~key:k ~value:(unsafe_dval_of_yojson_v0 v) )
        ~init:DvalMap.empty
  | _ ->
      Exception.internal "Not a json object"


let rec unsafe_dval_of_yojson_v1 (json : Yojson.Safe.t) : dval =
  (* sort so this isn't key-order-dependent. *)
  let json = Yojson.Safe.sort json in
  match json with
  | `Int i ->
      DInt (Dint.of_int i)
  | `Intlit i ->
      DInt (Dint.of_string_exn i)
  | `Float f ->
      DFloat f
  | `Bool b ->
      DBool b
  | `Null ->
      DNull
  | `String s ->
      dstr_of_string_exn s
  | `List l ->
      (* We shouldnt have saved dlist that have incompletes or error rails but we might have *)
      to_list (List.map ~f:unsafe_dval_of_yojson_v1 l)
  | `Variant v ->
      Exception.internal "We dont use variants"
  | `Tuple v ->
      Exception.internal "We dont use tuples"
  | `Assoc [("type", `String "response"); ("value", `List [a; b])] ->
      DResp
        (Result.ok_or_failwith (dhttp_of_yojson a), unsafe_dval_of_yojson_v1 b)
  | `Assoc
      [ ("constructor", `String constructor)
      ; ("type", `String tipe)
      ; ("values", `List vs) ] ->
      let expectOne ~f vs =
        match vs with
        | [v] ->
            f v
        | _ ->
            DObj (unsafe_dvalmap_of_yojson_v1 json)
      in
      ( match (tipe, constructor) with
      | "result", "Ok" ->
          vs
          |> expectOne ~f:(fun v ->
                 DResult (ResOk (unsafe_dval_of_yojson_v1 v)) )
      | "result", "Error" ->
          vs
          |> expectOne ~f:(fun v ->
                 DResult (ResError (unsafe_dval_of_yojson_v1 v)) )
      | _ ->
          DObj (unsafe_dvalmap_of_yojson_v1 json) )
  | `Assoc [("type", `String "incomplete"); ("value", `Null)] ->
      DIncomplete
  | `Assoc [("type", `String "option"); ("value", dv)] ->
      if dv = `Null
      then DOption OptNothing
      else DOption (OptJust (unsafe_dval_of_yojson_v1 dv))
  | `Assoc [("type", `String "block"); ("value", `Null)] ->
      DBlock (fun _ -> DError "Can't deserialize blocks")
  | `Assoc [("type", `String "errorrail"); ("value", dv)] ->
      DErrorRail (unsafe_dval_of_yojson_v1 dv)
  | `Assoc [("type", `String "date"); ("value", `String v)] ->
      DDate (Util.date_of_isostring v)
  | `Assoc [("type", `String "error"); ("value", `String v)] ->
      DError v
  | `Assoc [("type", `String "password"); ("value", `String v)] ->
      v |> B64.decode |> Bytes.of_string |> DPassword
  | `Assoc [("type", `String "datastore"); ("value", `String v)] ->
      DDB v
  | `Assoc [("type", `String "uuid"); ("value", `String v)] ->
      DUuid (Uuidm.of_string v |> Option.value_exn)
  | `Assoc [("type", `String "char"); ("value", `String v)]
  | `Assoc [("type", `String "character"); ("value", `String v)] ->
      DCharacter (Unicode_string.Character.unsafe_of_string v)
  | `Assoc [("type", `String "bytes"); ("value", `String v)] ->
      DBytes (v |> B64.decode |> RawBytes.of_string)
  | `Assoc _ ->
      DObj (unsafe_dvalmap_of_yojson_v1 json)


and unsafe_dvalmap_of_yojson_v1 (json : Yojson.Safe.t) : dval_map =
  match json with
  | `Assoc alist ->
      List.fold_left
        alist
        ~f:(fun m (k, v) ->
          DvalMap.insert m ~key:k ~value:(unsafe_dval_of_yojson_v1 v) )
        ~init:DvalMap.empty
  | _ ->
      Exception.internal "Not a json object"


let rec unsafe_dval_to_yojson_v0 ?(redact = true) (dv : dval) : Yojson.Safe.t =
  let tipe = dv |> tipe_of |> unsafe_tipe_to_yojson in
  let wrap_user_type value = `Assoc [("type", tipe); ("value", value)] in
  let wrap_constructed_type cons values =
    `Assoc [("type", tipe); ("constructor", cons); ("values", `List values)]
  in
  let wrap_user_str value = wrap_user_type (`String value) in
  match dv with
  (* basic types *)
  | DInt i ->
      Dint.to_yojson i
  | DFloat f ->
      `Float f
  | DBool b ->
      `Bool b
  | DNull ->
      `Null
  | DStr s ->
      Unicode_string.to_yojson s
  | DList l ->
      `List (List.map l (unsafe_dval_to_yojson_v0 ~redact))
  | DObj o ->
      o
      |> DvalMap.to_list
      |> List.map ~f:(fun (k, v) -> (k, unsafe_dval_to_yojson_v0 ~redact v))
      |> fun x -> `Assoc x
  | DBlock _ | DIncomplete ->
      wrap_user_type `Null
  | DCharacter c ->
      wrap_user_str (Unicode_string.Character.to_string c)
  | DError msg ->
      wrap_user_str msg
  | DResp (h, hdv) ->
      wrap_user_type
        (`List [dhttp_to_yojson h; unsafe_dval_to_yojson_v0 ~redact hdv])
  | DDB dbname ->
      wrap_user_str dbname
  | DDate date ->
      wrap_user_str (Util.isostring_of_date date)
  | DPassword hashed ->
      if redact
      then wrap_user_type `Null
      else hashed |> Bytes.to_string |> B64.encode |> wrap_user_str
  | DUuid uuid ->
      wrap_user_str (Uuidm.to_string uuid)
  | DOption opt ->
    ( match opt with
    | OptNothing ->
        wrap_user_type `Null
    | OptJust dv ->
        wrap_user_type (unsafe_dval_to_yojson_v0 ~redact dv) )
  | DErrorRail dv ->
      wrap_user_type (unsafe_dval_to_yojson_v0 ~redact dv)
  | DResult res ->
    ( match res with
    | ResOk dv ->
        wrap_constructed_type
          (`String "Ok")
          [unsafe_dval_to_yojson_v0 ~redact dv]
    | ResError dv ->
        wrap_constructed_type
          (`String "Error")
          [unsafe_dval_to_yojson_v0 ~redact dv] )
  | DBytes bytes ->
      bytes |> RawBytes.to_string |> B64.encode |> wrap_user_str


let unsafe_dval_to_yojson_v1 ?(redact = true) (dv : dval) : Yojson.Safe.t =
  unsafe_dval_to_yojson_v0 ~redact dv


(* ------------------------- *)
(* String representations *)
(* ------------------------- *)
(* We previously had a lot of code reuse, which made all these different
 * versions brittle, buggy, and difficult to change. After inlining everything,
 * they became a lot easier to reason about. *)

let dhttp_to_formatted_string (d : dhttp) : string =
  match d with
  | Redirect url ->
      "302 " ^ url
  | Response (c, hs) ->
      let string_of_headers hs =
        hs
        |> List.map ~f:(fun (k, v) -> k ^ ": " ^ v)
        |> String.concat ~sep:","
        |> fun s -> "{ " ^ s ^ " }"
      in
      string_of_int c ^ " " ^ string_of_headers hs


let rec to_nested_string ~(reprfn : dval -> string) (dv : dval) : string =
  let rec inner (indent : int) (dv : dval) : string =
    let nl = "\n" ^ String.make indent ' ' in
    let inl = "\n" ^ String.make (indent + 2) ' ' in
    let indent = indent + 2 in
    let recurse = inner indent in
    match dv with
    | DList l ->
        if List.is_empty l
        then "[]"
        else
          "[ "
          ^ inl
          ^ String.concat ~sep:", " (List.map ~f:recurse l)
          ^ nl
          ^ "]"
    | DObj o ->
        if DvalMap.is_empty o
        then "{}"
        else
          let strs =
            DvalMap.foldl o ~init:[] ~f:(fun ~key ~value l ->
                (key ^ ": " ^ recurse value) :: l )
          in
          "{ " ^ inl ^ String.concat ~sep:("," ^ inl) strs ^ nl ^ "}"
    | _ ->
        reprfn dv
  in
  inner 0 dv


(* ------------------------- *)
(* Roundtrippable - for events and traces *)
(* ------------------------- *)
let to_internal_roundtrippable_v0 dval : string =
  unsafe_dval_to_yojson_v1 ~redact:false dval |> Yojson.Safe.to_string


let of_internal_roundtrippable_json_v0 j =
  (* Switched to v1 cause it was a bug fix *)
  Result.try_with (fun _ -> unsafe_dval_of_yojson_v1 j)
  |> Result.map_error ~f:Exception.to_string


let of_internal_roundtrippable_v0 str : dval =
  str |> Yojson.Safe.from_string |> unsafe_dval_of_yojson_v1


(* ------------------------- *)
(* Queryable - for the DB *)
(* ------------------------- *)

let to_internal_queryable_v0 dval : string =
  dval |> unsafe_dval_to_yojson_v0 ~redact:false |> Yojson.Safe.to_string


let of_internal_queryable_v0 (str : string) : dval =
  str |> Yojson.Safe.from_string |> unsafe_dval_of_yojson_v0


let to_internal_queryable_v1 dval : string =
  match dval with
  | DObj _ ->
      dval |> unsafe_dval_to_yojson_v0 ~redact:false |> Yojson.Safe.to_string
  | _ ->
      Exception.internal
        "trying to serialize non-objects using an object-serializer"


let of_internal_queryable_v1 (str : string) : dval =
  (* The first level _must_ be an object at the moment *)
  let rec convert_top_level (json : Yojson.Safe.t) : dval =
    match json with
    | `Assoc alist ->
        DObj
          (List.fold_left
             alist
             ~f:(fun m (k, v) -> DvalMap.insert m ~key:k ~value:(convert v))
             ~init:DvalMap.empty)
    | _ ->
        Exception.internal "Value that isn't an object"
  and convert (json : Yojson.Safe.t) : dval =
    (* sort so this isn't key-order-dependent. *)
    let json = Yojson.Safe.sort json in
    match json with
    | `Int i ->
        DInt (Dint.of_int i)
    | `Intlit i ->
        DInt (Dint.of_string_exn i)
    | `Float f ->
        DFloat f
    | `Bool b ->
        DBool b
    | `Null ->
        DNull
    | `String s ->
        dstr_of_string_exn s
    | `List l ->
        (* We shouldnt have saved dlist that have incompletes or error rails but we might have *)
        to_list (List.map ~f:convert l)
    | `Variant v ->
        Exception.internal "We dont use variants"
    | `Tuple v ->
        Exception.internal "We dont use tuples"
    (* These are the only types that are allowed in the queryable
     * representation. We may allow more in the future, but the real thing to
     * do is to use the DB's type and version to encode/decode them correctly
     * *)
    | `Assoc [("type", `String "date"); ("value", `String v)] ->
        DDate (Util.date_of_isostring v)
    | `Assoc [("type", `String "password"); ("value", `String v)] ->
        v |> B64.decode |> Bytes.of_string |> DPassword
    | `Assoc [("type", `String "uuid"); ("value", `String v)] ->
        DUuid (Uuidm.of_string v |> Option.value_exn)
    | `Assoc alist ->
        DObj
          (List.fold_left
             alist
             ~f:(fun m (k, v) -> DvalMap.insert m ~key:k ~value:(convert v))
             ~init:DvalMap.empty)
  in
  str |> Yojson.Safe.from_string |> convert_top_level


(* ------------------------- *)
(* Other formats *)
(* ------------------------- *)
let rec to_enduser_readable_text_v0 dval =
  let rec nestedreprfn dv =
    (* If nesting inside an object or a list, wrap strings in quotes *)
    match dv with
    | DStr _ | DUuid _ | DCharacter _ ->
        "\"" ^ reprfn dv ^ "\""
    | _ ->
        reprfn dv
  and reprfn dv =
    match dv with
    | DInt i ->
        Dint.to_string i
    | DBool true ->
        "true"
    | DBool false ->
        "false"
    | DStr s ->
        Unicode_string.to_string s
    | DFloat f ->
        string_of_float f
    | DCharacter c ->
        Unicode_string.Character.to_string c
    | DNull ->
        "null"
    | DDate d ->
        Util.isostring_of_date d
    | DUuid uuid ->
        Uuidm.to_string uuid
    | DDB dbname ->
        "<DB: " ^ dbname ^ ">"
    | DError msg ->
        "Error: " ^ msg
    | DIncomplete ->
        "<Incomplete>"
    | DBlock _ ->
        "<Block>"
    | DPassword _ ->
        (* redacting, do not unredact *)
        "<Password>"
    | DObj o ->
        to_nested_string ~reprfn:nestedreprfn dv
    | DList l ->
        to_nested_string ~reprfn:nestedreprfn dv
    | DErrorRail d ->
        (* We don't print error here, because the errorrail value will know
           * whether it's an error or not. *)
        reprfn d
    | DResp (dh, dv) ->
        dhttp_to_formatted_string dh ^ "\n" ^ nestedreprfn dv ^ ""
    | DResult (ResOk d) ->
        reprfn d
    | DResult (ResError d) ->
        "Error: " ^ reprfn d
    | DOption (OptJust d) ->
        reprfn d
    | DOption OptNothing ->
        "Nothing"
    | DBytes bytes ->
        Bytes.to_string bytes
  in
  reprfn dval


let to_enduser_readable_html_v0 dv = to_enduser_readable_text_v0 dv

let rec to_developer_repr_v0 (dv : dval) : string =
  let rec to_repr_ (indent : int) (dv : dval) : string =
    let nl = "\n" ^ String.make indent ' ' in
    let inl = "\n" ^ String.make (indent + 2) ' ' in
    let indent = indent + 2 in
    let wrap str = "<" ^ pretty_tipename dv ^ ": " ^ str ^ ">" in
    let justtipe = "<" ^ pretty_tipename dv ^ ">" in
    match dv with
    | DPassword _ ->
        "<password>"
    | DStr s ->
        "\"" ^ Unicode_string.to_string s ^ "\""
    | DCharacter c ->
        "'" ^ Unicode_string.Character.to_string c ^ "'"
    | DInt i ->
        Dint.to_string i
    | DBool true ->
        "true"
    | DBool false ->
        "false"
    | DFloat f ->
        string_of_float f
    | DNull ->
        "null"
    | DBlock _ ->
        justtipe
    | DIncomplete ->
        justtipe
    | DError msg ->
        wrap msg
    | DDate d ->
        wrap (Util.isostring_of_date d)
    | DDB name ->
        wrap name
    | DUuid uuid ->
        wrap (Uuidm.to_string uuid)
    | DResp (h, hdv) ->
        dhttp_to_formatted_string h ^ nl ^ to_repr_ indent hdv
    | DList l ->
        if List.is_empty l
        then "[]"
        else
          "[ "
          ^ inl
          ^ String.concat ~sep:", " (List.map ~f:(to_repr_ indent) l)
          ^ nl
          ^ "]"
    | DObj o ->
        if DvalMap.is_empty o
        then "{}"
        else
          let strs =
            DvalMap.foldl o ~init:[] ~f:(fun ~key ~value l ->
                (key ^ ": " ^ to_repr_ indent value) :: l )
          in
          "{ " ^ inl ^ String.concat ~sep:("," ^ inl) strs ^ nl ^ "}"
    | DOption OptNothing ->
        "Nothing"
    | DOption (OptJust dv) ->
        "Just " ^ to_repr_ indent dv
    | DResult (ResOk dv) ->
        "Ok " ^ to_repr_ indent dv
    | DResult (ResError dv) ->
        "Error " ^ to_repr_ indent dv
    | DErrorRail dv ->
        "ErrorRail: " ^ to_repr_ indent dv
    | DBytes bytes ->
        bytes |> RawBytes.to_string |> B64.encode
  in
  to_repr_ 0 dv


let to_pretty_machine_json_v1 dval =
  let rec recurse dv =
    match dv with
    (* basic types *)
    | DInt i ->
        Dint.to_yojson i
    | DFloat f ->
        `Float f
    | DBool b ->
        `Bool b
    | DNull ->
        `Null
    | DStr s ->
        Unicode_string.to_yojson s
    | DList l ->
        `List (List.map l recurse)
    | DObj o ->
        o
        |> DvalMap.to_list
        |> List.map ~f:(fun (k, v) -> (k, recurse v))
        |> fun x -> `Assoc x
    | DBlock _ | DIncomplete ->
        `Null
    | DCharacter c ->
        `String (Unicode_string.Character.to_string c)
    | DError msg ->
        `Assoc [("Error", `String msg)]
    | DResp (h, hdv) ->
        recurse hdv
    | DDB dbname ->
        `String dbname
    | DDate date ->
        `String (Util.isostring_of_date date)
    | DPassword hashed ->
        `Assoc [("Error", `String "Password is redacted")]
    | DUuid uuid ->
        `String (Uuidm.to_string uuid)
    | DOption opt ->
      (match opt with OptNothing -> `Null | OptJust dv -> recurse dv)
    | DErrorRail dv ->
        recurse dv
    | DResult res ->
      ( match res with
      | ResOk dv ->
          recurse dv
      | ResError dv ->
          `Assoc [("Error", recurse dv)] )
    | DBytes bytes ->
        `String (bytes |> RawBytes.to_string |> B64.encode)
  in
  recurse dval |> Yojson.Safe.pretty_to_string


let of_unknown_json_v0 str =
  try str |> Yojson.Safe.from_string |> unsafe_dval_of_yojson_v0 with e ->
    Exception.code ~actual:str ("Invalid json: " ^ Exception.to_string e)


let of_unknown_json_v1 str =
  let rec convert json =
    match json with
    | `Int i ->
        DInt (Dint.of_int i)
    | `Intlit i ->
        DInt (Dint.of_string_exn i)
    | `Float f ->
        DFloat f
    | `Bool b ->
        DBool b
    | `Null ->
        DNull
    | `String s ->
        dstr_of_string_exn s
    | `List l ->
        to_list (List.map ~f:convert l)
    | `Variant v ->
        Exception.internal "We dont use variants"
    | `Tuple v ->
        Exception.internal "We dont use tuples"
    | `Assoc alist ->
        DObj
          (List.fold_left
             alist
             ~f:(fun m (k, v) -> DvalMap.insert m ~key:k ~value:(convert v))
             ~init:DvalMap.empty)
  in
  str |> Yojson.Safe.from_string |> convert


let rec show dv =
  match dv with
  | DInt i ->
      Dint.to_string i
  | DBool true ->
      "true"
  | DBool false ->
      "false"
  | DStr s ->
      Unicode_string.to_string s
  | DFloat f ->
      string_of_float f
  | DCharacter c ->
      Unicode_string.Character.to_string c
  | DNull ->
      "null"
  | DDate d ->
      Util.isostring_of_date d
  | DUuid uuid ->
      Uuidm.to_string uuid
  | DDB dbname ->
      "<DB: " ^ dbname ^ ">"
  | DError msg ->
      "<Error: " ^ msg ^ ">"
  | DIncomplete ->
      "<Incomplete>"
  | DBlock _ ->
      "<Block>"
  | DPassword _ ->
      (* redacting, do not unredact *)
      "<Password>"
  | DObj o ->
      to_nested_string ~reprfn:show dv
  | DList l ->
      to_nested_string ~reprfn:show dv
  | DErrorRail d ->
      (* We don't print error here, because the errorrail value will know
          * whether it's an error or not. *)
      "<ErrorRail: " ^ show d ^ ">"
  | DResp (dh, dv) ->
      dhttp_to_formatted_string dh ^ "\n" ^ show dv ^ ""
  | DResult (ResOk d) ->
      "Ok " ^ show d
  | DResult (ResError d) ->
      "Error " ^ show d
  | DOption (OptJust d) ->
      "Just " ^ show d
  | DOption OptNothing ->
      "Nothing"
  | DBytes bytes ->
      "<Bytes: length=" ^ string_of_int (RawBytes.length bytes) ^ ">"


let parse_literal (str : string) : dval option =
  let len = String.length str in
  (* Character *)
  if len > 2 && str.[0] = '\'' && str.[len - 1] = '\''
  then
    Some
      (DCharacter
         (Unicode_string.Character.unsafe_of_string
            (String.sub ~pos:1 ~len:(len - 2) str)))
    (* String *)
  else if len > 1 && str.[0] = '"' && str.[len - 1] = '"'
  then
    (* It might have \n characters in it (as well as probably other codes like
     * \r or some shit that we haven't taken into account), which need to be
     * converted manually to appropriate string chars. *)
    str
    |> String.sub ~pos:1 ~len:(len - 2)
    |> Util.string_replace "\\\"" "\""
    |> fun s -> Some (dstr_of_string_exn s)
  else if str = "null"
  then Some DNull
  else if str = "true"
  then Some (DBool true)
  else if str = "false"
  then Some (DBool false)
  else
    try Some (DInt (Dint.of_string_exn str)) with _ ->
      ( match float_of_string_opt str with
      | Some v ->
          Some (DFloat v)
      | None ->
          None )


(* ------------------------- *)
(* Conversion Functions *)
(* ------------------------- *)
let to_char dv : string option =
  match dv with
  | DCharacter c ->
      Some (Unicode_string.Character.to_string c)
  | _ ->
      None


let to_int dv : Dint.t option = match dv with DInt i -> Some i | _ -> None

let dint (i : int) : dval = DInt (Dint.of_int i)

let to_dobj_exn (pairs : (string * dval) list) : dval =
  match DvalMap.from_list_unique pairs with
  | Ok ok ->
      DObj ok
  | Error err ->
      DError err


let to_string_opt dv : string option =
  match dv with DStr s -> Some (Unicode_string.to_string s) | _ -> None


let to_string_exn dv : string =
  match to_string_opt dv with
  | Some s ->
      s
  | None ->
      Exception.code "expecting str" ~actual:(to_developer_repr_v0 dv)


let to_dval_pairs_exn dv : (string * dval) list =
  match dv with
  | DObj obj ->
      DvalMap.to_list obj
  | _ ->
      Exception.code "expecting str" ~actual:(to_developer_repr_v0 dv)


let to_string_pairs_exn dv : (string * string) list =
  dv |> to_dval_pairs_exn |> List.map ~f:(fun (k, v) -> (k, to_string_exn v))


(* For putting into URLs as query params *)
let rec to_url_string_exn (dv : dval) : string =
  match dv with
  | DBlock _ | DIncomplete | DPassword _ ->
      "<" ^ (dv |> tipename) ^ ">"
  | DInt i ->
      Dint.to_string i
  | DBool true ->
      "true"
  | DBool false ->
      "false"
  | DStr s ->
      Unicode_string.to_string s
  | DFloat f ->
      string_of_float f
  | DCharacter c ->
      Unicode_string.Character.to_string c
  | DNull ->
      "null"
  | DDate d ->
      Util.isostring_of_date d
  | DDB dbname ->
      dbname
  | DErrorRail d ->
      to_url_string_exn d
  | DError msg ->
      "error=" ^ msg
  | DUuid uuid ->
      Uuidm.to_string uuid
  | DResp (_, hdv) ->
      to_url_string_exn hdv
  | DList l ->
      "[ " ^ String.concat ~sep:", " (List.map ~f:to_url_string_exn l) ^ " ]"
  | DObj o ->
      let strs =
        DvalMap.foldl o ~init:[] ~f:(fun ~key ~value l ->
            (key ^ ": " ^ to_url_string_exn value) :: l )
      in
      "{ " ^ String.concat ~sep:", " strs ^ " }"
  | DOption OptNothing ->
      "none"
  | DOption (OptJust v) ->
      to_url_string_exn v
  | DResult (ResError v) ->
      "error=" ^ to_url_string_exn v
  | DResult (ResOk v) ->
      to_url_string_exn v
  | DBytes bytes ->
      bytes |> RawBytes.to_string |> B64.encode


(* ------------------------- *)
(* Forms and queries Functions *)
(* ------------------------- *)

let query_to_dval (query : (string * string list) list) : dval =
  query
  |> List.map ~f:(fun (key, vals) ->
         let dval =
           match vals with
           | [] ->
               DNull
           | [v] ->
               if v = "" then DNull else dstr_of_string_exn v
           | vals ->
               DList (List.map ~f:(fun x -> dstr_of_string_exn x) vals)
         in
         (key, dval) )
  |> DvalMap.from_list
  |> DObj


let dval_to_query (dv : dval) : (string * string list) list =
  match dv with
  | DObj kvs ->
      kvs
      |> DvalMap.to_list
      |> List.map ~f:(fun (k, value) ->
             match value with
             | DNull ->
                 (k, [])
             | DList l ->
                 (k, List.map ~f:to_url_string_exn l)
             | _ ->
                 (k, [to_url_string_exn value]) )
  | _ ->
      Exception.code "attempting to use non-object as query param"


let to_form_encoding (dv : dval) : string =
  dv |> dval_to_query |> Uri.encoded_of_query


let of_form_encoding (f : string) : dval =
  f |> Uri.query_of_encoded |> query_to_dval


(* ------------------------- *)
(* Hashes *)
(* ------------------------- *)

(* This has been used to save millions of values in our DB, so the format isn't
 * amenable to change without a migration. Don't change ANYTHING for existing
 * values, but continue to add representations for new values. Also, inline
 * everything! *)
let rec to_hashable_repr ?(indent = 0) (dv : dval) : string =
  let nl = "\n" ^ String.make indent ' ' in
  let inl = "\n" ^ String.make (indent + 2) ' ' in
  let indent = indent + 2 in
  match dv with
  | DDB dbname ->
      "<db: " ^ dbname ^ ">"
  | DInt i ->
      Dint.to_string i
  | DBool true ->
      "true"
  | DBool false ->
      "false"
  | DFloat f ->
      string_of_float f
  | DNull ->
      "null"
  | DStr s ->
      "\"" ^ Unicode_string.to_string s ^ "\""
  | DCharacter c ->
      "'" ^ Unicode_string.Character.to_string c ^ "'"
  | DIncomplete ->
      "<incomplete: <incomplete>>" (* Can't be used anyway *)
  | DBlock _ ->
      "<block: <block>>"
  | DError msg ->
      "<error: " ^ msg ^ ">"
  | DDate d ->
      "<date: " ^ Util.isostring_of_date d ^ ">"
  | DPassword _ ->
      "<password: <password>>"
  | DUuid id ->
      "<uuid: " ^ Uuidm.to_string id ^ ">"
  | DResp (h, hdv) ->
      (* deliberately inlined *)
      let dhttp_to_formatted_string (d : dhttp) : string =
        match d with
        | Redirect url ->
            "302 " ^ url
        | Response (c, hs) ->
            let string_of_headers hs =
              hs
              |> List.map ~f:(fun (k, v) -> k ^ ": " ^ v)
              |> String.concat ~sep:","
              |> fun s -> "{ " ^ s ^ " }"
            in
            string_of_int c ^ " " ^ string_of_headers hs
      in
      dhttp_to_formatted_string h ^ nl ^ to_hashable_repr ~indent hdv
  | DList l ->
      if List.is_empty l
      then "[]"
      else
        "[ "
        ^ inl
        ^ String.concat ~sep:", " (List.map ~f:(to_hashable_repr ~indent) l)
        ^ nl
        ^ "]"
  | DObj o ->
      if DvalMap.is_empty o
      then "{}"
      else
        let strs =
          DvalMap.foldl o ~init:[] ~f:(fun ~key ~value l ->
              (key ^ ": " ^ to_hashable_repr ~indent value) :: l )
        in
        "{ " ^ inl ^ String.concat ~sep:("," ^ inl) strs ^ nl ^ "}"
  | DOption OptNothing ->
      "Nothing"
  | DOption (OptJust dv) ->
      "Just " ^ to_hashable_repr ~indent dv
  | DErrorRail dv ->
      "ErrorRail: " ^ to_hashable_repr ~indent dv
  | DResult (ResOk dv) ->
      "ResultOk " ^ to_hashable_repr ~indent dv
  | DResult (ResError dv) ->
      "ResultError " ^ to_hashable_repr ~indent dv
  | DBytes bytes ->
      bytes |> RawBytes.to_string


(* Originally to prevent storing sensitive data to disk, this also reduces the
 * size of the data stored by only storing a hash *)
let hash (arglist : dval list) : string =
  arglist |> List.map ~f:to_hashable_repr |> String.concat |> Util.hash
