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
  | TChar ->
      "Char"
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
  | TID ->
      "ID"
  | TDate ->
      "Date"
  | TTitle ->
      "Title"
  | TUrl ->
      "Url"
  | TBelongsTo s ->
      s
  | THasMany s ->
      "[" ^ s ^ "]"
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
  | "char" ->
      TChar
  | "character" ->
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
  | "id" ->
      TID
  | "date" ->
      TDate
  | "title" ->
      TTitle
  | "url" ->
      TUrl
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
      else TBelongsTo str


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
  | "id" ->
      TDbList TID
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
      Exception.internal "todo"
  | "title" ->
      Exception.internal "todo"
  | "url" ->
      Exception.internal "todo"
  | table ->
      THasMany list_tipe


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
  | DID _ ->
      TID
  | DDate _ ->
      TDate
  | DTitle _ ->
      TTitle
  | DUrl _ ->
      TUrl
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


(* Users should not be aware of this *)
let tipename (dv : dval) : string =
  dv |> tipe_of |> tipe_to_string |> String.lowercase


let pretty_tipename (dv : dval) : string = dv |> tipe_of |> tipe_to_string

let tipe_to_yojson (t : tipe) : Yojson.Safe.json =
  `String (t |> tipe_to_string |> String.lowercase)


let tipe_of_yojson (json : Yojson.Safe.json) =
  match json with
  | `String s ->
      Ok (s |> String.lowercase |> tipe_of_string)
  | _ ->
      Exception.user "Invalid tipe"


(* ------------------------- *)
(* ErrorRail Functions *)
(* ------------------------- *)

let is_errorrail e = match e with DErrorRail _ -> true | _ -> false

let unwrap_from_errorrail (dv : dval) =
  match dv with DErrorRail dv -> dv | other -> other


let exception_to_dval exc = DError (Exception.to_string exc)

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
      Exception.user "was expecting objs"


let empty_dobj : dval = DObj DvalMap.empty

(* ------------------------- *)
(* Old Representations *)
(* ------------------------- *)
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


(* Returns the string within string-ish values, without adornment. *)
let as_string (dv : dval) : string =
  match dv with
  | DInt i ->
      string_of_int i
  | DBool true ->
      "true"
  | DBool false ->
      "false"
  | DStr s ->
      Unicode_string.to_string s
  | DFloat f ->
      string_of_float f
  | DChar c ->
      Char.to_string c
  | DCharacter c ->
      Unicode_string.Character.to_string c
  | DNull ->
      "null"
  | DID id ->
      Uuidm.to_string id
  | DDate d ->
      Util.isostring_of_date d
  | DTitle t ->
      t
  | DUrl url ->
      url
  | DDB dbname ->
      dbname
  | DError msg ->
      msg
  | DUuid uuid ->
      Uuidm.to_string uuid
  | _ ->
      "<" ^ (dv |> tipename) ^ ">"


let as_literal (dv : dval) : string =
  match dv with
  | DStr s ->
      "\"" ^ Unicode_string.to_string s ^ "\""
  | DChar c ->
      "'" ^ Char.to_string c ^ "'"
  | DCharacter c ->
      "'" ^ Unicode_string.Character.to_string c ^ "'"
  | _ ->
      as_string dv


let is_primitive (dv : dval) : bool =
  match dv with
  | DInt _ | DFloat _ | DBool _ | DNull | DChar _ | DCharacter _ | DStr _ ->
      true
  | _ ->
      false


let is_stringable (dv : dval) : bool =
  match dv with
  | DBlock _
  | DIncomplete
  | DError _
  | DID _
  | DDate _
  | DTitle _
  | DUrl _
  | DPassword _
  | DDB _
  | DUuid _ ->
      true
  | _ ->
      is_primitive dv


(* A simple representation, showing primitives as their expected literal
 * syntax, and odd types get type info in a readable format. Compound
 * types are listed as their type only *)
let to_simple_repr ?(open_ = "<") ?(close_ = ">") (dv : dval) : string =
  let wrap value = open_ ^ (dv |> tipename) ^ ": " ^ value ^ close_ in
  match dv with
  | dv when is_primitive dv ->
      as_literal dv
  | dv when is_stringable dv ->
      wrap (as_string dv)
  | _ ->
      open_ ^ (dv |> tipename) ^ close_


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
            DvalMap.fold o ~init:[] ~f:(fun ~key ~data l ->
                (key ^ ": " ^ recurse data) :: l )
          in
          "{ " ^ inl ^ String.concat ~sep:("," ^ inl) strs ^ nl ^ "}"
    | _ ->
        reprfn dv
  in
  inner 0 dv


(* A full representation, building on to_simple_repr, but including
 * lists and objects. *)
let rec to_repr
    ?(pp = true)
    ?(open_ = "<")
    ?(close_ = ">")
    ?(reprfn : dval -> string = to_simple_repr ~open_ ~close_)
    (dv : dval) : string =
  let rec to_repr_ (indent : int) (pp : bool) (dv : dval) : string =
    let nl = if pp then "\n" ^ String.make indent ' ' else " " in
    let inl = if pp then "\n" ^ String.make (indent + 2) ' ' else "" in
    let indent = indent + 2 in
    match dv with
    | dv when is_stringable dv ->
        reprfn dv
    | DResp (h, hdv) ->
        dhttp_to_formatted_string h ^ nl ^ to_repr_ indent pp hdv
    | DList l ->
        if List.is_empty l
        then "[]"
        else
          "[ "
          ^ inl
          ^ String.concat ~sep:", " (List.map ~f:(to_repr_ indent pp) l)
          ^ nl
          ^ "]"
    | DObj o ->
        if DvalMap.is_empty o
        then "{}"
        else
          let strs =
            DvalMap.fold o ~init:[] ~f:(fun ~key ~data l ->
                (key ^ ": " ^ to_repr_ indent pp data) :: l )
          in
          "{ " ^ inl ^ String.concat ~sep:("," ^ inl) strs ^ nl ^ "}"
    | DOption OptNothing ->
        "Nothing"
    | DOption (OptJust dv) ->
        "Just " ^ to_repr_ indent pp dv
    | DErrorRail dv ->
        "ErrorRail: " ^ to_repr_ indent pp dv
    | _ ->
        failwith ("printing an unprintable value:" ^ to_simple_repr dv)
  in
  to_repr_ 0 pp dv


(* ------------------------- *)
(* Json *)
(* ------------------------- *)
let is_json_primitive (dv : dval) : bool =
  match dv with
  | DInt _ | DFloat _ | DBool _ | DNull | DStr _ ->
      true
  (* everything else is a list, an actual object, or a wrapped object *)
  | _ ->
      false


(* The "unsafe" variations here are bad. They encode data ambiguously, and
 * though we mostly have the decoding right, it's brittle and unsafe.  This
 * should be considered append only. There's a ton of dangerous things in this,
 * and we really need to move off it, but for now we're here. Do not change
 * existing encodings - this will break everything.
 *)
let rec unsafe_dval_of_yojson_ (json : Yojson.Safe.json) : dval =
  (* sort so this isn't key-order-dependent. *)
  let json = Yojson.Safe.sort json in
  match json with
  | `Int i ->
      DInt i
  | `Float f ->
      DFloat f
  | `Bool b ->
      DBool b
  | `Null ->
      DNull
  | `String s ->
      dstr_of_string_exn s
  | `List l ->
      DList (List.map ~f:unsafe_dval_of_yojson_ l)
  | `Variant v ->
      Exception.internal "We dont use variants"
  | `Intlit v ->
      dstr_of_string_exn v
  | `Tuple v ->
      Exception.internal "We dont use tuples"
  | `Assoc [("type", `String "response"); ("value", `List [a; b])] ->
      DResp
        (Result.ok_or_failwith (dhttp_of_yojson a), unsafe_dval_of_yojson_ b)
  | `Assoc
      [ ("constructor", `String constructor)
      ; ("type", `String tipe)
      ; ("values", `List vs) ] ->
      let expectOne ~f vs =
        match vs with [v] -> f v | _ -> DObj (unsafe_dvalmap_of_yojson json)
      in
      ( match (tipe, constructor) with
      | "result", "Ok" ->
          vs
          |> expectOne ~f:(fun v -> DResult (ResOk (unsafe_dval_of_yojson_ v)))
      | "result", "Error" ->
          vs
          |> expectOne ~f:(fun v ->
                 DResult (ResError (unsafe_dval_of_yojson_ v)) )
      | _ ->
          DObj (unsafe_dvalmap_of_yojson json) )
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
        DObj (unsafe_dvalmap_of_yojson json) )
  | `Assoc [("type", `String tipe); ("value", `String v)] ->
    ( match tipe with
    | "date" ->
        DDate (Util.date_of_isostring v)
    | "id" ->
        DID (Util.uuid_of_string v)
    | "title" ->
        DTitle v
    | "url" ->
        DUrl v
    | "error" ->
        DError v
    | "char" ->
        DChar (Char.of_string v)
    | "password" ->
        v |> B64.decode |> Bytes.of_string |> DPassword
    | "datastore" ->
        DDB v
    | "uuid" ->
        DUuid (Uuidm.of_string v |> Option.value_exn)
    | "character" ->
        DCharacter (Unicode_string.Character.unsafe_of_string v)
    | _ ->
        DObj (unsafe_dvalmap_of_yojson json) )
  | `Assoc [("type", `String "option"); ("value", dv)] ->
      DOption (OptJust (unsafe_dval_of_yojson_ dv))
  | `Assoc [("type", `String "errorrail"); ("value", dv)] ->
      DErrorRail (unsafe_dval_of_yojson_ dv)
  | `Assoc _ ->
      DObj (unsafe_dvalmap_of_yojson json)


and unsafe_dvalmap_of_yojson (json : Yojson.Safe.json) : dval_map =
  match json with
  | `Assoc alist ->
      List.fold_left
        alist
        ~f:(fun m (k, v) -> DvalMap.set m k (unsafe_dval_of_yojson_ v))
        ~init:DvalMap.empty
  | _ ->
      Exception.internal "Not a json object"


let unsafe_dval_of_yojson (json : Yojson.Safe.json) : (dval, string) result =
  Result.Ok (unsafe_dval_of_yojson_ json)


let rec unsafe_dval_to_yojson ?(redact = true) (dv : dval) : Yojson.Safe.json =
  let tipe = dv |> tipe_of |> tipe_to_yojson in
  let wrap_user_type value = `Assoc [("type", tipe); ("value", value)] in
  let wrap_constructed_type cons values =
    `Assoc [("type", tipe); ("constructor", cons); ("values", `List values)]
  in
  let wrap_user_str value = wrap_user_type (`String value) in
  match dv with
  (* basic types *)
  | DInt i ->
      `Int i
  | DFloat f ->
      `Float f
  | DBool b ->
      `Bool b
  | DNull ->
      `Null
  | DStr s ->
      Unicode_string.to_yojson s
  | DList l ->
      `List (List.map l (unsafe_dval_to_yojson ~redact))
  | DObj o ->
      o
      |> DvalMap.to_alist
      |> List.map ~f:(fun (k, v) -> (k, unsafe_dval_to_yojson ~redact v))
      |> fun x -> `Assoc x
  (* opaque types *)
  | DBlock _ | DIncomplete ->
      wrap_user_type `Null
  (* user-ish types *)
  | DChar c ->
      wrap_user_str (Char.to_string c)
  | DCharacter c ->
      wrap_user_str (Unicode_string.Character.to_string c)
  | DError msg ->
      wrap_user_str msg
  | DResp (h, hdv) ->
      wrap_user_type
        (`List [dhttp_to_yojson h; unsafe_dval_to_yojson ~redact hdv])
  | DDB dbname ->
      wrap_user_str dbname
  | DID id ->
      wrap_user_str (Uuidm.to_string id)
  | DUrl url ->
      wrap_user_str url
  | DTitle title ->
      wrap_user_str title
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
        wrap_user_type (unsafe_dval_to_yojson ~redact dv) )
  | DErrorRail dv ->
      wrap_user_type (unsafe_dval_to_yojson ~redact dv)
  | DResult res ->
    ( match res with
    | ResOk dv ->
        wrap_constructed_type (`String "Ok") [unsafe_dval_to_yojson ~redact dv]
    | ResError dv ->
        wrap_constructed_type
          (`String "Error")
          [unsafe_dval_to_yojson ~redact dv] )


let parse_basic_json (str : string) : dval option =
  try
    str
    |> Yojson.Safe.from_string
    |> unsafe_dval_of_yojson_
    |> fun dv -> Some dv
  with Yojson.Json_error e -> None


let parse_literal (str : string) : dval option =
  (* str is a raw string that the user entered. It is not valid json,
   * or anything like it. We use the json parser to get values from int
   * and float literals, etc, but this is a total hack *)
  let len = String.length str in
  (* TODO: Doesn't handle nested characters. Replace with a custom parser,
     using the one in RealWorldOcaml, or just ripped out of Yojson *)
  if len > 0 && str.[0] = '\''
  then Some (DChar str.[1])
  else if len > 1 && str.[0] = '"' && str.[len - 1] = '"'
          (* It might have \n characters in it (as well as probably other
     * codes like \r or some shit that we haven't taken into account),
     * which are not valid json. So we convert them manually to
     * appropriate char sequences. *)
  then
    str
    |> String.sub ~pos:1 ~len:(len - 2)
    |> Util.string_replace "\\\"" "\""
    |> fun s -> Some (dstr_of_string_exn s)
  else if String.Caseless.equal "nothing" str
  then Some (DOption OptNothing)
  else parse_basic_json str


(* ------------------------- *)
(* Conversion Functions *)
(* ------------------------- *)
let to_char dv : string option =
  match dv with
  | DCharacter c ->
      Some (Unicode_string.Character.to_string c)
  | _ ->
      None


let to_char_deprecated dv : char option =
  match dv with DChar c -> Some c | _ -> None


let to_int dv : int option = match dv with DInt i -> Some i | _ -> None

let to_dobj_exn (pairs : (string * dval) list) : dval =
  try DObj (DvalMap.of_alist_exn pairs) with e ->
    DError "The same key occurs multiple times"


let to_string_exn dv : string =
  match dv with
  | DStr s ->
      Unicode_string.to_string s
  | _ ->
      Exception.user "expecting str" ~actual:(to_repr dv)


let to_dval_pairs_exn dv : (string * dval) list =
  match dv with
  | DObj obj ->
      obj |> DvalMap.to_alist
  | _ ->
      Exception.user "expecting str" ~actual:(to_repr dv)


let to_string_pairs_exn dv : (string * string) list =
  dv |> to_dval_pairs_exn |> List.map ~f:(fun (k, v) -> (k, to_string_exn v))


(* For putting into URLs as query params *)
let rec to_url_string_exn (dv : dval) : string =
  match dv with
  | dv when is_stringable dv ->
      as_string dv
  | DResp (_, hdv) ->
      to_url_string_exn hdv
  | DList l ->
      "[ " ^ String.concat ~sep:", " (List.map ~f:to_url_string_exn l) ^ " ]"
  | DObj o ->
      let strs =
        DvalMap.fold o ~init:[] ~f:(fun ~key ~data l ->
            (key ^ ": " ^ to_url_string_exn data) :: l )
      in
      "{ " ^ String.concat ~sep:", " strs ^ " }"
  | _ ->
      failwith "to_url_string of unurlable value"


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
  |> DvalMap.of_alist_exn
  |> fun x -> DObj x


let dval_to_query (dv : dval) : (string * string list) list =
  match dv with
  | DObj kvs ->
      kvs
      |> DvalMap.to_alist
      |> List.map ~f:(fun (k, value) ->
             match value with
             | DNull ->
                 (k, [])
             | DList l ->
                 (k, List.map ~f:to_url_string_exn l)
             | _ ->
                 (k, [to_url_string_exn value]) )
  | _ ->
      Exception.user "attempting to use non-object as query param"


let to_form_encoding (dv : dval) : string =
  dv
  |> to_string_pairs_exn
  (* TODO: forms are allowed take string lists as the value, not just strings *)
  |> List.map ~f:(fun (k, v) -> (k, [v]))
  |> Uri.encoded_of_query


let from_form_encoding (f : string) : dval =
  f |> Uri.query_of_encoded |> query_to_dval


(* ------------------------- *)
(* New Representations *)
(* ------------------------- *)
(* All the new representations. *)
let to_internal_roundtrippable_v0 dval : string =
  unsafe_dval_to_yojson ~redact:false dval |> Yojson.Safe.to_string


let of_internal_roundtrippable_json_v0 j = unsafe_dval_of_yojson j

let of_internal_roundtrippable_v0 str : dval =
  str
  |> Yojson.Safe.from_string
  |> unsafe_dval_of_yojson
  |> Result.ok_or_failwith


let to_internal_queryable_v0 dval : string =
  unsafe_dval_to_yojson ~redact:false dval |> Yojson.Safe.to_string


let of_internal_queryable_v0 str : dval =
  str
  |> Yojson.Safe.from_string
  |> unsafe_dval_of_yojson
  |> Result.ok_or_failwith


(* Utility function to handle the straightforward value-as-string conversions
 * that don't change based on formatting. Returns None if it's not a simple
 * stringable. *)
let as_simple_stringable dv : string option =
  match dv with
  | DInt i ->
      Some (string_of_int i)
  | DBool true ->
      Some "true"
  | DBool false ->
      Some "false"
  | DStr s ->
      Some (Unicode_string.to_string s)
  | DFloat f ->
      Some (string_of_float f)
  | DChar c ->
      Some (Char.to_string c)
  | DCharacter c ->
      Some (Unicode_string.Character.to_string c)
  | DNull ->
      Some "null"
  | DID id ->
      Some (Uuidm.to_string id)
  | DDate d ->
      Some (Util.isostring_of_date d)
  | DTitle t ->
      Some t
  | DUrl url ->
      Some url
  | DUuid uuid ->
      Some (Uuidm.to_string uuid)
  | _ ->
      None


let rec to_enduser_readable_text_v0 dval =
  let rec nestedreprfn dv =
    (* If nesting inside an object or a list, wrap strings in quotes *)
    match dv with
    | DStr _ | DID _ | DTitle _ | DUrl _ | DUuid _ | DCharacter _ ->
        "\"" ^ reprfn dv ^ "\""
    | _ ->
        reprfn dv
  and reprfn dv =
    match as_simple_stringable dv with
    | Some str ->
        str
    | None ->
      ( match dv with
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
          "ErrorRail: " ^ reprfn d
      | DResp (dh, dv) ->
          dhttp_to_formatted_string dh ^ "\n" ^ nestedreprfn dv ^ ""
      | DResult (ResOk d) ->
          reprfn d
      | DResult (ResError d) ->
          "Error: " ^ reprfn d
      | DOption (OptJust d) ->
          "Just " ^ reprfn d
      | DOption OptNothing ->
          "Nothing"
      | _ ->
          Exception.internal "Stringable not handled" )
  in
  reprfn dval


let to_enduser_readable_html_v0 dv = to_enduser_readable_text_v0 dv

let rec to_developer_repr_v0 (dv : dval) : string =
  let rec to_repr_ (indent : int) (dv : dval) : string =
    let nl = "\n" ^ String.make indent ' ' in
    let inl = "\n" ^ String.make (indent + 2) ' ' in
    let indent = indent + 2 in
    match dv with
    | dv when is_stringable dv ->
        to_simple_repr dv ~open_:"<" ~close_:">"
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
            DvalMap.fold o ~init:[] ~f:(fun ~key ~data l ->
                (key ^ ": " ^ to_repr_ indent data) :: l )
          in
          "{ " ^ inl ^ String.concat ~sep:("," ^ inl) strs ^ nl ^ "}"
    | DOption OptNothing ->
        "Nothing"
    | DOption (OptJust dv) ->
        "Just " ^ to_repr_ indent dv
    | DErrorRail dv ->
        "ErrorRail: " ^ to_repr_ indent dv
    | _ ->
        failwith ("printing an unprintable value:" ^ to_simple_repr dv)
  in
  to_repr_ 0 dv


let to_pretty_machine_json_v0 _dval =
  `Null |> Yojson.Basic.pretty_to_string ~std:false


let of_unknown_json_v0 str =
  try str |> Yojson.Safe.from_string |> unsafe_dval_of_yojson_ with e ->
    Exception.user ~actual:str ("Invalid json" ^ Exception.to_string e)


let rec show dv =
  match as_simple_stringable dv with
  | Some str ->
      "<" ^ pretty_tipename dv ^ ": " ^ str ^ ">"
  | None ->
    ( match dv with
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
    | _ ->
        Exception.internal "Stringable not handled" )


(* ------------------------- *)
(* Hashes *)
(* ------------------------- *)
let rec to_hashable_repr (dv : dval) : string =
  match dv with
  | DDB dbname ->
      "<db: " ^ dbname ^ ">"
  | dv when is_stringable dv ->
      to_simple_repr dv
  | _ ->
      to_repr ~reprfn:to_hashable_repr dv


(* Originally to prevent storing sensitive data to disk, this also reduces the
 * size of the data stored by only storing a hash *)
let hash (arglist : dval list) : string =
  arglist |> List.map ~f:to_hashable_repr |> String.concat |> Util.hash


(* ------------------------- *)
(* Old representations, here for testing *)
(* ------------------------- *)

let old_to_repr s = to_repr s
