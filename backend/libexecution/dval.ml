open Core_kernel
open Types
open Types.RuntimeT

let dstr_of_string (s : string) : dval option =
  (* the decoder has mutable state *)
  let decoder = Uutf.decoder ~encoding:`UTF_8 (`String s) in
  let rec validate_string () =
    match Uutf.decode decoder with
    | `Uchar ch when ch = Uchar.min ->
        `Err (* U+0000 is rejected by postgres *)
    | `Uchar _ ->
        validate_string ()
    | `End ->
        `Ok
    | `Await ->
        validate_string ()
    | `Malformed _ ->
        `Err
  in
  match validate_string () with `Ok -> Some (DStr s) | `Err -> None


let dstr_of_string_exn (s : string) : dval =
  s |> dstr_of_string |> Option.value_exn ~message:("Invalid UTF-8 string:" ^ s)


let repr_of_dhttp (d : dhttp) : string =
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


(* Users should not be aware of this *)

let tipename (dv : dval) : string =
  dv |> tipe_of |> tipe_to_string |> String.lowercase


(* ------------------------- *)
(* Representation *)
(* ------------------------- *)

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
      s
  | DFloat f ->
      string_of_float f
  | DChar c ->
      Char.to_string c
  | DCharacter c ->
      c
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
      "\"" ^ s ^ "\""
  | DChar _ ->
      "'" ^ as_string dv ^ "'"
  | DCharacter _ ->
      "'" ^ as_string dv ^ "'"
  | _ ->
      as_string dv


let is_primitive (dv : dval) : bool =
  match dv with
  | DInt _ | DFloat _ | DBool _ | DNull | DChar _ | DCharacter _| DStr _ ->
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
 * syntax, and odd types get type info in a readable format. Compund
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
        repr_of_dhttp h ^ nl ^ to_repr_ indent pp hdv
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


(* Not for external consumption *)
let rec to_internal_repr (dv : dval) : string =
  match dv with
  | DDB dbname ->
      "<db: " ^ dbname ^ ">"
  | dv when is_stringable dv ->
      to_simple_repr dv
  | _ ->
      to_repr ~reprfn:to_internal_repr dv


(* If someone returns a string or int, that's probably a web page. If
 * someone returns something else, show the structure so they can figure
 * out how to get it into a string. *)
let rec to_human_repr (dv : dval) : string =
  match dv with
  | dv when is_stringable dv ->
      as_string dv
  (* contents of lists and objs should still be quoted *)
  | _ ->
      to_repr dv


(* For putting into URLs as query params *)
let rec to_url_string (dv : dval) : string =
  match dv with
  | dv when is_stringable dv ->
      as_string dv
  | DResp (_, hdv) ->
      to_url_string hdv
  | DList l ->
      "[ " ^ String.concat ~sep:", " (List.map ~f:to_url_string l) ^ " ]"
  | DObj o ->
      let strs =
        DvalMap.fold o ~init:[] ~f:(fun ~key ~data l ->
            (key ^ ": " ^ to_url_string data) :: l )
      in
      "{ " ^ String.concat ~sep:", " strs ^ " }"
  | _ ->
      failwith "to_url_string of unurlable value"


(* ------------------------- *)
(* Conversion Functions *)
(* ------------------------- *)
let to_char dv : string option =
  match dv with
    DCharacter c ->
    if (1 = (Uuseg_string.fold_utf_8 `Grapheme_cluster (fun acc _ -> acc + 1) 0 c))
    then Some c
    else None
  | _ -> None

let to_char_deprecated dv : char option
 = match dv with DChar c -> Some c | _ -> None

let to_int dv : int option = match dv with DInt i -> Some i | _ -> None

let to_string_exn dv : string =
  match dv with
  | DStr s ->
      s
  | _ ->
      Exception.user "expecting str" ~actual:(to_repr dv)


let to_string_pairs dv : (string * string) list =
  match dv with
  | DObj obj ->
      obj
      |> DvalMap.to_alist
      |> List.map ~f:(fun (k, v) -> (k, to_string_exn v))
  | _ ->
      Exception.user "expecting str" ~actual:(to_repr dv)


let to_dobj (pairs : (string * dval) list) : dval =
  try DObj (DvalMap.of_alist_exn pairs) with e ->
    DError "The same key occurs multiple times"


let is_errorrail e = match e with DErrorRail _ -> true | _ -> false

let unwrap_from_errorrail (dv : dval) =
  match dv with DErrorRail dv -> dv | other -> other


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
(* JSON *)
(* ------------------------- *)

let tipe_to_yojson (t : tipe) : Yojson.Safe.json =
  `String (t |> tipe_to_string |> String.lowercase)


let tipe_of_yojson (json : Yojson.Safe.json) =
  match json with
  | `String s ->
      Ok (s |> String.lowercase |> tipe_of_string)
  | _ ->
      Exception.user "Invalid tipe"


(* The "unsafe" variations here are bad. They encode data ambiguously, and
 * though we mostly have the decoding right, it's brittle and unsafe.
 *
 * The first fix is to use derived encoders, and use those anywhere we want to
 * roundtrip data. We can't use them for user_db because of queries, but
 * anywhere else should be fine.
 *
 * The only fit-for-purpose use of the unsafe json encoders/decoders is for
 * showing to users. However, even then our random types and their weird
 * encoding are a bad fit. We need to fix our type-system. *)

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
        DCharacter v
    | "password" ->
        v |> B64.decode |> Bytes.of_string |> DPassword
    | "datastore" ->
        DDB v
    | "uuid" ->
        DUuid (Uuidm.of_string v |> Option.value_exn)
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


let rec unsafe_dvalmap_to_yojson ?(redact = true) (dvalmap : dval_map) :
    Yojson.Safe.json =
  dvalmap
  |> DvalMap.to_alist
  |> List.map ~f:(fun (k, v) -> (k, unsafe_dval_to_yojson ~redact v))
  |> fun a -> `Assoc a


and unsafe_dval_to_yojson ?(redact = true) (dv : dval) : Yojson.Safe.json =
  let tipe = dv |> tipe_of |> tipe_to_yojson in
  let wrap_user_type value = `Assoc [("type", tipe); ("value", value)] in
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
      `String s
  | DList l ->
      `List (List.map l (unsafe_dval_to_yojson ~redact))
  | DObj o ->
      unsafe_dvalmap_to_yojson ~redact o
  (* opaque types *)
  | DBlock _ | DIncomplete ->
      wrap_user_type `Null
  (* user-ish types *)
  | DChar c ->
      wrap_user_str (Char.to_string c)
  | DCharacter c ->
      wrap_user_str c
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


let is_json_primitive (dv : dval) : bool =
  match dv with
  | DInt _ | DFloat _ | DBool _ | DNull | DStr _ ->
      true
  (* everything else is a list, an actual object, or a wrapped object *)
  | _ ->
      false


let unsafe_dval_to_json_string ?(redact = true) (v : dval) : string =
  v |> unsafe_dval_to_yojson ~redact |> Yojson.Safe.to_string


let unsafe_dval_of_json_string (s : string) : dval =
  s
  |> Yojson.Safe.from_string
  |> unsafe_dval_of_yojson
  |> Result.ok_or_failwith


let unsafe_dval_to_pretty_json_string ?(redact = true) (v : dval) : string =
  v |> unsafe_dval_to_yojson ~redact |> Yojson.Safe.pretty_to_string


let unsafe_dvalmap_to_string ?(redact = true) (m : dval_map) : string =
  DObj m |> unsafe_dval_to_yojson ~redact |> Yojson.Safe.to_string


(* ------------------------- *)
(* Parsing *)
(* ------------------------- *)
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
                 (k, List.map ~f:to_url_string l)
             | _ ->
                 (k, [to_url_string value]) )
  | _ ->
      Exception.user "attempting to use non-object as query param"


let to_form_encoding (dv : dval) : string =
  dv
  |> to_string_pairs
  (* TODO: forms are allowed take string lists as the value, not just strings *)
  |> List.map ~f:(fun (k, v) -> (k, [v]))
  |> Uri.encoded_of_query


let from_form_encoding (f : string) : dval =
  f |> Uri.query_of_encoded |> query_to_dval


let exception_to_dval exc = DError (Exception.to_string exc)

(* Originally to prevent storing sensitive data to disk, this also reduces the size of the data stored by only storing a hash *)
let hash (arglist : dval list) : string =
  arglist |> List.map ~f:to_internal_repr |> String.concat |> Util.hash
