open Core
open Types
open Types.RuntimeT

let repr_of_dhttp (d: dhttp) : string =
  match d with
  | Redirect url -> "302 " ^ url
  | Response (c, hs) ->
    let string_of_headers hs =
      hs
      |> List.map
        ~f:(fun (k, v) -> k ^ ": " ^ v)
      |> String.concat ~sep:","
      |> fun s -> "{ " ^ s ^ " }"
    in
    (string_of_int c) ^ " " ^ (string_of_headers hs)


(* ------------------------- *)
(* Types *)
(* ------------------------- *)
let rec tipe_to_string t : string =
  match t with
  | TAny -> "Any"
  | TInt -> "Int"
  | TFloat -> "Float"
  | TBool -> "Bool"
  | TNull -> "Nothing"
  | TChar -> "Char"
  | TStr -> "Str"
  | TList -> "List"
  | TObj -> "Obj"
  | TBlock -> "Block"
  | TIncomplete -> "Incomplete"
  | TError -> "Error"
  | TResp -> "Response"
  | TDB -> "Datastore"
  | TID -> "ID"
  | TDate -> "Date"
  | TTitle -> "Title"
  | TUrl -> "Url"
  | TBelongsTo s -> s
  | THasMany s -> "[" ^ s ^ "]"
  | TDbList tipe -> "[" ^ (tipe_to_string tipe) ^ "]"

let rec tipe_of_string str : tipe =
  match String.lowercase str with
  | "any" -> TAny
  | "int" -> TInt
  | "integer" -> TInt
  | "float" -> TFloat
  | "bool" -> TBool
  | "nothing" -> TNull
  | "char" -> TChar
  | "str" -> TStr
  | "string" -> TStr
  | "list" -> TList
  | "obj" -> TObj
  | "block" -> TBlock
  | "incomplete" -> TIncomplete
  | "error" -> TError
  | "response" -> TResp
  | "datastore" -> TDB
  | "id" -> TID
  | "date" -> TDate
  | "title" -> TTitle
  | "url" -> TUrl
  | other ->
    if String.is_prefix other "["  && String.is_suffix other "]"
    then
      other
      |> fun s -> String.drop_prefix s 1
      |> fun s -> String.drop_suffix s 1
      |> parse_list_tipe
    else
      TBelongsTo other
and parse_list_tipe (list_tipe : string) : tipe =
  match list_tipe with
  | "str" -> TDbList TStr
  | "string" -> TDbList TStr
  | "int" -> TDbList TInt
  | "integer" -> TDbList TInt
  | "float" -> TDbList TFloat
  | "bool" -> TDbList TBool
  | "boolean" -> TDbList TBool
  | "obj" -> Exception.internal "todo"
  | "block" -> Exception.internal "todo"
  | "incomplete" -> Exception.internal "todo"
  | "error" -> Exception.internal "todo"
  | "response" -> Exception.internal "todo"
  | "datastore" -> Exception.internal "todo"
  | "id" -> Exception.internal "todo"
  | "date" -> Exception.internal "todo"
  | "title" -> Exception.internal "todo"
  | "url" -> Exception.internal "todo"
  | table -> THasMany table

let tipe_of (dv : dval) : tipe =
  match dv with
  | DInt _ -> TInt
  | DFloat _ -> TFloat
  | DBool _ -> TBool
  | DNull -> TNull
  | DChar _ -> TChar
  | DStr _ -> TStr
  | DList _ -> TList
  | DObj _ -> TObj
  | DBlock _ -> TBlock
  | DError _ -> TError
  | DIncomplete -> TIncomplete
  | DResp _ -> TResp
  | DDB _ -> TDB
  | DID _ -> TID
  | DDate _ -> TDate
  | DTitle _ -> TTitle
  | DUrl _ -> TUrl


let tipename (dv: dval) : string =
  dv |> tipe_of |> tipe_to_string |> String.lowercase

let rec equal_dval (a: dval) (b: dval) =
  match (a,b) with
  | DInt i1, DInt i2 -> i1 = i2
  | DFloat f1, DFloat f2 -> f1 = f2
  | DBool b1, DBool b2 -> b1 = b2
  | DNull, DNull -> true
  | DChar c1, DChar c2 -> c1 = c2
  | DStr s1, DStr s2 -> s1 = s2
  | DList l1, DList l2 -> List.equal ~equal:equal_dval l1 l2
  | DObj o1, DObj o2 -> DvalMap.equal equal_dval o1 o2
  | DIncomplete, DIncomplete -> true
  | DBlock _, _ -> false
  | DResp (h1, dv1), DResp (h2, dv2) -> h1 = h2 && (equal_dval dv1 dv2)
  | DDB db1, DDB db2 -> db1 = db2
  | DID d1, DID d2 -> d1 = d2
  | DDate d1, DDate d2 -> d1 = d2
  | DTitle t1, DTitle t2 -> t1 = t2
  | DUrl u1, DUrl u2 -> u1 = u2
  | _, _ -> false



(* ------------------------- *)
(* Representation *)
(* ------------------------- *)

let isostring_of_date (d: Time.t) : string =
  (* for conduit tests. May do something different later *)
  Time.format d "%FT%TZ" Time.Zone.utc

let date_of_isostring (str: string) : Time.t =
  Time.parse str "%FT%TZ" Time.Zone.utc

let sqlstring_of_date (d: Time.t) : string =
  Time.format d "%Y-%m-%d %H:%M:%S" Time.Zone.utc

let date_of_sqlstring (str: string) : Time.t =
  Time.parse str "%Y-%m-%d %H:%M:%S" Time.Zone.utc

(* Returns the string within string-ish values, without adornment. *)
let as_string (dv : dval) : string =
  match dv with
  | DInt i -> string_of_int i
  | DBool true -> "true"
  | DBool false -> "false"
  | DStr s -> s
  | DFloat f -> string_of_float f
  | DChar c -> Char.to_string c
  | DNull -> "null"
  | DID id -> Uuid.to_string id
  | DDate d -> d |> isostring_of_date
  | DTitle t -> t
  | DUrl url -> url
  | DDB db -> db.display_name
  | DError msg -> msg
  | _ -> "<" ^ (dv |> tipename) ^ ">"

let as_literal (dv : dval) : string =
  match dv with
  | DStr s -> "\"" ^ s ^ "\""
  | DChar _ -> "'" ^ as_string dv ^ "'"
  | _ -> as_string dv

let is_primitive (dv : dval) : bool =
  match dv with
  | DInt _ | DFloat _ | DBool _ | DNull | DStr _ -> true
  |  _ -> false

let is_stringable (dv : dval) : bool =
  match dv with
  | DBlock _ | DIncomplete | DError _
  | DID _ | DDate _ | DTitle _ | DUrl _
  | DDB _ -> true
  |  _ -> is_primitive dv

(* A simple representation, showing primitives as their expected literal
 * syntax, and odd types get type info in a readable format. Compund
 * types are listed as their type only *)
let to_simple_repr ?(open_="<") ?(close_=">") (dv : dval) : string =
  let wrap value = open_ ^ (dv |> tipename) ^ ": " ^ value ^ close_ in
  match dv with
  | dv when is_primitive dv -> as_literal dv
  | dv when is_stringable dv -> wrap (as_string dv)
  | _ -> open_ ^ (dv |> tipename) ^ close_

(* A full representation, building on to_simple_repr, but including
 * lists and objects. *)
let rec to_repr ?(pp=true) ?(open_="<") ?(close_=">")
    ?(reprfn:(dval -> string)=to_simple_repr ~open_ ~close_)
    (dv : dval) : string =
  let rec to_repr_ (indent: int) (pp : bool) (dv : dval) : string =
    let nl = if pp then "\n" ^ (String.make indent ' ') else " " in
    let inl = if pp then "\n" ^ (String.make (indent + 2) ' ') else "" in
    let indent = indent + 2 in
    match dv with
    | dv when is_stringable dv -> reprfn dv
    | DResp (h, hdv) -> (repr_of_dhttp h) ^ nl ^ (to_repr_ indent pp hdv)
    | DList l ->
        if List.is_empty l
        then "[]"
        else
          "[ " ^ inl ^
          (String.concat ~sep:", " (List.map ~f:(to_repr_ indent pp) l))
          ^ nl ^ "]"
    | DObj o ->
      if DvalMap.is_empty o
      then "{}"
      else
          let strs = DvalMap.fold o
            ~init:[]
            ~f:(fun ~key ~data l -> (key ^ ": " ^ (to_repr_ indent pp data)) :: l) in
          "{ " ^ inl ^
          (String.concat ~sep:("," ^ inl) strs)
          ^ nl ^ "}"
    | _ -> failwith "should never happen"
    in to_repr_ 0 pp dv

(* For livevalue representation in the frontend *)
let rec to_livevalue_repr (dv : dval) : string =
  match dv with
  | dv when is_primitive dv -> as_literal dv
  | dv when is_stringable dv -> as_string dv
  | _ -> to_repr ~reprfn:to_livevalue_repr dv


(* Not for external consumption *)
let rec to_internal_repr (dv : dval) : string =
  match dv with
  | DDB db -> "<db: " ^ db.display_name ^ ">"
  | dv when is_stringable dv -> to_simple_repr dv
  | _ -> to_repr ~reprfn:to_internal_repr dv

(* If someone returns a string or int, that's probably a web page. If
 * someone returns something else, show the structure so they can figure
 * out how to get it into a string. *)
let rec to_human_repr (dv: dval) : string =
  match dv with
  | dv when is_stringable dv -> as_string dv
  (* contents of lists and objs should still be quoted *)
  | _ -> to_repr dv

let pp = Log.debug ~f:to_repr
let pP = Log.debuG ~f:to_repr

(* For putting into URLs as query params *)
let rec to_url_string (dv : dval) : string =
  match dv with
  | dv when is_stringable dv -> as_string dv

  | DResp (_, hdv) -> to_url_string hdv
  | DList l ->
    "[ " ^ ( String.concat ~sep:", " (List.map ~f:to_url_string l)) ^ " ]"
  | DObj o ->
    let strs = DvalMap.fold o
        ~init:[]
        ~f:(fun ~key ~data l -> (key ^ ": " ^ to_url_string data) :: l) in
    "{ " ^ (String.concat ~sep:", " strs) ^ " }"
  | _ -> failwith "should never happen"




(* ------------------------- *)
(* Conversion Functions *)
(* ------------------------- *)
let to_char dv : char option =
  match dv with
  | DChar c -> Some c
  | _ -> None

let to_int dv : int option =
  match dv with
  | DInt i -> Some i
  | _ -> None

let to_string_exn dv : string =
  match dv with
  | DStr s -> s
  | _ -> Exception.user "expecting str" ~actual:(to_repr dv)

let to_string_pairs dv : (string * string) list =
  match dv with
  | DObj obj ->
      obj
      |> DvalMap.to_alist
      |> List.map ~f:(fun (k,v) -> (k, to_string_exn v))
  | _ -> Exception.user "expecting str" ~actual:(to_repr dv)

let to_dobj (pairs: (string*dval) list) : dval =
  DObj (DvalMap.of_alist_exn pairs)

(* ------------------------- *)
(* Obj Functions *)
(* ------------------------- *)
let obj_merge (l: dval) (r: dval) : dval =
  match l, r with
  | DObj l, DObj r -> DObj (Util.merge_left l r)
  | DNull, DObj r -> DObj r
  | DObj l, DNull -> DObj l
  | _ -> Exception.user "was expecting objs"

let empty_dobj : dval =
  DObj (DvalMap.empty)

let is_obj (dv : dval) : bool =
  match dv with
  | DObj _ -> true
  | _ -> false


(* ------------------------- *)
(* JSON *)
(* ------------------------- *)

let tipe_to_yojson (t: tipe) : Yojson.Safe.json =
  `String (t |> tipe_to_string |> String.lowercase)

let rec dval_of_yojson_ (json : Yojson.Safe.json) : dval =
  match json with
  | `Int i -> DInt i
  | `Float f -> DFloat f
  | `Bool b -> DBool b
  | `Null -> DNull
  | `String s -> DStr s
  | `List l -> DList (List.map ~f:dval_of_yojson_ l)
  | `Variant v -> Exception.client "We dont use variants"
  | `Intlit v -> Exception.client "We dont use intlits"
  | `Tuple v -> Exception.client "We dont use tuples"
  | `Assoc [("type", `String "resp"); ("value", `List [a;b])] ->
    DResp (Result.ok_or_failwith (dhttp_of_yojson a), dval_of_yojson_ b)
  | `Assoc [("type", `String tipe); ("value", `String v)] ->
    (match tipe with
    | "date" -> DDate (date_of_isostring v)
    | "id" -> DID (Uuid.of_string v)
    | "title" -> DTitle v
    | "url" -> DUrl v
    | "error" -> DError v
    | "incomplete" -> DIncomplete
    | "char" -> DChar (Char.of_string v)
    | "db" -> Exception.client "Can't deserialize DBs"
    | "block" -> Exception.client "Can't deserialize blocks"
    | _ -> Exception.client ("Can't deserialize type: " ^ tipe)
    )
  | `Assoc alist ->
       DObj (List.fold_left alist
               ~f:(fun m (k,v) -> DvalMap.set m k (dval_of_yojson_ v))
               ~init:DvalMap.empty)

let dval_of_yojson (json : Yojson.Safe.json) : (dval, string) result =
  Result.Ok (dval_of_yojson_ json)

let rec dval_to_yojson ?(livevalue=false) (dv : dval) : Yojson.Safe.json =
  let tipe = dv |> tipe_of |> tipe_to_yojson in
  let wrap_user_type value =
    if livevalue
    then value
    else
      `Assoc [ ("type", tipe)
             ; ("value", value)]
  in
  let wrap_user_str value = wrap_user_type (`String value) in
  match dv with
  (* basic types *)
  | DInt i -> `Int i
  | DFloat f -> `Float f
  | DBool b -> `Bool b
  | DNull -> `Null
  | DStr s -> `String s
  | DList l -> `List (List.map l dval_to_yojson)
  | DObj o -> o
              |> DvalMap.to_alist
              |> List.map ~f:(fun (k,v) -> (k, dval_to_yojson v))
              |> (fun a -> `Assoc a)

  (* opaque types *)
  | DBlock _ | DIncomplete ->
    wrap_user_type `Null

  (* user-ish types *)
  | DChar c -> wrap_user_str (Char.to_string c)
  | DError msg -> wrap_user_str msg

  | DResp (h, hdv) ->
    wrap_user_type (`List [ dhttp_to_yojson h ; dval_to_yojson hdv])

  | DDB db -> wrap_user_str db.display_name
  | DID id -> wrap_user_str (Uuid.to_string id)
  | DUrl url -> wrap_user_str url
  | DTitle title -> wrap_user_str title
  | DDate date -> wrap_user_str (isostring_of_date date)

let dval_to_json_string (v: dval) : string =
  v |> dval_to_yojson |> Yojson.Safe.to_string

let dval_to_pretty_json_string (v: dval) : string =
  v |> dval_to_yojson |> Yojson.Safe.pretty_to_string

let dvalmap_to_string (m:dval_map) : string =
  DObj m |> dval_to_yojson |> Yojson.Safe.to_string

let dvallist_to_string (l:dval list) : string =
  DList l |> dval_to_yojson |> Yojson.Safe.to_string



(* ------------------------- *)
(* Parsing *)
(* ------------------------- *)
let parse (str : string) : dval =
  (* str is a raw string that the user entered. It is not valid json,
   * or anything like it. We use the json parser to get values from int
   * and float literals, etc, but this is a total hack *)
  let len = String.length str in
  (* TODO: Doesn't handle nested characters. Replace with a custom parser,
     using the one in RealWorldOcaml, or just ripped out of Yojson *)
  if len > 0 && String.get str 0 = '\''
  then DChar (String.get str 1)
  else if len > 1
    && String.get str 0 = '"'
    && String.get str (len - 1) = '"'
    (* It might have \n characters in it (as well as probably other
     * codes like \r or some shit that we haven't taken into account),
     * which are not valid json. So we convert them manually to
     * appropriate char sequences. *)
  then str
       |> String.sub ~pos:1 ~len:(len - 2)
       |> Util.string_replace "\\\"" "\""
       |> fun s -> DStr s
  else
    try
      (* TODO: doesn't handle nested sequences at all unless they're
       * valid json *)
      str
      |> Yojson.Safe.from_string
      |> dval_of_yojson_
    with Yojson.Json_error e ->
      Exception.user ~actual:str ("Not a valid value: '" ^ str ^ "'")

let query_to_dval (query: (string * string list) list) : dval =
  query
  |> List.map ~f:(fun (key,vals) ->
                   let dval =
                     match vals with
                     | [] -> DNull
                     | [v] -> if v = "" then DNull else DStr v
                     | vals -> DList (List.map ~f:(fun x -> DStr x) vals)
                   in (key, dval))
  |> DvalMap.of_alist_exn
  |> fun x -> DObj x

let dval_to_query (dv: dval) : ((string * string list) list) =
  match dv with
  | DObj kvs ->
      kvs
      |> DvalMap.to_alist
      |> List.map ~f:(fun (k,value) ->
                       match value with
                       | DNull -> (k,[])
                       | DList l -> (k, List.map ~f:to_url_string l)
                       | _ -> (k, [to_url_string value]))
  | _ -> Exception.user "attempting to use non-object as query param"


let to_form_encoding (dv: dval) : string =
  dv
  |> to_string_pairs
  (* TODO: forms are allowed take string lists as the value, not just strings *)
  |> List.map ~f:(fun (k,v) -> (k, [v]))
  |> Uri.encoded_of_query

let from_form_encoding (f: string) : dval =
  f |> Uri.query_of_encoded |> query_to_dval



