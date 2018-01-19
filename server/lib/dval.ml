open Core
open Types
open Types.RuntimeT

let repr_of_dhttp (d: dhttp) : string =
  match d with
  | Redirect url -> "302 " ^ url
  | Response (c, hs) -> (string_of_int c) ^ " (TODO: some headers)"


(* ------------------------- *)
(* Types *)
(* ------------------------- *)
let tipe_to_string t : string =
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
  | TResp -> "Response"
  | TDB -> "Datastore"
  | TID -> "ID"
  | TDate -> "Date"
  | TTitle -> "Title"
  | TUrl -> "Url"
  | TForeignKey s -> s

let tipe_of_string str : tipe =
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
  | "response" -> TResp
  | "datastore" -> TDB
  | "id" -> TID
  | "date" -> TDate
  | "title" -> TTitle
  | "url" -> TUrl
  | other -> TForeignKey other


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

let string_of_date (d: Time.t) : string =
  Time.format d "%Y-%m-%d %H:%M:%S" Time.Zone.utc

let date_of_string (str: string) : Time.t =
  Time.parse str "%Y-%m-%d %H:%M:%S" Time.Zone.utc

let to_simple_repr (open_: string) (close_: string) (dv : dval) : string =
  let wrap value = open_ ^ (dv |> tipename) ^ ": " ^ value ^ close_ in
  let wrap_int i = wrap (string_of_int i) in
  let wrap_string str = wrap ("\"" ^ str ^ "\"") in
  match dv with
  | DInt i -> string_of_int i
  | DBool true -> "true"
  | DBool false -> "false"
  | DStr s -> s
  | DFloat f -> string_of_float f
  | DChar c -> Char.to_string c
  | DNull -> "null"
  | DID id -> wrap_int id
  | DDate d -> d |> string_of_date |> wrap_string
  | DTitle t -> wrap_string t
  | DUrl url -> wrap_string url
  | DDB db -> wrap db.display_name
  | _ -> open_
         ^ (dv |> tipename)
         ^ close_

let to_repr ?(pp : bool = true) (dv : dval) : string =
  let rec to_repr_ (indent: int) (pp : bool) (dv : dval) : string =
    let nl = if pp then "\n" ^ (String.make indent ' ') else " " in
    let inl = if pp then "\n" ^ (String.make (indent + 2) ' ') else "" in
    let indent = indent + 2 in
    match dv with
    | DInt _ | DFloat _ | DBool _ | DNull
    | DBlock _ | DIncomplete
    | DID _ | DDate _ | DTitle _ | DUrl _
      ->
      to_simple_repr "<" ">" dv

    | DStr s -> "\"" ^ s ^ "\""
    | DChar c -> "'" ^ (Char.to_string c) ^ "'"
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
    | DDB db -> "<db>"
    in to_repr_ 0 pp dv

(* If someone returns a string or int, that's probably a web page. If
 * someone returns something else, show the structure so they can figure
 * out how to get it into a string. *)
let to_human_repr (dv: dval) : string =
  match dv with
  | DStr str -> str
  | DInt i ->  string_of_int i
  | _ -> to_repr dv




let to_error_repr (dv : dval) : string =
  (to_repr dv) ^ " (" ^ (tipename dv) ^ ")"

let pp = Log.pp ~f:to_repr
let pP = Log.pP ~f:to_repr


let rec to_url_string (dv : dval) : string =
  match dv with
  | DInt _ | DFloat _ | DBool _ | DNull
  | DChar _ | DStr _
  | DBlock _ | DIncomplete
  | DDB _
  | DID _ | DDate _ | DTitle _ | DUrl _
    ->
    to_simple_repr "" "" dv

  | DResp (_, hdv) -> to_url_string hdv
  | DList l ->
    "[ " ^ ( String.concat ~sep:", " (List.map ~f:to_url_string l)) ^ " ]"
  | DObj o ->
    let strs = DvalMap.fold o
        ~init:[]
        ~f:(fun ~key ~data l -> (key ^ ": " ^ to_url_string data) :: l) in
    "{ " ^ (String.concat ~sep:", " strs) ^ " }"




(* ------------------------- *)
(* Clonversion Functions *)
(* ------------------------- *)
let to_char dv =
  match dv with
  | DChar c -> Some c
  | _ -> None

let to_int dv =
  match dv with
  | DInt i -> Some i
  | _ -> None

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
  | `String s -> DStr s
  | `Bool b -> DBool b
  | `Float f -> DFloat f
  | `Null -> DNull
  | `Assoc [("type", `String "date"); ("value", `String v)] ->
    DDate (date_of_string v)
  | `Assoc [("type", `String "id"); ("value", `Int v)] -> DID v
  | `Assoc [("type", `String "title"); ("value", `String v)] -> DTitle v
  | `Assoc [("type", `String "url"); ("value", `String v)] -> DUrl v
  (* DB doesnt make sense *)
  (* response is a weird format, dunno why *)
  | `Assoc alist -> DObj (List.fold_left
                        alist
                        ~f:(fun m (k,v) -> DvalMap.add m k (dval_of_yojson_ v))
                        ~init:DvalMap.empty)
  | `List l -> DList (List.map ~f:dval_of_yojson_ l)
  | j -> DStr ( "<todo, incomplete conversion: "
                ^ (Yojson.Safe.to_string j)
                ^ ">")

let dval_of_yojson (json : Yojson.Safe.json) : (dval, string) result =
  Result.Ok (dval_of_yojson_ json)

let rec dval_to_yojson (dv : dval) : Yojson.Safe.json =
  let tipe = dv |> tipe_of |> tipe_to_yojson in
  let wrap_user_type value = `Assoc [ ("type", tipe)
                                    ; ("value", value)] in
  let wrap_user_int value = wrap_user_type (`Int value) in
  let wrap_user_str value = wrap_user_type (`String value) in
  match dv with
  | DInt i -> `Int i
  | DFloat f -> `Float f
  | DBool b -> `Bool b
  | DNull -> `Null
  | DChar c -> `String (Char.to_string c)
  | DStr s -> `String s
  | DList l -> `List (List.map l dval_to_yojson)
  | DObj o -> o
              |> DvalMap.to_alist
              |> List.map ~f:(fun (k,v) -> (k, dval_to_yojson v))
              |> (fun a -> `Assoc a)

  | DBlock _ | DIncomplete ->
    `String ("<" ^ (tipename dv) ^ ">")

  | DResp (h, hdv) ->
    wrap_user_type (`List [ dhttp_to_yojson h ; dval_to_yojson hdv])

  | DDB db -> wrap_user_str db.display_name
  | DID id -> wrap_user_int id
  | DUrl url -> wrap_user_str url
  | DTitle title -> wrap_user_str title
  | DDate date -> wrap_user_type (`String (string_of_date date))

let dval_to_json_string (v: dval) : string =
  v |> dval_to_yojson |> Yojson.Safe.to_string

let dvalmap_to_string (m:dval_map) : string =
  DObj m |> dval_to_yojson |> Yojson.Safe.to_string

let dvallist_to_string (l:dval list) : string =
  DList l |> dval_to_yojson |> Yojson.Safe.to_string



(* ------------------------- *)
(* Parsing *)
(* ------------------------- *)
let parse (str : string) : dval =
  (* TODO: Doesn't handle characters. Replace with a custom parser,
     using the one in RealWorldOcaml, or just ripped out of Yojson *)
  if String.length str > 0 && String.get str 0 = '\''
  then DChar (String.get str 1)
  else
    try
      str |> Yojson.Safe.from_string |> dval_of_yojson_
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




