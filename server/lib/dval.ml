open Core
open Types
open Types.RuntimeT

let repr_of_dhttp (d: dhttp) : string =
  match d with
  | Redirect url -> "302 " ^ url
  | Response c -> string_of_int c
(* ------------------------- *)
(* Values *)
(* ------------------------- *)

let rec to_repr_ (indent: int) (pp : bool) (dv : dval) : string =
  let nl = if pp then "\n" ^ (String.make indent ' ') else " " in
  let inl = if pp then "\n" ^ (String.make (indent + 2) ' ') else "" in
  let indent = indent + 2 in
  match dv with
  | DInt i -> string_of_int i
  | DDB db -> "<db>"
  | DBool true -> "true"
  | DBool false -> "false"
  | DStr s -> "\"" ^ s ^ "\""
  | DFloat f -> string_of_float f
  | DChar c -> "'" ^ (Char.to_string c) ^ "'"
  | DBlock _ -> "<block>"
  | DIncomplete -> "<incomplete>"
  | DNull -> "null"
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

let to_repr ?(pp : bool = true) (dv : dval) : string =
  to_repr_ 0 pp dv

let to_comparable_repr (dvm : dval_map) : string =
  Map.to_alist ~key_order:`Increasing dvm
  |> List.map ~f:(fun (s, dv) -> s ^ (to_repr dv))
  |> List.fold ~f:(fun a b -> a ^ b) ~init:""

let dummy_compare dv1 dv2 =
  String.compare (to_repr dv1) (to_repr dv2)

let rec to_url_string (dv : dval) : string =
  match dv with
  | DInt i -> string_of_int i
  | DBool true -> "true"
  | DBool false -> "false"
  | DStr s -> s
  | DFloat f -> string_of_float f
  | DChar c -> Char.to_string c
  | DBlock _ -> "<block>"
  | DIncomplete -> "<incomplete>"
  | DNull -> "null"
  | DResp (_, hdv) -> to_url_string hdv
  | DDB db -> "<db>"
  | DList l ->
    "[ " ^ ( String.concat ~sep:", " (List.map ~f:to_url_string l)) ^ " ]"
  | DObj o ->
    let strs = DvalMap.fold o
        ~init:[]
        ~f:(fun ~key ~data l -> (key ^ ": " ^ to_url_string data) :: l) in

    "{ " ^ (String.concat ~sep:", " strs) ^ " }"

let rec equal_dval (a: dval) (b: dval) =
  match (a,b) with
  | DInt i1, DInt i2 -> i1 = i2
  | DBool b1, DBool b2 -> b1 = b2
  | DStr s1, DStr s2 -> s1 = s2
  | DFloat f1, DFloat f2 -> f1 = f2
  | DChar c1, DChar c2 -> c1 = c2
  | DNull, DNull -> true
  | DIncomplete, DIncomplete -> true
  | DList l1, DList l2 -> List.equal ~equal:equal_dval l1 l2
  | DObj o1, DObj o2 -> DvalMap.equal equal_dval o1 o2
  | _, _ -> false

type tipe = TInt
          | TStr
          | TChar
          | TBool
          | TFloat
          | TObj
          | TList
          | TAny
          | TBlock
          | TNull
          | TResp
          | TDB
          | TIncomplete
          [@@deriving eq, show, yojson]

let tipe2str t : string =
  match t with
  | TInt -> "Int"
  | TStr -> "Str"
  | TChar -> "Char"
  | TBool -> "Bool"
  | TFloat -> "Float"
  | TObj -> "Obj"
  | TList -> "List"
  | TBlock -> "Block"
  | TNull -> "Nothing"
  | TAny -> "Any"
  | TResp -> "Response"
  | TDB -> "Datastore"
  | TIncomplete -> "<incomplete>"


let tipeOf (dv : dval) : tipe =
  match dv with
  | DInt _ -> TInt
  | DStr _ -> TStr
  | DBool _ -> TBool
  | DFloat _ -> TFloat
  | DChar _ -> TChar
  | DNull -> TNull
  | DBlock _ -> TBlock
  | DList _ -> TList
  | DObj _ -> TObj
  | DResp _ -> TResp
  | DDB _ -> TDB
  | DIncomplete -> TIncomplete

  let tipename (dv: dval) : string =
    dv |> tipeOf |> tipe2str

let tipe_to_yojson (t: tipe) : Yojson.Safe.json =
  `String (t |> tipe2str)

let to_error_repr (dv : dval) : string =
  (to_repr dv) ^ " (" ^ (tipename dv) ^ ")"

let pp = Log.pp ~f:to_repr
let pP = Log.pP ~f:to_repr


(* ------------------------- *)
(* Functions *)
(* ------------------------- *)
let obj_merge (l: dval) (r: dval) : dval =
  match l, r with
  | DObj l, DObj r -> DObj (Util.merge_left l r)
  | DNull, DObj r -> DObj r
  | DObj l, DNull -> DObj l
  | _ -> Exception.user "was expecting objs"

let to_char dv =
  match dv with
  | DChar c -> Some c
  | _ -> None

let to_int dv =
  match dv with
  | DInt i -> Some i
  | _ -> None



(* ------------------------- *)
(* JSON *)
(* ------------------------- *)

let rec dval_of_yojson_ (json : Yojson.Safe.json) : dval =
  match json with
  | `Int i -> DInt i
  | `String s -> DStr s
  | `Bool b -> DBool b
  | `Float f -> DFloat f
  | `Null -> DNull
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

let rec dval_to_yojson (v : dval) : Yojson.Safe.json =
  match v with
  | DInt i -> `Int i
  | DBool b -> `Bool b
  | DStr s -> `String s
  | DFloat f -> `Float f
  | DNull -> `Null
  | DChar c -> `String (Char.to_string c)
  | DBlock _ -> `String "<block>"
  | DIncomplete -> `String "<incomplete>"
  | DList l -> `List (List.map l dval_to_yojson)
  | DResp (h, hdv) -> `List [dhttp_to_yojson h; dval_to_yojson hdv]
  | DDB _ -> `String "<db>"
  | DObj o -> o
              |> DvalMap.to_alist
              |> List.map ~f:(fun (k,v) -> (k, dval_to_yojson v))
              |> (fun a -> `Assoc a)

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

let to_dobj (pairs: (string*dval) list) : dval =
  DObj (DvalMap.of_alist_exn pairs)

let empty_dobj : dval =
  DObj (DvalMap.empty)

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



