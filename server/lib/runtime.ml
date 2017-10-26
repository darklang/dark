open Core
open Types



(* ------------------------- *)
(* Values *)
(* ------------------------- *)
module DvalMap = String.Map
type dval_map = dval DvalMap.t [@opaque]
and dval = DInt of int
         | DStr of string
         | DChar of char
         | DFloat of float
         | DBool of bool
         | DBlock of id * (dval list -> dval)
         | DList of dval list
         (* TODO: make null more like option. Maybe that's for the type
            system *)
         | DNull
         | DObj of dval_map
         | DIncomplete [@@deriving show]

let rec to_repr_ (indent: int) (pp : bool) (dv : dval) : string =
  let nl = if pp then "\n" ^ (String.make indent ' ') else " " in
  let inl = if pp then "\n" ^ (String.make (indent + 2) ' ') else "" in
  let indent = indent + 2 in
  match dv with
  | DInt i -> string_of_int i
  | DBool true -> "true"
  | DBool false -> "false"
  | DStr s -> "\"" ^ s ^ "\""
  | DFloat f -> string_of_float f
  | DChar c -> "'" ^ (Char.to_string c) ^ "'"
  | DBlock (id, _) -> "<block:" ^ string_of_int id ^ ">"
  | DIncomplete -> "<incomplete>"
  | DNull -> "null"
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
          | TIncomplete
          [@@deriving yojson, show]

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
      Exception.user ~actual:str ("Not a valid value: \"" ^ str ^ "\"")



(* ------------------------- *)
(* Functions *)
(* ------------------------- *)
type execute_t = (dval_map -> dval)
type argument = AEdge of int
              | AConst of dval [@@deriving yojson, show]

let blank_arg = AConst DIncomplete
module ArgMap = String.Map
type arg_map = argument ArgMap.t

type param = { name: string
             ; tipe: tipe
             ; block_args : string list
             ; optional : bool
             ; description : string
            } [@@deriving yojson, show]

let param_to_string (param: param) : string =
  param.name
  ^ (if param.optional then "?" else "")
  ^ " : "
  ^ (tipe2str param.tipe)


type ccfunc = InProcess of (dval list -> dval)
            | API of (dval_map -> dval)

type fn = { name : string
          ; other_names : string list
          ; parameters : param list
          ; return_type : tipe
          ; description : string
          ; preview : (dval list -> int -> dval list) option
          ; func : ccfunc
          ; pure : bool
          }

let error ?(actual=DIncomplete) ?(result=DIncomplete) ?(info=[]) ?(expected="") ?(workarounds=[]) ?(long="") (short: string) =
 raise
   (Exception.DarkException
    { short = short
    ; long = long
    ; tipe = "Runtime"
    ; actual = actual |> dval_to_yojson |> Yojson.Safe.pretty_to_string
    ; actual_tipe = actual |> tipename 
    ; result = result |> dval_to_yojson |> Yojson.Safe.pretty_to_string
    ; result_tipe = result |> tipename
    ; expected = expected
    ; info = info
    ; workarounds = workarounds
    })

exception TypeError of dval list

module Scope = Int.Map
type scope = dval Scope.t

let exe ?(ind=0) (fn: fn) (args: dval_map) : dval =
  match fn.func with
  | InProcess f ->
      let arglist = fn.parameters
                    |> List.map ~f:(fun (p: param) -> p.name)
                    |> List.map ~f:(DvalMap.find_exn args) in
      (try
        f arglist
       with
       | TypeError _ ->
           Log.pP ~name:"execution" ~ind "exception caught" args ~f:dvalmap_to_string;
           let range = List.range 0 (List.length arglist) in
           let all = List.map3_exn range fn.parameters arglist ~f:(fun i p a -> (i,p,a)) in
           let invalid = List.filter_map all
                           ~f:(fun (i,p,a) -> if tipeOf a <> p.tipe
                                              && p.tipe <> TAny
                               then Some (i,p,a)
                               else None) in
           (* let invalid_count = List.length invalid in *)
           match invalid with
           | [] -> Exception.internal "There was an type error in the arguments, but we had an error and can't find it"

           | (i,p,DIncomplete) :: _ ->
              Exception.user
                ~expected:(tipe2str p.tipe)
                ~actual:"missing"
                (fn.name ^ " is missing an argument: " ^ p.name)

           | (i,p,a) :: _ ->
              error
                ~actual:a
                ~expected:(tipe2str p.tipe)
                (fn.name ^ " was called with the wrong type to parameter: " ^ p.name))

  | API f ->
      try
        f args
      with
      | TypeError args ->
          error (fn.name ^ " is missing a parameter")
            ~expected:(fn.parameters |> List.map ~f:param_to_string |> String.concat ~sep:", ")
            ~actual:DIncomplete

