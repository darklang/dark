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
         | DAnon of id * (dval list -> dval)
         | DList of dval list
         (* TODO: make null more like option. Maybe that's for the type
            system *)
         | DNull
         | DObj of dval_map
         | DIncomplete [@@deriving show]

let rec to_repr (dv : dval) : string =
  match dv with
  | DInt i -> string_of_int i
  | DBool true -> "true"
  | DBool false -> "false"
  | DStr s -> "\"" ^ s ^ "\""
  | DFloat f -> string_of_float f
  | DChar c -> "'" ^ (Char.to_string c) ^ "'"
  | DAnon (id, _) -> "<anon:" ^ string_of_int id ^ ">"
  | DIncomplete -> "<incomplete>"
  | DNull -> "null"
  | DList l ->
    "[ " ^ ( String.concat ~sep:", " (List.map ~f:to_repr l)) ^ " ]"
  | DObj o ->
    let strs = DvalMap.fold o
        ~init:[]
        ~f:(fun ~key ~data l -> (key ^ ": " ^ to_repr data) :: l) in

    "{ " ^ (String.concat ~sep:", " strs) ^ " }"

let to_comparable_repr (dvm : dval_map) : string =
  Map.to_alist ~key_order:`Increasing dvm
  |> List.map ~f:(fun (s, dv) -> s ^ (to_repr dv))
  |> List.fold ~f:(fun a b -> a ^ b) ~init:""

let rec to_string (dv : dval) : string =
  match dv with
  | DInt i -> string_of_int i
  | DBool true -> "true"
  | DBool false -> "false"
  | DStr s -> s
  | DFloat f -> string_of_float f
  | DChar c -> Char.to_string c
  | DAnon _ -> "<anon>"
  | DIncomplete -> "<incomplete>"
  | DNull -> "null"
  | DList l ->
    "[ " ^ ( String.concat ~sep:", " (List.map ~f:to_string l)) ^ " ]"
  | DObj o ->
    let strs = DvalMap.fold o
        ~init:[]
        ~f:(fun ~key ~data l -> (key ^ ": " ^ to_string data) :: l) in

    "{ " ^ (String.concat ~sep:", " strs) ^ " }"

let equal_dval (a: dval) (b: dval) = (to_repr a) = (to_repr b)

let get_type (dv : dval) : string =
  match dv with
  | DInt _ -> "Integer"
  | DStr _ -> "String"
  | DBool _ -> "Bool"
  | DFloat _ -> "Float"
  | DChar _ -> "Char"
  | DNull -> "Nothing"
  | DAnon _ -> "Anonymous function"
  | DList _ -> "List"
  | DObj _ -> "Object"
  | DIncomplete -> "n/a"
  (* | _ -> failwith "get_type not implemented yet" *)

let to_error_repr (dv : dval) : string =
  (to_repr dv) ^ " (" ^ (get_type dv) ^ ")"

let to_char (dv : dval) : char =
  match dv with
  | DChar c -> c
  | _ -> Exception.raise "Not a char"

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

let rec dval_of_yojson (json : Yojson.Safe.json) : (dval, string) result =
  Result.Ok (dval_of_yojson_ json)

let rec dval_to_yojson (v : dval) : Yojson.Safe.json =
  match v with
  | DInt i -> `Int i
  | DBool b -> `Bool b
  | DStr s -> `String s
  | DFloat f -> `Float f
  | DNull -> `Null
  | DChar c -> `String (Char.to_string c)
  | DAnon _ -> `String "<anon>"
  | DIncomplete -> `String "<incomplete>"
  | DList l -> `List (List.map l dval_to_yojson)
  | DObj o -> o
              |> DvalMap.to_alist
              |> List.map ~f:(fun (k,v) -> (k, dval_to_yojson v))
              |> (fun a -> `Assoc a)

let dval_to_json_string (v: dval) : string =
  v |> dval_to_yojson |> Yojson.Safe.to_string

(* ------------------------- *)
(* Parsing *)
(* ------------------------- *)
let parse (str : string) : dval =
  (* TODO: Doesn't handle characters. Replace with a custom parser,
     using the one in RealWorldOcaml, or just ripped out of Yojson *)
  str |> Yojson.Safe.from_string |> dval_of_yojson_


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
             ; tipe: string
             ; optional : bool
             ; description : string
             } [@@deriving yojson, show]
(* types  *)
type tipe = string
let tInt = "Integer"
let tStr = "String"
let tChar = "Char"
let tBool = "Bool"
let tObj = "Object"
let tList = "List"
(* placeholder until typesystem becomes more complete *)
let tAny = "Any"
let tFun = "Function"

type ccfunc = InProcess of (dval list -> dval)
            | API of (dval_map -> dval)

type fn = { name : string
          ; other_names : string list
          ; parameters : param list
          ; return_type : tipe
          ; description : string
          ; preview : (dval list -> dval) option
          ; func : ccfunc
          }

let param_to_string (param: param) : string =
  param.name
  ^ (if param.optional then "?" else "")
  ^ " : "
  ^ param.tipe


exception TypeError of dval list

let exe (fn: fn) (args: dval_map) : dval =
  try
    match fn.func with
    | InProcess f -> fn.parameters
                     |> List.map ~f:(fun (p: param) -> p.name)
                     |> List.map ~f:(DvalMap.find_exn args)
                     |> f
    | API f -> f args
  with
  | TypeError args ->
    Exception.raise
      ("Incorrect type to fn "
       ^ fn.name
       ^ ": expected ["
       ^ String.concat ~sep:", " (List.map ~f:param_to_string fn.parameters)
       ^ "], got ["
       ^ String.concat ~sep:", " (List.map ~f:to_error_repr args)
       ^ "]")

let exe_dv (fn : dval) (_: dval list) : dval =
  match fn with
  | dv -> dv
          |> to_error_repr
          |> (^) "Calling non-function: "
          |> Exception.raise
