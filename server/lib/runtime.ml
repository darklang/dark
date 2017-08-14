open Core
open Types

(* ------------------------- *)
(* Values *)
(* ------------------------- *)
module ObjMap = String.Map
type objmap = dval ObjMap.t [@opaque]
and dval = DInt of int
         | DStr of string
         | DChar of char
         | DFloat of float
         | DBool of bool
         | DAnon of id * (dval -> dval)
         | DList of dval list
         | DObj of objmap
         | DIncomplete [@@deriving show]

module ParamMap = String.Map
type param_map = dval ParamMap.t


let rec to_repr (dv : dval) : string =
  match dv with
  | DInt i -> string_of_int i
  | DBool true -> "true"
  | DBool false -> "false"
  | DStr s -> "\"" ^ s ^ "\""
  | DFloat f -> string_of_float f
  | DChar c -> "'" ^ (Char.to_string c) ^ "'"
  | DAnon _ -> "<anon>"
  | DIncomplete -> "<incomplete>"
  | DList l ->
    "[ " ^ ( String.concat ~sep:", " (List.map ~f:to_repr l)) ^ " ]"
  | DObj o ->
    let strs = ObjMap.fold o
        ~init:[]
        ~f:(fun ~key ~data l -> (key ^ ": " ^ to_repr data) :: l) in

    "{ " ^ (String.concat ~sep:", " strs) ^ " }"

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
  | DList l ->
    "[ " ^ ( String.concat ~sep:", " (List.map ~f:to_string l)) ^ " ]"
  | DObj o ->
    let strs = ObjMap.fold o
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

let rec json2dval_ (json : Yojson.Safe.json) : dval =
  match json with
  | `Int i -> DInt i
  | `String s -> DStr s
  | `Bool b -> DBool b
  | `Assoc alist -> DObj (List.fold_left
                        alist
                        ~f:(fun m (k,v) -> ObjMap.add m k (json2dval_ v))
                        ~init:ObjMap.empty)
  | `List l -> DList (List.map ~f:json2dval_ l)
  | j -> DStr ( "<todo, incomplete conversion: "
                ^ (Yojson.Safe.to_string j)
                ^ ">")

let json2dval (json : string) : dval =
  json |> Yojson.Safe.from_string |> json2dval_

let dval2json (v : dval) : string = "{todo: 5}"

(* ------------------------- *)
(* Parsing *)
(* ------------------------- *)
let parse (str : string) : dval =
  (* TODO: Doesn't handle characters. Replace with a custom parser,
     using the one in RealWorldOcaml, or just ripped out of Yojson *)
  json2dval str


(* ------------------------- *)
(* Functions *)
(* ------------------------- *)
type fn = { name : string
          ; other_names : string list
          ; types : string list
          ; parameters : string list
          ; func : ((dval list) -> dval)
          }

let exe (fn : fn) (args : param_map) : dval =
  if ParamMap.length args < List.length fn.parameters then
    (* TODO: If there aren't enough parameters, curry it *)
    DIncomplete
  else
    let args = List.map ~f:(ParamMap.find_exn args) fn.parameters in
    fn.func args

let exe_dv (fn : dval) (_: dval list) : dval =
  match fn with
  | dv -> dv
          |> to_error_repr
          |> (^) "Calling non-function: "
          |> Exception.raise
