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

let parse (str : string) : dval =
  if String.equal str "" then
    "Values cannot be blank" |> Exception.raise
  else if String.length str >= 2
       && Char.equal '"' (String.nget str 0)
       && Char.equal '"' (String.nget str (-1))
  then DStr (String.slice str 1 (-1))
  else if String.length str = 3
       && Char.equal '\'' (String.nget str 0)
       && Char.equal '\'' (String.nget str (-1))
  then DChar (String.get str 1)
  else
    try DInt (int_of_string str)
    with
    | Failure _ ->
      try DFloat (float_of_string str)
      with
      | Failure _ ->
        Exception.raise ("Cannot parse value: " ^ str)

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
  | _ -> failwith "to_repr not implemented yet"


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

let dval2json (v: dval) : string = "{}"


(* ------------------------- *)
(* Functions *)
(* ------------------------- *)
type fn = { name : string
          ; other_names : string list
          ; parameters : string list
          ; func : ((dval list) -> dval)
          }

let exe (fn : fn) (args : param_map) : dval =
  (* TODO: we're going to have to use named params before the currying works properly *)
  if ParamMap.length args < List.length fn.parameters then
    (* If there aren't enough parameters, curry it *)
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
