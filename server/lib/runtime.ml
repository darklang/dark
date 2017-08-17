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
         (* TODO: make null more like option. Maybe that's for the type
            system *)
         | DNull
         | DObj of objmap
         | DIncomplete [@@deriving show]


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
  | DNull -> "null"
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
  | DNull -> "null"
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

let rec json2dval_ (json : Yojson.Safe.json) : dval =
  match json with
  | `Int i -> DInt i
  | `String s -> DStr s
  | `Bool b -> DBool b
  | `Float f -> DFloat f
  | `Null -> DNull
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

let rec dval2json_ (v : dval) : Yojson.Safe.json =
  match v with
  | DInt i -> `Int i
  | DBool b -> `Bool b
  | DStr s -> `String s
  | DFloat f -> `Float f
  | DNull -> `Null
  | DChar c -> `String (Char.to_string c)
  | DAnon _ -> `String "<anon>"
  | DIncomplete -> `String "<incomplete>"
  | DList l -> `List (List.map l dval2json_)
  | DObj o -> o
              |> ObjMap.to_alist
              |> List.map ~f:(fun (k,v) -> (k, dval2json_ v))
              |> (fun a -> `Assoc a)

let dval2json (v : dval) : string =
  v
  |> dval2json_
  |> Yojson.Safe.to_string


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

module ArgMap = String.Map
type arg_map = dval ArgMap.t

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
            | API of (arg_map -> dval)

type fn = { name : string
          ; other_names : string list
          ; parameters : param list
          ; return_type : tipe
          ; description : string
          ; func : ccfunc
          }

let param_to_string (param: param) : string =
  param.name
  ^ (if param.optional then "?" else "")
  ^ " : "
  ^ param.tipe


let fetch_arg (args: arg_map) (param: param) : dval =
  let arg = ArgMap.find args param.name in
  match arg with
  | None -> if param.optional then DNull else DIncomplete
  | Some dv -> dv

exception TypeError of dval list

let exe (fn: fn) (args: arg_map) : dval =
  try
    match fn.func with
    | InProcess f -> fn.parameters
                     |> List.map ~f:(fetch_arg args)
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
