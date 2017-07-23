open Core

(* ------------------------- *)
(* Values *)
(* ------------------------- *)
type fn = { name : string
          ; parameters : string list
          ; func : ((dval list) -> dval)
          }
and dval = DInt of int
         | DStr of string
         | DChar of char
         | DFloat of float
         | DIncomplete

module SMap = String.Map
type param_map = dval SMap.t

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
    try str |> int_of_string |> DInt
    with
    | Failure _ ->
      try str |> float_of_string |> DFloat
      with
      | Failure _ ->
        Exception.raise ("Cannot parse value: " ^ str)

let get_type (dv : dval) : string =
  match dv with
  | DInt _ -> "Integer"
  | DStr _ -> "String"
  | DFloat _ -> "Float"
  | DChar _ -> "Char"
  | DIncomplete -> "n/a"

and to_repr (dv : dval) : string =
  match dv with
  | DInt i -> string_of_int i
  | DStr s -> "\"" ^ s ^ "\""
  | DFloat f -> string_of_float f
  | DChar c -> "'" ^ (Char.to_string c) ^ "'"
  | DIncomplete -> "<incomplete>"



let to_error_repr (dv : dval) : string =
  (to_repr dv) ^ " (" ^ (get_type dv) ^ ")"

let to_char (dv : dval) : char =
  match dv with
  | DChar c -> c
  | _ -> Exception.raise "Not a char"

let equal_dval (a: dval) (b: dval) = (to_repr a) = (to_repr b)


(* ------------------------- *)
(* Functions *)
(* ------------------------- *)
let exe (fn : fn) (args : param_map) : dval =
  (* TODO: we're going to have to use named params before the currying works properly *)
  if SMap.length args < List.length fn.parameters then
    (* If there aren't enough parameters, curry it *)
    DIncomplete
  else
    let args = List.map ~f:(SMap.find_exn args) fn.parameters in
    fn.func args

let exe_dv (fn : dval) (_: dval list) : dval =
  match fn with
  | dv -> dv
          |> to_error_repr
          |> (^) "Calling non-function: "
          |> Exception.raise
