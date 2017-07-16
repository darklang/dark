open Core

type dval = DInt of int
          | DStr of string
          | DFloat of float

let parse (str : string) : dval =
  if String.equal str "" then
    "Values cannot be blank" |> Exception.raise
  else if String.length str >= 2
       && Char.equal '"' (String.nget str 0)
       && Char.equal '"' (String.nget str (-1))
  then DStr (String.slice str 1 (-1))
  else
    try str |> int_of_string |> DInt
    with
    | Failure "int_of_string" ->
      try str |> float_of_string |> DFloat
      with
      | Failure "float_of_string" ->
        Exception.raise ("Cannot parse value: " ^ str)

let to_string (dv : dval) : string =
  match dv with
  | DInt i -> string_of_int i
  | DStr s -> "\"" ^ s ^ "\""
  | DFloat f -> string_of_float f

let get_type (dv : dval) : string =
  match dv with
  | DInt _ -> "Integer"
  | DStr _ -> "String"
  | DFloat _ -> "Float"
