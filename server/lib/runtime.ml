module Map = Core.Map.Poly

(* ------------------------- *)
(* Values *)
(* ------------------------- *)
type fn = { name : string
          ; parameters : string list
          ; func : (dval list) -> dval
          ; partials : (dval list)
          }
and dval = DInt of int
         | DStr of string
         | DChar of char
         | DFloat of float
         | DFn of fn

let parse (str : string) : dval =
  if String.equal str "" then
    "Values cannot be blank" |> Exception.raise
  else if String.length str >= 2
       && Char.equal '"' (Core.String.nget str 0)
       && Char.equal '"' (Core.String.nget str (-1))
  then DStr (Core.String.slice str 1 (-1))
  else if String.length str = 3
       && Char.equal '\'' (Core.String.nget str 0)
       && Char.equal '\'' (Core.String.nget str (-1))
  then DChar (Core.String.get str 1)
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
  | DFn fn -> if fn.partials = [] then
      "Function"
    else "Partial"

let rec to_repr (dv : dval) : string =
  match dv with
  | DInt i -> string_of_int i
  | DStr s -> "\"" ^ s ^ "\""
  | DFloat f -> string_of_float f
  | DChar c -> "'" ^ (Core.Char.to_string c) ^ "'"
  | DFn fn ->
    let (prefix, suffix) = if fn.partials <> [] then
        let partials = fn.partials
                       |> List.map to_repr
                       |> String.concat ", "
        in "Fn", " partially applied with: [" ^ partials ^ "]"
      else
        "Fn", ""
    in prefix
       ^ " "
       ^ fn.name
       ^ "("
       ^ (String.concat ", " fn.parameters)
       ^ ")"
       ^ suffix

let to_error_repr (dv : dval) : string =
  (to_repr dv) ^ " (" ^ (get_type dv) ^ ")"

let to_char (dv : dval) : char =
  match dv with
  | DChar c -> c
  | _ -> Exception.raise "Not a char"



(* ------------------------- *)
(* Functions *)
(* ------------------------- *)
let exe (fn : fn) (args : dval list) : dval =
  if List.length args < List.length fn.parameters then
    (* If there aren't enough parameters, curry it *)
    (* TODO: we're going to have to use named params before the currying works properly *)
    DFn { fn with partials = fn.partials @ args }
  else
    fn.func @@ fn.partials @ args

let exe_dv (fn : dval) (args : dval list) : dval =
  match fn with
  | DFn fn -> exe fn args
  | dv -> dv
          |> to_error_repr
          |> (^) "Calling non-function: "
          |> Exception.raise
