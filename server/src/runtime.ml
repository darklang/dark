type dval = DVal of string

let parse (str : string) : dval =
  DVal str

let to_string (dv : dval) : string =
  match dv with
  | DVal v -> v
