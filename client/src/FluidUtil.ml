open Tc

let literalToString
    (v : [> `Bool of bool | `Int of string | `Null | `Float of string * string])
    : string =
  match v with
  | `Int i ->
      i
  | `String str ->
      "\"" ^ str ^ "\""
  | `Bool b ->
      if b then "true" else "false"
  | `Null ->
      "null"
  | `Float (whole, fraction) ->
      whole ^ "." ^ fraction


(* truncateStringTo63BitInt only supports positive numbers for now, but we should change this once fluid supports negative numbers *)
(* If it is not an int after truncation, returns an error *)
let truncateStringTo63BitInt (s : string) : (string, string) Result.t =
  let is62BitInt s =
    match Native.BigInt.asUintN ~nBits:62 s with
    | Some i ->
        Native.BigInt.toString i = s
    | None ->
        false
  in
  (* 4611686018427387903 is largest 62 bit number, which has 19 characters *)
  (* We use 62 bit checks instead of 63 because the most significanty bit is for sign in two's complement -- which is not yet handled *)
  let trunc19 = String.left ~count:19 s in
  if is62BitInt trunc19
  then Ok trunc19
  else
    let trunc18 = String.left ~count:18 s in
    if is62BitInt trunc18
    then Ok trunc18
    else Error "Invalid 63bit number even after truncate"


(* Only supports positive numbers for now, but we should change this once fluid supports negative numbers *)
let coerceStringTo63BitInt (s : string) : string =
  Result.withDefault (truncateStringTo63BitInt s) ~default:"0"


(* Only supports positive numbers for now, but we should change this once fluid supports negative numbers *)
let is63BitInt (s : string) : bool = Result.isOk (truncateStringTo63BitInt s)

let trimQuotes s : string =
  let open String in
  s
  |> fun v ->
  (if endsWith ~suffix:"\"" v then dropRight ~count:1 v else v)
  |> fun v -> if startsWith ~prefix:"\"" v then dropLeft ~count:1 v else v


let removeCharAt str offset : string =
  if offset < 0
  then str
  else
    String.slice ~from:0 ~to_:offset str
    ^ String.slice ~from:(offset + 1) ~to_:(String.length str) str


let isNumber (str : string) = Js.Re.test_ [%re "/[0-9]+/"] str

let isIdentifierChar (str : string) = Js.Re.test_ [%re "/[_a-zA-Z0-9]+/"] str

let isFnNameChar str =
  Js.Re.test_ [%re "/[_:a-zA-Z0-9]/"] str && String.length str = 1
