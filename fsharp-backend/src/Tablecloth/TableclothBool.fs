module Tablecloth.Bool


// Functions for working with boolean ([true] or [false]) values.

type t = bool

let fromInt i =
  match i with
  | 0 -> Some false
  | 1 -> Some true
  | _ -> None

let from_int i = fromInt i

let fromString string =
  match string with
  | "false" -> Some false
  | "true" -> Some true
  | _ -> None


let from_string s = fromString s

let and_ (b1 : bool) (b2 : bool) : bool = b1 && b2

let or_ (b1 : bool) (b2 : bool) : bool = b1 || b2

let xor a b = (a && not b) || ((not a) && b)

let not b = not b

let equal (b1 : bool) (b2 : bool) = b1 = b2

let compare (b1 : bool) (b2 : bool) = b1.CompareTo b2

let toString b = if b = true then "true" else "false"

let to_string b = toString b

let toInt t =
  match t with
  | true -> 1
  | false -> 0

let to_int b = toInt b
