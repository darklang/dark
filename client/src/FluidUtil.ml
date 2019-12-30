open Tc
open Prelude

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


let parseString str :
    [> `Bool of bool
    | `Int of string
    | `Null
    | `Float of string * string
    | `Unknown ] =
  let asBool =
    if str = "true"
    then Some (`Bool true)
    else if str = "false"
    then Some (`Bool false)
    else if str = "null"
    then Some `Null
    else None
  in
  let asInt = if is63BitInt str then Some (`Int str) else None in
  let asFloat =
    try
      (* for the exception *)
      ignore (float_of_string str) ;
      match String.split ~on:"." str with
      | [whole; fraction] ->
          Some (`Float (whole, fraction))
      | _ ->
          None
    with _ -> None
  in
  let asString =
    if String.startsWith ~prefix:"\"" str && String.endsWith ~suffix:"\"" str
    then
      Some
        (`String (str |> String.dropLeft ~count:1 |> String.dropRight ~count:1))
    else None
  in
  asInt
  |> Option.or_ asString
  |> Option.or_ asBool
  |> Option.or_ asFloat
  |> Option.withDefault ~default:`Unknown


let splitFnName (fnName : string) : string option * string * string =
  let pattern = Js.Re.fromString "^((\\w+)::)?([^_]+)(_v(\\d+))?$" in
  let mResult = Js.Re.exec_ pattern fnName in
  match mResult with
  | Some result ->
      let captures =
        result
        |> Js.Re.captures
        |> Belt.List.fromArray
        |> List.map ~f:Js.toOption
      in
      ( match captures with
      | [_; _; mod_; Some fn; _; Some v] ->
          (mod_, fn, v)
      | [_; _; mod_; Some fn; _; None] ->
          (mod_, fn, "0")
      | _ ->
          recover "invalid fn name" ~debug:fnName (None, fnName, "0") )
  | None ->
      (None, fnName, "0")


(* Get just the function mod and name *)
let fnDisplayName (fnName : string) : string =
  let mod_, name, _ = splitFnName fnName in
  match mod_ with Some mod_ -> mod_ ^ "::" ^ name | None -> name


(* Get just the function version *)
let versionDisplayName (fnName : string) : string =
  let _, _, version = splitFnName fnName in
  if version = "0" then "" else "v" ^ version


(* Get the function mod, name and version (without underscore) *)
let partialName (name : string) : string =
  let version = versionDisplayName name in
  let name = fnDisplayName name in
  name ^ version
