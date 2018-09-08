open Core_kernel

(* ------------------------ *)
(* equal, compare, sexp and yojson defs for types we use *)
(* ------------------------ *)

(* block *)
type 'a block = 'a list -> 'a [@opaque][@@deriving show, sexp]
let equal_block _ _ _ = false
let compare_block _ _ _ = -1
let block_to_yojson x _ = failwith "Cant serialize blocks"
let block_of_yojson _ _ = failwith "Cant deserialize blocks"

(* uuid *)
type uuid = Uuidm.t [@opaque][@@deriving show, eq, compare]
let uuid_to_yojson uuid = `String (Uuidm.to_string uuid)
let uuid_of_yojson json =
  match json with
  | `String s ->
    Uuidm.of_string s
    |> Result.of_option ~error:"can't be parsed"
  | _ -> Error "not a string"

let uuid_of_sexp st =
  match st with
  | Sexp.Atom s ->
    Option.value_exn
      ~message:"failure uuid_of_sexp"
      (Uuidm.of_string s)
  | _ -> failwith "failure uuid_of_sexp"
let sexp_of_uuid u = Sexp.Atom (Uuidm.to_string u)

(* time *)
type time = Time.Stable.With_utc_sexp.V2.t
            [@opaque]
            [@@deriving compare, sexp, show]
let equal_time t1 t2 = t1 = t2
let time_to_yojson time =
    time
    |> sexp_of_time
    |> string_of_sexp
    |> fun s -> `String s

let time_of_yojson json =
  match json with
  | `String s ->
    s
    |> sexp_of_string
    |> time_of_sexp
  |> Ok
  | _ -> Error "Invalid time"



(* Bytes *)
module PasswordBytes = struct
  include Bytes
  let to_yojson a = failwith "todo"
  let of_yojson a = failwith "todo"
end


