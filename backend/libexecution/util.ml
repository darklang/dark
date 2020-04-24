open Core_kernel
open Libcommon

(* ------------------- *)
(* random *)
(* ------------------- *)
let create_id () : Int63.t = Unshared.gid ()

let create_uuid () : Uuidm.t = Uuidm.v `V4

let uuid_of_string (id : string) : Uuidm.t =
  Uuidm.of_string id |> Option.value_exn ~message:("Bad UUID: " ^ id)


let isostring_of_date (d : Time.t) : string =
  let date, sec = Time.to_date_ofday ~zone:Time.Zone.utc d in
  Date.to_string date ^ "T" ^ Time.Ofday.to_sec_string sec ^ "Z"


let date_of_isostring (str : string) : Time.t = Time.of_string str

let string_replace (search : string) (replace : string) (str : string) : string
    =
  String.Search_pattern.replace_all
    (String.Search_pattern.create search)
    ~in_:str
    ~with_:replace


let random_string length =
  let gen () =
    match Random.int (26 + 26 + 10) with
    | n when n < 26 ->
        int_of_char 'a' + n
    | n when n < 26 + 26 ->
        int_of_char 'A' + n - 26
    | n ->
        int_of_char '0' + n - 26 - 26
  in
  let gen _ = String.make 1 (char_of_int (gen ())) in
  String.concat ~sep:"" (Array.to_list (Array.init length gen))


(* Merge both maps, picking the value from the first argument if the key
 * exists in both *)
let merge_left =
  Map.merge ~f:(fun ~key v ->
      match v with
      | `Left v ->
          Some v
      | `Right v ->
          Some v
      | `Both (v1, v2) ->
          Some v1)


(* Merge both maps, picking the value from the second argument if the key
 * exists in both *)
let merge_right =
  Map.merge ~f:(fun ~key v ->
      match v with
      | `Left v ->
          Some v
      | `Right v ->
          Some v
      | `Both (v1, v2) ->
          Some v2)


let int_sum (l : int list) : int = List.fold_left ~f:( + ) ~init:0 l

let maybe_chop_suffix ~suffix str =
  String.chop_suffix ~suffix str |> Option.value ~default:str


let hash (str : string) = Libtarget.digest384 str

let hash_bytes (bytes : Bytes.t) = Libtarget.digest384_bytes bytes
