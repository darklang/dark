open Core_kernel


(* ------------------- *)
(* random *)
(* ------------------- *)
let create_id  () : int =
  Random.int (Int.pow 2 29)

let create_uuid () : Uuidm.t =
  (Uuidm.v `V4)


let string_replace (search: string) (replace: string) (str: string) : string =
  String.Search_pattern.replace_all (String.Search_pattern.create search) ~in_:str ~with_:replace

let random_string length =
    let gen () = match Random.int(26+26+10) with
        n when n < 26 -> int_of_char 'a' + n
      | n when n < 26 + 26 -> int_of_char 'A' + n - 26
      | n -> int_of_char '0' + n - 26 - 26 in
    let gen _ = String.make 1 (char_of_int(gen())) in
    String.concat ~sep:"" (Array.to_list (Array.init length gen))

(* Given ~regex, return Err if it doesn't match, or list of captures if
 * it does. First elem of the list is the first capture, not the whole
 * match. *)
let string_match ~(regex: string) (str: string) : string list Or_error.t =
  let reg = Re2.create_exn (regex) in
  str
  |> Re2.find_submatches reg
  |> Result.map ~f:Array.to_list
  |> Result.map ~f:List.tl_exn (* skip full match *)
  |> Result.map ~f:(List.map ~f:(Option.value ~default:""))


let list_repeat times item =
  List.init times ~f:(fun _ -> item)

let merge_left = Map.merge ~f:(fun ~key v ->
    match v with
    | `Left v -> Some v
    | `Right v -> Some v
    | `Both (v1, v2) -> Some v1)

let merge_right = Map.merge ~f:(fun ~key v ->
    match v with
    | `Left v -> Some v
    | `Right v -> Some v
    | `Both (v1, v2) -> Some v2)

let list_any ~(f: 'a -> 'b) (l: 'a list) : bool =
  List.length (List.filter ~f l) > 0

let int_sum (l: int list) : int =
  List.fold_left ~f:(+) ~init:0 l


let maybe_chop_prefix ~prefix msg =
  String.chop_prefix ~prefix msg
  |> Option.value ~default:msg

let charset (headers: (string * string) list)
  : [> `Latin1 | `Other | `Utf8] =
  let canonicalize s =
    s
    |> String.strip
    |> String.lowercase
  in
  headers
  |> List.map ~f:(Tuple.T2.map_fst ~f:canonicalize)
  |> List.map ~f:(Tuple.T2.map_snd ~f:canonicalize)
  |> List.filter_map
    ~f:(function
        | ("content-type",v) ->
          (match string_match ~regex:".*;\\s*charset=(.*)$" v with
           | Result.Ok (["utf-8"]) -> Some `Utf8
           | Result.Ok (["utf8"]) -> Some `Utf8
           | Result.Ok (["us-ascii"]) -> Some `Latin1 (* should work *)
           | Result.Ok (["iso-8859-1"]) -> Some `Latin1
           | Result.Ok (["iso_8859-1"]) -> Some `Latin1
           | Result.Ok (["latin1"]) -> Some `Latin1
           | _ -> None)
        | _ -> None)
  |> List.hd
  |> Option.value ~default:`Other



(* ------------------- *)
(* html *)
(* ------------------- *)
let html_escape (html : string) : string =
  String.concat_map html
    (fun c -> match c with
             '<' -> "&lt;"
           | '>' -> "&gt;"
           | '&' -> "&amp;"
           (* include these for html-attribute-escaping
              even though they're not strictly necessary
              for html-escaping proper. *)
           | '"' -> "&quot;"
           (* &apos; doesn't work in IE.... *)
           | '\'' -> "&#x27;"
           | _ -> String.of_char c)
