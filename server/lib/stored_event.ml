open Core

module RTT = Types.RuntimeT

(* ------------------------- *)
(* Internal *)
(* ------------------------- *)

let group_dir = "events"
let file_ext  = ".events.json"

let dir_name (host: string) (id: int) : string =
  group_dir ^ "/" ^ host ^ "-" ^ (string_of_int id)

let mkdir (host: string) (id: int) : unit =
  Unix.mkdir_p (dir_name host id)

let is_request (name: string) : bool =
  String.is_suffix name ~suffix:file_ext

let parse_seq_idx (name: string) : int option =
  let re = Str.regexp "\\([0-9]+\\)" in
  try
    (* this throws if Not found *)
    let _ = Str.search_forward re name 0 in
    let first_match = Str.matched_group 0 name in
    (* this throws if we can't parse as an int for whatever reason *)
    Some (int_of_string first_match)
  with
  | _ -> None

let parse_seq_idx_exn (name: string) : int =
  match parse_seq_idx name with
  | Some s -> s
  | None -> failwith ("parse_seq_idx_exn failed to parse number from: " ^ name)

let ls (host: string) (id: int) : string list =
  Sys.ls_dir (dir_name host id)
  |> List.filter ~f:is_request
  |> List.sort
    ~cmp:(fun x y ->
        Pervasives.compare (parse_seq_idx_exn x) (parse_seq_idx_exn y))

let next_seq (files: string list) : int =
  files
  |> List.filter_map ~f:parse_seq_idx
  |> List.fold_left
       ~f:(fun acc x -> max acc x) ~init:1
  |> (+) 1

let next_filename (host: string) (id: int) : string =
  let next_number =
    try
      ls host id
      |> next_seq
    with
    | _ -> 1
  in
  let dir = dir_name host id in
  dir ^ "/" ^ (string_of_int next_number) ^ file_ext

(* ------------------------- *)
(* Exported *)
(* ------------------------- *)

(* TODO(ian): tag the event so if a handler changes from http -> async
 * that namespace them etc. *)
let store (host: string) (id: int) (event: RTT.dval) : unit =
  mkdir host id;
  let filename = next_filename host id in
  let s = Dval.dval_to_yojson event in
  Yojson.Safe.to_file filename s

let load_all (host: string) (id: int) : RTT.dval list =
  ls host id
  |> List.map
    ~f:(fun x -> (dir_name host id) ^ "/" ^ x)
  |> List.map
       ~f:(fun f -> f |> Yojson.Safe.from_file |> Dval.dval_of_yojson_)

