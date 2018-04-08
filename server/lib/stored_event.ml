open Core

module RTT = Types.RuntimeT

type event_desc = string * string * string
                [@@deriving show]
type four_oh_four = (event_desc * Types.RuntimeT.dval list)
                  [@@deriving show]

(* ------------------------- *)
(* Internal *)
(* ------------------------- *)

let file_ext  = ".events.json"

let host_dir_name (host: string) : string =
  Config.events_dir ^ host

let space_dir_name (host: string) (space: string) : string =
  host_dir_name host ^ "/" ^ space

let dir_name (host: string) (space, _, modifier : event_desc) : string =
  space_dir_name host space ^ "/" ^ modifier

let ls_dir dir =
  (* avoid exceptions during lsdirs. We don't want to wrap everything in
   * a try/with incase it masks other errors. *)
  Unix.mkdir_p dir;
  Sys.ls_dir dir
  |> Log.pp ("ls of " ^ dir)

let is_request (name: string) : bool =
  String.is_suffix name ~suffix:file_ext

let parse_seq_idx ~path (filename: string) : int option =
  let prefix = path ^ "::" in
  if String.is_prefix ~prefix filename
  then
    filename
    |> String.chop_prefix_exn ~prefix
    |> String.chop_suffix_exn ~suffix:file_ext
    |> int_of_string
    |> fun idx -> Some idx
  else
    None

let ls_host (host: string) : event_desc list =
  ls_dir (host_dir_name host)
  |> List.map ~f:(fun space ->
      ls_dir (space_dir_name host space)
      |> List.map ~f:(fun mod_ ->
          ls_dir (dir_name host (space, "", mod_))
          |> List.map ~f:(fun path ->
              path
              |> String.lsplit2_exn ~on:':'
              |> fun (path, _) ->
                  (space, path, mod_))))
  |> List.concat
  |> List.concat



let ls_descs (host: string) (desc : event_desc) : string list =
  ls_dir (dir_name host desc)
  |> List.filter ~f:is_request

let next_seq (files: string list) ~path : int =
  files
  |> List.filter_map ~f:(parse_seq_idx ~path)
  |> List.fold_left
       ~f:(fun acc x -> max acc x) ~init:(-1)
  |> (+) 1

let next_filename (host: string) (desc : event_desc) : string =
  let (_, path, _) = desc in
  let next_number =
    try
      ls_descs host desc
      |> next_seq ~path
    with
    | _ -> 1
  in
  let dir = dir_name host desc in
  dir ^ "/"
    ^ String.escaped path
    ^ "::"
    ^ (string_of_int next_number)
    ^ file_ext

(* ------------------------- *)
(* Exported *)
(* ------------------------- *)

let store_event host desc event : unit =
  let filename = next_filename host desc in
  let s = Dval.dval_to_yojson event in
  Yojson.Safe.to_file filename s

(* We store a set of events for each host. The events may or may not
 * belong to a toplevel. We provide a list in advance so that they can
 * be partitioned effectively *)
let list_events (host: string) : (event_desc) list =
  ls_host host

let load_events (host: string) (desc : event_desc) : RTT.dval list =
  ls_descs host desc
  |> List.map
    ~f:(fun x -> (dir_name host desc) ^ "/" ^ x)
  |> List.map
       ~f:(fun f -> f |> Yojson.Safe.from_file |> Dval.dval_of_yojson_)


let four_oh_four_to_yojson (((space, path, modifier), dvals) : four_oh_four) : Yojson.Safe.json =
  `List [ `String space
        ; `String path
        ; `String modifier
        ; `List (List.map ~f:Dval.dval_to_yojson dvals)
        ]

