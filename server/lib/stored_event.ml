open Core

module RTT = Types.RuntimeT

type event_desc = string * string * string
                [@@deriving show, yojson]
type four_oh_four = (event_desc * Types.RuntimeT.dval list)
                  [@@deriving show]

(* TODO SECURITY: the host isn't checked for ../, etc *)

(* ------------------------- *)
(* Internal *)
(* ------------------------- *)

(* for security and performance, and also because it's tricky to get
 * right otherwise, we store this in the following directory structure:
 * /hostname/
 *   descriptors/
 *     {sha1s of event_desc} -> json with (path, space, modifier)
 *   events/
 *     {sha1s of event_desc}/
 *       1.event.json -> json of dval
 *       n.event.json -> json of dval
 *)


(* -------------------------- *)
(* Utils *)
(* -------------------------- *)

let file_ext  = ".events.json"

let is_request (name: string) : bool =
  String.is_suffix name ~suffix:file_ext

type hash = string
let desc_hash ((space, path, modifier): event_desc) : hash =
  [space ; path; modifier]
  |> String.concat
  |> Util.hash

let (/) = Filename.concat


(* -------------------------- *)
(* Dirs *)
(* -------------------------- *)

let host_dir (host: string) : string =
  Config.events_dir / host

let descriptor_dir (host: string) : string =
  host_dir host / "descriptors"

let eventdata_dir (host: string) (desc : event_desc) : string =
  host_dir host / "eventdata" / (desc_hash desc)

(* -------------------------- *)
(* Descriptors *)
(* -------------------------- *)
let read_descriptor (host: string) (hash : hash) : event_desc =
  (descriptor_dir host / hash)
  |> Util.readfile
  |> Yojson.Safe.from_string
  |> event_desc_of_yojson
  |> Result.ok_or_failwith

let list_descriptors (host : string) : hash list =
  let dir = descriptor_dir host in
  Util.mkdir dir;
  Util.lsdir dir

let save_descriptor (host: string) (desc: event_desc) : unit =
  let dir = descriptor_dir host in
  Util.mkdir dir;
  let filename = dir / desc_hash desc in
  desc
  |> event_desc_to_yojson
  |> Yojson.Safe.to_string
  |> Util.writefile filename



(* -------------------------- *)
(* Event data *)
(* -------------------------- *)
let list_data_files (host: string) (desc : event_desc) : string list =
  eventdata_dir host desc
  |> Util.lsdir
  |> List.filter ~f:is_request

let read_data (host: string) (desc: event_desc) : RTT.dval list =
  let dir = eventdata_dir host desc in
  Util.mkdir dir;
  dir
  |> Util.lsdir
  |> List.sort ~cmp:Pervasives.compare
  |> List.map ~f:(fun file -> dir / file
                              |> Util.readfile
                              |> Yojson.Safe.from_string
                              |> Dval.dval_of_yojson
                              |> Result.ok_or_failwith)

let next_data_filename (host: string) (desc : event_desc) : string =
  let dir = eventdata_dir host desc in
  Util.mkdir dir;
  list_data_files host desc
  |> List.map ~f:(String.chop_suffix_exn ~suffix:file_ext)
  |> List.map ~f:int_of_string
  |> List.fold_left
    ~f:(fun acc x -> max acc x) ~init:0
  |> (+) 1
  |> fun index -> dir / (string_of_int index) ^ file_ext

let save_data (host: string) (desc: event_desc) (dval : RTT.dval) =
  let filename = next_data_filename host desc |> Log.pp "filename" in
  dval
  |> Dval.dval_to_yojson
  |> Yojson.Safe.to_string
  |> Util.writefile filename



(* ------------------------- *)
(* Exported *)
(* ------------------------- *)

let store_event (host : string) (desc : event_desc) (event: RTT.dval)
  : unit =
  save_descriptor host desc;
  save_data host desc event;
  ()

let list_events (host: string) : event_desc list =
  list_descriptors host
  |> List.map ~f:(read_descriptor host)

let load_events (host: string) (desc : event_desc) : RTT.dval list =
  read_data host desc

let clear_events (host: string) : unit =
  Core_extended.Shell.rm ~r:() ~f:() (host_dir host)


let four_oh_four_to_yojson (((space, path, modifier), dvals) : four_oh_four) : Yojson.Safe.json =
  `List [ `String space
        ; `String path
        ; `String modifier
        ; `List (List.map ~f:Dval.dval_to_yojson dvals)
        ]

