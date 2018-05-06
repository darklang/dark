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
 * /host/
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

let root = Config.Events

let name2num (name:string) : int =
  name
  |> String.chop_suffix_exn ~suffix:file_ext
  |> int_of_string

let num2name (num:int) : string =
  num
  |> string_of_int
  |> fun index -> index ^ file_ext


(* -------------------------- *)
(* Dirs *)
(* -------------------------- *)

let descriptor_dir (host: string) : string =
  host / "descriptors"

let eventdata_dir (host: string) (desc : event_desc) : string =
  host / "eventdata" / (desc_hash desc)

(* -------------------------- *)
(* Descriptors *)
(* -------------------------- *)
let read_descriptor (host: string) (hash : hash) : event_desc =
  (descriptor_dir host / hash)
  |> Util.readjsonfile ~root:Events ~conv:event_desc_of_yojson

let list_descriptors (host : string) : hash list =
  let dir = descriptor_dir host in
  Util.mkdir ~root dir;
  Util.lsdir  ~root dir

let save_descriptor (host: string) (desc: event_desc) : unit =
  let dir = descriptor_dir host in
  Util.mkdir ~root dir;
  let filename = dir / desc_hash desc in
  Util.writejsonfile ~root ~conv:event_desc_to_yojson ~value:desc filename



(* -------------------------- *)
(* Event data *)
(* -------------------------- *)
let list_data_files (host: string) (desc : event_desc) : string list =
  eventdata_dir host desc
  |> Util.lsdir ~root
  |> List.filter ~f:is_request

let read_data (host: string) (desc: event_desc) : RTT.dval list =
  let dir = eventdata_dir host desc in
  Util.mkdir ~root dir;
  dir
  |> Util.lsdir ~root
  |> List.map ~f:name2num
  |> List.sort ~cmp:Int.descending

  (* limit to 20 for now, show last 20 in reverse order *)
  |> fun l -> List.take l 20
  |> List.map ~f:num2name

  |> List.map ~f:(fun file ->
      dir / file
      |> Util.readjsonfile ~root ~conv:Dval.dval_of_yojson)

let next_data_filename (host: string) (desc : event_desc) : string =
  let dir = eventdata_dir host desc in
  Util.mkdir ~root dir;
  list_data_files host desc
  |> List.map ~f:name2num
  |> List.fold_left
    ~f:(fun acc x -> max acc x) ~init:0
  |> (+) 1
  |> num2name
  |> fun file -> dir / file

let save_data (host: string) (desc: event_desc) (dval : RTT.dval) =
  let filename = next_data_filename host desc in
  Util.writejsonfile ~root ~conv:Dval.dval_to_yojson ~value:dval filename



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
  Util.rmRF ~root host


let four_oh_four_to_yojson (((space, path, modifier), dvals) : four_oh_four) : Yojson.Safe.json =
  `List [ `String space
        ; `String path
        ; `String modifier
        ; `List (List.map ~f:Dval.dval_to_yojson dvals)
        ]

