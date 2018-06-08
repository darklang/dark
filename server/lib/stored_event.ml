open Core

module RTT = Types.RuntimeT

module Dbp = Dbprim

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
(* New *)
(* ------------------------- *)
let store_event_new (canvas_id: Uuid.t) _  ((module_, path, modifier): event_desc) (event: RTT.dval) : unit =
  Printf.sprintf
    "INSERT INTO stored_events
    (canvas_id, module, path, modifier, timestamp, value)
    VALUES (%s, %s, %s, %s, CURRENT_TIMESTAMP, %s)"
    (Dbp.uuid canvas_id)
    (Dbp.string module_)
    (Dbp.string path)
    (Dbp.string modifier)
    (Dbp.dvaljson event)
  |> Db.run_sql

let list_events_new (canvas_id: Uuid.t) _  : event_desc list =
  Printf.sprintf
    "SELECT DISTINCT module, path, modifier FROM stored_events
     WHERE canvas_id = %s"
    (Dbp.uuid canvas_id)
  |> Db.fetch_via_sql
  |> List.map ~f:(function
      | [module_; path; modifier] -> (module_, path, modifier)
      | _ -> Exception.internal "Bad DB format for stored_events")

let load_events_new (canvas_id: Uuid.t) _ ((module_, path, modifier): event_desc) : RTT.dval list =
  Printf.sprintf
    "SELECT value, timestamp FROM stored_events
    WHERE canvas_id = %s
      AND module = %s
      AND path = %s
      AND modifier = %s
    LIMIT 20"
    (Dbp.uuid canvas_id)
    (Dbp.string module_)
    (Dbp.string path)
    (Dbp.string modifier)
  |> Db.fetch_via_sql
  |> List.map ~f:(function
      | [dval; _ts] -> Dval.dval_of_json_string dval
      | _ -> Exception.internal "Bad DB format for stored_events")

let clear_events_new (canvas_id: Uuid.t) _ : unit =
   Printf.sprintf
    "DELETE FROM stored_events
     WHERE canvas_id = %s"
    (Dbp.uuid canvas_id)
  |> Db.run_sql


(* ------------------------- *)
(* Old *)
(* ------------------------- *)

let store_event_old _ (host : string) (desc : event_desc) (event: RTT.dval)
  : unit =
  save_descriptor host desc;
  save_data host desc event;
  ()

let list_events_old _ (host: string) : event_desc list =
  list_descriptors host
  |> List.map ~f:(read_descriptor host)

let load_events_old _ (host: string) (desc : event_desc) : RTT.dval list =
  read_data host desc

let clear_events_old _ (host: string) : unit =
  Util.rmRF ~root host


(* ------------------------- *)
(* Both *)
(* ------------------------- *)

let list_events_both canvas (host: string) : event_desc list =
  list_events_new canvas host @ list_events_old canvas host

let load_events_both canvas (host: string) (desc : event_desc) : RTT.dval list =
  load_events_new canvas host desc @ load_events_old canvas host desc

let clear_events_both canvas (host: string) : unit =
  clear_events_old canvas host;
  clear_events_new canvas host;
  ()

let four_oh_four_to_yojson (((space, path, modifier), dvals) : four_oh_four) : Yojson.Safe.json =
  `List [ `String space
        ; `String path
        ; `String modifier
        ; `List (List.map ~f:Dval.dval_to_yojson dvals)
        ]

let store_event = store_event_new
let load_events = load_events_both
let list_events = list_events_both
let clear_events = clear_events_both
