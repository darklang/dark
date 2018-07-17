open Core_kernel
open Libcommon
open Libexecution

(* We serialize oplists for each toplevel in the DB. This affect making
 * changes to almost any fundamental type in Dark, so must be
 * understood.
 *
 * The most important thing to understand is that if you make any change
 * you'll need to do a data migration.
 *
 * The oplists for toplevels are stored in per-tlid oplists, serialized
 * to binary. Since they're serialized to binary, they are not easily
 * editable. As a result, we also serialize the entire canvas to json in
 * the background. (We do the entire canvas rather than just individual
 * toplevels as it seems less likely to fail, and it was easier in the
 * first pass. It has significantly worse performance so we should
 * change it later, perhaps once we gain more confidence in everything).
 *
 * To do a migration, first change the type - that's enough to create
 * the new binary serialization functions. Then create a function to
 * preprocess the json in the old format into the new format. That will
 * allow live migrations without breaking customers who are currently
 * using the site.
 *
 * Then you need to migrate the data - we didn't used to do this and it
 * led to a lot of problems. All data should be in the same
 * format (the `digest` field in the DB should tell us what format it
 * is). Data is migrated by calling the /admin/check-all-oplists
 * endpoint. You should check that it works locally first using
 * scripts/download-gcp-db).
 *
 * (Note: at time of writing, the code isn't really set up to fully
 * support what this comment describes, but it's close enough, and
 * should be fully converted soon.)
 *)



let digest = Op.bin_shape_oplist
             |> Bin_prot.Shape.eval_to_digest_string

let write_shape_data () =
  if Config.should_write_shape_data
  then
    let shape_string = Op.bin_shape_oplist
                     |> Bin_prot.Shape.eval
                     |> Bin_prot.Shape.Canonical.to_string_hum
    in
    File.writefile ~root:Serialization digest shape_string
  else
    ()

let is_test (name: string) : bool =
  String.is_prefix ~prefix:"test_" name
  || String.is_prefix ~prefix:"test-" name

let json_unversioned_filename name =
  name ^ "." ^ "json"

(* ------------------------- *)
(* oplists *)
(* ------------------------- *)
let strs2tlid_oplists strs : Op.tlid_oplists =
  strs
  |> List.map
   ~f:(fun results ->
       match results with
       | [data] -> data
       | _ -> Exception.internal "Shape of per_tlid oplists")
  |> List.map ~f:(fun str ->
      let ops = Op.oplist_of_string str in
      (* there must be at least one op *)
      let tlid = ops |> List.hd_exn |> Op.tlidOf |> Option.value_exn in
      (tlid, ops))

let load_all_from_db ~host ~(canvas_id: Uuidm.t) () : Op.tlid_oplists =
  Db.fetch
    ~name:"load_per_tlid_oplists"
    "SELECT data FROM toplevel_oplists
     WHERE canvas_id = $1"
    ~params:[Uuid canvas_id]
    ~result:BinaryResult
  |> strs2tlid_oplists

let load_only_for_tlids ~host ~(canvas_id: Uuidm.t)
    ~(tlids: Types.tlid list) () : Op.tlid_oplists =
  let tlid_params = tlids
                    |> List.map ~f:Int.to_string
                    |> String.concat ~sep:", "
  in
  Db.fetch
    ~name:"load_only_for_tilds"
    ("SELECT data FROM toplevel_oplists
      WHERE canvas_id = $1
        AND (tlid = ANY('{" ^ tlid_params ^ "}'::bigint[])
             OR tipe <> 'handler'::toplevel_type)")
    ~params:[Db.Uuid canvas_id]
    ~result:BinaryResult
  |> strs2tlid_oplists

let load_for_http ~host ~(canvas_id: Uuidm.t)
    ~(path: string) ~(verb: string) () : Op.tlid_oplists =
  Db.fetch
    ~name:"load_for_http"
    (* The pattern `$2 like name` is deliberate, to leverage the DB's
     * pattern matching to solve our routing. *)
    ("SELECT data FROM toplevel_oplists
      WHERE canvas_id = $1
        AND ((module = 'HTTP'
              AND $2 like name
              AND modifier = $3)
              OR tipe <> 'handler'::toplevel_type)")
    ~params:[ Db.Uuid canvas_id
            ; String path
            ; String verb]
    ~result:BinaryResult
  |> strs2tlid_oplists


let save_toplevel_oplist
    ~(tlid:Types.tlid) ~(canvas_id: Uuidm.t) ~(account_id: Uuidm.t)
    ~(tipe)
    ~(name:string option) ~(module_: string option)
    ~(modifier:string option)
    (ops:Op.oplist)
  : unit =
  let string_option o =
    match o with
    | Some str -> Db.String str
    | None -> Db.Null
  in
  let tipe_str =
    match tipe with
    | `DB -> "db"
    | `Handler -> "handler"
    | `User_function -> "user_function"
  in

  Db.run
    ~name:"save per tlid oplist"
    "INSERT INTO toplevel_oplists
    (canvas_id, account_id, tlid, digest, tipe, name, module, modifier, data)
    VALUES ($1, $2, $3, $4, $5::toplevel_type, $6, $7, $8, $9)
    ON CONFLICT (canvas_id, tlid) DO UPDATE
    SET account_id = $2,
        digest = $4,
        tipe = $5::toplevel_type,
        name = $6,
        module = $7,
        modifier = $8,
        data = $9;"
    ~params:[ Uuid canvas_id
            ; Uuid account_id
            ; Int tlid
            ; String digest
            ; String tipe_str
            ; string_option name
            ; string_option module_
            ; string_option modifier
            ; Binary (Op.oplist_to_string ops)]


(* ------------------------- *)
(* JSON *)
(* ------------------------- *)
let load_json_from_disk ~root ?(preprocess=ident) ~(host:string)
    ~(canvas_id: Uuidm.t) () : Op.tlid_oplists =
  Log.infO "serialization" ~params:[ "load", "disk"
                                   ; "format", "json"
                                   ; "host", host];
  let filename = json_unversioned_filename host in
  File.maybereadjsonfile ~root filename
    ~conv:Op.oplist_of_yojson ~stringconv:preprocess
  |> Option.map ~f:Op.oplist2tlid_oplists
  |> Option.value ~default:[]

let save_json_to_disk ~root (filename: string) (ops: Op.tlid_oplists) : unit =
  Log.infO "serialization" ~params:[ "save_to", "disk"
                                   ; "format", "json"
                                   ; "filename", filename];
  ops
  |> Op.tlid_oplists2oplist
  |> Op.oplist_to_yojson
  |> Yojson.Safe.pretty_to_string
  |> (fun s -> s ^ "\n")
  |> File.writefile ~root filename

(* ------------------------- *)
(* per-tlid oplists *)
(* ------------------------- *)


(* save is in canvas.ml *)


(* ------------------------- *)
(* hosts *)
(* ------------------------- *)
let current_hosts () : string list =
  Db.fetch
    ~name:"oplists"
    "SELECT DISTINCT host FROM oplists
     UNION
     SELECT DISTINCT host FROM json_oplists
     UNION
     SELECT DISTINCT name FROM canvases"
    ~params:[]
  |> List.map ~f:List.hd_exn
  |> List.filter ~f:(fun h ->
      not (String.is_prefix ~prefix:"test-" h))
  |> List.dedup_and_sort ~compare




(* https://stackoverflow.com/questions/15939902/is-select-or-insert-in-a-function-prone-to-race-conditions/15950324#15950324 *)
let fetch_canvas_id (owner:Uuidm.t) (host:string) : Uuidm.t =
  Db.fetch_one
    ~name:"fetch_canvas_id"
    "SELECT canvas_id($1, $2, $3)"
    ~params:[Uuid (Util.create_uuid ()); Uuid owner; String host]
  |> List.hd_exn
  |> Uuidm.of_string
  |> Option.value_exn



