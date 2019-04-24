open Core_kernel
open Libcommon
open Libexecution

(* We serialize oplists for each toplevel in the DB. This affects making
 * changes to almost any fundamental type in Dark, so must be
 * understood.
 *
 * The most important thing to understand is that if you make any change
 * you'll need to do a data migration.
 *
 * The oplists for toplevels are stored in per-tlid oplists, serialized
 * to binary. Since they're serialized to binary, they are not easily
 * editable.
 *
 * To do a migration, first make a copy of the type, making sure it has
 * a bin_prot serializer. That will allow you to read both the old and
 * new formats from the DB, which will allow live migrations without
 * breaking customers who are currently using the site.
 *
 * Then you need to migrate the data - we didn't used to do this and it
 * led to a lot of problems. All data should be in the same format (the
 * `digest` field in the DB should tell us what format it is). Data is
 * migrated by calling the /check-all-oplists endpoint. You should
 * check that it works locally first using scripts/download-gcp-db).
 * Write the code to do the migration in Canvas.check_all_hosts.
 *)

let digest = Op.bin_shape_oplist |> Bin_prot.Shape.eval_to_digest_string

let write_shape_data () =
  if Config.should_write_shape_data
  then
    let shape_string =
      Op.bin_shape_oplist
      |> Bin_prot.Shape.eval
      |> Bin_prot.Shape.Canonical.to_string_hum
    in
    File.writefile ~root:Serialization digest shape_string
  else ()


let is_test (name : string) : bool = String.is_prefix ~prefix:"test-" name

let json_filename name = name ^ "." ^ "json"

let try_multiple ~(fs : (string * ('a -> 'b)) list) (value : 'a) : 'b =
  let result =
    List.fold_left ~init:None fs ~f:(fun result (name, f) ->
        match result with
        | Some r ->
            result
        | None ->
          ( try Some (f value) with e ->
              let bt = Exception.get_backtrace () in
              Log.debuG ~bt name ~data:(Exception.exn_to_string e) ;
              None ) )
  in
  match result with Some r -> r | None -> Exception.internal "No fn worked"


(* ------------------------- *)
(* convert from deprecated *)
(* ------------------------- *)

(* let read_and_convert_deprecated str : Op.oplist = *)
(*   str *)
(*   |> Deprecated_op_flagged.oplist_of_string *)
(*   |> List.map ~f:Deprecated_op_flagged.convert_flagged *)
(*   |> Deprecated_op_flagged.oplist_to_yojson *)
(*   |> Op.oplist_of_yojson *)
(*   |> Result.ok_or_failwith *)

(* ------------------------- *)
(* oplists *)
(* ------------------------- *)
let strs2tlid_oplists strs : Op.tlid_oplists =
  strs
  |> List.map ~f:(fun results ->
         match results with
         | [data] ->
             data
         | _ ->
             Exception.internal "Shape of per_tlid oplists" )
  |> List.map ~f:(fun str ->
         let ops : Op.oplist =
           try_multiple str ~fs:[("oplist", Op.oplist_of_string)]
         in
         (* there must be at least one op *)
         let tlid = ops |> List.hd_exn |> Op.tlidOf |> Option.value_exn in
         (tlid, ops) )


let load_all_from_db ~host ~(canvas_id : Uuidm.t) () : Op.tlid_oplists =
  Db.fetch
    ~name:"load_all_from_db"
    "SELECT data FROM toplevel_oplists
     WHERE canvas_id = $1"
    ~params:[Uuid canvas_id]
    ~result:BinaryResult
  |> strs2tlid_oplists


let load_only_tlids ~host ~(canvas_id : Uuidm.t) ~(tlids : Types.tlid list) ()
    : Op.tlid_oplists =
  let tlid_params = List.map ~f:(fun x -> Db.ID x) tlids in
  Db.fetch
    ~name:"load_only_tlids"
    "SELECT data FROM toplevel_oplists
      WHERE canvas_id = $1
      AND tlid = ANY (string_to_array($2, $3)::bigint[])"
    ~params:[Db.Uuid canvas_id; Db.List tlid_params; String Db.array_separator]
    ~result:BinaryResult
  |> strs2tlid_oplists


let load_with_context
    ~host ~(canvas_id : Uuidm.t) ~(tlids : Types.tlid list) () :
    Op.tlid_oplists =
  let tlid_params = List.map ~f:(fun x -> Db.ID x) tlids in
  Db.fetch
    ~name:"load_with_context"
    "SELECT data FROM toplevel_oplists
      WHERE canvas_id = $1
      AND tlid = ANY (string_to_array($2, $3)::bigint[])
             OR tipe <> 'handler'::toplevel_type)"
    ~params:[Db.Uuid canvas_id; Db.List tlid_params; String Db.array_separator]
    ~result:BinaryResult
  |> strs2tlid_oplists


let load_all_dbs ~host ~(canvas_id : Uuidm.t) () : Op.tlid_oplists =
  Db.fetch
    ~name:"load_all_dbs"
    "SELECT data FROM toplevel_oplists
      WHERE canvas_id = $1
        AND tipe = 'db'::toplevel_type"
    ~params:[Db.Uuid canvas_id]
    ~result:BinaryResult
  |> strs2tlid_oplists


let load_for_http
    ~host ~(canvas_id : Uuidm.t) ~(path : string) ~(verb : string) () :
    Op.tlid_oplists =
  Db.fetch
    ~name:"load_for_http"
    (* The pattern `$2 like name` is deliberate, to leverage the DB's
     * pattern matching to solve our routing. *)
    "SELECT data FROM toplevel_oplists
      WHERE canvas_id = $1
        AND ((module = 'HTTP'
              AND $2 like name
              AND modifier = $3)
              OR tipe <> 'handler'::toplevel_type)"
    ~params:[Db.Uuid canvas_id; String path; String verb]
    ~result:BinaryResult
  |> strs2tlid_oplists


let load_for_cron ~host ~(canvas_id : Uuidm.t) () : Op.tlid_oplists =
  (* TODO: doesn't this need dbs and user_functions? *)
  Db.fetch
    ~name:"load_for_cron"
    "SELECT data FROM toplevel_oplists
      WHERE canvas_id = $1
        AND module = 'CRON'"
    ~params:[Db.Uuid canvas_id]
    ~result:BinaryResult
  |> strs2tlid_oplists


let save_toplevel_oplist
    ~(tlid : Types.tlid)
    ~(canvas_id : Uuidm.t)
    ~(account_id : Uuidm.t)
    ~tipe
    ~(name : string option)
    ~(module_ : string option)
    ~(modifier : string option)
    (ops : Op.oplist) : unit =
  let string_option o =
    match o with Some str -> Db.String str | None -> Db.Null
  in
  let tipe_str =
    match tipe with
    | Toplevel.TLDB ->
        "db"
    | Toplevel.TLHandler ->
        "handler"
    | Toplevel.TLUserFunction ->
        "user_function"
    | Toplevel.TLUserTipe ->
        "user_tipe"
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
    ~params:
      [ Uuid canvas_id
      ; Uuid account_id
      ; ID tlid
      ; String digest
      ; String tipe_str
      ; string_option name
      ; string_option module_
      ; string_option modifier
      ; Binary (Op.oplist_to_string ops) ]


(* ------------------------- *)
(* JSON *)
(* ------------------------- *)
let load_json_from_disk
    ~root ?(preprocess = ident) ~(host : string) ~(canvas_id : Uuidm.t) () :
    Op.tlid_oplists =
  Log.infO
    "serialization"
    ~params:[("load", "disk"); ("format", "json"); ("host", host)] ;
  let filename = json_filename host in
  File.maybereadjsonfile
    ~root
    filename
    ~conv:Op.oplist_of_yojson
    ~stringconv:preprocess
  |> Option.map ~f:Op.oplist2tlid_oplists
  |> Option.value ~default:[]


let save_json_to_disk ~root (filename : string) (ops : Op.tlid_oplists) : unit
    =
  Log.infO
    "serialization"
    ~params:[("save_to", "disk"); ("format", "json"); ("filename", filename)] ;
  ops
  |> Op.tlid_oplists2oplist
  |> Op.oplist_to_yojson
  |> Yojson.Safe.pretty_to_string
  |> (fun s -> s ^ "\n")
  |> File.writefile ~root filename


(* ------------------------- *)
(* hosts *)
(* ------------------------- *)
let current_hosts () : string list =
  Db.fetch ~name:"oplists" "SELECT DISTINCT name FROM canvases" ~params:[]
  |> List.map ~f:List.hd_exn
  |> List.filter ~f:(fun h -> not (String.is_prefix ~prefix:"test-" h))
  |> List.dedup_and_sort ~compare


let tier_one_hosts () : string list =
  [ "ian-httpbin"
  ; "paul-slackermuse"
  ; "listo"
  ; "ellen-battery2"
  ; "julius-tokimeki-unfollow" ]


(* https://stackoverflow.com/questions/15939902/is-select-or-insert-in-a-function-prone-to-race-conditions/15950324#15950324 *)
let fetch_canvas_id (owner : Uuidm.t) (host : string) : Uuidm.t =
  Db.fetch_one
    ~name:"fetch_canvas_id"
    "SELECT canvas_id($1, $2, $3)"
    ~params:[Uuid (Util.create_uuid ()); Uuid owner; String host]
  |> List.hd_exn
  |> Uuidm.of_string
  |> Option.value_exn
