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
let save_oplists ~(host: string) ~(digest: string) (data: string) : unit =
  Db.run
    ~name:"save_oplists"
    "INSERT INTO oplists
    (host, digest, data)
    VALUES ($1, $2, $3)
    ON CONFLICT (host, digest) DO UPDATE
    SET data = $3;"
    ~params:[String host; String digest; Binary data]



let load_oplists ~(host: string) ~(digest: string) : string option =
  Db.fetch_one_option
    ~name:"load_oplists"
    "SELECT data FROM oplists
     WHERE host = $1
     AND digest = $2;"
    ~params:[String host; String digest]
    ~result:BinaryResult
  |> Option.map ~f:List.hd_exn

let load_per_tlid_oplists (canvas_id: Uuidm.t) : string list =
  Db.fetch
    ~name:"load_per_tlid_oplists"
    "SELECT data FROM toplevel_oplists
     WHERE canvas_id = $1"
    ~params:[Uuid canvas_id]
    ~result:BinaryResult
  |> List.map
    ~f:(fun results ->
        match results with
        | [data] -> data
        | _ -> Exception.internal "Shape of per_tlid oplists")

let save_toplevel_oplist
    ~(tlid:Types.tlid) ~(canvas_id: Uuidm.t) ~(account_id: Uuidm.t)
    ~(name:string option) ~(module_: string option)
    ~(modifier:string option)
    (ops:Op.oplist)
  : unit =
  let string_option o =
    match o with
    | Some str -> Db.String str
    | None -> Db.Null
  in
  Db.run
    ~name:"save per tlid oplist"
    "INSERT INTO toplevel_oplists
    (canvas_id, account_id, tlid, digest, name, module, modifier, data)
    VALUES ($1, $2, $3, $4, $5, $6, $7, $8)
    ON CONFLICT (canvas_id, tlid) DO UPDATE
    SET account_id = $2,
        digest = $4,
        name = $5,
        module = $6,
        modifier = $7,
        data = $8;"
    ~params:[ Uuid canvas_id
            ; Uuid account_id
            ; Int tlid
            ; String digest
            ; string_option name
            ; string_option module_
            ; string_option modifier
            ; Binary (Op.oplist_to_string ops)]

let load_json_oplists ~(host: string) : string option =
  Db.fetch_one_option
    ~name:"load_json_oplists"
    "SELECT data FROM json_oplists
     WHERE host = $1"
    ~params:[String host]
  |> Option.map ~f:List.hd_exn

let save_json_oplists ~(host: string) ~(digest: string) (data: string) : unit =
  (* this is an upsert *)
  Db.run
    ~name:"save_json_oplists"
    "INSERT INTO json_oplists
    (host, digest, data)
    VALUES ($1, $2, $3)
    ON CONFLICT (host) DO UPDATE
    SET data = $3,
        digest = $2;"
    ~params:[String host; String digest; String data]

let all_oplists () : string list =
  Db.fetch
    ~name:"oplists"
    "SELECT DISTINCT host FROM oplists
     UNION
     SELECT DISTINCT host FROM json_oplists"
    ~params:[]
  |> List.map ~f:List.hd_exn
  |> List.filter ~f:(fun h ->
      not (String.is_prefix ~prefix:"test-" h))



(* ------------------------- *)
(* JSON *)
(* ------------------------- *)
let load_json_from_disk ~root ?(preprocess=ident) ~(host:string)
    ~(canvas_id: Uuidm.t) () : Op.tlid_oplists option =
  Log.infO "serialization" ~params:[ "load", "disk"
                                   ; "format", "json"
                                   ; "host", host];
  let filename = json_unversioned_filename host in
  File.maybereadjsonfile ~root filename
    ~conv:Op.oplist_of_yojson ~stringconv:preprocess
  |> Option.map ~f:Op.oplist2tlid_oplists

let load_json_from_db ?(preprocess=ident) ~(host: string)
    ~(canvas_id: Uuidm.t) () : Op.tlid_oplists option =
  Log.infO "serialization" ~params:[ "load_from", "db"
                                   ; "format", "json"
                                   ; "host", host];
  host
  |> load_json_oplists
  |> Option.map ~f:(fun ops_string ->
      ops_string
      |> preprocess
      |> Yojson.Safe.from_string
      |> Op.oplist_of_yojson
      |> Result.ok_or_failwith
      |> Op.oplist2tlid_oplists)

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

let save_json_to_db (host: string) (ops: Op.tlid_oplists) : unit =
  Log.infO "serialization" ~params:[ "save_to", "db"
                                   ; "format", "json"
                                   ; "digest", digest
                                   ; "host", host];
  ops
  |> Op.tlid_oplists2oplist
  |> Op.oplist_to_yojson
  |> Yojson.Safe.to_string
  |> save_json_oplists ~host ~digest

(* ------------------------- *)
(* per-tlid oplists *)
(* ------------------------- *)
let load_all_from_db ~(host:string)
    ~(canvas_id: Uuidm.t) () : Op.tlid_oplists option =
  canvas_id
  |> load_per_tlid_oplists
  |> List.map ~f:(fun str ->
      let ops = Op.oplist_of_string str in
      (* there must be at least one op *)
      let tlid = ops |> List.hd_exn |> Op.tlidOf |> Option.value_exn in
      (tlid, ops))
  |> (fun ops ->
        if ops = []
        then None
        else Some ops)

(* save is in canvas.ml *)




(* ------------------------- *)
(* deserialing algorithm *)
(* ------------------------- *)
let deserialize_ordered
    (host : string)
    (canvas_id : Uuidm.t)
    (descs :
       (host:string -> canvas_id:Uuidm.t -> unit -> Op.tlid_oplists option) list)
    : Op.tlid_oplists =
  (* try each in turn. If the file exists, try it, and return if
    successful. If it fails, save the error and try the next one *)
  let (result, errors) =
    List.fold descs ~init:(None, [])
      ~f:(fun (prev, errors) fn ->
          match prev with
          | Some r -> (prev, errors)
          | None ->
            (try
               match fn ~host ~canvas_id () with
               | Some oplist -> (Some oplist, errors)
               | None -> (prev, errors)
             with
             | e ->
               Log.erroR "deserialization" ~data:(Exn.to_string e);
               (None, (errors @ [e]))))
  in
  match (result, errors) with
  | (Some r, _) -> r
  | (None, []) -> []
  | (None, es) ->
    let msgs =
      List.mapi ~f:(fun i ex -> (string_of_int i, Exn.to_string ex))
        es
    in
    Exception.internal ~info:msgs ("storage errors with " ^ host)

let load_from_backup ~host ~canvas_id ()
    : Op.tlid_oplists option =
  if is_test host
  then
    load_json_from_disk ~root:Testdata ~preprocess:ident
      ~host ~canvas_id ()
  else
    load_json_from_db ~preprocess:ident ~host ~canvas_id ()


let search_and_load (host: string) (canvas_id: Uuidm.t)
  : Op.tlid_oplists =
  (* This was the old way of doing a ton of deserializations and picking
   * the right one. However, it became easier and more correct to do
   * live migrations rather than layering things here, so this may end
   * up being useless. Keeping it around in case it's needed for a while
   * anyway. *)
  deserialize_ordered host canvas_id
    [ load_all_from_db
    ; load_from_backup
    ]

(* ------------------------- *)
(* hosts *)
(* ------------------------- *)
let current_hosts () : string list =
  all_oplists ()

(* https://stackoverflow.com/questions/15939902/is-select-or-insert-in-a-function-prone-to-race-conditions/15950324#15950324 *)
let fetch_canvas_id (owner:Uuidm.t) (host:string) : Uuidm.t =
  Db.fetch_one
    ~name:"fetch_canvas_id"
    "SELECT canvas_id($1, $2, $3)"
    ~params:[Uuid (Util.create_uuid ()); Uuid owner; String host]
  |> List.hd_exn
  |> Uuidm.of_string
  |> Option.value_exn



