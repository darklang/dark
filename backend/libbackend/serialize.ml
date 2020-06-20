open Core_kernel
open Libcommon
open Libexecution
module SF = Serialization_format
module Span = Telemetry.Span

let handler_of_binary_string (str : string) : Types.RuntimeT.HandlerT.handler =
  Core_extended.Bin_io_utils.of_line
    str
    (SF.RuntimeT.HandlerT.bin_handler SF.RuntimeT.bin_expr)
  |> Serialization_converters.handler_to_fluid


let handler_to_binary_string (h : Types.RuntimeT.HandlerT.handler) : string =
  h
  |> Serialization_converters.handler_of_fluid
  |> Core_extended.Bin_io_utils.to_line
       (SF.RuntimeT.HandlerT.bin_handler SF.RuntimeT.bin_expr)
  |> Bigstring.to_string


let db_of_binary_string (str : string) : Types.RuntimeT.DbT.db =
  Core_extended.Bin_io_utils.of_line
    str
    (SF.RuntimeT.DbT.bin_db SF.RuntimeT.bin_expr)
  |> Serialization_converters.db_to_fluid


let db_to_binary_string (db : Types.RuntimeT.DbT.db) : string =
  db
  |> Serialization_converters.db_of_fluid
  |> Core_extended.Bin_io_utils.to_line
       (SF.RuntimeT.DbT.bin_db SF.RuntimeT.bin_expr)
  |> Bigstring.to_string


let user_fn_of_binary_string (str : string) : Types.RuntimeT.user_fn =
  Core_extended.Bin_io_utils.of_line
    str
    (SF.RuntimeT.bin_user_fn SF.RuntimeT.bin_expr)
  |> Serialization_converters.user_fn_to_fluid


let user_fn_to_binary_string (ufn : Types.RuntimeT.user_fn) : string =
  ufn
  |> Serialization_converters.user_fn_of_fluid
  |> Core_extended.Bin_io_utils.to_line
       (SF.RuntimeT.bin_user_fn SF.RuntimeT.bin_expr)
  |> Bigstring.to_string


let user_tipe_of_binary_string (str : string) : Types.RuntimeT.user_tipe =
  Core_extended.Bin_io_utils.of_line str SF.RuntimeT.bin_user_tipe


let user_tipe_to_binary_string (ut : Types.RuntimeT.user_tipe) : string =
  ut
  |> Core_extended.Bin_io_utils.to_line SF.RuntimeT.bin_user_tipe
  |> Bigstring.to_string


let oplist_to_binary_string (ops : Types.oplist) : string =
  ops
  |> Serialization_converters.oplist_of_fluid
  |> Core_extended.Bin_io_utils.to_line (SF.bin_oplist SF.RuntimeT.bin_expr)
  |> Bigstring.to_string


let oplist_of_binary_string (str : string) : Types.oplist =
  Core_extended.Bin_io_utils.of_line str (SF.bin_oplist SF.RuntimeT.bin_expr)
  |> Serialization_converters.oplist_to_fluid


let translate_handler_as_binary_string
    (str : string)
    ~(f : Types.RuntimeT.HandlerT.handler -> Types.RuntimeT.HandlerT.handler) :
    string =
  str |> handler_of_binary_string |> f |> handler_to_binary_string


let translate_db_as_binary_string
    (str : string) ~(f : Types.RuntimeT.DbT.db -> Types.RuntimeT.DbT.db) :
    string =
  str |> db_of_binary_string |> f |> db_to_binary_string


let translate_user_function_as_binary_string
    (str : string) ~(f : Types.RuntimeT.user_fn -> Types.RuntimeT.user_fn) :
    string =
  str |> user_fn_of_binary_string |> f |> user_fn_to_binary_string


let translate_user_tipe_as_binary_string
    (str : string) ~(f : Types.RuntimeT.user_tipe -> Types.RuntimeT.user_tipe) :
    string =
  str |> user_tipe_of_binary_string |> f |> user_tipe_to_binary_string


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

let digest =
  SF.bin_shape_oplist SF.RuntimeT.bin_shape_expr
  |> Bin_prot.Shape.eval_to_digest_string


let write_shape_data () =
  if Config.should_write_shape_data
  then
    let shape_string =
      SF.bin_shape_oplist SF.RuntimeT.bin_shape_expr
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
          ( try Some (f value)
            with e ->
              let bt = Exception.get_backtrace () in
              Log.debuG ~bt name ~data:(Exception.exn_to_string e) ;
              None ))
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
let strs2tlid_oplists strs : Types.tlid_oplists =
  strs
  |> List.map ~f:(fun results ->
         match results with
         | [data] ->
             data
         | _ ->
             Exception.internal "Shape of per_tlid oplists")
  |> List.map ~f:(fun str ->
         let ops : Types.oplist =
           try_multiple str ~fs:[("oplist", oplist_of_binary_string)]
         in
         (* there must be at least one op *)
         let tlid = ops |> List.hd_exn |> Op.tlidOf in
         (tlid, ops))


type rendered_oplist_cache_query_result =
  (Types.RuntimeT.HandlerT.handler * Types.pos) Types.IDMap.t
  * (Types.RuntimeT.DbT.db * Types.pos) Types.IDMap.t
  * Types.RuntimeT.user_fn Types.IDMap.t
  * Types.RuntimeT.user_tipe Types.IDMap.t

let strs2rendered_oplist_cache_query_result strs :
    rendered_oplist_cache_query_result =
  let module IDMap = Types.IDMap in
  let handlers = IDMap.empty in
  let dbs = IDMap.empty in
  let user_fns = IDMap.empty in
  let user_tipes = IDMap.empty in
  let try_parse ~f data = try Some (f data) with _ -> None in
  strs
  |> List.map ~f:(fun results ->
         match results with
         | [data; pos] ->
             let pos =
               (* Yojson likes to raise if handed the empty string, which
                * it might be if `pos` was null (ie. for user functions) *)
               try
                 pos
                 |> Yojson.Safe.from_string
                 |> Types.pos_of_yojson
                 |> Result.ok
               with _ -> None
             in
             (data, pos)
         | _ ->
             Exception.internal "Shape of per_tlid cached reprs")
  |> List.fold
       ~init:(handlers, dbs, user_fns, user_tipes)
       ~f:(fun (handlers, dbs, user_fns, user_tipes) (str, pos) ->
         (* This is especially nasty because we don't know the
          * tlid of the blob we're trying to parse, so we have
          * to try all 4 of our various serializers -- which
          * helpfully throw exceptions which we have to catch for
          * control-flow
          *
          * Ordered as follows based on expected numbers
          * Fns > Dbs > Handlers > types
          *
          * *)
         match try_parse ~f:user_fn_of_binary_string str with
         | Some ufn ->
             ( handlers
             , dbs
             , IDMap.add_exn user_fns ~key:ufn.tlid ~data:ufn
             , user_tipes )
         | None ->
           ( match try_parse ~f:db_of_binary_string str with
           | Some db ->
               let dbs =
                 pos
                 |> Option.map ~f:(fun pos ->
                        IDMap.add_exn dbs ~key:db.tlid ~data:(db, pos))
                 |> Option.value ~default:dbs
               in
               (handlers, dbs, user_fns, user_tipes)
           | None ->
             ( match try_parse ~f:handler_of_binary_string str with
             | Some h ->
                 let handlers =
                   pos
                   |> Option.map ~f:(fun pos ->
                          IDMap.add_exn handlers ~key:h.tlid ~data:(h, pos))
                   |> Option.value ~default:handlers
                 in
                 (handlers, dbs, user_fns, user_tipes)
             | None ->
               ( match try_parse ~f:user_tipe_of_binary_string str with
               | Some t ->
                   ( handlers
                   , dbs
                   , user_fns
                   , IDMap.add_exn user_tipes ~key:t.tlid ~data:t )
               | None ->
                   (handlers, dbs, user_fns, user_tipes) ) ) ))


let load_all_from_db ~host ~(canvas_id : Uuidm.t) () : Types.tlid_oplists =
  Db.fetch
    ~name:"load_all_from_db"
    "SELECT data FROM toplevel_oplists
     WHERE canvas_id = $1"
    ~params:[Uuid canvas_id]
    ~result:BinaryResult
  |> strs2tlid_oplists


let load_only_tlids ~host ~(canvas_id : Uuidm.t) ~(tlids : Types.tlid list) () :
    Types.tlid_oplists =
  let tlid_params = List.map ~f:(fun x -> Db.ID x) tlids in
  Db.fetch
    ~name:"load_only_tlids"
    "SELECT data FROM toplevel_oplists
      WHERE canvas_id = $1
      AND tlid = ANY (string_to_array($2, $3)::bigint[])"
    ~params:[Db.Uuid canvas_id; Db.List tlid_params; String Db.array_separator]
    ~result:BinaryResult
  |> strs2tlid_oplists


let load_only_undeleted_tlids
    ~host ~(canvas_id : Uuidm.t) ~(tlids : Types.tlid list) () :
    Types.tlid_oplists =
  let tlid_params = List.map ~f:(fun x -> Db.ID x) tlids in
  Db.fetch
    ~name:"load_only_undeleted_tlids"
    "SELECT data FROM toplevel_oplists
      WHERE canvas_id = $1
      AND tlid = ANY (string_to_array($2, $3)::bigint[])
      AND deleted IS NOT TRUE"
    ~params:[Db.Uuid canvas_id; Db.List tlid_params; String Db.array_separator]
    ~result:BinaryResult
  |> strs2tlid_oplists


(* This is a special `load_*` function that specifically loads toplevels
 * via the `rendered_oplist_cache` column on `toplevel_oplists`. This column
 * stores a binary-serialized representation of the toplevel after the oplist
 * is applied. This should be much faster because we don't have to ship
 * the full oplist across the network from Postgres to the OCaml boxes,
 * and similarly they don't have to apply the full history of the canvas
 * in memory before they can execute the code.
 * *)
let load_only_rendered_tlids
    ~host ~(canvas_id : Uuidm.t) ~(tlids : Types.tlid list) () :
    rendered_oplist_cache_query_result =
  let tlid_params = List.map ~f:(fun x -> Db.ID x) tlids in
  (* We specifically only load where `deleted` IS FALSE (even though the column is nullable). This
   * means we will not load undeleted handlers from the cache if we've never written their `deleted` state. This
   * is less efficient, but still correct, as they'll still be loaded via their oplist. It avoids loading deleted
   * handlers that have had their cached version written but never their deleted state, which could be true for
   * some handlers that were touched between the addition of the `rendered_oplist_cache` column and the addition
   * of the `deleted` column. *)
  Db.fetch
    ~name:"load_only_rendered_tlids"
    "SELECT rendered_oplist_cache, pos FROM toplevel_oplists
      WHERE canvas_id = $1
      AND tlid = ANY (string_to_array($2, $3)::bigint[])
      AND deleted IS FALSE
      AND (((tipe = 'handler'::toplevel_type OR tipe = 'db'::toplevel_type) AND pos IS NOT NULL)
             OR tipe = 'user_function'::toplevel_type OR tipe = 'user_tipe'::toplevel_type)"
    ~params:[Db.Uuid canvas_id; Db.List tlid_params; String Db.array_separator]
    ~result:BinaryResult
  |> strs2rendered_oplist_cache_query_result


let load_with_dbs ~host ~(canvas_id : Uuidm.t) ~(tlids : Types.tlid list) () :
    Types.tlid_oplists =
  let tlid_params = List.map ~f:(fun x -> Db.ID x) tlids in
  Db.fetch
    ~name:"load_with_dbs"
    "SELECT data FROM toplevel_oplists
      WHERE canvas_id = $1
        AND (tlid = ANY (string_to_array($2, $3)::bigint[])
             OR tipe = 'db'::toplevel_type)"
    ~params:[Db.Uuid canvas_id; Db.List tlid_params; String Db.array_separator]
    ~result:BinaryResult
  |> strs2tlid_oplists


let fetch_relevant_tlids_for_http ~host ~canvas_id ~path ~verb () :
    Types.tlid list =
  Db.fetch
    ~name:"fetch_relevant_tlids_for_http"
    (* The pattern `$2 like name` is deliberate, to leverage the DB's
     * pattern matching to solve our routing. *)
    "SELECT tlid FROM toplevel_oplists
      WHERE canvas_id = $1
        AND ((module = 'HTTP'
              AND $2 like name
              AND modifier = $3)
              OR tipe <> 'handler'::toplevel_type)"
    ~params:[Db.Uuid canvas_id; String path; String verb]
  |> List.map ~f:(fun l ->
         match l with
         | [data] ->
             Types.id_of_string data
         | _ ->
             Exception.internal "Shape of per_tlid oplists")


let fetch_relevant_tlids_for_execution ~host ~canvas_id () : Types.tlid list =
  Db.fetch
    ~name:"fetch_relevant_tlids_for_execution"
    "SELECT tlid FROM toplevel_oplists
      WHERE canvas_id = $1
      AND tipe <> 'handler'::toplevel_type"
    ~params:[Db.Uuid canvas_id]
  |> List.map ~f:(fun l ->
         match l with
         | [data] ->
             Types.id_of_string data
         | _ ->
             Exception.internal "Shape of per_tlid oplists")


let fetch_relevant_tlids_for_event ~(event : Event_queue.t) ~canvas_id () :
    Types.tlid list =
  Db.fetch
    ~name:"fetch_relevant_tlids_for_event"
    "SELECT tlid FROM toplevel_oplists
      WHERE canvas_id = $1
        AND ((module = $2
              AND name = $3
              AND modifier = $4)
              OR tipe <> 'handler'::toplevel_type)"
    ~params:
      [ Db.Uuid canvas_id
      ; String event.space
      ; String event.name
      ; String event.modifier ]
  |> List.map ~f:(fun l ->
         match l with
         | [data] ->
             Types.id_of_string data
         | _ ->
             Exception.internal "Shape of per_tlid oplists")


let fetch_relevant_tlids_for_cron_checker ~canvas_id () : Types.tlid list =
  Db.fetch
    ~name:"fetch_relevant_tlids_for_cron_checker"
    "SELECT tlid FROM toplevel_oplists
      WHERE canvas_id = $1
      AND module = 'CRON'"
    ~params:[Db.Uuid canvas_id]
  |> List.map ~f:(fun l ->
         match l with
         | [data] ->
             Types.id_of_string data
         | _ ->
             Exception.internal "Shape of per_tlid oplists")


let fetch_tlids_for_all_dbs ~(canvas_id : Uuidm.t) () : Types.tlid list =
  Db.fetch
    ~name:"fetch_tlids_for_all_dbs"
    "SELECT tlid FROM toplevel_oplists
      WHERE canvas_id = $1
        AND tipe = 'db'::toplevel_type"
    ~params:[Db.Uuid canvas_id]
  |> List.map ~f:(fun l ->
         match l with
         | [data] ->
             Types.id_of_string data
         | _ ->
             Exception.internal "Shape of per_tlid oplists")


let fetch_all_tlids ~(canvas_id : Uuidm.t) () : Types.tlid list =
  Db.fetch
    ~name:"fetch_tlids_for_all_dbs"
    "SELECT tlid FROM toplevel_oplists
     WHERE canvas_id = $1"
    ~params:[Db.Uuid canvas_id]
  |> List.map ~f:(fun l ->
         match l with
         | [data] ->
             Types.id_of_string data
         | _ ->
             Exception.internal "Shape of per_tlid oplists")


let transactionally_migrate_oplist
    ~(canvas_id : Uuidm.t)
    ~host
    ~tlid
    ~(oplist_f : Types.oplist -> Types.oplist)
    ~(handler_f :
       Types.RuntimeT.HandlerT.handler -> Types.RuntimeT.HandlerT.handler)
    ~(db_f : Types.RuntimeT.DbT.db -> Types.RuntimeT.DbT.db)
    ~(user_fn_f : Types.RuntimeT.user_fn -> Types.RuntimeT.user_fn)
    ~(user_tipe_f : Types.RuntimeT.user_tipe -> Types.RuntimeT.user_tipe)
    () : (string, unit) Tc.Result.t =
  Log.inspecT "migrating oplists for" (host, tlid) ;
  try
    Db.transaction ~name:"oplist migration" (fun () ->
        let oplist, rendered =
          Db.fetch
            ~name:"load_all_from_db"
            (* SELECT FOR UPDATE locks row! *)
            "SELECT data, rendered_oplist_cache FROM toplevel_oplists
         WHERE canvas_id = $1
         AND tlid = $2
         FOR UPDATE"
            ~params:[Uuid canvas_id; ID tlid]
            ~result:BinaryResult
          |> List.hd_exn
          |> function
          | [data; rendered_oplist_cache] ->
              (oplist_of_binary_string data, rendered_oplist_cache)
          | _ ->
              Exception.internal "invalid oplists"
        in
        let try_convert f () = try Some (f rendered) with _ -> None in
        let rendered =
          if rendered = ""
          then Db.Null
          else
            try_convert (translate_handler_as_binary_string ~f:handler_f) ()
            |> Tc.Option.or_else_lazy
                 (try_convert (translate_db_as_binary_string ~f:db_f))
            |> Tc.Option.or_else_lazy
                 (try_convert
                    (translate_user_function_as_binary_string ~f:user_fn_f))
            |> Tc.Option.or_else_lazy
                 (try_convert
                    (translate_user_tipe_as_binary_string ~f:user_tipe_f))
            |> Tc.Option.map ~f:(fun str -> Db.Binary str)
            |> Tc.Option.or_else_lazy (fun () ->
                   Exception.internal "none of the decoders worked on the cache")
            |> Tc.Option.withDefault ~default:Db.Null
        in
        let converted_oplist = oplist |> oplist_f in
        Db.run
          ~name:"save per tlid oplist"
          "UPDATE toplevel_oplists
       SET data = $1,
           digest = $2,
           rendered_oplist_cache = $3
       WHERE canvas_id = $4
         AND tlid = $5"
          ~params:
            [ Binary (oplist_to_binary_string converted_oplist)
            ; String digest
            ; rendered
            ; Uuid canvas_id
            ; ID tlid ]) ;
    Ok ()
  with e -> Error (Exception.to_string e)


let save_toplevel_oplist
    ~(binary_repr : string option)
    ~(tlid : Types.tlid)
    ~(canvas_id : Uuidm.t)
    ~(account_id : Uuidm.t)
    ~tipe
    ~(deleted : bool option)
    ~(pos : Types.pos option)
    ~(name : string option)
    ~(module_ : string option)
    ~(modifier : string option)
    (ops : Types.oplist) : unit =
  let string_option o =
    match o with Some str -> Db.String str | None -> Db.Null
  in
  let binary_option o =
    match o with Some bin -> Db.Binary bin | None -> Db.Null
  in
  let bool_option o =
    match o with Some boolean -> Db.Bool boolean | None -> Db.Null
  in
  let tipe_str = Toplevel.tl_tipe_to_string tipe in
  let pos_option pos =
    match pos with
    | Some pos ->
        Db.String (pos |> Types.pos_to_yojson |> Yojson.Safe.to_string)
    | None ->
        Db.Null
  in
  Db.run
    ~name:"save per tlid oplist"
    "INSERT INTO toplevel_oplists
    (canvas_id, account_id, tlid, digest, tipe, name, module, modifier, data, rendered_oplist_cache, deleted, pos)
    VALUES ($1, $2, $3, $4, $5::toplevel_type, $6, $7, $8, $9, $10, $11, $12)
    ON CONFLICT (canvas_id, tlid) DO UPDATE
    SET account_id = $2,
        digest = $4,
        tipe = $5::toplevel_type,
        name = $6,
        module = $7,
        modifier = $8,
        data = $9,
        rendered_oplist_cache = $10,
        deleted = $11,
        pos = $12;
        "
    ~params:
      [ Uuid canvas_id
      ; Uuid account_id
      ; ID tlid
      ; String digest
      ; String tipe_str
      ; string_option name
      ; string_option module_
      ; string_option modifier
      ; Binary (ops |> oplist_to_binary_string)
      ; binary_option binary_repr
      ; bool_option deleted
      ; pos_option pos ]


(* ------------------------- *)
(* JSON *)
(* ------------------------- *)
let load_json_from_disk
    ~root ?(preprocess = ident) ~(host : string) ~(canvas_id : Uuidm.t) () :
    Types.tlid_oplists =
  Log.infO
    "serialization"
    ~params:[("load", "disk"); ("format", "json"); ("host", host)] ;
  let filename = json_filename host in
  File.maybereadjsonfile
    ~root
    filename
    ~conv:(SF.oplist_of_yojson SF.RuntimeT.expr_of_yojson)
    ~stringconv:preprocess
  |> Option.map ~f:Serialization_converters.oplist_to_fluid
  |> Option.map ~f:Op.oplist2tlid_oplists
  |> Option.value ~default:[]


let save_json_to_disk ~root (filename : string) (ops : Types.tlid_oplists) :
    unit =
  Log.infO
    "serialization"
    ~params:[("save_to", "disk"); ("format", "json"); ("filename", filename)] ;
  ops
  |> Op.tlid_oplists2oplist
  |> Serialization_converters.oplist_of_fluid
  |> SF.oplist_to_yojson SF.RuntimeT.expr_to_yojson
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


let hosts_for (account_name : string) : string list =
  Db.fetch
    ~name:"hosts_for"
    "SELECT DISTINCT c.name
     FROM canvases c
     JOIN accounts acc ON c.account_id = acc.id
     WHERE acc.username = $1"
    ~params:[String account_name]
  |> List.map ~f:List.hd_exn
  |> List.dedup_and_sort ~compare


(* NB: this returns canvases an account has access to via an organization, not
 * the organization(s) themselves *)
let orgs_for (account_name : string) : string list =
  Db.fetch
    ~name:"fetch_orgs_for"
    "SELECT c.name
     FROM access
     INNER JOIN accounts as user_ on access.access_account = user_.id
     INNER JOIN accounts as org on access.organization_account = org.id
     INNER JOIN canvases as c on org.id = account_id
     WHERE user_.username = $1"
    ~params:[String account_name]
  |> List.map ~f:List.hd_exn
  |> List.dedup_and_sort ~compare


let orgs (account_name : string) : string list =
  Db.fetch
    ~name:"fetch_orgs"
    "SELECT org.username
     FROM access
     INNER JOIN accounts as user_ on access.access_account = user_.id
     INNER JOIN accounts as org on access.organization_account = org.id
     WHERE user_.username = $1"
    ~params:[String account_name]
  |> List.map ~f:List.hd_exn
  |> List.dedup_and_sort ~compare


let tier_one_hosts () : string list =
  [ "ian-httpbin"
  ; "paul-slackermuse"
  ; "listo"
  ; "ellen-battery2"
  ; "julius-tokimeki-unfollow" ]


(* https://stackoverflow.com/questions/15939902/is-select-or-insert-in-a-function-prone-to-race-conditions/15950324#15950324 *)
let fetch_canvas_id (owner : Uuidm.t) (host : string) : Uuidm.t =
  let host_length = String.length host in
  if host_length > 64
  then
    Exception.internal
      (Printf.sprintf "Canvas name was %i chars, must be <= 64." host_length)
  else
    Db.fetch_one
      ~name:"fetch_canvas_id"
      "SELECT canvas_id($1, $2, $3)"
      ~params:[Uuid (Util.create_uuid ()); Uuid owner; String host]
    |> List.hd_exn
    |> Uuidm.of_string
    |> Option.value_exn


type cron_schedule_data =
  { canvas_id : Uuidm.t
  ; owner : Uuidm.t
  ; host : string
  ; tlid : string
  ; name : string
  ; modifier : string }

(** Fetch cron handlers from the DB. Active here means:
 * - a non-null interval field in the spec
 * - not deleted (When a CRON handler is deleted, we set (module, modifier,
 *   deleted) to (NULL, NULL, True);  so our query `WHERE module = 'CRON'`
 *   ignores deleted CRONs.)
 *)
let fetch_active_crons (span : Span.t) : cron_schedule_data list =
  Telemetry.with_span span "Serialize.fetch_crons" (fun _ ->
      Db.fetch
        ~name:"get crons for scheduler"
        "SELECT canvas_id,
                tlid,
                modifier,
                toplevel_oplists.name,
                toplevel_oplists.account_id,
                canvases.name
         FROM toplevel_oplists
         JOIN canvases ON toplevel_oplists.canvas_id = canvases.id
         WHERE module = 'CRON'
           AND modifier IS NOT NULL
           AND toplevel_oplists.name IS NOT NULL"
        ~params:[]
      |> List.map ~f:(function
             | [canvas_id; tlid; modifier; name; account_id; host] ->
                 { canvas_id = canvas_id |> Uuidm.of_string |> Option.value_exn
                 ; tlid
                 ; modifier
                 ; name
                 ; owner = account_id |> Uuidm.of_string |> Option.value_exn
                 ; host }
             | _ ->
                 Exception.internal
                   "Wrong shape from get_crons_for_scheduler query"))
