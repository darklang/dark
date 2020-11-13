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

let shape_string =
  SF.bin_shape_oplist SF.RuntimeT.bin_shape_expr
  |> Bin_prot.Shape.eval
  |> Bin_prot.Shape.Canonical.to_string_hum


let is_test (name : string) : bool = String.is_prefix ~prefix:"test-" name

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


