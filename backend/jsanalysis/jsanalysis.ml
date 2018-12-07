open Core_kernel
open Libexecution
open Libcommon
open Types
open Types.RuntimeT

let init () =
  Libs.init [] ;
  print_endline "libfrontend reporting in"


type handler_list = HandlerT.handler list [@@deriving yojson]

let dval_of_yojson = Dval.unsafe_dval_of_yojson

type input_vars = (string * dval) list (* list of vars *)
[@@deriving of_yojson]

(* We bring in our own definition because the deserializer is in Dval but the
 * definition is in Types, and that's challenging *)

type our_tipe = tipe

let our_tipe_of_yojson json = Dval.tipe_of_yojson json

type our_col = string or_blank * our_tipe or_blank [@@deriving of_yojson]

type our_db_migration =
  { starting_version : int
  ; version : int
  ; state : DbT.db_migration_state
  ; rollforward : expr
  ; rollback : expr
  ; cols : our_col list }
[@@deriving of_yojson]

type our_db =
  { tlid : tlid
  ; name : string
  ; cols : our_col list
  ; version : int
  ; old_migrations : our_db_migration list
  ; active_migration : our_db_migration option }
[@@deriving of_yojson]

let convert_col ((name, tipe) : our_col) : DbT.col = (name, tipe)

let convert_migration (m : our_db_migration) : DbT.db_migration =
  { starting_version = m.starting_version
  ; version = m.version
  ; state = m.state
  ; rollforward = m.rollforward
  ; rollback = m.rollback
  ; cols = List.map ~f:convert_col m.cols }


let convert_db (db : our_db) : DbT.db =
  { tlid = db.tlid
  ; name = db.name
  ; cols = List.map ~f:convert_col db.cols
  ; version = db.version
  ; old_migrations = List.map ~f:convert_migration db.old_migrations
  ; active_migration = Option.map ~f:convert_migration db.active_migration }


type analysis_param =
  { handler : HandlerT.handler
  ; trace : Analysis_types.trace
  ; dbs : our_db list
  ; user_fns : user_fn list }
[@@deriving of_yojson]

type analysis_envelope = uuid * Analysis_types.analysis [@@deriving to_yojson]

let load_from_trace results (tlid, fnname, caller_id) args :
    (dval * Time.t) option =
  results
  |> List.filter_map ~f:(fun (rfnname, rcaller_id, hash, dval) ->
         if fnname = rfnname && caller_id = rcaller_id && hash = Dval.hash args
         then Some dval
         else None )
  |> List.hd
  (* We don't use the time, so just hack it to get the interface right. *)
  |> Option.map ~f:(fun dv -> (dv, Time.now ()))


let perform_analysis (str : string) : string =
  let {handler; dbs; user_fns; trace} =
    str
    |> Yojson.Safe.from_string
    |> analysis_param_of_yojson
    |> Result.ok_or_failwith
  in
  let dbs : DbT.db list = List.map ~f:convert_db dbs in
  let input_vars = trace.input in
  ( trace.id
  , Execution.analyse_ast
      handler.ast
      ~tlid:handler.tlid
      ~execution_id:(Types.id_of_int 1)
      ~account_id:(Util.create_uuid ())
      ~canvas_id:(Util.create_uuid ())
      ~input_vars
      ~dbs
      ~user_fns
      ~load_fn_result:(load_from_trace trace.function_results)
      ~load_fn_arguments:Execution.load_no_arguments )
  |> analysis_envelope_to_yojson
  |> Yojson.Safe.to_string


open Js_of_ocaml

type js_string = Js.js_string Js.t

let () =
  init () ;
  Js.export
    "darkAnalysis"
    (object%js
       method performAnalysis (tlids : js_string) : js_string =
         try tlids |> Js.to_string |> perform_analysis |> Js.string with e ->
           print_endline ("Exception in jsanalysis: " ^ Exn.to_string e) ;
           Exception.reraise e
    end) ;
  ()
