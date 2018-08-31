open Core_kernel

open Libexecution
open Libcommon
open Types
open Types.RuntimeT


let init () =
  Libs.init [];
  print_endline "libfrontend reporting in"

let host = "test"

type handler_list = HandlerT.handler list [@@deriving yojson]
type analysis_list = Analysis_types.analysis list [@@deriving to_yojson]

let dval_of_yojson = Dval.dval_of_yojson
type input_vars = (string * dval) list (* list of vars *)
                  [@@deriving of_yojson]

(* We bring in our own definition because the deserializer is in Dval but the
 * definition is in Types, and that's challenging *)

type our_tipe = tipe

let our_tipe_of_yojson json =
    Dval.tipe_of_yojson json

type our_col = string or_blank * our_tipe or_blank
             [@@deriving of_yojson]

type our_db = { tlid: tlid
              ; name: string
              ; cols: our_col list
              ; version: int
              ; old_migrations : DbT.db_migration list
              ; active_migration : DbT.db_migration option
              } [@@deriving of_yojson]

let convert_tipe (tipe: our_tipe or_blank) : tipe_ or_blank =
  match tipe with
  | Blank _ -> tipe
  | Filled (id, t) -> Filled (id, t)

let convert_col ((name, tipe): our_col) : DbT.col =
  (name, tipe)


let convert_db (db : our_db) : DbT.db =
  { tlid = db.tlid
  ; name = db.name
  ; cols = List.map ~f:convert_col db.cols
  ; version = db.version
  ; old_migrations = db.old_migrations
  ; active_migration = db.active_migration
  }

type analysis_param = { handlers : handler_list
                      (* for each handler, list of input_vars each representing
                       * a single request *)
                      ; input_vars : input_vars list list
                      ; dbs : our_db list
                      ; user_fns : user_fn list
                      } [@@deriving of_yojson]



let perform_analysis (str : string) : string =
  let { handlers; dbs; user_fns; input_vars } =
    str
    |> Yojson.Safe.from_string
    |> analysis_param_of_yojson
    |> Result.ok_or_failwith
  in
  let dbs : DbT.db list = List.map ~f:convert_db dbs in

  handlers
  |> List.zip_exn input_vars
  |> List.map ~f:(fun (input_vars_list, h) ->
      let input_vars_list =
        if input_vars_list = []
        then [Execution.sample_input_vars h]
        else input_vars_list
      in
      ( h.tlid
      , List.map input_vars_list
          ~f:(fun input_vars ->
              Execution.analyse_ast h.ast
                ~tlid:h.tlid
                ~exe_fn_ids:[]
                ~execution_id:(Types.id_of_int 1)
                ~account_id:(Util.create_uuid ())
                ~canvas_id:(Util.create_uuid ())
                ~input_vars
                ~dbs
                ~user_fns
                ~load_fn_result:Execution.load_no_results
                ~load_fn_arguments:Execution.load_no_arguments)))
  |> Analysis_types.analysis_result_list_to_yojson
  |> Yojson.Safe.to_string

