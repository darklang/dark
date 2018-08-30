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

(* This is sent as a json string, so do extra parsing step *)
let dval_of_yojson (json: Yojson.Safe.json ) =
  match json with
  | `String str ->
    str
    |> Yojson.Safe.from_string
    |> Dval.dval_of_yojson
  | _ -> Exception.internal "wrong format"

type input_vars = (string * dval) list (* list of vars *)
                  [@@deriving of_yojson]

type analysis_param = { handlers : handler_list
                      (* for each handler, list of input_vars each representing a single request *)
                      ; input_vars : input_vars list list
                      ; dbs : DbT.db list
                      ; user_fns : user_fn list
                      } [@@deriving of_yojson]

let perform_analysis (str : string) : string =
  let { handlers; dbs; user_fns; input_vars } =
    str
    |> Yojson.Safe.from_string
    |> analysis_param_of_yojson
    |> Result.ok_or_failwith
  in

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
                ~store_fn_result:Execution.store_no_results
                ~load_fn_arguments:Execution.load_no_arguments
                ~store_fn_arguments:Execution.store_no_arguments)))
  |> Analysis_types.analysis_result_list_to_yojson
  |> Yojson.Safe.to_string

