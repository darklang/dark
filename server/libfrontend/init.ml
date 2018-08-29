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

type analysis_param = { handlers : handler_list
                      ; dbs : DbT.db list
                      } [@@deriving yojson]

let perform_analysis (str : string) : string =
  let { handlers ; dbs } =
    str
    |> Yojson.Safe.from_string
    |> analysis_param_of_yojson
    |> Result.ok_or_failwith
  in
  let input_vars = [] in

  handlers
  |> List.map ~f:(fun h ->
      ( h.tlid
      , [Execution.analyse_ast h.ast
          ~tlid:h.tlid
          ~exe_fn_ids:[]
          ~execution_id:(Types.id_of_int 1)
          ~account_id:(Util.create_uuid ())
          ~canvas_id:(Util.create_uuid ())
          ~user_fns:[]
          ~input_vars
          ~dbs
          ~load_fn_result:Execution.load_no_results
          ~store_fn_result:Execution.store_no_results
          ~load_fn_arguments:Execution.load_no_arguments
          ~store_fn_arguments:Execution.store_no_arguments]
      ))
  |> Analysis_types.analysis_result_list_to_yojson
  |> Yojson.Safe.to_string

