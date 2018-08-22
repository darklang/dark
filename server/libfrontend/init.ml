open Core_kernel

open Libexecution
open Libcommon
open Types.RuntimeT

module Analysis = Libexecution.Ast_analysis


let init () =
  Libs.init [];
  print_endline "libfrontend reporting in"

let state env : Libexecution.Types.RuntimeT.exec_state =
  { tlid = Libexecution.Types.id_of_int 0
  ; host = "test"
  ; account_id = Libexecution.Util.create_uuid ()
  ; canvas_id = Libexecution.Util.create_uuid ()
  ; user_fns = []
  ; exe_fn_ids = []
  ; env = env
  ; fail_fn = None
  ; input_cursor = 0
  ; dbs = []
  ; execution_id = Libexecution.Types.id_of_int 1
  ; load_fn_result = Ast_analysis.load_no_results
  ; store_fn_result = Ast_analysis.store_no_results
  ; load_fn_arguments = Ast_analysis.load_no_arguments
  ; store_fn_arguments = Ast_analysis.store_no_arguments
  }

type handler_list = HandlerT.handler list [@@deriving yojson]
type analysis_list = Ast_analysis.analysis list [@@deriving to_yojson]

let perform_analysis (str : string) : string =
  let env = DvalMap.empty in
  let handlers =
    str
    |> Yojson.Safe.from_string
    |> handler_list_of_yojson
    |> Result.ok_or_failwith
  in
  handlers
  |> List.map ~f:(Ast_analysis.execute_handler_for_analysis (state env))
  |> analysis_list_to_yojson
  |> Yojson.Safe.to_string

