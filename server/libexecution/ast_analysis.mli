open Core_kernel

(* ----------------- *)
(* Types *)
(* ----------------- *)
type dval_store = Types.RuntimeT.dval Int.Table.t

module SymSet = String.Set

type sym_set = SymSet.t
type sym_store = sym_set Int.Table.t

type livevalue = { value: string
                 ; tipe: string [@key "type"]
                 ; json: string
                 ; exc: Exception.exception_data option
                 } [@@deriving to_yojson, show]

type sym_list = (string * livevalue) list
                [@@deriving to_yojson]

type analysis =
  { ast_value: livevalue
  ; live_values : dval_store
  ; available_varnames : sym_store
  ; input_values : sym_list
  } [@@deriving to_yojson]

type analysis_list = analysis list
                     [@@deriving to_yojson]


(* ----------------- *)
(* Functions *)
(* ----------------- *)

val store_no_results :
  Types.RuntimeT.function_desc ->
  Types.RuntimeT.dval list ->
  Types.RuntimeT.dval ->
  unit

val load_no_results:
  Types.RuntimeT.function_desc ->
  Types.RuntimeT.dval list ->
  (Types.RuntimeT.dval * Time.t) option

val environment_for_user_fn :
  Types.RuntimeT.user_fn ->
  Types.RuntimeT.dval_map

val execute_user_fn_for_analysis :
  Types.RuntimeT.exec_state ->
  Types.RuntimeT.user_fn ->
  analysis

val execute_handler_for_analysis :
  Types.RuntimeT.exec_state ->
  Types.RuntimeT.HandlerT.handler ->
  analysis

val execute_handler :
  Types.RuntimeT.exec_state ->
  Types.RuntimeT.HandlerT.handler ->
  Types.RuntimeT.dval

val execute_ast :
  Types.RuntimeT.exec_state ->
  Types.RuntimeT.dval_map ->
  Types.RuntimeT.expr ->
  Types.RuntimeT.dval

val execute_userfn :
  Types.RuntimeT.exec_state ->
  string ->
  Types.id ->
  Types.RuntimeT.dval list ->
  Types.RuntimeT.dval

val store_no_arguments :
  Types.RuntimeT.user_fn_desc ->
  Types.RuntimeT.dval_map ->
  unit

val load_no_arguments :
  Types.RuntimeT.user_fn_desc ->
  (Types.RuntimeT.dval_map * Time.t) list

