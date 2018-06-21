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

val store_nothing :
  Types.RuntimeT.function_desc ->
  Types.RuntimeT.dval list ->
  Types.RuntimeT.dval ->
  unit

val load_nothing :
  Types.RuntimeT.function_desc ->
  Types.RuntimeT.dval list ->
  (Types.RuntimeT.dval * Time.t) option

val environment_for_user_fn : Types.RuntimeT.user_fn -> Types.RuntimeT.dval_map
val execute_function_for_analysis : Types.RuntimeT.exec_state -> Types.RuntimeT.user_fn -> analysis

val execute_handler_for_analysis : Types.RuntimeT.exec_state -> Handler.handler -> analysis
val execute_handler : Types.RuntimeT.exec_state -> Handler.handler -> Types.RuntimeT.dval
