open Core_kernel
open Libexecution
open Types
open Analysis_types

type 'expr_type oplist = 'expr_type Op.op list [@@deriving yojson]

type 'expr_type add_op_rpc_params =
  { ops : 'expr_type oplist
  ; opCtr : int
        (* option means that we can still deserialize if this field is null, as
         * doc'd at https://github.com/ocaml-ppx/ppx_deriving_yojson *)
  ; clientOpCtrId : string option }
[@@deriving yojson]

type db_stats_rpc_params = {tlids : tlid list} [@@deriving yojson]

type get_trace_data_rpc_params =
  { tlid : tlid
  ; trace_id : traceid }
[@@deriving yojson]

type 'expr_type execute_function_rpc_params =
  { tlid : tlid
  ; trace_id : RuntimeT.uuid
  ; caller_id : id
  ; args : 'expr_type RuntimeT.dval list
  ; fnname : string }
[@@deriving yojson]

type 'expr_type upload_function_rpc_params = {fn : 'expr_type RuntimeT.user_fn}
[@@deriving yojson]

let to_upload_function_rpc_params (payload : string) :
    Types.fluid_expr upload_function_rpc_params =
  let json = payload |> Yojson.Safe.from_string in
  json
  |> upload_function_rpc_params_of_yojson Fluid.expr_json_to_fluid
  |> Tc.Result.or_else_lazy (fun () ->
         upload_function_rpc_params_of_yojson Types.fluid_expr_of_yojson json)
  |> Result.ok_or_failwith


type 'expr_type trigger_handler_rpc_params =
  { tlid : tlid
  ; trace_id : RuntimeT.uuid
  ; input : 'expr_type input_vars }
[@@deriving yojson]

type route_params =
  { space : string
  ; path : string
  ; modifier : string }
[@@deriving yojson]

let to_add_op_rpc_params (payload : string) : Types.fluid_expr add_op_rpc_params
    =
  let json = payload |> Yojson.Safe.from_string in
  json
  |> add_op_rpc_params_of_yojson Fluid.expr_json_to_fluid
  |> Tc.Result.or_else_lazy (fun () ->
         add_op_rpc_params_of_yojson Types.fluid_expr_of_yojson json)
  |> Result.ok_or_failwith


let to_db_stats_rpc_params (payload : string) : db_stats_rpc_params =
  payload
  |> Yojson.Safe.from_string
  |> db_stats_rpc_params_of_yojson
  |> Result.ok_or_failwith


type worker_stats_rpc_params = {tlid : tlid} [@@deriving yojson]

let to_worker_stats_rpc_params (payload : string) : worker_stats_rpc_params =
  payload
  |> Yojson.Safe.from_string
  |> worker_stats_rpc_params_of_yojson
  |> Result.ok_or_failwith


type worker_schedule_update_rpc_params =
  { name : string
  ; schedule : string }
[@@deriving yojson]

let to_worker_schedule_update_rpc_params (payload : string) :
    worker_schedule_update_rpc_params =
  payload
  |> Yojson.Safe.from_string
  |> worker_schedule_update_rpc_params_of_yojson
  |> Result.ok_or_failwith


let to_get_trace_data_rpc_params (payload : string) : get_trace_data_rpc_params
    =
  payload
  |> Yojson.Safe.from_string
  |> get_trace_data_rpc_params_of_yojson
  |> Result.ok_or_failwith


let to_execute_function_rpc_params (payload : string) :
    fluid_expr execute_function_rpc_params =
  let json = payload |> Yojson.Safe.from_string in
  json
  |> execute_function_rpc_params_of_yojson Fluid.expr_json_to_fluid
  |> Tc.Result.or_else_lazy (fun () ->
         execute_function_rpc_params_of_yojson Types.fluid_expr_of_yojson json)
  |> Result.ok_or_failwith


let to_trigger_handler_rpc_params (payload : string) :
    fluid_expr trigger_handler_rpc_params =
  let json = payload |> Yojson.Safe.from_string in
  json
  |> trigger_handler_rpc_params_of_yojson Fluid.expr_json_to_fluid
  |> Tc.Result.or_else_lazy (fun () ->
         trigger_handler_rpc_params_of_yojson Types.fluid_expr_of_yojson json)
  |> Result.ok_or_failwith


let to_route_params (payload : string) : route_params =
  payload
  |> Yojson.Safe.from_string
  |> route_params_of_yojson
  |> Result.ok_or_failwith


type insert_secret_params = RuntimeT.secret [@@deriving yojson]

let to_insert_secret_params (payload : string) : insert_secret_params =
  payload
  |> Yojson.Safe.from_string
  |> insert_secret_params_of_yojson
  |> Result.ok_or_failwith


type secrets_list_results = {secrets : RuntimeT.secret list}
[@@deriving to_yojson]

let to_secrets_list_results (secrets : RuntimeT.secret list) : string =
  {secrets} |> secrets_list_results_to_yojson |> Yojson.Safe.to_string ~std:true


let causes_any_changes (ps : 'expr_type add_op_rpc_params) : bool =
  List.exists ~f:Op.has_effect ps.ops


(*------------------*)
(* Functions *)
(*------------------*)
type param_metadata =
  { name : string
  ; tipe : string
  ; block_args : string list
  ; optional : bool
  ; description : string }
[@@deriving yojson]

type function_metadata =
  { name : string
  ; parameters : param_metadata list
  ; description : string
  ; return_type : string
  ; infix : bool
  ; preview_safety : RuntimeT.fn_preview_safety
  ; deprecated : bool }
[@@deriving yojson]

let functions ~username =
  !Libs.static_fns
  |> String.Map.to_alist
  |> List.filter ~f:(fun (k, _) ->
         Account.can_access_operations username
         || not (String.is_prefix ~prefix:"DarkInternal::" k))
  |> List.map ~f:(fun (k, (v : Types.fluid_expr RuntimeT.fn)) ->
         { name = k
         ; parameters =
             List.map
               ~f:(fun p ->
                 ( { name = p.name
                   ; tipe = Dval.tipe_to_string p.tipe
                   ; block_args = p.block_args
                   ; optional = p.optional
                   ; description = p.description }
                   : param_metadata ))
               v.parameters
         ; description = v.description
         ; return_type = Dval.tipe_to_string v.return_type
         ; preview_safety = v.preview_safety
         ; infix = List.mem ~equal:( = ) v.infix_names k
         ; deprecated = v.deprecated })
  |> List.map ~f:function_metadata_to_yojson
  |> (fun l -> `List l)
  |> Yojson.Safe.pretty_to_string
