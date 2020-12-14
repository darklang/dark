module ApiServer.Api

open Prelude

module RT = LibExecution.RuntimeTypes



// type add_op_rpc_params =
//   { ops : oplist
//   ; opCtr : int
//         (* option means that we can still deserialize if this field is null, as
//          * doc'd at https://github.com/ocaml-ppx/ppx_deriving_yojson *)
//   ; clientOpCtrId : string option }
// [@@deriving yojson]
//
// type db_stats_rpc_params = {tlids : tlid list} [@@deriving yojson]
//
// type get_trace_data_rpc_params =
//   { tlid : tlid
//   ; trace_id : traceid }
// [@@deriving yojson]
//
// type execute_function_rpc_params =
//   { tlid : tlid
//   ; trace_id : RuntimeT.uuid
//   ; caller_id : id
//   ; args : RuntimeT.dval list
//   ; fnname : string }
// [@@deriving yojson]
//
// type upload_function_rpc_params = {fn : RuntimeT.user_fn} [@@deriving yojson]
//
// let to_upload_function_rpc_params (payload : string) :
//     upload_function_rpc_params =
//   payload
//   |> Yojson.Safe.from_string
//   |> upload_function_rpc_params_of_yojson
//   |> Result.ok_or_failwith
//
//
// type trigger_handler_rpc_params =
//   { tlid : tlid
//   ; trace_id : RuntimeT.uuid
//   ; input : input_vars }
// [@@deriving yojson]
//
// type route_params =
//   { space : string
//   ; path : string
//   ; modifier : string }
// [@@deriving yojson]
//
// let to_add_op_rpc_params (payload : string) : add_op_rpc_params =
//   payload
//   |> Yojson.Safe.from_string
//   |> add_op_rpc_params_of_yojson
//   |> Result.ok_or_failwith
//
//
// let to_db_stats_rpc_params (payload : string) : db_stats_rpc_params =
//   payload
//   |> Yojson.Safe.from_string
//   |> db_stats_rpc_params_of_yojson
//   |> Result.ok_or_failwith
//
//
// type worker_stats_rpc_params = {tlid : tlid} [@@deriving yojson]
//
// let to_worker_stats_rpc_params (payload : string) : worker_stats_rpc_params =
//   payload
//   |> Yojson.Safe.from_string
//   |> worker_stats_rpc_params_of_yojson
//   |> Result.ok_or_failwith
//
//
// type worker_schedule_update_rpc_params =
//   { name : string
//   ; schedule : string }
// [@@deriving yojson]
//
// let to_worker_schedule_update_rpc_params (payload : string) :
//     worker_schedule_update_rpc_params =
//   payload
//   |> Yojson.Safe.from_string
//   |> worker_schedule_update_rpc_params_of_yojson
//   |> Result.ok_or_failwith
//
//
// let to_get_trace_data_rpc_params (payload : string) : get_trace_data_rpc_params
//     =
//   payload
//   |> Yojson.Safe.from_string
//   |> get_trace_data_rpc_params_of_yojson
//   |> Result.ok_or_failwith
//
//
// let to_execute_function_rpc_params (payload : string) :
//     execute_function_rpc_params =
//   payload
//   |> Yojson.Safe.from_string
//   |> execute_function_rpc_params_of_yojson
//   |> Result.ok_or_failwith
//
//
// let to_trigger_handler_rpc_params (payload : string) :
//     trigger_handler_rpc_params =
//   payload
//   |> Yojson.Safe.from_string
//   |> trigger_handler_rpc_params_of_yojson
//   |> Result.ok_or_failwith
//
//
// let to_route_params (payload : string) : route_params =
//   payload
//   |> Yojson.Safe.from_string
//   |> route_params_of_yojson
//   |> Result.ok_or_failwith
//
//
// type insert_secret_params = RuntimeT.secret [@@deriving yojson]
//
// let to_insert_secret_params (payload : string) : insert_secret_params =
//   payload
//   |> Yojson.Safe.from_string
//   |> insert_secret_params_of_yojson
//   |> Result.ok_or_failwith
//
//
// type secrets_list_results = {secrets : RuntimeT.secret list}
// [@@deriving to_yojson]
//
// let to_secrets_list_results (secrets : RuntimeT.secret list) : string =
//   {secrets} |> secrets_list_results_to_yojson |> Yojson.Safe.to_string ~std:true
//
//
// let causes_any_changes (ps : add_op_rpc_params) : bool =
//   List.exists ~f:Op.has_effect ps.ops
//
//
// ------------------
//  Functions
// ------------------

// FSCLEANUP
// These types are to match the existing OCaml serializations that the frontend
// can read
type ParamMetadata =
  { name : string
    tipe : string
    block_args : string list
    optional : bool
    description : string }

type PreviewSafety =
  | Safe
  | Unsafe

type FunctionMetadata =
  { name : string
    parameters : ParamMetadata list
    description : string
    return_type : string
    infix : bool
    preview_safety : PreviewSafety
    deprecated : bool
    is_supported_in_query : bool }

let allFunctions = LibBackend.StdLib.StdLib.fns @ LibExecution.StdLib.StdLib.fns

let typToApiString (typ : RT.DType) : string =
  match typ with
  | RT.TVariable _
  | RT.TAny -> "Any"
  | RT.TInt -> "Int"
  | RT.TFloat -> "Float"
  | RT.TBool -> "Bool"
  | RT.TNull -> "Nothing"
  | RT.TChar -> "Character"
  | RT.TStr -> "Str"
  | RT.TList _ -> "List"
  | RT.TRecord _
  | RT.TDict _ -> "Dict"
  | RT.TFn _
  | RT.TLambda -> "Block"
  | RT.TIncomplete -> "Incomplete"
  | RT.TError -> "Error"
  | RT.THttpResponse _ -> "Response"
  | RT.TDB -> "Datastore"
  | RT.TDate -> "Date"
  // | TDbList tipe ->
  //     "[" ^ tipe_to_string tipe ^ "]"
  | RT.TPassword -> "Password"
  | RT.TUuid -> "UUID"
  | RT.TOption _ -> "Option"
  | RT.TErrorRail -> "ErrorRail"
  | RT.TResult _ -> "Result"
  | RT.TUserType (name, _) -> name
  | RT.TBytes -> "Bytes"
// | TDeprecated1
// | TDeprecated2
// | TDeprecated3
// | TDeprecated4 _
// | TDeprecated5 _
// | TDeprecated6 ->
// Exception.internal "Deprecated type"


let functionsToString (fns : RT.BuiltInFn list) : string =
  fns
  |> List.map (fun (fn : RT.BuiltInFn) ->
       { name = fn.name.ToString()
         parameters =
           List.map (fun (p : RT.Param) ->
             ({ name = p.name
                tipe = typToApiString p.typ
                block_args = []
                optional = false
                description = p.doc } : ParamMetadata)) fn.parameters
         description = fn.description
         return_type = typToApiString fn.returnType
         preview_safety = if fn.previewable = RT.Pure then Safe else Unsafe
         infix = LibExecution.StdLib.StdLib.isInfixName fn.name
         deprecated = fn.deprecated <> RT.NotDeprecated
         is_supported_in_query = fn.sqlSpec <> RT.NotQueryable })
  |> Prelude.Json.AutoSerialize.serialize

let adminFunctions : string = allFunctions |> functionsToString

let userFunctions =
  allFunctions
  |> List.filter (fun fn -> fn.name.module_ <> "DarkInternal")
  |> functionsToString

let functions (username : string) =
  if false // FSTODO: is admin
  then
    adminFunctions
  else
    userFunctions
