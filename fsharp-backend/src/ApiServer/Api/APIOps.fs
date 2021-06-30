module ApiServer.AddOps

// Functions and API endpoints for the API

open Microsoft.AspNetCore.Http
open Giraffe
open Giraffe.EndpointRouting

open System.Threading.Tasks
open FSharp.Control.Tasks
open FSharpPlus
open Prelude
open Tablecloth

module PT = LibExecution.ProgramTypes
module OT = LibExecution.OCamlTypes
module ORT = LibExecution.OCamlTypes.RuntimeT
module AT = LibExecution.AnalysisTypes
module Convert = LibExecution.OCamlTypes.Convert

// type add_op_rpc_params =
//   { ops : oplist
//   ; opCtr : int
//         (* option means that we can still deserialize if this field is null, as
//          * doc'd at https://github.com/ocaml-ppx/ppx_deriving_yojson *)
//   ; clientOpCtrId : string option }

// let causes_any_changes (ps : add_op_rpc_params) : bool =
//   List.exists ~f:Op.has_effect ps.ops

// | `POST, ["api"; canvas; "add_op"] ->
//     when_can_edit ~canvas (fun _ ->
//         wrap_editor_api_headers
//           (admin_add_op_handler ~execution_id ~user parent canvas body))
// (* Toplevel deletion:
//  * The server announces that a toplevel is deleted by it appearing in
//  * deleted_toplevels. The server announces it is no longer deleted by it
//  * appearing in toplevels again. *)
//
// (* A subset of responses to be merged in *)
// type add_op_rpc_result =
//   { toplevels : TL.toplevel list (* replace *)
//   ; deleted_toplevels : TL.toplevel list (* replace, see note above *)
//   ; user_functions : RTT.user_fn list (* replace *)
//   ; deleted_user_functions : RTT.user_fn list
//   ; userTypes : RTT.user_tipe list
//   ; deletedUserTypes : RTT.user_tipe list (* replace, see deleted_toplevels *)
//   }
//
// let empty_to_add_op_rpc_result =
//   { toplevels = []
//   ; deleted_toplevels = []
//   ; user_functions = []
//   ; deleted_user_functions = []
//   ; userTypes = []
//   ; deletedUserTypes = [] }
//
// type add_op_stroller_msg =
//   { result : add_op_rpc_result
//   ; params : Api.add_op_rpc_params }
//
// let to_add_op_rpc_result (c : Canvas.canvas) : add_op_rpc_result =
//   { toplevels = IDMap.data c.dbs @ IDMap.data c.handlers
//   ; deleted_toplevels = IDMap.data c.deleted_handlers @ IDMap.data c.deleted_dbs
//   ; user_functions = IDMap.data c.user_functions
//   ; deleted_user_functions = IDMap.data c.deleted_user_functions
//   ; userTypes = IDMap.data c.userTypes
//   ; deletedUserTypes = IDMap.data c.deletedUserTypes }
