open Core_kernel
open Libcommon
open Libexecution
open Types
open Types.RuntimeT
module RT = Runtime

let find_functions (expr : fluid_expr) : string list =
  let fns : string list ref = ref [] in
  Ast.iter expr ~f:(fun e ->
      match e with
      | EBinOp (_, name, _, _, _) | EFnCall (_, name, _, _) ->
          fns := name :: !fns
      | _ ->
          ()) ;
  !fns


let find_fields (expr : fluid_expr) : string list =
  let fieldnames : string list ref = ref [] in
  Ast.iter expr ~f:(fun e ->
      match e with
      | EFieldAccess (_, _, fieldname) ->
          fieldnames := fieldname :: !fieldnames
      | _ ->
          ()) ;
  !fieldnames
