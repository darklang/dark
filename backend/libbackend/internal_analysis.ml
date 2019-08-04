open Core_kernel
open Libcommon
open Libexecution
open Types
open Types.RuntimeT
module RT = Runtime

let find_functions (expr : expr) : string list =
  let fns : string list ref = ref [] in
  Ast.iter expr ~f:(fun e ->
      match e with
      | Filled (_, FnCall (name, _)) ->
          fns := name :: !fns
      | _ ->
          () ) ;
  !fns


let find_fields (expr : expr) : string list =
  let fieldnames : string list ref = ref [] in
  Ast.iter expr ~f:(fun e ->
      match e with
      | Filled (_, FieldAccess (_, Filled (_, fieldname))) ->
          fieldnames := fieldname :: !fieldnames
      | _ ->
          () ) ;
  !fieldnames
