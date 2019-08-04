open Core_kernel
open Libcommon
open Libexecution
open Types
open Types.RuntimeT
module RT = Runtime

let find_functions (expr : expr) : string list =
  let fns : string list ref = ref [] in
  let rec f e =
    ( match e with
    | Filled (_, FnCall (name, _)) ->
        fns := name :: !fns ;
        ()
    | _ ->
        () ) ;
    Ast.traverse ~f e
  in
  f expr |> ignore ;
  !fns
