open Core_kernel
open Types
open Lib
module RTT = RuntimeT
module RT = Runtime
module FnMap = String.Map

type fnmap = RuntimeT.fn FnMap.t

let add_fn (m : fnmap) (f : RuntimeT.fn) : fnmap =
  List.fold_left
    ~f:(fun m1 n ->
      FnMap.update m1 n ~f:(fun v ->
          match v with
          | Some v ->
              Exception.internal ("duplicate library function: " ^ n)
          | None ->
              f))
    ~init:m
    (f.prefix_names @ f.infix_names)


let static_fns : fnmap ref = ref FnMap.empty

let add_short_fn (s : shortfn) : unit =
  let (def : RuntimeT.fn) =
    { prefix_names = s.pns
    ; infix_names = s.ins
    ; return_type = s.r
    ; parameters = s.p
    ; description = s.d
    ; func = s.f
    ; preview_execution_safe = s.ps
    ; deprecated = s.dep }
  in
  static_fns := add_fn !static_fns def


let fns (user_fns : RuntimeT.user_fn list) : fnmap =
  user_fns
  |> List.filter_map ~f:RuntimeT.user_fn_to_fn
  |> List.fold_left ~init:!static_fns ~f:(fun map uf -> add_fn map uf)


(* Give access to other modules *)
let get_fn ~(user_fns : RuntimeT.user_fn list) (name : string) :
    RuntimeT.fn option =
  FnMap.find (fns user_fns) name


let get_fn_exn ~(user_fns : RuntimeT.user_fn list) (name : string) : RuntimeT.fn
    =
  match get_fn ~user_fns name with
  | Some fn ->
      fn
  | None ->
      Exception.client ("No function named '" ^ name ^ "' exists")


(* We sometimes want to test execution similar to how it's run in the
 * frontend, which do not have any preview_execution_safe functions available
 * (it's more complicated than that, it's really backend-only tests that
 * they're missing, but we dont have a way to tell easily so this will have
 * to do. *)
let filter_out_non_preview_safe_functions_for_tests ~(f : unit -> unit) () :
    unit =
  let old_fns = !static_fns in
  let new_fns =
    Prelude.StrDict.filter ~f:(fun fn -> fn.preview_execution_safe) old_fns
  in
  static_fns := new_fns ;
  f () ;
  static_fns := old_fns ;
  ()


let init (libs : shortfn list) : unit =
  List.iter ~f:add_short_fn libs ;
  ()
