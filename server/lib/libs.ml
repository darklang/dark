open Core
open Types
open Lib

module RT = Runtime
module F = Functions

module FnMap = String.Map

type fnmap = RuntimeT.fn FnMap.t
let fns : fnmap =
  let add_fn (m : fnmap) (s : shortfn) : fnmap =
    let (def: RuntimeT.fn) = { prefix_names = s.pns
                             ; infix_names = s.ins
                             ; return_type = s.r
                             ; parameters = s.p
                             ; description = s.d
                             ; func = s.f
                             ; preview = s.pr
                             ; previewExecutionSafe = s.ps
                             } in
    List.fold_left ~f:(fun m1 n -> FnMap.set m1 ~key:n ~data:def) ~init:m (s.pns @ s.ins)
  in
  List.fold_left ~f:add_fn ~init:FnMap.empty
    (List.concat [ Stdlib.fns
                 ; Libdb.fns
                 ; Libhttp.fns
                 ; Libhttpclient.fns
                 ; Libevent.fns
                 (* ; Libtwitter.fns *)
                 ])

(* Give access to other modules *)
let get_fn (name : string) : RuntimeT.fn option =
  FnMap.find fns name

let get_fn_exn (name : string) : RuntimeT.fn =
  match FnMap.find fns name with
  | Some fn -> fn
  | None -> RT.raise_error ("No function named '" ^ name ^ "' exists")
