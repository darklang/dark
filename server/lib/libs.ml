open Core
open Types
open Lib

module RT = Runtime

module FnMap = String.Map

type fnmap = RuntimeT.fn FnMap.t

let add_fn (m : fnmap) (f : RuntimeT.fn) : fnmap =
  List.fold_left
    ~f:(fun m1 n -> FnMap.set m1 ~key:n ~data:f)
    ~init:m
    (f.prefix_names @ f.infix_names)

let static_fns : fnmap =
  let add_short_fn (m : fnmap) (s : shortfn) : fnmap =
    let (def: RuntimeT.fn) = { prefix_names = s.pns
                             ; infix_names = s.ins
                             ; return_type = s.r
                             ; parameters = s.p
                             ; description = s.d
                             ; func = s.f
                             ; preview = s.pr
                             ; preview_execution_safe = s.ps
                             } in
    add_fn m def
  in
  List.fold_left ~f:add_short_fn ~init:FnMap.empty
    (List.concat [ Stdlib.fns
                 ; Libdb.fns
                 ; Libhttp.fns
                 ; Libhttpclient.fns
                 ; Libevent.fns
                 ; Libtwitter.fns
                 ])

let fns (user_fns: RuntimeT.user_fn list) : fnmap =
  user_fns
  |> List.filter_map
    ~f:RuntimeT.user_fn_to_fn
  |> List.fold_left
    ~init:static_fns
    ~f:(fun map uf -> add_fn map uf)

(* Give access to other modules *)
let get_fn ~(user_fns: RuntimeT.user_fn list) (name : string) : RuntimeT.fn option =
  FnMap.find (fns user_fns) name

let get_fn_exn ~(user_fns: RuntimeT.user_fn list) (name : string) : RuntimeT.fn =
  match get_fn ~user_fns name with
  | Some fn -> fn
  | None -> RT.raise_error ("No function named '" ^ name ^ "' exists")

