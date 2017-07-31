open Core

open Lib
open Runtime


module SMap = String.Map

type fnmap = fn SMap.t
let fns : fnmap =
  let add_fn (m : fnmap) (s : shortfn) : fnmap =
    let def = { name = s.n
              ; other_names = s.o
              ; parameters = s.p
              ; func = s.f} in
    List.fold_left ~f:(fun m1 n -> SMap.add m1 ~key:n ~data:def) ~init:m (s.n::s.o)
  in
  List.fold_left ~f:add_fn ~init:SMap.empty (List.append Stdlib.fns Libtwitter.fns)

(* Give access to other modules *)
let get_fn (name : string) : fn option =
  SMap.find fns name

let get_fn_exn (name : string) : fn =
  match SMap.find fns name with
  | Some fn -> fn
  | None -> "No function named '" ^ name ^ "' exists" |> Exception.raise
