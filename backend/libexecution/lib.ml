open Core_kernel
open Types.RuntimeT
module RT = Runtime

let par ?(d : string = "") ?(args = []) ?(opt = false) name tipe : param =
  {name; tipe; optional = opt; block_args = args; description = d}


let func ?(d : string = "") ?(name : string = "f") args : param =
  par name TBlock ~args ~d


(* Shorthand - see Libs.ml's add_short_fn for expansions *)
type shortfn =
  { pns : string list
  ; ins : string list
  ; p : param list
  ; r : tipe
  ; f : funcimpl
  ; d : string
  ; ps : bool
  ; dep : bool }

let fail_fn (fnname : string) (fn : fn) (args : dval list) ?msg () : dval =
  let bt = Exception.get_backtrace () in
  let all = List.zip_exn fn.parameters args in
  let invalid =
    List.filter all ~f:(fun (p, a) ->
        Dval.tipe_of a <> p.tipe && p.tipe <> TAny)
  in
  match invalid with
  | [] ->
    ( match msg with
    | Some str ->
        RT.error ~bt str
    | None ->
        Exception.internal ~bt ("unknown error calling " ^ fnname) )
  | (p, a) :: _ ->
      RT.error
        ~bt
        ~actual:a
        ~expected:(Dval.tipe_to_string p.tipe)
        (fnname ^ " was called with the wrong type to parameter: " ^ p.name)


let fail ?msg ((state, args) : exec_state * dval list) : dval =
  match state.fail_fn with Some fn -> fn ?msg () | None -> DNull
