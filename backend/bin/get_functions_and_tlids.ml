open Core_kernel
module Util = Libexecution.Util
module Account = Libbackend.Account
open Libbackend
open Libexecution
module RTT = Types.RuntimeT
open Libcommon
open Types
open RTT.HandlerT

let flatmap ~(f : 'a -> 'b list) : 'a list -> 'b list =
  List.fold ~init:[] ~f:(fun acc e -> f e @ acc)


let fnnames_of_expr (expr : fluid_expr) : RTT.fnname list =
  Internal_analysis.find_functions expr


let usage () =
  Format.printf
    "Usage: %s <fnNames...>\n  Where <fnNames> is a space-separated list of functions to look for"
    Sys.argv.(0) ;
  exit 1


let prompt str =
  print_string str ;
  Out_channel.flush Out_channel.stdout ;
  match In_channel.input_line In_channel.stdin with None -> "" | Some s -> s


type fn =
  { host : string
  ; handler : string
  ; tlid : string
  ; fnname : RTT.fnname }

let pairs_of_fn (fn : fn) : (string * string) list =
  [ ("host", fn.host)
  ; ("handler", fn.handler)
  ; ("tlid", fn.tlid)
  ; ("fnname", fn.fnname) ]


let process_canvas (canvas : Canvas.canvas ref) : fn list =
  let handler_name (handler : handler) =
    let spec = handler.spec in
    String.concat
      ( [spec.module_; spec.name; spec.modifier]
      |> List.map ~f:(function Filled (_, s) -> s | Partial _ | Blank _ -> "")
      )
      ~sep:"-"
  in
  let handlers =
    !(canvas : Canvas.canvas ref).handlers
    |> IDMap.data
    |> List.filter_map ~f:Toplevel.as_handler
  in
  handlers
  |> List.fold
       ~init:[]
       ~f:(fun acc (handler : Types.RuntimeT.HandlerT.handler) ->
         acc
         @ ( fnnames_of_expr handler.ast
           |> List.map ~f:(fun fnname ->
                  { host = !canvas.host
                  ; handler = handler_name handler
                  ; tlid = Types.string_of_id handler.tlid
                  ; fnname }) ))


(*
let () =
  Libs.init [] ;
  ignore (Libs.FnMap.keys !Libs.static_fns |> List.map ~f:(fun s -> Log.infO s)) ;
  ()
  *)

let filterFnsNotInStaticFns (fn : fn) =
  let (realfn : RuntimeT.fn option) =
    Libs.FnMap.find !Libs.static_fns fn.fnname
  in
  match realfn with Some _ -> false | None -> true


let isDeprecated (fn : fn) =
  let (realfn : RuntimeT.fn option) =
    Libs.FnMap.find !Libs.static_fns fn.fnname
  in
  match realfn with Some realfn -> realfn.deprecated | None -> false


let filterFunction _ = true

let () =
  let fnNames =
    match Array.to_list Sys.argv with _ :: fnNames -> fnNames | [] -> usage ()
  in
  if List.is_empty fnNames then usage () else Libs.init [] ;
  let filter : fn -> bool =
    if fnNames = ["--package_fns"]
    then fun fn -> fn.fnname |> String.is_substring ~substring:"/"
    else fun fn -> List.mem ~equal:( = ) fnNames fn.fnname
  in
  ignore
    (let hosts = Serialize.current_hosts () in
     hosts
     |> List.map ~f:(fun host ->
            let canvas =
              try
                Some
                  ( Canvas.load_all_from_cache host
                  |> Result.map_error ~f:(String.concat ~sep:", ")
                  |> Prelude.Result.ok_or_internal_exception "Canvas load error"
                  )
              with
              | Pageable.PageableExn e ->
                  Log.erroR
                    "Can't load canvas"
                    ~params:[("host", host); ("exn", Exception.exn_to_string e)] ;
                  None
              | Exception.DarkException _ as e ->
                  Log.erroR
                    "DarkException"
                    ~params:[("host", host); ("exn", Exception.exn_to_string e)] ;
                  None
            in
            canvas
            |> Option.map ~f:process_canvas
            |> Option.value ~default:[]
            |> List.filter ~f:filter
            |> List.map ~f:(fun fn ->
                   Log.infO "function" ~params:(pairs_of_fn fn)))) ;
  ()
