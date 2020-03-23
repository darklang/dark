open Core_kernel
module Util = Libexecution.Util
open Libexecution
module RTT = Libexecution.Types.RuntimeT
open Libbackend
open Libcommon

let usage () : unit =
  Format.printf
    "Usage: %s [--force] from-canvas to-canvas\n\nCopies from-canvas (toplevels, not traces or events) to-canvas.\n\nFirst used as a development harness for the getting-started canvas feature, but\ncan also be used to copy a canvas from one account (or org) to another.\n\nDoes not remove the from-canvas, this is `cp`, not `mv`.\n\nIf from-canvas is not a sample canvas, you must call this with --force.\n"
    Sys.argv.(0) ;
  exit 1


let () =
  let clone_canvas_or_fail from_canvas_name to_canvas_name : unit =
    match Canvas.Clone.clone_canvas from_canvas_name to_canvas_name with
    | Ok _ ->
        ()
    | Error e ->
        Caml.print_endline e ;
        exit 1
  in
  match (Array.length Sys.argv, Array.to_list Sys.argv) with
  | 3, [_; from_canvas_name; to_canvas_name] ->
      if Account.auth_domain_for from_canvas_name = "sample"
      then clone_canvas_or_fail from_canvas_name to_canvas_name
      else (
        Caml.print_endline
          ( "You're trying to
copy from "
          ^ from_canvas_name
          ^ "; copying from a non-sample canvas requires the --force flag" ) ;
        exit 1 )
  | 4, [_; "--force"; from_canvas_name; to_canvas_name] ->
      clone_canvas_or_fail from_canvas_name to_canvas_name
  | 2, [_; "-h"] | 2, [_; "--help"] | _ ->
      usage ()
