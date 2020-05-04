open Core_kernel
module Util = Libexecution.Util
open Libexecution
module RTT = Libexecution.Types.RuntimeT
open Libbackend
open Libcommon

let usage () : unit =
  Format.printf
    "Usage: %s [--force] from-canvas to-canvas --preserve-history=[true|false]\n\nCopies from-canvas (toplevels, not traces or events) to-canvas.\n\nFirst used as a development harness for the gettingstarted canvas feature, but\ncan also be used to copy a canvas from one account (or org) to another.\n\nDoes not remove the from-canvas, this is `cp`, not `mv`.\n\nIf from-canvas is not a sample canvas, you must call this with --force.\n\nNote: argument order matters.\n"
    Sys.argv.(0) ;
  exit 1


let preserve_history (flag : string) : bool =
  match flag with
  | "--preserve-history=true" ->
      true
  | "--preserve-history=false" ->
      false
  | _ ->
      Caml.print_endline
        ("--preserve-history=true|false, but you provided " ^ flag) ;
      exit 1


let () =
  let clone_canvas_or_fail
      ~(from_canvas_name : string)
      ~(to_canvas_name : string)
      ~(preserve_history : bool) : unit =
    match
      Canvas_clone.clone_canvas
        ~from_canvas_name
        ~to_canvas_name
        ~preserve_history
    with
    | Ok _ ->
        ()
    | Error e ->
        Caml.print_endline e ;
        exit 1
  in
  match Array.to_list Sys.argv with
  | [_; from_canvas_name; to_canvas_name; preserve_history_flag] ->
      let preserve_history = preserve_history preserve_history_flag in
      if Account.auth_domain_for from_canvas_name = "sample"
      then
        clone_canvas_or_fail ~from_canvas_name ~to_canvas_name ~preserve_history
      else (
        Caml.print_endline
          ( "You're trying to
copy from "
          ^ from_canvas_name
          ^ "; copying from a non-sample canvas requires the --force flag" ) ;
        exit 1 )
  | [_; "--force"; from_canvas_name; to_canvas_name; preserve_history_flag] ->
      let preserve_history = preserve_history preserve_history_flag in
      clone_canvas_or_fail ~from_canvas_name ~to_canvas_name ~preserve_history
  | [_; "-h"] | [_; "--help"] | _ ->
      usage ()
