open Core_kernel
open Libbackend
open Libcommon

(* Iterates over all users to send an Identify event to segment *)

let usage () =
  Format.printf "Usage: %s\n" Sys.argv.(0) ;
  exit 1


let () =
  Account.get_users ()
  |> List.iter ~f:(fun username ->
         try
           Stroller.segment_identify_user username ;
           Log.infO "Identified user" ~params:[("username", username)]
         with e ->
           Log.erroR (Printf.sprintf "Failed to identify user: %s" username)) ;
  ()
