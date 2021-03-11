open Core_kernel
open Libbackend
open Libcommon
module Account = Libbackend_basics.Account

(* Iterates over all users to send an Identify event to heapio *)

let usage () =
  Format.printf "Usage: %s\n" Sys.argv.(0) ;
  exit 1


let () =
  Account.get_users ()
  |> List.iter ~f:(fun username ->
         try
           Stroller.heapio_identify_user username ;
           Log.infO "Identified user" ~params:[("username", username)]
         with e ->
           Log.erroR (Printf.sprintf "Failed to identify user: %s" username)) ;
  ()
