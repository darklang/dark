open Core_kernel
open Lwt
open Libcommon
open Libservice

let health_check (shutdown : bool ref) () : unit =
  let run_health_check_server =
    if Array.length Sys.argv >= 2
    then not (Sys.argv.(1) = "--no-health-check")
    else true
  in
  if run_health_check_server
  then
    while not !shutdown do
      ( match Dbconnection.status () with
      | `Healthy ->
          ()
      | _ ->
          shutdown := true ) ;
      Unix.sleep 30 ;
      Thread.yield ()
    done
  else ()
