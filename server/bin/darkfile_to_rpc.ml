open Core

module C = Canvas
module DReq = Dark_request

let _ =
  Printexc.record_backtrace true;
  Exn.initialize_module ();
  let filename = Sys.argv.(1) in
  let c = C.load ~filename:(Some filename) "testcanvas" [] in
  let global = DReq.sample |> DReq.to_dval in
  print_endline (C.to_frontend_string global !c)


