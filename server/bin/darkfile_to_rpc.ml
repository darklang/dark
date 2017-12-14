open Core

module C = Dark.Canvas
module DReq = Dark.Dark_request
module Ast = Dark.Ast

let _ =
  Printexc.record_backtrace true;
  Exn.initialize_module ();
  let filename = Sys.argv.(1) in
  let c = C.load ~filename:(Some filename) "testcanvas" [] in
  let global = DReq.sample |> DReq.to_dval in
  let state = Ast.Symtable.singleton "request" global in
  print_endline (C.to_frontend_string state !c)


