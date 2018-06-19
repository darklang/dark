open Core_kernel

module C = Dark.Canvas
module PReq = Dark.Parsed_request
module Ast = Dark.Ast

let _ =
  Dark.Init.init ();
  let filename = Sys.argv.(1) in
  let c = C.load ~filename:(Some filename) "testcanvas" [] in
  let global = PReq.sample |> PReq.to_dval in
  let state = Ast.Symtable.singleton "request" global in
  print_endline (C.to_frontend_string state !c)


