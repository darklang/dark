open Core_kernel


module Ast = Migrate_parsetree.Ast_404


let toOCaml (m: Elm.module_)  : (Ast.Parsetree.structure * Reason_comment.t list) =
  let five = Ast.Ast_helper.Const.int 8 in
  let v = Ast.Ast_helper.Exp.constant five in
  let prim = Ast.Ast_helper.Str.eval v in
  ([prim], [])

let _ =
  if Array.length Sys.argv > 1 && Sys.argv.(1) = "--compile"
  then
    try
      let m =
        In_channel.stdin
        |> Yojson.Basic.from_channel
        |> Elm.moduleJ
      in
      toOCaml m
      |> Reason_toolchain.RE.print_implementation_with_comments
           Format.str_formatter;
      Format.flush_str_formatter ()
      |> print_endline;
      ()
    with
    | (Elm.E (msg, json)) ->
      Printexc.print_backtrace stderr;
      print_endline (Yojson.Basic.pretty_to_string json);
      prerr_endline msg;
      ()
    | e ->
      Printexc.print_backtrace stderr;
      prerr_endline (Exn.to_string e);
      ()
  else
    try
      In_channel.stdin
      |> Yojson.Basic.from_channel
      |> Elm.moduleJ
      |> Elm.show_module_
      |> Str.global_replace (Str.regexp "Translate\\.") ""
      |> print_endline
    with (Elm.E (msg, json)) ->
      Printexc.print_backtrace stderr;
      print_endline (Yojson.Basic.pretty_to_string json);
      prerr_endline msg;
      ()



