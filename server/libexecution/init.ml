open Core_kernel

let has_inited : bool ref =
  ref false

let init log_level log_format libs =
  if !has_inited
  then ()
  else
    Caml.print_endline "Libexecution Initialization Begins";
    Printexc.record_backtrace true;
    Exn.initialize_module ();

    Log.init ~level:log_level ~format:log_format ();
    Libs.init libs;

    Log.infO "Libexecution" ~data:"Initialization Complete";
    has_inited := true

