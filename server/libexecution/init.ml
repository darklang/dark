open Core_kernel

let has_inited : bool ref =
  ref false

let init log_level libs =
  if !has_inited
  then ()
  else
    Printexc.record_backtrace true;
    Exn.initialize_module ();

    Log.set_level log_level;
    Libs.init libs;

    Log.infO "Libexecution" "Initialization Complete";
    has_inited := true

