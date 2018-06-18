open Core_kernel

let has_inited : bool ref =
  ref false

let init log_level  =
  if !has_inited
  then ()
  else
    Printexc.record_backtrace true;
    Exn.initialize_module ();

    Log.set_level log_level;

    Log.infO "Libexecution" "Initialization Complete";
    has_inited := true;

