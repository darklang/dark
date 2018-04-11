open Core

let has_inited : bool ref =
  ref false

let init () =
  if !has_inited
  then ()
  else
    Log.level := Config.log_level;
    Printexc.record_backtrace true;
    Exn.initialize_module ();
    (* init the Random module, will be seeded from /dev/urandom on Linux *)
    Random.self_init ();
    Db.init ();
    Event_queue.init ();
    Httpclient.init ();
    Log.infO "SYSTEM" "Initialization Complete";
    has_inited := true;

