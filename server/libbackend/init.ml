open Core

let has_inited : bool ref =
  ref false

let init () =
  if !has_inited
  then ()
  else
    (* Ocaml runtime stuff *)
    Log.set_level Config.log_level;
    Printexc.record_backtrace true;
    Exn.initialize_module ();
    Util.init ();
    (* init the Random module, will be seeded from /dev/urandom on Linux *)
    Random.self_init ();
    Httpclient.init ();

    (* Dark-specific stuff *)
    Migrations.init ();
    Account.init ();
    Serialize.write_shape_data ();

    Log.infO "SYSTEM" "Initialization Complete";
    has_inited := true;

