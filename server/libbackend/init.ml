open Core_kernel

let has_inited : bool ref =
  ref false

let init () =
  if !has_inited
  then ()
  else
    (* Ocaml runtime stuff *)
    Printexc.record_backtrace true;
    Exn.initialize_module ();

    (* libexecution *)
    Libexecution.Init.init Config.log_level;

    (* init the Random module, will be seeded from /dev/urandom on Linux *)
    Random.self_init ();

    (* Dark-specific stuff *)
    File.init ();
    Httpclient.init ();
    Migrations.init ();
    Account.init ();
    Serialize.write_shape_data ();

    Libexecution.Log.infO "Libbackend" "Initialization Complete";
    has_inited := true;

