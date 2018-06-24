open Core_kernel

let has_inited : bool ref =
  ref false

let init () =
  try
    if !has_inited
    then ()
    else
      (* Ocaml runtime stuff *)
      Caml.print_endline "Libbackend Initialization Begins";
      Printexc.record_backtrace true;
      Exn.initialize_module ();

      (* libexecution *)
      let libs = Libdb.fns
                 @ Libevent.fns
                 @ Libhttp.fns
                 @ Libhttpclient.fns
                 (* @ Libtwitter.fns  *)
      in

      Libexecution.Init.init Config.log_level Config.log_format libs;

      (* init the Random module, will be seeded from /dev/urandom on Linux *)
      Random.self_init ();

      (* Dark-specific stuff *)
      File.init ();
      Httpclient.init ();
      Migrations.init ();
      Account.init ();
      Serialize.write_shape_data ();

      Libexecution.Log.infO "Libbackend" ~data:"Initialization Complete";
      has_inited := true;
  with e ->
    Rollbar.last_ditch e "backend initialization"

