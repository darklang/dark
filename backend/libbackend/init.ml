open Core_kernel
module Config = Libbackend_basics.Config
module File = Libbackend_basics.File

let has_inited : bool ref = ref false

let init ~run_side_effects ~run_migrations =
  try
    if not !has_inited
    then (
      (* Ocaml runtime stuff *)
      Caml.print_endline "Libbackend Initialization Begins" ;
      Printexc.record_backtrace true ;
      Exn.initialize_module () ;
      (* libexecution *)
      let non_client_fns =
        Libbackend_stdlib.Libdb.fns
        @ Libbackend_stdlib.Libdb2.fns
        @ Libbackend_stdlib.Libevent.fns
        @ Libhttpclient.fns
        @ Libbackend_stdlib.Libcrypto.fns
        @ Libtwilio.fns
        @ Libdarkinternal.fns
        @ Libstaticassets.fns
        @ Libbackend_stdlib.Libjwt.fns
        @ Libbackend_stdlib.Libx509.fns
      in
      Libexecution.Init.init
        Libservice.Config.log_level
        Libservice.Config.log_format
        non_client_fns ;
      (* init the Random module, will be seeded from /dev/urandom on Linux *)
      Random.self_init () ;
      (* Dark-specific stuff *)
      File.init () ;
      Httpclient.init () ;
      if run_migrations then Migrations.init () ;
      if run_side_effects
      then (
        Account.init () ;
        Canvas.write_shape_data () ) ;
      if Config.check_tier_one_hosts then Canvas.check_tier_one_hosts () ;
      Libcommon.Log.infO "Libbackend" ~data:"Initialization Complete" ;
      has_inited := true )
  with e ->
    let bt = Libexecution.Exception.get_backtrace () in
    Rollbar.last_ditch e ~bt "backend initialization" "no execution id" ;
    raise e
