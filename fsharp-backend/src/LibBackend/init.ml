open Core_kernel

let has_inited : bool ref = ref false

let init ~run_side_effects =
  try
    if not !has_inited
    then (
      (* Ocaml runtime stuff *)
      Caml.print_endline "Libbackend Initialization Begins" ;
      Printexc.record_backtrace true ;
      Exn.initialize_module () ;
      (* libexecution *)
      let non_client_fns =
        Libdb.fns
        @ Libdb2.fns
        @ Libevent.fns
        @ Libhttpclient.fns
        @ Libcrypto.fns
        @ Libtwilio.fns
        @ Libdarkinternal.fns
        @ Libstaticassets.fns
        @ Libjwt.fns
        @ Libx509.fns
      in
      Libexecution.Init.init Config.log_level Config.log_format non_client_fns ;
      (* init the Random module, will be seeded from /dev/urandom on Linux *)
      Random.self_init () ;
      (* Dark-specific stuff *)
      File.init () ;
      Httpclient.init () ;
      if run_side_effects
      then (
        Migrations.init () ;
        Account.init () ;
        Canvas.write_shape_data () ) ;
      if Config.check_tier_one_hosts then Canvas.check_tier_one_hosts () ;
      Libcommon.Log.infO "Libbackend" ~data:"Initialization Complete" ;
      has_inited := true )
  with e ->
    let bt = Libexecution.Exception.get_backtrace () in
    Rollbar.last_ditch e ~bt "backend initialization" "no execution id" ;
    raise e
