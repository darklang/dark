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
      let replacements =
        Libdb.replacements
        @ Libdb2.replacements
        @ Libevent.replacements
        @ Libhttpclient.replacements
        @ Libcrypto.replacements
        @ Libtwilio.replacements
        @ Libdarkinternal.replacements
        @ Libstaticassets.replacements
        (* @ Libtwitter.fns  *)
      in
      Libexecution.Init.init Config.log_level Config.log_format replacements ;
      Libexecution.Libs.assert_all_libs_available () ;
      (* init the Random module, will be seeded from /dev/urandom on Linux *)
      Random.self_init () ;
      (* Dark-specific stuff *)
      File.init () ;
      Httpclient.init () ;
      if run_side_effects
      then (
        Migrations.init () ;
        Account.init () ;
        Serialize.write_shape_data () ) ;
      if Config.check_tier_one_hosts then Canvas.check_tier_one_hosts () ;
      Libcommon.Log.infO "Libbackend" ~data:"Initialization Complete" ;
      has_inited := true )
  with e ->
    let bt = Libexecution.Exception.get_backtrace () in
    Rollbar.last_ditch e ~bt "backend initialization" "no execution id" ;
    raise e
