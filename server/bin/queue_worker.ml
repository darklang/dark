open Core_kernel
open Lwt

let () =
  Libbackend.Init.init ();
  let execution_id = Libexecution.Util.create_id () in
  (* spin up health http server *)
  Lwt.async
    (fun () ->
       catch (fun () -> Libservice.Health_check.run ())
         (fun e ->
            print_endline "err";
            let bt = Libexecution.Exception.get_backtrace () in
            Libbackend.Rollbar.report_lwt e bt (EventQueue) (string_of_int execution_id) >>=
            fun _ -> fail e));
  print_endline "lol"
