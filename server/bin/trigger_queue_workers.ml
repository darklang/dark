open Core

let _ =
  Libbackend.Init.init ();
  Libbackend.Queue_worker.dequeue_and_evaluate_all ()
  |> Out_channel.print_endline


