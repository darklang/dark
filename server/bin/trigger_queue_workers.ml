open Core

let _ =
  Backend.Init.init ();
  Backend.Queue_worker.dequeue_and_evaluate_all ()
  |> Out_channel.print_endline


