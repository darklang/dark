open Core

let _ =
  Dark.Init.init ();
  Dark.Queue_worker.dequeue_and_evaluate_all ()
  |> Out_channel.print_endline


