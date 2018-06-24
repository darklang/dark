open Core_kernel

let _ =
  try
    Libbackend.Init.init ();
    Libbackend.Queue_worker.dequeue_and_evaluate_all ()
    |> Out_channel.print_endline
  with e ->
    Libbackend.Rollbar.last_ditch e "trigger queue workers"


