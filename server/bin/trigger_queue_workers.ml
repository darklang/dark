open Core_kernel

let _ =
  try
    Libbackend.Init.init ~run_side_effects:false;
    Libbackend.Queue_worker.dequeue_and_evaluate_all ()
    |> Out_channel.print_endline
  with e ->
    let bt = Libexecution.Exception.get_backtrace () in
    Libbackend.Rollbar.last_ditch e ~bt "trigger queue workers" "no execution id"


