open Core

let _ =
  let _ =
    if String.is_substring ~substring:"server/" (Sys.getcwd ())
    then ()
    else
      try
        Sys.chdir "server"
      with
      | e ->
        raise e
  in
  Dark.Init.init ();
  Dark.Queue_worker.dequeue_and_evaluate_all ()
  |> Out_channel.print_endline


