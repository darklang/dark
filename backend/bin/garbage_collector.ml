open Core_kernel

(*
 * For initial testing:
 * 43e58c86-2580-45e8-a569-b02740ec189b is paulshen-animalcrossing
 * *)
let usage () : 'a =
  Format.printf
    "Usage: %s [count|delete] <limit> [all|<canvas_name>]\n\nIf given 'all', it will iterate across all canvases; if given a specific canvas_name,\nit will only gc that canvas\n"
    Sys.argv.(0) ;
  exit 1


let () =
  match Sys.argv with
  | [|_argv0; action_arg; limit; canvas_arg|] ->
      let action : Libbackend.Stored_event.trim_events_action =
        match action_arg with
        | "count" ->
            Count
        | "delete" ->
            Delete
        | _ ->
            usage ()
      in
      let canvas : Libbackend.Stored_event.trim_events_canvases =
        match canvas_arg with "all" -> All | name -> JustOne name
      in
      let limit = limit |> int_of_string in

      Libbackend.Garbage_collection.collect action limit canvas
  | _ ->
      usage ()
