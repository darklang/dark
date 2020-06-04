open Core_kernel
module Util = Libexecution.Util
open Libexecution
module RTT = Libexecution.Types.RuntimeT
open Libbackend
open Libcommon

(*
 * For initial testing:
 * 43e58c86-2580-45e8-a569-b02740ec189b is paulshen-animalcrossing
 * *)
let usage () : unit =
  Format.printf "Usage: %s [count|delete] <limit> <canvas_id>" Sys.argv.(0) ;
  exit 1


let () =
  let (action, limit, canvas_id)
        : Stored_event.trim_events_action * int * Uuidm.t =
    match Sys.argv with
    | [|action; limit; canvas_id|] ->
        let action : Stored_event.trim_events_action =
          match action with
          | "count" ->
              Count
          | "delete" ->
              Delete
          | _ ->
              usage () ;
              Exception.internal "Can't happen, unreachable code"
        in
        ( action
        , limit |> int_of_string
        , Option.value_exn (canvas_id |> Uuidm.of_string) )
    | _ ->
        usage () ;
        Exception.internal "Can't happen, unreachable code"
  in
  Log.infO
    "garbage_collect_args"
    ~jsonparams:
      [ ("limit", `Int limit)
      ; ("canvas_id", `String (canvas_id |> Uuidm.to_string)) ] ;
  Stored_event.trim_events_for_canvas ~action canvas_id limit |> ignore
