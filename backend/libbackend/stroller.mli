open Libexecution

type heapio_type =
  | Track
  | Identify

val heapio_track :
     canvas_id:Uuidm.t
  -> user_id:Uuidm.t
  -> canvas:string
  -> execution_id:Types.id
  -> event:string
  -> heapio_type
  -> Yojson.Safe.t
  -> unit Lwt.t

val heapio_identify_user : string -> unit

val push :
  ?execution_id:Types.id -> canvas_id:Uuidm.t -> event:string -> string -> unit

val push_new_trace_id :
     execution_id:Types.id
  -> canvas_id:Uuidm.t
  -> Uuidm.t
  -> Types.tlid list
  -> unit

val push_new_404 :
     execution_id:Types.id
  -> canvas_id:Uuidm.t
  -> Stored_event.four_oh_four
  -> unit

val push_new_static_deploy :
     execution_id:Types.id
  -> canvas_id:Uuidm.t
  -> Static_assets.static_deploy
  -> unit

val push_new_event :
  execution_id:Types.id -> canvas_id:Uuidm.t -> event:string -> string -> unit

val push_worker_states :
     execution_id:Types.id
  -> canvas_id:Uuidm.t
  -> Libbackend_basics.Event_queue.Worker_states.t
  -> unit

val status : unit -> [> `Healthy | `Unconfigured | `Unhealthy of string] Lwt.t
