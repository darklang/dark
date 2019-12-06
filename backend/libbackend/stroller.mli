open Libexecution

type segment_type =
  | Track
  | Identify

val segment_track :
     canvas_id:Uuidm.t
  -> canvas:string
  -> username:string
  -> execution_id:Types.id
  -> event:string
  -> segment_type
  -> Yojson.Safe.t
  -> unit Lwt.t

val segment_identify_user : string -> unit

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
  -> Event_queue.Worker_states.t
  -> unit

val status : unit -> [> `Healthy | `Unconfigured | `Unhealthy of string ] Lwt.t
