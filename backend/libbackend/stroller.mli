open Libexecution

val push :
  execution_id:Types.id -> canvas_id:Uuidm.t -> event:string -> string -> unit

val push_new_trace_id :
  execution_id:Types.id -> canvas_id:Uuidm.t -> Types.tlid -> Uuidm.t -> unit

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

val status : unit -> [> `Healthy | `Unconfigured | `Unhealthy of string] Lwt.t
