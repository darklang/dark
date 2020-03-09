val gid : unit -> ID.t

type jsonType = Js.Json.t

module Rollbar : sig
  val init : jsonType -> unit

  val send : string -> string option -> jsonType -> unit
end

val reportError : string -> 'm -> unit
