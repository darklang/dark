val gid : unit -> UnsharedTypes.id

val deID : UnsharedTypes.id -> string

type jsonType = Yojson.Safe.t

module Rollbar : sig
  val init : jsonType -> unit

  val send : string -> string option -> jsonType -> unit
end

val reportError : string -> 'm -> unit
