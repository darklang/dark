val gid : unit -> UnsharedTypes.id

type jsonType = Yojson.Safe.t

module Rollbar : sig
  val init : jsonType -> unit

  val send : string -> string option -> jsonType -> unit
end

val reportError : string -> 'm -> unit
