type t = Types.fluidPattern

val toPattern : Types.fluidPattern -> Types.pattern

val fromPattern : Types.id -> Types.pattern -> Types.fluidPattern

val id : t -> Types.id

val matchID : t -> Types.id

val clone : Types.id -> t -> t
