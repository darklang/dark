type t = Types.fluidExpr

val toExpr : ?inPipe:bool -> t -> Types.expr

val toPattern : Types.fluidPattern -> Types.pattern

val id : t -> Types.id

val find : Types.id -> t -> t option

val isEmpty : t -> bool

val isBlank : t -> bool
