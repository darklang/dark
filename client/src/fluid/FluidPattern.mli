type t = Types.fluidPattern

val id : t -> Types.id

val matchID : t -> Types.id

val clone : Types.id -> t -> t

val variableNames : t -> string list

val hasVariableNamed : string -> t -> bool

(** [findPattern patID within] returns Some pattern
  with id = [patID] in the [within] tree, or None. *)
val findPattern : Types.id -> t -> t option
