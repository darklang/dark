(* Unshared are the base types that are different between frontend and backend *)
type id = UnsharedTypes.id
[@@ppx.deriving show {with_path = false}]

type analysisID = UnsharedTypes.analysisID
[@@ppx.deriving show {with_path = false}]

val gid : unit -> UnsharedTypes.id

module Recover : sig
  val recover : ?sendToRollbar:bool -> ?debug:'d -> string -> 'r -> 'r

  val recoverOpt :
    ?sendToRollbar:bool -> ?debug:'d -> string -> default:'r -> 'r option -> 'r

  val recoverOption :
    ?sendToRollbar:bool -> ?debug:'d -> string -> 'r option -> 'r option

  val assert_ : ?sendToRollbar:bool -> ?debug:'d -> string -> bool -> 'r -> 'r

  val asserT : ?sendToRollbar:bool -> ?debug:'d -> string -> bool -> unit

  val assertFn :
    ?sendToRollbar:bool -> ?debug:'d -> string -> f:('r -> bool) -> 'r -> 'r

  val asserTFn :
    ?sendToRollbar:bool -> ?debug:'d -> string -> f:(unit -> bool) -> unit

  val todo : ?sendToRollbar:bool -> string -> 'b -> 'b
end
