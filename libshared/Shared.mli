(* Unshared are the base types that are different between frontend and backend *)
type id = UnsharedTypes.id [@@deriving show {with_path = false}, eq]

type analysisID = UnsharedTypes.analysisID
[@@deriving show {with_path = false}, eq]

val gid : unit -> UnsharedTypes.id

module Recover : sig
  val recover : ?debug:'d -> string -> 'r -> 'r

  val recoverOpt : ?debug:'d -> string -> default:'r -> 'r option -> 'r

  val recoverOption : ?debug:'d -> string -> 'r option -> 'r option

  val assert_ : ?debug:'d -> string -> bool -> 'r -> 'r

  val asserT : ?debug:'d -> string -> bool -> unit

  val assertFn : ?debug:'d -> string -> f:('r -> bool) -> 'r -> 'r

  val asserTFn : ?debug:'d -> string -> f:(unit -> bool) -> unit

  val todo : string -> 'b -> 'b
end
