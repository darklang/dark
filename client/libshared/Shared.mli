(* Unshared are the base types that are different between frontend and backend *)
module UnsharedTypes : sig
  type id = ID of string [@@deriving show {with_path = false}]

  type analysisID = id [@@deriving show {with_path = false}]
end

module Unshared : sig
  val gid : unit -> UnsharedTypes.id
end

type id = UnsharedTypes.id [@@deriving show {with_path = false}]

type analysisID = UnsharedTypes.analysisID [@@deriving show {with_path = false}]

val gid : unit -> UnsharedTypes.id
