(* Unshared are the base types that are different between frontend and backend *)
type id = UnsharedTypes.id [@@deriving show {with_path = false}, eq]

type analysisID = UnsharedTypes.analysisID
[@@deriving show {with_path = false}, eq]

val gid : unit -> UnsharedTypes.id
