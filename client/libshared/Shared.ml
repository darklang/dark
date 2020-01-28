type id = UnsharedTypes.id [@@deriving show {with_path = false}, eq]

type analysisID = id [@@deriving show {with_path = false}, eq]

let gid = Unshared.gid
