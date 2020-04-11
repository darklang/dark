type id = ID of string [@@deriving show {with_path = false}, eq, ord]

type analysisID = id [@@deriving show {with_path = false}, eq, ord]
