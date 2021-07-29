type id = ID of string [@@ppx.deriving show {with_path = false}, eq, ord]

type analysisID = id [@@ppx.deriving show {with_path = false}, eq, ord]
