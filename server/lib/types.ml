type loc = { x: int; y: int} [@@deriving eq, yojson]
type id = int [@@deriving eq, yojson]
type param = string [@@deriving eq, yojson]
