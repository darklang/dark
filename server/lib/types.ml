type loc = { x: int; y: int} [@@deriving eq, yojson, show]
type id = int [@@deriving eq, yojson, show]
type chrome = Chrome of id * id list [@@deriving eq, yojson, show]
