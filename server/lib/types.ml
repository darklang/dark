open Core

type loc = { x: int; y: int} [@@deriving eq, yojson, show]
type id = int [@@deriving eq, yojson, show, sexp]
