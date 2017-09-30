open Core

type pos_ = { x:int; y:int }[@@deriving yojson, eq, show, sexp]
type pos = pos_ option [@@deriving yojson, eq, show, sexp]

type id = int [@@deriving eq, yojson, show, sexp]
