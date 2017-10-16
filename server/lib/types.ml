open Core

type pos_ = { x:int; y:int }[@@deriving yojson, eq, show, sexp]
type pos = Free
         | Root of pos_
         | Dependent
         [@@deriving yojson, eq, show, sexp]

type id = int [@@deriving eq, yojson, show, sexp]
