open Core

type pos_ = { x:int; y:int }[@@deriving yojson, eq, show]
type pos = Free
         | Root of pos_
         | Dependent
         | NoPos
         [@@deriving yojson, eq, show]

type id = int [@@deriving eq, yojson, show]
