open Core

type pos = { x:int; y:int }[@@deriving eq, show, yojson]

type tlid = int [@@deriving eq, show, yojson]
type id = int [@@deriving eq, show, yojson]

type 'a or_hole = Empty of id
                | Full of 'a
                [@@deriving eq, show, yojson]



