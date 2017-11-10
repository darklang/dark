open Core

type pos = { x:int; y:int }[@@deriving eq, show, yojson]

type id = int [@@deriving eq, show, yojson]

