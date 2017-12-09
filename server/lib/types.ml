open Core

type pos = { x:int; y:int }[@@deriving eq, show, yojson]

type tlid = int [@@deriving eq, show, yojson]
type id = int [@@deriving eq, show, yojson]

type 'a or_hole = Empty of id
                | Full of 'a
                [@@deriving eq, show, yojson]

let hole_to_maybe (h: 'a or_hole) : 'a option =
  match h with
  | Empty _ -> None
  | Full a -> Some a



module DbT = struct
  type row = string or_hole * string or_hole
             [@@deriving eq, show, yojson]

  type db = { tlid: tlid
            ; name: string
            ; rows: row list
            } [@@deriving eq, show, yojson]
end



module RuntimeT = struct
  (* ------------------------ *)
  (* Dvals*)
  (* ------------------------ *)
  type dhttp = Redirect of string
             | Response of int [@@deriving show, eq, yojson]

  module DvalMap = String.Map
  type dval_map = dval DvalMap.t [@opaque]
  and dval = DInt of int
           | DStr of string
           | DChar of char
           | DFloat of float
           | DBool of bool
           | DBlock of (dval list -> dval)
           | DList of dval list
           (* TODO: make null more like option. Maybe that's for the type
              system *)
           | DNull
           | DObj of dval_map
           | DResp of (dhttp * dval)
           | DDB of DbT.db
           | DIncomplete [@@deriving show]

end
