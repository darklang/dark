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
  and dval =
    (* basic types  *)
    | DInt of int
    | DFloat of float
    | DBool of bool
    | DNull (* TODO: make null more like option *)
    | DChar of char
    | DStr of string
    (* compound types *)
    | DList of dval list
    | DObj of dval_map
    (* special types *)
    | DIncomplete
    | DBlock of (dval list -> dval)
    (* user types: awaiting a better type system *)
    | DResp of (dhttp * dval)
    | DDB of DbT.db
    [@@deriving show]

  type tipe =
    | TAny (* extra type meaning anything *)
    | TInt
    | TFloat
    | TBool
    | TNull
    | TChar
    | TStr
    | TList
    | TObj
    | TIncomplete
    | TBlock
    | TResp
    | TDB
    [@@deriving eq, show, yojson]



  exception TypeError of dval list
end
