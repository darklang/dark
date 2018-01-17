open Core

module RT = Runtime
open Types

type tldata = Handler of Handler.handler
            | DB of DbT.db
            [@@deriving eq, show, yojson]

type toplevel = { tlid: id
                ; pos: pos
                ; data: tldata
                } [@@deriving eq, show, yojson]

type toplevel_list = toplevel list [@@deriving eq, show, yojson]

let as_handler (tl: toplevel) : Handler.handler option =
  match tl.data with
  | Handler h -> Some h
  | _ -> None

let as_db (tl: toplevel) : DbT.db option =
  match tl.data with
  | DB db -> Some db
  | _ -> None

let handlers (tls: toplevel_list) : Handler.handler list =
  List.filter_map ~f:as_handler tls

let dbs (tls: toplevel_list) : DbT.db list =
  List.filter_map ~f:as_db tls

let cur_dbs : DbT.db list ref =
  ref []
