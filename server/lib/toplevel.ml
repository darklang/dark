open Core

module RT = Runtime

type tldata = Handler of Handler.handler
            | DB of Db.db
            [@@deriving eq, show, yojson]

type toplevel = { tlid: Types.id
                ; pos: Types.pos
                ; data: tldata
                } [@@deriving eq, show, yojson]

type toplevel_list = toplevel list [@@deriving eq, show, yojson]

let as_handler (tl: toplevel) : Handler.handler option =
  match tl.data with
  | Handler h -> Some h
  | _ -> None

let as_db (tl: toplevel) : Db.db option =
  match tl.data with
  | DB db -> Some db
  | _ -> None

let handlers (tls: toplevel_list) : Handler.handler list =
  List.filter_map ~f:as_handler tls

let dbs (tls: toplevel_list) : Db.db list =
  List.filter_map ~f:as_db tls
