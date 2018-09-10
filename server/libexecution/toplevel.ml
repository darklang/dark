open Core_kernel

module RT = Runtime
open Types
open Types.RuntimeT
open Types.RuntimeT.HandlerT

type tldata = Handler of handler
            | DB of RuntimeT.DbT.db
            [@@deriving eq, show, yojson]

type toplevel = { tlid: id
                ; pos: pos
                ; data: tldata
                } [@@deriving eq, show, yojson]

type toplevel_list = toplevel list [@@deriving eq, show, yojson]

let as_handler (tl: toplevel) : handler option =
  match tl.data with
  | Handler h -> Some h
  | _ -> None

let as_db (tl: toplevel) : RuntimeT.DbT.db option =
  match tl.data with
  | DB db -> Some db
  | _ -> None

let http_handlers (tls: toplevel_list) : handler list =
  tls
  |> List.filter_map ~f:as_handler
  |> List.filter ~f:Handler.is_http

let handlers (tls: toplevel_list) : handler list =
  List.filter_map ~f:as_handler tls

let dbs (tls: toplevel_list) : RuntimeT.DbT.db list =
  List.filter_map ~f:as_db tls

let set_expr (id : id) (expr : RuntimeT.expr) (tl : toplevel) : toplevel =
  match tl.data with
  | DB db ->
    let newdb =
      (match db.active_migration with
      | None -> db
      | Some am ->
        let replace = Ast.set_expr ~search:id ~replacement:expr in
        let newam = { am with rollback = replace am.rollback
                            ; rollforward = replace am.rollforward
                    }
        in
        { db with active_migration = Some newam })
    in
    { tl with data = DB newdb }
  | _ -> failwith "not implemented yet"


