open Core_kernel
module RT = Runtime
open Types
open Types.RuntimeT
open Types.RuntimeT.HandlerT

type tldata =
  | Handler of handler
  | DB of RuntimeT.DbT.db
[@@deriving eq, show, yojson]

type tl_tipe =
  | TLHandler
  | TLDB
  | TLUserFunction
  | TLUserTipe
[@@deriving eq, show]

let tl_tipe_to_string t =
  match t with
  | TLDB ->
      "db"
  | TLHandler ->
      "handler"
  | TLUserFunction ->
      "user_function"
  | TLUserTipe ->
      "user_tipe"


let tl_tipe_of_string s =
  match s with
  | "db" ->
      Some TLDB
  | "handler" ->
      Some TLHandler
  | "user_function" ->
      Some TLUserFunction
  | "user_tipe" ->
      Some TLUserTipe
  | _ ->
      None


type toplevel =
  { tlid : id
  ; pos : pos
  ; data : tldata }
[@@deriving eq, show, yojson]

type toplevels = toplevel IDMap.t [@@deriving eq, show, yojson]

let as_handler (tl : toplevel) : handler option =
  match tl.data with Handler h -> Some h | _ -> None


let as_db (tl : toplevel) : RuntimeT.DbT.db option =
  match tl.data with DB db -> Some db | _ -> None


let handlers (tls : toplevels) : handler list =
  tls |> IDMap.data |> List.filter_map ~f:as_handler


let http_handlers (tls : toplevels) : handler list =
  tls |> handlers |> List.filter ~f:Handler.is_http


let dbs (tls : toplevels) : RuntimeT.DbT.db list =
  tls |> IDMap.data |> List.filter_map ~f:as_db


let set_expr (id : id) (expr : fluid_expr) (tl : toplevel) : toplevel =
  match tl.data with
  | DB db ->
      let newdb =
        match db.active_migration with
        | None ->
            db
        | Some am ->
            let replace = Ast.set_expr ~search:id ~replacement:expr in
            let newam =
              { am with
                rollback = replace am.rollback
              ; rollforward = replace am.rollforward }
            in
            {db with active_migration = Some newam}
      in
      {tl with data = DB newdb}
  | _ ->
      failwith "not implemented yet"
