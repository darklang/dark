open Core_kernel
module RT = Runtime
open Types
open Types.RuntimeT
open Types.RuntimeT.HandlerT

type 'expr_type tldata =
  | Handler of 'expr_type handler
  | DB of 'expr_type RuntimeT.DbT.db
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


type 'expr_type toplevel =
  { tlid : id
  ; pos : pos
  ; data : 'expr_type tldata }
[@@deriving eq, show, yojson]

type 'expr_type toplevels = 'expr_type toplevel IDMap.t
[@@deriving eq, show, yojson]

let as_handler (tl : 'expr_type toplevel) : 'expr_type handler option =
  match tl.data with Handler h -> Some h | _ -> None


let as_db (tl : 'expr_type toplevel) : 'expr_type RuntimeT.DbT.db option =
  match tl.data with DB db -> Some db | _ -> None


let handlers (tls : 'expr_type toplevels) : 'expr_type handler list =
  tls |> IDMap.data |> List.filter_map ~f:as_handler


let http_handlers (tls : 'expr_type toplevels) : 'expr_type handler list =
  tls |> handlers |> List.filter ~f:Handler.is_http


let dbs (tls : 'expr_type toplevels) : 'expr_type RuntimeT.DbT.db list =
  tls |> IDMap.data |> List.filter_map ~f:as_db


let set_expr (id : id) (expr : RuntimeT.expr) (tl : RuntimeT.expr toplevel) :
    RuntimeT.expr toplevel =
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


let handler_to_fluid (h : expr RuntimeT.HandlerT.handler) :
    fluid_expr RuntimeT.HandlerT.handler =
  {ast = Fluid.toFluidExpr h.ast; spec = h.spec; tlid = h.tlid}


let handler_of_fluid (h : fluid_expr RuntimeT.HandlerT.handler) :
    expr RuntimeT.HandlerT.handler =
  {ast = Fluid.fromFluidExpr h.ast; spec = h.spec; tlid = h.tlid}


let db_migration_to_fluid (dbm : expr RuntimeT.DbT.db_migration) :
    fluid_expr RuntimeT.DbT.db_migration =
  { starting_version = dbm.starting_version
  ; version = dbm.version
  ; state = dbm.state
  ; rollforward = Fluid.toFluidExpr dbm.rollforward
  ; rollback = Fluid.toFluidExpr dbm.rollback
  ; cols = dbm.cols }


let db_migration_of_fluid (dbm : fluid_expr RuntimeT.DbT.db_migration) :
    expr RuntimeT.DbT.db_migration =
  { starting_version = dbm.starting_version
  ; version = dbm.version
  ; state = dbm.state
  ; rollforward = Fluid.fromFluidExpr dbm.rollforward
  ; rollback = Fluid.fromFluidExpr dbm.rollback
  ; cols = dbm.cols }


let db_to_fluid (db : expr RuntimeT.DbT.db) : fluid_expr RuntimeT.DbT.db =
  { cols = db.cols
  ; name = db.name
  ; tlid = db.tlid
  ; version = db.version
  ; old_migrations = List.map ~f:db_migration_to_fluid db.old_migrations
  ; active_migration = Option.map ~f:db_migration_to_fluid db.active_migration
  }


let db_of_fluid (db : fluid_expr RuntimeT.DbT.db) : expr RuntimeT.DbT.db =
  { cols = db.cols
  ; name = db.name
  ; tlid = db.tlid
  ; version = db.version
  ; old_migrations = List.map ~f:db_migration_of_fluid db.old_migrations
  ; active_migration = Option.map ~f:db_migration_of_fluid db.active_migration
  }


let to_fluid (tl : expr toplevel) : fluid_expr toplevel =
  let fluid_data =
    match tl.data with
    | Handler h ->
        Handler (handler_to_fluid h)
    | DB db ->
        DB (db_to_fluid db)
  in
  {tlid = tl.tlid; pos = tl.pos; data = fluid_data}


let of_fluid (tl : fluid_expr toplevel) : expr toplevel =
  let fluid_data =
    match tl.data with
    | Handler h ->
        Handler (handler_of_fluid h)
    | DB db ->
        DB (db_of_fluid db)
  in
  {tlid = tl.tlid; pos = tl.pos; data = fluid_data}


let user_fn_to_fluid (fn : expr user_fn) : fluid_expr user_fn =
  {tlid = fn.tlid; metadata = fn.metadata; ast = Fluid.toFluidExpr fn.ast}


let user_fn_of_fluid (fn : fluid_expr user_fn) : expr user_fn =
  {tlid = fn.tlid; metadata = fn.metadata; ast = Fluid.fromFluidExpr fn.ast}


(* This has a clone on the frontend in AST.ml. Any changes to
 * this should likely be reflected there too. *)
let rec expr_to_string ~(indent : int) (e : expr) : string =
  let bs = Ast.blank_to_string in
  let rec nexpr_to_string ~indent (nexpr : nexpr) : string =
    let needs_parens arg =
      match arg with
      | Filled (_, Value _)
      | Filled (_, ListLiteral _)
      | Filled (_, ObjectLiteral _)
      | Filled (_, Variable _)
      | Filled (_, FieldAccess _)
      | Filled (_, FnCall (_, []))
      | Blank _ ->
          false
      | _ ->
          true
    in
    let nli = "\n" ^ String.make (indent + 2) ' ' in
    let nl = "\n" ^ String.make indent ' ' in
    let esi = expr_to_string ~indent:(indent + 2) in
    let es ?(indent = indent) = expr_to_string ~indent in
    match nexpr with
    | FluidPartial (_, expr)
    | FluidRightPartial (_, expr)
    | FluidLeftPartial (_, expr) ->
        es expr
    | Value v ->
        v
    | Variable name ->
        name
    | Let (lhs, rhs, body) ->
        "let "
        ^ bs lhs
        ^ " = "
        ^ es ~indent:(indent + String.length (bs lhs) + 7) rhs
        ^ " in"
        ^ nl
        ^ es body
    | If (cond, ifbody, elsebody) ->
        "if "
        ^ es ~indent:(indent + 3) cond
        ^ nl
        ^ "then"
        ^ nli
        ^ esi ifbody
        ^ nl
        ^ "else "
        ^ nli
        ^ esi elsebody
    | FnCall (name, args) ->
        List.fold ~init:name args ~f:(fun old arg ->
            let argstr =
              let old_length =
                String.make indent ' ' ^ old ^ " "
                |> String.split ~on:'\n'
                |> List.last
                |> Option.value ~default:""
                |> String.length
              in
              let indent = old_length + if needs_parens arg then 1 else 0 in
              if needs_parens arg
              then "(" ^ es ~indent arg ^ ")"
              else es ~indent arg
            in
            old ^ " " ^ argstr)
    | FnCallSendToRail (name, exprs) ->
        nexpr_to_string ~indent (FnCall (name ^ "-with-rail", exprs))
    | Lambda (vars, body) ->
        "\\("
        ^ (List.map vars ~f:bs |> String.concat ~sep:", ")
        ^ " -> "
        ^ nli
        ^ es ~indent:(indent + 3) body
        ^ " )"
    | Thread exprs ->
        List.map ~f:(es ~indent:(indent + 3)) exprs
        |> String.concat ~sep:(nl ^ "|> ")
    | FieldAccess (obj, field) ->
        if needs_parens obj
        then "(" ^ es obj ^ ")." ^ bs field
        else es obj ^ "." ^ bs field
    | ListLiteral exprs ->
        List.fold ~init:"" exprs ~f:(fun old arg ->
            let argstr =
              let indent =
                indent
                + 1
                + String.length old
                + if needs_parens arg then 1 else 0
              in
              if needs_parens arg
              then "(" ^ expr_to_string ~indent arg ^ "), "
              else expr_to_string ~indent arg ^ ", "
            in
            old ^ " " ^ argstr)
    | ObjectLiteral pairs ->
        "{"
        ^ String.concat
            ~sep:""
            (List.map pairs ~f:(fun (k, v) ->
                 nli
                 ^ bs k
                 ^ ": "
                 ^ es ~indent:(indent + 2 + String.length (bs k)) v))
        ^ nl
        ^ "}"
    | FeatureFlag (msg, cond, a, b) ->
        "ff("
        ^ bs msg
        ^ ") "
        ^ nl
        ^ "ff-condition"
        ^ nli
        ^ esi cond
        ^ nl
        ^ "ff-a"
        ^ nli
        ^ esi a
        ^ nl
        ^ "ff-b"
        ^ nli
        ^ esi b
    | Match (cond, pats) ->
        "match " ^ es ~indent:(indent + 6) cond ^ "TODO patterns"
    | Constructor (name, args) ->
        bs name ^ " " ^ (args |> List.map ~f:es |> String.concat ~sep:" ")
  in
  e |> Ast.blank_map ~f:(nexpr_to_string ~indent) |> bs


let user_fn_to_string (uf : 'expr_type RuntimeT.user_fn) : string =
  let bs = Ast.blank_to_string in
  let params =
    uf.metadata.parameters
    |> List.map ~f:(fun p ->
           "("
           ^ bs p.name
           ^ ": "
           ^ bs (Ast.blank_map ~f:Dval.tipe_to_string p.tipe)
           ^ ")")
    |> String.concat ~sep:", "
  in
  "Fn:"
  ^ bs uf.metadata.name
  ^ " : "
  ^ params
  ^ "\n"
  ^ expr_to_string ~indent:0 uf.ast


let to_string (tl : RuntimeT.expr toplevel) : string =
  let bs = Ast.blank_to_string in
  let col_to_string (f, t) =
    bs f ^ ": " ^ bs (Ast.blank_map ~f:Dval.tipe_to_string t)
  in
  match tl.data with
  | Handler h ->
      bs h.spec.module_
      ^ " "
      ^ bs h.spec.name
      ^ " "
      ^ bs h.spec.modifier
      ^ "\n"
      ^ expr_to_string ~indent:0 h.ast
  | DB db ->
      "DB: "
      ^ bs db.name
      ^ "\n  "
      ^ (db.cols |> List.map ~f:col_to_string |> String.concat ~sep:"\n  ")
