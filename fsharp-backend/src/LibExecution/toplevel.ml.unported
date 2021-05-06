
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
