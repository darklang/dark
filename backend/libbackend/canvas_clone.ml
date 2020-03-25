open Core_kernel
open Libexecution
open Util
open Canvas

(** [only_ops_since_last_savepoint ops] When we clone a canvas, we sometimes
   * want to only copy over ops since the last TLSavepoint - this erases
   * history, which is invisible and we don't really know at clone-time what's
   * in there, because we don't have any UI for inspecting history, nor do we
   * store timestamps or edited-by-user for ops
   * ("git blame"). *)
let only_ops_since_last_savepoint (ops : Op.op list) : Op.op list =
  ops
  (* Accumulate ops until we get to a TLSavepoint, at
   * which point we're done - we
   * throw out that op and everything after it in the
   * list (before it in history) *)
  |> List.fold_right
       ~init:(false, [])
       ~f:(fun currOp (found_tlsavepoint, ops) ->
         match (found_tlsavepoint, ops, (currOp : Op.op)) with
         | true, ops, _ | false, ops, TLSavepoint _ ->
             (true, ops)
         | false, ops, currOp ->
             (false, currOp :: ops))
  |> fun (_, ops) -> ops


(** [update_hosts_in_op ojp ~old_host ~new_host] Given an [op], and an
   * [old_host] and a [new_host], update string literals from
   * the old to the new host. Say your canvas contains a string literal that is
   * (or contains) a url pointing to the [old_host]
   * ("://oldhost.builtwithdark.com/stuff", or its localhost equivalent), the
   * [op] will be transformed to refer to the [new_host] *)
let update_hosts_in_op (op : Op.op) ~(old_host : string) ~(new_host : string) :
    Op.op =
  (* It might be nice if expr had an equivalent of FluidExpression.walk *)
  let rec update_hosts_in_pattern (pattern : Types.RuntimeT.pattern) :
      Types.RuntimeT.pattern =
    let literal_is_dstr s : bool =
      match Dval.parse_literal s with Some (DStr _) -> true | _ -> false
    in
    let host canvas : string =
      Printf.sprintf "://%s.%s" canvas Config.user_content_host
    in
    match pattern with
    | Blank _ | Partial _ ->
        pattern
    | Filled (id, npattern) ->
        Filled
          ( id
          , match npattern with
            | PVariable varname as v ->
                v
            | PLiteral str when literal_is_dstr str ->
                PLiteral
                  (str |> Util.string_replace (host old_host) (host new_host))
            | PLiteral str ->
                PLiteral str
            | PConstructor (constructor_name, patterns) ->
                PConstructor
                  ( constructor_name
                  , patterns |> List.map ~f:update_hosts_in_pattern ) )
  in
  let rec update_hosts_in_expr
      (expr : Types.RuntimeT.expr) ~(old_host : string) ~(new_host : string) :
      Types.RuntimeT.expr =
    (* Helper function; its only purpose is to not have to pass ~old_host and
     * ~new_host around anymore *)
    let rec f (expr : Types.RuntimeT.expr) : Types.RuntimeT.expr =
      let literal_is_dstr s : bool =
        match Dval.parse_literal s with Some (DStr _) -> true | _ -> false
      in
      match expr with
      | Blank _ | Partial _ ->
          expr
      | Filled (id, nexpr) ->
          Filled
            ( id
            , match nexpr with
              | Value str when literal_is_dstr str ->
                  (* This match covers string literals containing
                   * ://old_host.builtwithdark.com/stuff (or the localhost
                   * equivalent), and replaces them with
                   * ://new_host.builtwithdark.com. *)
                  let host canvas : string =
                    Printf.sprintf "://%s.%s" canvas Config.user_content_host
                  in
                  Value
                    (str |> Util.string_replace (host old_host) (host new_host))
              | Value str ->
                  (* Non-string-literal value *)
                  Value str
              | If (e1, e2, e3) ->
                  If (e1 |> f, e2 |> f, e3 |> f)
              | Thread exprs ->
                  Thread (exprs |> List.map ~f)
              | FnCall (fnname, exprs) ->
                  FnCall (fnname, exprs |> List.map ~f)
              | Variable varname ->
                  Variable varname
              | Let (varbind, e1, e2) ->
                  Let (varbind, e1 |> f, e2 |> f)
              | Lambda (varbinds, expr) ->
                  Lambda (varbinds, expr |> f)
              | FieldAccess (expr, field) ->
                  FieldAccess (expr |> f, field)
              | ObjectLiteral kvs ->
                  ObjectLiteral (kvs |> List.map ~f:(fun (k, v) -> (k, v |> f)))
              | ListLiteral exprs ->
                  ListLiteral (exprs |> List.map ~f)
              | FeatureFlag (string_or, e1, e2, e3) ->
                  FeatureFlag (string_or, e1 |> f, e2 |> f, e3 |> f)
              | FnCallSendToRail (fnname, exprs) ->
                  FnCallSendToRail (fnname, exprs |> List.map ~f)
              | Match (expr, arms) ->
                  Match
                    ( expr |> f
                    , arms
                      |> List.map ~f:(fun (pattern, expr) ->
                             (pattern |> update_hosts_in_pattern, expr |> f)) )
              | Constructor (name, exprs) ->
                  Constructor (name, exprs |> List.map ~f)
              | FluidPartial (str, expr) ->
                  FluidPartial (str, expr |> f)
              | FluidRightPartial (str, expr) ->
                  FluidRightPartial (str, expr |> f) )
    in
    f expr
  in
  let old_ast = Op.ast_of op in
  let new_ast =
    old_ast |> Option.map ~f:(update_hosts_in_expr ~old_host ~new_host)
  in
  new_ast
  |> Option.map ~f:(fun new_ast ->
         match op with
         | SetFunction userfn ->
             Op.SetFunction {userfn with ast = new_ast}
         | SetExpr (tlid, id, _) ->
             SetExpr (tlid, id, new_ast)
         | SetHandler (tlid, id, handler) ->
             SetHandler (tlid, id, {handler with ast = new_ast})
         | CreateDB (_, _, _)
         | AddDBCol (_, _, _)
         | SetDBColName (_, _, _)
         | SetDBColType (_, _, _)
         | DeleteTL _
         | MoveTL (_, _)
         | TLSavepoint _
         | UndoTL _
         | RedoTL _
         | DeleteFunction _
         | ChangeDBColName (_, _, _)
         | ChangeDBColType (_, _, _)
         | DeprecatedInitDbm (_, _, _, _, _)
         | CreateDBMigration (_, _, _, _)
         | AddDBColToDBMigration (_, _, _)
         | SetDBColNameInDBMigration (_, _, _)
         | SetDBColTypeInDBMigration (_, _, _)
         | DeleteColInDBMigration (_, _)
         | AbandonDBMigration _
         | DeleteDBCol (_, _)
         | RenameDBname (_, _)
         | CreateDBWithBlankOr (_, _, _, _)
         | DeleteTLForever _
         | DeleteFunctionForever _
         | SetType _
         | DeleteType _
         | DeleteTypeForever _ ->
             Exception.internal
               (Printf.sprintf
                  "Can't copy canvas %s, got an
unexpected ast-containing op."
                  old_host))
  |> Option.value ~default:op


(** Given two canvas names, clone TLs from one to the other.
   * - returns an error if from_canvas doesn't exist, or if to_canvas does
   *   ("don't clobber an existing canvas")
   * - optionally removes history - only copies ops since the last TLSavepoint (per TL)
   * - if there are string literals referring to the old canvas' url, rewrite them to
   *   refer to the new one (see update_hosts_in_op)
   * - runs in a DB transaction, so this should be all-or-nothing
   * *)
let clone_canvas ~from_canvas_name ~to_canvas_name ~(preserve_history : bool) :
    (unit, string) result =
  (* Ensure we can copy from and to - from_canvas must exist, to_canvas must
   * not. Yes, this is potentially racy, if to_canvas gets created by user
   * before we finish this function. I think this is unlikely enough as to be
   * an acceptable risk - users would have to get their welcome to dark email,
   * reset their password, and log in, before we finish running clone_canvas.
   * *)
  ( match
      (id_for_name_option from_canvas_name, id_for_name_option to_canvas_name)
    with
  | None, _ ->
      Error
        (Printf.sprintf
           "Can't clone from %s, no  such canvas exists"
           from_canvas_name)
  | _, Some _ ->
      Error
        (Printf.sprintf
           "Can't clone onto %s, that canvas already exists"
           to_canvas_name)
  | Some _, None ->
      Ok (from_canvas_name, to_canvas_name) )
  |> Result.bind ~f:(fun (from_canvas_name, _) ->
         (* Load from_canvas *)
         let from_canvas = load_all from_canvas_name [] in
         from_canvas |> Result.map_error ~f:(String.concat ~sep:", "))
  |> Result.map ~f:(fun (from_canvas : canvas ref) ->
         (* Transform the ops - remove pre-savepoint ops and update hosts
          * (canvas names) in string literals *)
         let to_ops =
           !from_canvas.ops
           |> List.map ~f:(fun (tlid, ops) ->
                  if preserve_history
                  then (tlid, ops)
                  else (tlid, ops |> only_ops_since_last_savepoint))
           |> List.map ~f:(fun (tlid, ops) ->
                  let new_ops =
                    ops
                    |> List.map
                         ~f:
                           (update_hosts_in_op
                              ~old_host:from_canvas_name
                              ~new_host:to_canvas_name)
                  in
                  (tlid, new_ops))
         in
         let owner : Uuidm.t =
           Account.auth_domain_for to_canvas_name
           |> Account.id_of_username
           |> Option.value_exn
         in
         (* In a transaction, save the new ops into to_canvas *)
         try
           (* fetch_canvas_id is what actually creates the canvas record,
            * which must preceed save_all *)
           Db.run ~name:"clone_canvas begin" "BEGIN" ~params:[] ;
           let to_id = Serialize.fetch_canvas_id owner to_canvas_name in
           let to_canvas : canvas ref =
             ref
               { !from_canvas with
                 host = to_canvas_name
               ; owner
               ; ops = to_ops
               ; id = to_id }
           in
           save_all !to_canvas ;
           Db.run ~name:"clone_canvas commit" "COMMIT" ~params:[]
         with e ->
           Db.run ~name:"clone_canvas commit" "ROLLBACK" ~params:[] ;
           raise e)
