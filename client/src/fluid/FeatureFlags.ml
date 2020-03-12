open Prelude
module E = FluidExpression

(** [wrap ast id] finds the expression having [id] and wraps it in a feature
 * flag (making it into the "old code" of the flag. *)
let wrap (ast : FluidAST.t) (id : ID.t) : FluidAST.t =
  let inFF =
    FluidAST.ancestors id ast
    |> List.any ~f:(function E.EFeatureFlag _ -> true | _ -> false)
  in
  if inFF (* don't nest flags! *)
  then ast
  else
    let replacement = function
      | E.ELet (id, var, rhs, body) ->
          let ff =
            E.EFeatureFlag (gid (), "flag-name", E.newB (), rhs, E.newB ())
          in
          E.ELet (id, var, ff, body)
      | e ->
          E.EFeatureFlag (gid (), "flag-name", E.newB (), e, E.newB ())
    in
    FluidAST.update ~f:replacement id ast


(** [wrap m tl id] returns a [modification] that calls [wrap] on the TL's AST
 * with the given [id]. *)
let wrapCmd (_ : model) (tl : toplevel) (id : ID.t) : modification =
  Toplevel.getAST tl
  |> Option.map ~f:(fun ast -> wrap ast id |> Toplevel.setASTMod tl)
  |> Option.withDefault ~default:NoChange


(** [unwrap m tl id]  returns a [modification] which unwraps the feature flag
 * expression having [id] in toplevel [tl], replacing it with its default case.
 * If the expression having [id] is not a FeatureFlag, does nothing. *)
let unwrap (_ : model) (tl : toplevel) (id : ID.t) : modification =
  let replacement e : E.t =
    match e with
    | E.EFeatureFlag (_id, _name, _cond, default, _enabled) ->
        default
    | expr ->
        recover "tried to remove non-feature flag" expr
  in
  Toplevel.getAST tl
  |> Option.map ~f:(FluidAST.update ~f:replacement id >> Toplevel.setASTMod tl)
  |> Option.withDefault ~default:NoChange
