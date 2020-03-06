open Prelude

(* Dark *)
module B = BlankOr
module P = Pointer
module TL = Toplevel
module E = FluidExpression

let toFlagged (expr : E.t) : E.t =
  match expr with
  | EFeatureFlag (_, _, _, _, _) ->
      recover "cant convert flagged to flagged" ~debug:expr expr
  | _ ->
      EFeatureFlag (gid (), "", E.newB (), expr, E.newB ())


let fromFlagged (pick : pick) (expr : E.t) : E.t =
  match expr with
  | EFeatureFlag (_, _, _, a, b) ->
    (match pick with PickA -> a | PickB -> b)
  | _ ->
      recover "cant convert flagged to flagged" ~debug:expr expr


(** [wrap m tl id]  returns a [modification] which finds the expression
  * having [id] in toplevel [tl] and makes it into the default case of a
  * new feature flag. *)
let wrap (_ : model) (tl : toplevel) (id : ID.t) : modification =
  let replacement e : E.t =
    EFeatureFlag (gid (), "flag-name", E.newB (), e, E.newB ())
  in
  TL.getAST tl
  |> Option.map ~f:(FluidAST.update ~f:replacement id >> TL.setASTMod tl)
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
  TL.getAST tl
  |> Option.map ~f:(FluidAST.update ~f:replacement id >> TL.setASTMod tl)
  |> Option.withDefault ~default:NoChange


let start (_m : model) : modification =
  (* TODO: needs to be reimplmented in fluid *)
  NoChange


let end_ (_m : model) (_id : ID.t) (_pick : pick) : modification =
  (* TODO: needs to be reimplmented in fluid *)
  NoChange
