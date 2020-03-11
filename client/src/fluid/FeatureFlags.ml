open Prelude

(* Dark *)
module B = BlankOr
module P = Pointer
module TL = Toplevel
module E = FluidExpression

(** [wrap m tl id]  returns a [modification] which finds the expression
  * having [id] in toplevel [tl] and makes it into the default case of a
  * new feature flag. *)
let wrap (_ : model) (tl : toplevel) (id : ID.t) : modification =
  let flagId = gid () in
  let flagName = "flag-" ^ ID.toString flagId in
  let replacement e : E.t =
    EFeatureFlag (flagId, flagName, E.newB (), e, E.newB ())
  in
  let setAST =
    TL.getAST tl
    |> Option.map ~f:(FluidAST.update ~f:replacement id >> TL.setASTMod tl)
    |> Option.withDefault ~default:NoChange
  in
  let focusFlagPanel =
    ReplaceAllModificationsWithThisOne
      (fun m ->
        let m =
          m
          |> FluidModel.toggleFlagPanel (TL.id tl) flagId true
          |> FluidModel.focusPanel flagId
          |> fun m ->
          (* FIXME(ds) should use a caretTarget instead *)
          { m with
            fluidState = {m.fluidState with newPos = 0; selectionStart = None}
          }
        in
        (m, Tea.Cmd.NoCmd))
  in
  Many [setAST; focusFlagPanel]


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
  let setAST =
    TL.getAST tl
    |> Option.map ~f:(FluidAST.update ~f:replacement id >> TL.setASTMod tl)
    |> Option.withDefault ~default:NoChange
  in
  let deactivateFlagPanel =
    ReplaceAllModificationsWithThisOne
      (fun m ->
        let m =
          m
          |> FluidModel.toggleFlagPanel (TL.id tl) id false
          |> FluidModel.focusMainEditor
        in
        (m, Tea.Cmd.NoCmd))
  in
  Many [setAST; deactivateFlagPanel]
