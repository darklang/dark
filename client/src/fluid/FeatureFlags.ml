open Prelude
module E = FluidExpression

type unwrapKeep =
  | KeepOld
  | KeepNew

(** [ancestorFlag ast id] returns the first ancestor of the expression having
 * [id] that is a feature flag *)
let ancestorFlag (ast : FluidAST.t) (id : ID.t) : FluidExpression.t option =
  FluidAST.ancestors id ast
  |> List.find ~f:(function E.EFeatureFlag _ -> true | _ -> false)


(** [wrap ast id] finds the expression having [id] and wraps it in a feature * *
    flag (making it into the "old code" of the flag. *)
let wrap (ast : FluidAST.t) (id : ID.t) : FluidAST.t =
  match ancestorFlag ast id with
  | Some _ ->
      ast (* don't nest flags! *)
  | None ->
      let flagName = "flag-" ^ (gid () |> ID.toString) in
      FluidAST.update id ast ~f:(function
          | E.ELet (id, var, rhs, body) ->
              let ff =
                E.EFeatureFlag (gid (), flagName, E.newB (), rhs, E.newB ())
              in
              E.ELet (id, var, ff, body)
          | e ->
              E.EFeatureFlag (gid (), flagName, E.newB (), e, E.newB ()))


(** [wrapCmd m tl id] returns a [modification] that calls [wrap] with the
    [tl]'s AST. *)
let wrapCmd (_ : model) (tl : toplevel) (id : ID.t) : modification =
  Toplevel.getAST tl
  |> Option.map ~f:(fun ast -> wrap ast id |> Toplevel.setASTMod tl)
  |> Option.withDefault ~default:NoChange


(** [unwrap keep ast id] finds the expression having [id] and unwraps it,
 * removing any feature flag in its ancestry and replacing it with either the
 * old or new code, based on [keep]. *)
let unwrap (keep : unwrapKeep) (ast : FluidAST.t) (id : ID.t) : FluidAST.t =
  (* Either the given ID is a FF or it's somewhere in the ancestor chain. Find
     it (hopefully). *)
  FluidAST.find id ast
  |> Option.andThen ~f:(function E.EFeatureFlag _ as e -> Some e | _ -> None)
  |> Option.orElseLazy (fun _ -> ancestorFlag ast id)
  |> Option.map ~f:(fun flag ->
         (* once we've found the flag, remove it, keeping the correct thing *)
         FluidAST.update (E.toID flag) ast ~f:(function
             | E.EFeatureFlag (_id, _name, _cond, oldCode, newCode) ->
               (match keep with KeepOld -> oldCode | KeepNew -> newCode)
             | e ->
                 recover "updating flag found non-flag expression" e))
  |> Option.withDefault ~default:ast


(** [unwrapCmd keep m tl id] returns a [modification] that calls [unwrap] with
 * the [tl]'s AST. *)
let unwrapCmd (keep : unwrapKeep) (_ : model) (tl : toplevel) (id : ID.t) :
    modification =
  Toplevel.getAST tl
  |> Option.map ~f:(fun ast -> unwrap keep ast id |> Toplevel.setASTMod tl)
  |> Option.withDefault ~default:NoChange
