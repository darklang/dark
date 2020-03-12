open Prelude
module E = FluidExpression

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
      FluidAST.update id ast ~f:(function
          | E.ELet (id, var, rhs, body) ->
              let ff =
                E.EFeatureFlag (gid (), "flag-name", E.newB (), rhs, E.newB ())
              in
              E.ELet (id, var, ff, body)
          | e ->
              E.EFeatureFlag (gid (), "flag-name", E.newB (), e, E.newB ()))


(** [wrapCmd m tl id] returns a [modification] that calls [wrap] with the *
    [tl]'s AST and the given [id]. *)
let wrapCmd (_ : model) (tl : toplevel) (id : ID.t) : modification =
  Toplevel.getAST tl
  |> Option.map ~f:(fun ast -> wrap ast id |> Toplevel.setASTMod tl)
  |> Option.withDefault ~default:NoChange


(** [unwrap ast id] finds the expression having [id] and unwraps it, removing *
    any feature flag in its ancestry and replacing it with the "old code" of the
    flag. *)
let unwrap (ast : FluidAST.t) (id : ID.t) : FluidAST.t =
  (* Either the given ID is a FF or it's somewhere in the ancestor chain. Find
     it (hopefully). *)
  FluidAST.find id ast
  |> Option.andThen ~f:(function E.EFeatureFlag _ as e -> Some e | _ -> None)
  |> Option.orElseLazy (fun _ -> ancestorFlag ast id)
  |> Option.map ~f:(fun flag ->
         (* once we've found the flag, remove it *)
         FluidAST.update (E.toID flag) ast ~f:(function
             | E.EFeatureFlag (_id, _name, _cond, oldCode, _newCode) ->
                 oldCode
             | e ->
                 e (* ???? *)))
  |> Option.withDefault ~default:ast


(** [unwrapCmd m tl id] returns a [modification] that calls [unwrap] with the *
    [tl]'s AST and the given [id]. *)
let unwrapCmd (_ : model) (tl : toplevel) (id : ID.t) : modification =
  Toplevel.getAST tl
  |> Option.map ~f:(fun ast -> unwrap ast id |> Toplevel.setASTMod tl)
  |> Option.withDefault ~default:NoChange
