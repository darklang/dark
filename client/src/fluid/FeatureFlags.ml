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
          (* Somewhat arbitrary decision: when flagging a let, only wrap the
           * RHS. This avoids surprising behavior where multiple "lines" may
           * be wrapped if we wrapped the body. Consider:
           *   let a = 1
           *   let b = 2
           *   let c = 3
           *   a + b + c
           *
           * Wrapping with the `let b` could wrap either `2` (the RHS) or
           * `let c = 3 \n a + b + c` (the body). To make things feel more line based,
           * we choose to only wrap the RHS. *)
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
  let ast = Toplevel.getAST tl in
  (* Disable creating FF there is an existing FF in the AST.
   *
   * Note this is intended to be a _temporary_ measue as we figure out
   * the UX/UI around feature flags.
   *
   * See:
   *   https://www.notion.so/darklang/FF8-Quirks-6bbeba3ee9114781a5f321b167d72f56
   *   https://www.notion.so/darklang/Feature-Flags-v2-8fc5580cce9e491b9ee5767f54917434
   * *)
  let hasExistingFF =
    match ast with
    | Some ast ->
        ast
        |> FluidAST.filter ~f:(function E.EFeatureFlag _ -> true | _ -> false)
        |> List.isEmpty
        |> not
    | None ->
        false
  in
  if hasExistingFF
  then
    (* Show a toast, because this is unexpected *)
    ReplaceAllModificationsWithThisOne
      (fun m ->
        ({m with toast = Toast.show ErrorOnlyOneFeatureFlag}, Tea.Cmd.none))
  else
    ast
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
