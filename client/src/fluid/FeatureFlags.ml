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


(** [wrap fluidState ast id] finds the expression having [id] and wraps it in a feature * *
    flag (making it into the "old code" of the flag.

    Returns a Some of the ID of the newly created EFeatureFlag (or None if one
    wasn't created) and the new AST *)
let wrap (s : Types.fluidState) (ast : FluidAST.t) (id : ID.t) :
    ID.t option * FluidAST.t =
  match ancestorFlag ast id with
  | Some _ ->
      (None, ast) (* don't nest flags! *)
  | None ->
      let flagName = "flag-" ^ (gid () |> ID.toString) in
      let flagID = gid () in
      let expr = FluidAST.toExpr ast in
      let isSelectAll =
        let tokenInfos = FluidTokenizer.tokenize expr in
        let tokenStart, tokenEnd =
          List.last tokenInfos
          |> Option.map ~f:(fun last -> (0, last.endPos))
          |> Option.withDefault ~default:(-1, -1)
        in
        let selectStart, selectEnd = FluidUtil.getSelectionRange s in
        (tokenStart, tokenEnd) = (selectStart, selectEnd)
      in
      if isSelectAll
      then
        (* selected all - wrap the whole thing *)
        ( Some flagID
        , FluidAST.ofExpr
            (E.EFeatureFlag (flagID, flagName, E.newB (), expr, E.newB ())) )
      else
        let ast =
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
                    E.EFeatureFlag (flagID, flagName, E.newB (), rhs, E.newB ())
                  in
                  E.ELet (id, var, ff, body)
              | e ->
                  E.EFeatureFlag (flagID, flagName, E.newB (), e, E.newB ()))
        in
        (Some flagID, ast)


let hasFlag (ast : FluidAST.t) : bool =
  ast
  |> FluidAST.filter ~f:(function E.EFeatureFlag _ -> true | _ -> false)
  |> List.isEmpty
  |> not


(** [wrapCmd m tl id] returns a [modification] that calls [wrap] with the
    [tl]'s AST. *)
let wrapCmd (m : model) (tl : toplevel) (id : ID.t) : modification =
  match Toplevel.getAST tl with
  | Some ast when not (hasFlag ast) ->
      let maybeId, ast = wrap m.fluidState ast id in
      let mFn =
        match maybeId with
        | Some flagId ->
            fun m ->
              { m with
                fluidState =
                  { m.fluidState with
                    newPos = 5 (* pos 5 is the blank after "when" *)
                  ; upDownCol = None
                  ; activeEditor = FeatureFlagEditor (Toplevel.id tl, flagId) }
              }
        | None ->
            fun m -> m
      in
      Modifications.fullstackASTUpdate ~mFn tl ast
  | Some _ | None ->
      NoChange


(** [unwrap keep ast id] finds the expression having [id] and unwraps it,
 * removing any feature flag in its ancestry and replacing it with either the
 * old or new code, based on [keep].
 *
 * Returns the new AST if the flag was successfuly removed. *)
let unwrap (keep : unwrapKeep) (ast : FluidAST.t) (id : ID.t) :
    FluidAST.t option =
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


(** [unwrapCmd keep m tl id] returns a [modification] that calls [unwrap] with
 * the [tl]'s AST. *)
let unwrapCmd (keep : unwrapKeep) (_ : model) (tl : toplevel) (id : ID.t) :
    modification =
  Toplevel.getAST tl
  |> Option.andThen ~f:(fun ast -> unwrap keep ast id)
  |> Option.map ~f:(fun ast ->
         let mFn m =
           { m with
             fluidState =
               { m.fluidState with
                 newPos =
                   0
                   (* should probably be the last place the caret was
                    * in the main editor, but we don't store that *)
               ; upDownCol = None
               ; activeEditor = MainEditor (Toplevel.id tl) } }
         in
         Modifications.fullstackASTUpdate ~mFn tl ast)
  |> Option.withDefault ~default:NoChange


(** shouldShowAddFlagCmd shows the add flag command as long as there is no
 * other feature flag in the AST *)
let shouldShowAddFlagCmd (_ : model) (tl : toplevel) (_ : E.t) : bool =
  Toplevel.getAST tl
  |> Option.map ~f:(hasFlag >> not)
  |> Option.withDefault ~default:false


(** shouldShowRemoveFlagCmds shows the flag removal commands when the
 * expression or one of it's ancestors is a feature flag *)
let shouldShowRemoveFlagCmds (_ : model) (tl : toplevel) (e : E.t) : bool =
  match e with
  | EFeatureFlag _ ->
      true
  | _ ->
      Toplevel.getAST tl
      |> Option.map ~f:(fun ast -> ancestorFlag ast (E.toID e) |> Option.isSome)
      |> Option.withDefault ~default:false
