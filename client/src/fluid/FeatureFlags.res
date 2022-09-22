open Prelude
module E = ProgramTypes.Expr
type tokenInfo = FluidTypes.TokenInfo.t
type modification = AppTypes.modification
module Mod = AppTypes.Modification

type unwrapKeep =
  | KeepOld
  | KeepNew

@ocaml.doc(" [ancestorFlag ast id] returns the first ancestor of the expression having
 * [id] that is a feature flag ")
let ancestorFlag = (ast: FluidAST.t, id: id): option<FluidExpression.t> =>
  FluidAST.ancestors(id, ast) |> List.find(~f=x =>
    switch x {
    | E.EFeatureFlag(_) => true
    | _ => false
    }
  )

@ocaml.doc(" [wrap fluidState ast id] finds the expression having [id] and wraps it in a feature * *
    flag (making it into the \"old code\" of the flag.

    Returns a Some of the ID of the newly created EFeatureFlag (or None if one
    wasn't created) and the new AST ")
let wrap = (s: AppTypes.fluidState, ast: FluidAST.t, id: id): (option<id>, FluidAST.t) =>
  switch ancestorFlag(ast, id) {
  | Some(_) => (None, ast) // don't nest flags!
  | None =>
    let newB = FluidExpression.newB
    let flagName = "flag-" ++ (gid() |> ID.toString)
    let flagID = gid()
    let expr = FluidAST.toExpr(ast)
    let isSelectAll = {
      let tokenInfos = FluidTokenizer.tokenize(expr)
      let (tokenStart, tokenEnd) =
        List.last(tokenInfos)
        |> Option.map(~f=(last: tokenInfo) => (0, last.endPos))
        |> Option.unwrap(~default=(-1, -1))

      let (selectStart, selectEnd) = FluidUtil.getSelectionRange(s)
      (tokenStart, tokenEnd) == (selectStart, selectEnd)
    }

    if isSelectAll {
      // selected all - wrap the whole thing
      (Some(flagID), FluidAST.ofExpr(E.EFeatureFlag(flagID, flagName, newB(), expr, newB())))
    } else {
      let ast = FluidAST.update(id, ast, ~f=x =>
        /* Somewhat arbitrary decision: when flagging a let, only wrap the
         * RHS. This avoids surprising behavior where multiple "lines" may
         * be wrapped if we wrapped the body. Consider:
         *   let a = 1
         *   let b = 2
         *   let c = 3
         *   a + b + c
         *
         * Wrapping with the `let b` could wrap either `2` (the RHS) or
         * `let c = 3 \n a + b + c` (the body). To make things feel more line based,
         * we choose to only wrap the RHS. */
        switch x {
        | E.ELet(id, var, rhs, body) =>
          let ff = E.EFeatureFlag(flagID, flagName, newB(), rhs, newB())

          E.ELet(id, var, ff, body)
        | e => E.EFeatureFlag(flagID, flagName, newB(), e, newB())
        }
      )

      (Some(flagID), ast)
    }
  }

let hasFlag = (ast: FluidAST.t): bool =>
  ast
  |> FluidAST.filter(~f=x =>
    switch x {
    | E.EFeatureFlag(_) => true
    | _ => false
    }
  )
  |> List.isEmpty
  |> not

@ocaml.doc(" [wrapCmd m tl id] returns a [modification] that calls [wrap] with the
    [tl]'s AST. ")
let wrapCmd = (m: AppTypes.model, tl: toplevel, id: id): modification =>
  switch Toplevel.getAST(tl) {
  | Some(ast) if !hasFlag(ast) =>
    let (maybeId, ast) = wrap(m.fluidState, ast, id)
    let setAST = Toplevel.setASTMod(tl, ast)
    switch maybeId {
    | Some(flagId) =>
      Many(list{
        setAST,
        /* This is bad, but we can't use Fluid.ml here due to
         * dependency cycle issues D: */
        ReplaceAllModificationsWithThisOne(
          m => (
            {
              ...m,
              fluidState: {
                ...m.fluidState,
                newPos: 5 /* pos 5 is the blank after "when" */,
                upDownCol: None,
                activeEditor: FeatureFlagEditor(Toplevel.id(tl), flagId),
              },
            },
            Tea.Cmd.none,
          ),
        ),
      })
    | None => setAST
    }
  | Some(_) | None => NoChange
  }

@ocaml.doc(" [unwrap keep ast id] finds the expression having [id] and unwraps it,
 * removing any feature flag in its ancestry and replacing it with either the
 * old or new code, based on [keep].
 *
 * Returns the new AST if the flag was successfuly removed. ")
let unwrap = (keep: unwrapKeep, ast: FluidAST.t, id: id): option<FluidAST.t> =>
  /* Either the given ID is a FF or it's somewhere in the ancestor chain. Find
   it (hopefully). */
  FluidAST.findExpr(id, ast)
  |> Option.andThen(~f=x =>
    switch x {
    | E.EFeatureFlag(_) as e => Some(e)
    | _ => None
    }
  )
  |> Option.orElseLazy(_ => ancestorFlag(ast, id))
  |> Option.map(~f=flag =>
    // once we've found the flag, remove it, keeping the correct thing
    FluidAST.update(FluidExpression.toID(flag), ast, ~f=x =>
      switch x {
      | E.EFeatureFlag(_id, _name, _cond, oldCode, newCode) =>
        switch keep {
        | KeepOld => oldCode
        | KeepNew => newCode
        }
      | e => recover("updating flag found non-flag expression", e)
      }
    )
  )

@ocaml.doc(" [unwrapCmd keep m tl id] returns a [modification] that calls [unwrap] with
 * the [tl]'s AST. ")
let unwrapCmd = (keep: unwrapKeep, _: AppTypes.model, tl: toplevel, id: id): modification =>
  Toplevel.getAST(tl)
  |> Option.andThen(~f=ast => unwrap(keep, ast, id))
  |> Option.map(~f=ast => Mod.Many(list{
    Toplevel.setASTMod(tl, ast),
    ReplaceAllModificationsWithThisOne(
      m => (
        {
          ...m,
          fluidState: {
            ...m.fluidState,
            newPos: 0,
            /* should probably be the last place the caret was
             * in the main editor, but we don't store that */
            upDownCol: None,
            activeEditor: MainEditor(Toplevel.id(tl)),
          },
        },
        Tea.Cmd.none,
      ),
    ),
  }))
  |> Option.unwrap(~default=Mod.NoChange)

@ocaml.doc(" shouldShowAddFlagCmd shows the add flag command as long as there is no
 * other feature flag in the AST ")
let shouldShowAddFlagCmd = (_: AppTypes.model, tl: toplevel, _: E.t): bool =>
  Toplevel.getAST(tl) |> Option.map(~f=\">>"(hasFlag, not)) |> Option.unwrap(~default=false)

@ocaml.doc(" shouldShowRemoveFlagCmds shows the flag removal commands when the
 * expression or one of it's ancestors is a feature flag ")
let shouldShowRemoveFlagCmds = (_: AppTypes.model, tl: toplevel, e: E.t): bool =>
  switch e {
  | EFeatureFlag(_) => true
  | _ =>
    Toplevel.getAST(tl)
    |> Option.map(~f=ast => ancestorFlag(ast, FluidExpression.toID(e)) |> Option.isSome)
    |> Option.unwrap(~default=false)
  }
