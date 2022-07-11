open Prelude
module TL = Toplevel

let alwaysShow = (_, _, _) => true

let commands: list<command> = {
  open ProgramTypes.Expr
  list{
    {
      commandName: "extract-function",
      action: Refactor.extractFunction,
      shouldShow: alwaysShow,
      doc: "Extract expression into a function",
    },
    {
      commandName: "extract-variable",
      action: Refactor.extractVariable,
      shouldShow: alwaysShow,
      doc: "Extract expression into a variable",
    },
    {
      commandName: "wrap-if-condition",
      action: Refactor.wrap(Refactor.WIfCond),
      shouldShow: alwaysShow,
      doc: "Wrap the expression in an if, using the expression as the condition",
    },
    {
      commandName: "wrap-if-then",
      action: Refactor.wrap(Refactor.WIfThen),
      shouldShow: alwaysShow,
      doc: "Wrap the expression in an if, putting this expression in the `then` body",
    },
    {
      commandName: "wrap-if-else",
      action: Refactor.wrap(Refactor.WIfElse),
      shouldShow: alwaysShow,
      doc: "Wrap the expression in an if, putting this expression in the `else` body",
    },
    {
      commandName: "convert-if-to-match",
      action: IfToMatch.refactor,
      shouldShow: (_, tl, e) =>
        Toplevel.getAST(tl)
        |> Option.map(~f=ast => IfToMatch.findIf(ast, e) |> Option.isSome)
        |> Option.unwrap(~default=false),
      doc: "Convert the if expression into a match expression",
    },
    {
      commandName: "insert-let-above",
      action: Refactor.wrap(Refactor.WLetBody),
      shouldShow: alwaysShow,
      doc: "Add a let on the line above",
    },
    {
      commandName: "wrap-in-let",
      action: Refactor.wrap(Refactor.WLetRHS),
      shouldShow: alwaysShow,
      doc: "Wrap expression in a let",
    },
    {
      commandName: "wrap-in-match",
      action: Refactor.wrap(Refactor.WMatchExpr),
      shouldShow: alwaysShow,
      doc: "Wrap expr in a match",
    },
    {
      commandName: "wrap-in-match-arm",
      action: Refactor.wrap(Refactor.WMatchArm),
      shouldShow: alwaysShow,
      doc: "Put expr in the arm of a match",
    },
    {
      commandName: "add-feature-flag",
      action: FeatureFlags.wrapCmd,
      shouldShow: FeatureFlags.shouldShowAddFlagCmd,
      doc: "Add a feature flag around the expression",
    },
    {
      commandName: "discard-feature-flag",
      action: FeatureFlags.unwrapCmd(FeatureFlags.KeepOld),
      shouldShow: FeatureFlags.shouldShowRemoveFlagCmds,
      doc: "Remove the flag, keeping the old code",
    },
    {
      commandName: "commit-feature-flag",
      action: FeatureFlags.unwrapCmd(FeatureFlags.KeepNew),
      shouldShow: FeatureFlags.shouldShowRemoveFlagCmds,
      doc: "Remove the flag, keeping the new code",
    },
    {
      commandName: "take-function-off-rail",
      action: Refactor.takeOffRail,
      shouldShow: (_, _, e) =>
        switch e {
        | EFnCall(_, _, _, Rail) => true
        | _ => false
        },
      doc: "Handle errors that arise from this function yourself",
    },
    {
      commandName: "put-function-on-rail",
      action: Refactor.putOnRail,
      shouldShow: (m, _, e) =>
        switch e {
        | EFnCall(_, name, _, NoRail) => Refactor.isRailable(m, name)
        | _ => false
        },
      doc: "Errors that arise from this function will be handled on the error rail",
    },
    {
      commandName: "create-type",
      action: (m, tl, id) => {
        let tlid = TL.id(tl)
        let tipe =
          Analysis.getSelectedTraceID(m, tlid)
          |> Option.andThen(~f=Analysis.getLiveValue(m, id))
          |> Refactor.generateUserType

        switch tipe {
        | Ok(tipe) =>
          let nameId = BlankOr.toID(tipe.utName)
          AddOps(list{SetType(tipe)}, FocusNext(tipe.utTLID, Some(nameId)))
        | Error(s) => Model.updateErrorMod(Error.set("Can't create-type: " ++ s))
        }
      },
      shouldShow: alwaysShow,
      doc: "Create a type from a live value",
    },
    {
      commandName: "copy-request-as-curl",
      action: (m, tl, id) => {
        let name =
          TL.getAST(tl)
          |> Option.andThen(~f=FluidAST.find(id))
          |> Option.andThen(~f=x =>
            switch x {
            | EFnCall(_, fluidName, _, _) => Some(fluidName)
            | _ => None
            }
          )
          |> Option.unwrap(~default="")

        let tlid = Toplevel.id(tl)
        let data = CurlCommand.curlFromHttpClientCall(m, tlid, id, name)
        let toastMessage = switch data {
        | Some(data) =>
          Native.Clipboard.copyToClipboard(data)
          Some("Copied!")
        | None => Some("Could not copy, try again after clicking this handler.")
        }

        ReplaceAllModificationsWithThisOne(
          m => /* TODO: toastPos is a vPos, how do we get a vPos without a
           * mouseEvent? */
          ({...m, toast: {toastMessage: toastMessage, toastPos: None}}, Tea.Cmd.none),
        )
      },
      shouldShow: (_, _, e) => {
        let re = Util.Regex.regex("HttpClient::(delete|get|head|options|patch|post|put)")

        switch e {
        | EFnCall(_, name, _, _) => Util.Regex.contains(~re, name)
        | _ => false
        }
      },
      doc: "Copy request as curl command",
    },
  }
}
