open Prelude
module TL = Toplevel

let alwaysShow _ _ _ = true

let commands : command list =
  let open FluidExpression in
  [ { commandName = "extract-function"
    ; action = Refactor.extractFunction
    ; shouldShow = alwaysShow
    ; doc = "Extract expression into a function" }
  ; { commandName = "extract-variable"
    ; action = Refactor.extractVariable
    ; shouldShow = alwaysShow
    ; doc = "Extract expression into a variable" }
  ; { commandName = "wrap-if-condition"
    ; action = Refactor.wrap Refactor.WIfCond
    ; shouldShow = alwaysShow
    ; doc =
        "Wrap the expression in an if, using the expression as the condition" }
  ; { commandName = "wrap-if-then"
    ; action = Refactor.wrap Refactor.WIfThen
    ; shouldShow = alwaysShow
    ; doc =
        "Wrap the expression in an if, putting this expression in the `then` body"
    }
  ; { commandName = "wrap-if-else"
    ; action = Refactor.wrap Refactor.WIfElse
    ; shouldShow = alwaysShow
    ; doc =
        "Wrap the expression in an if, putting this expression in the `else` body"
    }
  ; { commandName = "convert-if-to-match"
    ; action = IfToMatch.refactor
    ; shouldShow =
        (fun _ tl e ->
          Toplevel.getAST tl
          |> Option.map ~f:(fun ast -> IfToMatch.findIf ast e |> Option.isSome)
          |> Option.withDefault ~default:false)
    ; doc = "Convert the if expression into a match expression" }
  ; { commandName = "insert-let-above"
    ; action = Refactor.wrap Refactor.WLetBody
    ; shouldShow = alwaysShow
    ; doc = "Add a let on the line above" }
  ; { commandName = "wrap-in-let"
    ; action = Refactor.wrap Refactor.WLetRHS
    ; shouldShow = alwaysShow
    ; doc = "Wrap expression in a let" }
  ; { commandName = "wrap-in-match"
    ; action = Refactor.wrap Refactor.WMatchExpr
    ; shouldShow = alwaysShow
    ; doc = "Wrap expr in a match" }
  ; { commandName = "wrap-in-match-arm"
    ; action = Refactor.wrap Refactor.WMatchArm
    ; shouldShow = alwaysShow
    ; doc = "Put expr in the arm of a match" }
  ; { commandName = "add-feature-flag"
    ; action = FeatureFlags.wrapCmd
    ; shouldShow = FeatureFlags.shouldShowAddFlagCmd
    ; doc = "Add a feature flag around the expression" }
  ; { commandName = "discard-feature-flag"
    ; action = FeatureFlags.unwrapCmd FeatureFlags.KeepOld
    ; shouldShow = FeatureFlags.shouldShowRemoveFlagCmds
    ; doc = "Remove the flag, keeping the old code" }
  ; { commandName = "commit-feature-flag"
    ; action = FeatureFlags.unwrapCmd FeatureFlags.KeepNew
    ; shouldShow = FeatureFlags.shouldShowRemoveFlagCmds
    ; doc = "Remove the flag, keeping the new code" }
  ; { commandName = "take-function-off-rail"
    ; action = Refactor.takeOffRail
    ; shouldShow =
        (fun _ _ e ->
          match e with EFnCall (_, _, _, Rail) -> true | _ -> false)
    ; doc = "Handle errors that arise from this function yourself" }
  ; { commandName = "put-function-on-rail"
    ; action = Refactor.putOnRail
    ; shouldShow =
        (fun m _ e ->
          match e with
          | EFnCall (_, name, _, NoRail) ->
              Refactor.isRailable m name
          | _ ->
              false)
    ; doc =
        "Errors that arise from this function will be handled on the error rail"
    }
  ; { commandName = "create-type"
    ; action =
        (fun m tl id ->
          let tlid = TL.id tl in
          let tipe =
            Analysis.getSelectedTraceID m tlid
            |> Option.andThen ~f:(Analysis.getLiveValue m id)
            |> Refactor.generateUserType
          in
          match tipe with
          | Ok tipe ->
              let nameId = BlankOr.toID tipe.utName in
              AddOps ([SetType tipe], FocusNext (tipe.utTLID, Some nameId))
          | Error s ->
              Model.updateErrorMod (Error.set ("Can't create-type: " ^ s)))
    ; shouldShow = alwaysShow
    ; doc = "Create a type from a live value" }
  ; { commandName = "copy-request-as-curl"
    ; action =
        (fun m tl id ->
          let name =
            TL.getAST tl
            |> Option.andThen ~f:(FluidAST.find id)
            |> Option.andThen ~f:(function
                   | EFnCall (_, fluidName, _, _) ->
                       Some fluidName
                   | _ ->
                       None)
            |> Option.withDefault ~default:""
          in
          let tlid = Toplevel.id tl in
          let data = CurlCommand.curlFromHttpClientCall m tlid id name in
          let toastMessage =
            match data with
            | Some data ->
                Native.Clipboard.copyToClipboard data ;
                Some "Copied!"
            | None ->
                Some "Could not copy, try again after clicking this handler."
          in
          ReplaceAllModificationsWithThisOne
            (fun m ->
              (* TODO: toastPos is a vPos, how do we get a vPos without a
               * mouseEvent? *)
              ({m with toast = {toastMessage; toastPos = None}}, Tea.Cmd.none)))
    ; shouldShow =
        (fun _ _ e ->
          let re =
            Util.Regex.regex
              "HttpClient::(delete|get|head|options|patch|post|put)"
          in
          match e with
          | EFnCall (_, name, _, _) ->
              Util.Regex.contains ~re name
          | _ ->
              false)
    ; doc = "Copy request as curl command" } ]
