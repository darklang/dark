open Prelude

(* Dark *)
module TL = Toplevel

(* commands *)
let takeFunctionOffRail =
  { commandName = "take-function-off-rail"
  ; action = Refactor.takeOffRail
  ; doc = "Handle errors that arise from this function yourself" }


let putFunctionOnRail =
  { commandName = "put-function-on-rail"
  ; action = Refactor.putOnRail
  ; doc =
      "Errors that arise from this function will be handled on the error rail"
  }


let commands : command list =
  let open FluidExpression in
  [ { commandName = "extract-function"
    ; action = Refactor.extractFunction
    ; doc = "Extract expression into a function" }
  ; { commandName = "extract-variable"
    ; action = Refactor.extractVariable
    ; doc = "Extract expression into a variable" }
  ; { commandName = "wrap-if-condition"
    ; action = Refactor.wrap Refactor.WIfCond
    ; doc =
        "Wrap the expression in an if, using the expression as the condition" }
  ; { commandName = "wrap-if-then"
    ; action = Refactor.wrap Refactor.WIfThen
    ; doc =
        "Wrap the expression in an if, putting this expression in the `then` body"
    }
  ; { commandName = "wrap-if-else"
    ; action = Refactor.wrap Refactor.WIfElse
    ; doc =
        "Wrap the expression in an if, putting this expression in the `else` body"
    }
  ; { commandName = "insert-let-above"
    ; action = Refactor.wrap Refactor.WLetBody
    ; doc = "Add a let on the line above" }
  ; { commandName = "wrap-in-let"
    ; action = Refactor.wrap Refactor.WLetRHS
    ; doc = "Wrap expression in a let" }
  ; { commandName = "wrap-in-match"
    ; action = Refactor.wrap Refactor.WMatchExpr
    ; doc = "Wrap expr in a match" }
  ; { commandName = "wrap-in-match-arm"
    ; action = Refactor.wrap Refactor.WMatchArm
    ; doc = "Put expr in the arm of a match" }
  ; { commandName = "add-feature-flag"
    ; action = FeatureFlags.wrapCmd
    ; doc = "Add a feature flag around the expression" }
  ; { commandName = "discard-feature-flag"
    ; action = FeatureFlags.unwrapCmd FeatureFlags.KeepOld
    ; doc = "Remove the flag, keeping the old code" }
  ; { commandName = "commit-feature-flag"
    ; action = FeatureFlags.unwrapCmd FeatureFlags.KeepNew
    ; doc = "Remove the flag, keeping the new code" }
  ; putFunctionOnRail
  ; takeFunctionOffRail
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
                Toast.DidCopy
            | None ->
                Toast.FailedCopy
          in
          ReplaceAllModificationsWithThisOne
            (fun m -> ({m with toast = Toast.show toastMessage}, Tea.Cmd.none)))
    ; doc = "Copy request as curl command" } ]
