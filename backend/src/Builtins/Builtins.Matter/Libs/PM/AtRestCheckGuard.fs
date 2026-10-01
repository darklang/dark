/// Catch checker runtime errors that Dark cannot catch itself.
/// Return CheckFailure so the checker can report the affected work as Incomplete.
module Builtins.Matter.Libs.PM.AtRestCheckGuard

open Prelude
open LibExecution.RuntimeTypes
open LibExecution.Builtin.Shortcuts

module Dval = LibExecution.Dval
module PackageRefs = LibExecution.PackageRefs
module Exe = LibExecution.Execution


/// `LanguageTools.AtRestTypeChecker.CheckFailure`
let private checkFailureName () =
  FQTypeName.fqPackage (
    PackageRefs.Type.LanguageTools.AtRestTypeChecker.checkFailure ()
  )

let private checkFailure (caseName : string) (fields : List<Dval>) : Dval =
  DEnum(checkFailureName (), checkFailureName (), [], caseName, fields)

let private failureResult (typeArgs : List<ValueType>) (failure : Dval) : Dval =
  DEnum(Dval.resultType (), Dval.resultType (), typeArgs, "Error", [ failure ])


let fns : List<BuiltInFn> =
  let varA = TVariable "a"
  let varB = TVariable "b"
  let failureType = TCustomType(NameResolution.ok (checkFailureName ()), [])

  [ { name = fn "atRestCheckGuarded" 0
      typeParams = [ "a"; "b" ]
      parameters =
        [ Param.makeWithArgs
            "check"
            (TFn(NEList.singleton varA, varB))
            "The check to run"
            [ "input" ]
          Param.make "input" varA "What to check" ]
      returnType = TypeReference.result varB failureType
      description =
        "Runs part of an at-rest check on <param input>. If the check itself fails, the result is an Error saying how, rather than a runtime error."
      fn =
        (function
        | exeState, vm, _, [| DApplicable check; input |] ->
          uply {
            let failureValueType =
              ValueType.Known(KTCustomType(checkFailureName (), []))

            let typeArgs = [ ValueType.Unknown; failureValueType ]

            let failureWithMessage (detail : string) : Dval =
              failureResult typeArgs (checkFailure "Failed" [ DString detail ])

            try
              match! Exe.executeApplicable1 exeState vm.activeAccess check input with
              | Ok result ->
                return
                  DEnum(
                    Dval.resultType (),
                    Dval.resultType (),
                    [ Dval.toValueType result; failureValueType ],
                    "Ok",
                    [ result ]
                  )
              | Error(RuntimeError.UncaughtException(message, _), _) when
                message = Exe.outOfStackMessage
                ->
                // Nesting deep enough to exhaust the native stack.
                return failureResult typeArgs (checkFailure "TooDeep" [])
              | Error(rte, _) ->
                let! rendered = Exe.runtimeErrorToString exeState rte
                match rendered with
                | Ok(DString message) -> return failureWithMessage message
                | _ -> return failureWithMessage $"{rte}"
            with ex ->
              return failureWithMessage ex.Message
          }
        | _ -> incorrectArgs ())
      sqlSpec = NotQueryable
      previewable = Impure
      callEffects = Set.empty
      deprecated = NotDeprecated } ]

let builtins = LibExecution.Builtin.make [] fns
