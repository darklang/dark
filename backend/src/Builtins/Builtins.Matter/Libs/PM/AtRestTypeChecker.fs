/// The guard around the at-rest type checker.
///
/// The checker is Darklang (`LanguageTools.AtRestTypeChecker`), and Dark code cannot catch
/// its own runtime error. Authoring and commit act on the checker's report, so a defect in
/// the checker must not make either unavailable: this runs a piece of the check and hands
/// back a failure of the checker itself as a value, for Dark to report. See
/// docs/at-rest-type-checker.md.
module Builtins.Matter.Libs.PM.AtRestTypeChecker

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

let private failed (typeArgs : List<ValueType>) (failure : Dval) : Dval =
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

            let failedWith (detail : string) : Dval =
              failed typeArgs (checkFailure "Failed" [ DString detail ])

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
                return failed typeArgs (checkFailure "TooDeep" [])
              | Error(rte, _) ->
                let! rendered = Exe.runtimeErrorToString exeState rte
                match rendered with
                | Ok(DString message) -> return failedWith message
                | _ -> return failedWith $"{rte}"
            with ex ->
              return failedWith ex.Message
          }
        | _ -> incorrectArgs ())
      sqlSpec = NotQueryable
      previewable = Impure
      callEffects = Set.empty
      deprecated = NotDeprecated } ]

let builtins = LibExecution.Builtin.make [] fns
