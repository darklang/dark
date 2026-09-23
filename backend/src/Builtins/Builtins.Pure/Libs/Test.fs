/// Runtime support for Dark test helpers.
module Builtins.Pure.Libs.Test

open Prelude
open LibExecution.RuntimeTypes
open LibExecution.Builtin.Shortcuts

module PackageRefs = LibExecution.PackageRefs
module Dval = LibExecution.Dval
module NR = LibExecution.RuntimeTypes.NameResolution
module Exe = LibExecution.Execution


let fns () : List<BuiltInFn> =
  [ { name = fn "testCall" 0
      typeParams = []
      parameters =
        [ Param.makeCallback
            "fn"
            (TVariable "fn")
            "the function under test, of any arity"
          Param.make
            "inputs"
            (TVariable "inputs")
            "what to call it with: one value, or a tuple of as many as it has parameters" ]
      returnType =
        TypeReference.result
          (TVariable "result")
          (TCustomType(
            NR.ok (
              FQTypeName.fqPackage (PackageRefs.Type.Stdlib.testApplyProblem ())
            ),
            []
          ))
      description =
        "Calls <param fn> with <param inputs> and returns what it gave back, or the "
        + "runtime error it raised, with that error's message and its Dark call stack. "
        + "The function says how many parameters it has: a tuple is spread across "
        + "several, and passed whole to one. `Stdlib.Test.apply` wraps it: "
        + "`Stdlib.Test.table` uses it to keep running later rows after a row raises, "
        + "and `Stdlib.Test.raisesWith` uses it to check the expected error."
      fn =
        let problem (caseName : string) (fields : List<Dval>) : Dval =
          let name =
            FQTypeName.fqPackage (PackageRefs.Type.Stdlib.testApplyProblem ())
          let resultName = Dval.resultType ()
          DEnum(
            resultName,
            resultName,
            [ ValueType.Unknown; ValueType.Known(KTCustomType(name, [])) ],
            "Error",
            [ DEnum(name, name, [], caseName, fields) ]
          )
        let invalid (message : string) : Dval =
          problem "InvalidInputs" [ DString message ]

        // Remaining arity, or None if the callable cannot be found.
        let remaining
          (state : ExecutionState)
          (app : Applicable)
          : Ply<Option<int>> =
          uply {
            match app with
            | AppLambda lambda ->
              match state.lambdaInstrCache.TryGetValue lambda.exprId with
              | true, impl ->
                return
                  Some(NEList.length impl.patterns - List.length lambda.argsSoFar)
              | false, _ -> return None
            | AppNamedFn named ->
              let given = List.length named.argsSoFar
              match named.name with
              | FQFnName.Builtin builtin ->
                match state.fns.builtIn.TryGetValue builtin with
                | true, found -> return Some(List.length found.parameters - given)
                | false, _ -> return None
              | FQFnName.Package pkg ->
                match! state.fns.package pkg with
                | Some found -> return Some(NEList.length found.parameters - given)
                | None -> return None
          }

        (function
        | state, vm, [], [| DApplicable app; inputs |] ->
          uply {
            let! remaining = remaining state app
            // A function with one parameter gets the inputs as they are, even a tuple.
            // A function with several gets a tuple of exactly that many, spread out.
            let args : Result<NEList<Dval>, string> =
              match remaining, inputs with
              | Some n, DTuple(first, second, rest) when n > 1 ->
                let width = 2 + List.length rest
                if width = n then
                  Ok(NEList.ofList first (second :: rest))
                else
                  Error
                    $"the function takes {n} arguments, but the inputs are a tuple of {width}"
              | Some n, _ when n > 1 ->
                Error
                  $"the function takes {n} arguments, so the inputs must be a tuple of {n}"
              | _ -> Ok(NEList.singleton inputs)

            match args with
            | Error message -> return invalid message
            | Ok args ->
              // A callback error is converted to `ApplyProblem.Raised` below, so
              // any permission denial that caused it was handled here too. Keep
              // those records local: leaving them in the enclosing run's sink can
              // make a later, unrelated error look like that caught denial.
              let callbackState =
                { state with deniedRequests = ResizeArray<PermissionDenialRecord>() }
              match!
                Exe.executeApplicable callbackState vm.activeAccess app args
              with
              | Ok value ->
                let resultName = Dval.resultType ()
                let problemName =
                  FQTypeName.fqPackage (PackageRefs.Type.Stdlib.testApplyProblem ())
                // `Dval.resultOk` needs the Ok type up front. Here only the value knows
                // its type, so build the enum directly.
                return
                  DEnum(
                    resultName,
                    resultName,
                    [ Dval.toValueType value
                      ValueType.Known(KTCustomType(problemName, [])) ],
                    "Ok",
                    [ value ]
                  )
              | Error(rte, cs) ->
                let! stack = Exe.callStackString callbackState cs
                // Resolve error names on the test's branch.
                let! rendered = Exe.runtimeErrorToString callbackState rte
                let message =
                  match rendered with
                  | Ok(DString message) -> message
                  // Report printer failure without raising another error.
                  | Ok other -> $"(could not render the error: {other})"
                  | Error(printerError, _) ->
                    $"(could not render the error: {printerError})"
                return
                  problem
                    "Raised"
                    [ LibExecution.RuntimeTypesToDarkTypes.RuntimeError.toDT rte
                      DString message
                      DString stack ]
          }
        | _, _, [], [| _; _ |] -> Ply(invalid "expected a function")
        | _ -> incorrectArgs ())
      sqlSpec = NotQueryable
      previewable = Impure
      callEffects = Set.empty
      deprecated = NotDeprecated } ]


let builtins () = LibExecution.Builtin.make [] (fns ())
