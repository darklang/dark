/// Runtime support for package tests: execution, counters, and cache analysis.
module Builtins.Matter.Libs.PM.PackageTests

open Prelude
open LibExecution.RuntimeTypes
open LibExecution.Builtin.Shortcuts
open LibExecution.Effects
open Fumble
open LibDB.Sqlite

module Dval = LibExecution.Dval
module PT = LibExecution.ProgramTypes
module PT2RT = LibExecution.ProgramTypesToRuntimeTypes
module PT2DT = LibExecution.ProgramTypesToDarkTypes
module RT2DT = LibExecution.RuntimeTypesToDarkTypes
module NR = LibExecution.RuntimeTypes.NameResolution
module Execution = LibExecution.Execution
module PackageRefs = LibExecution.PackageRefs
module Permissions = LibExecution.Permissions
module PackagePermissions = LibDB.PackagePermissions


let private requireRunningPackageTest
  (state : ExecutionState)
  (helper : string)
  : unit =
  if not state.test.isPackageTest then
    raiseUntargetedRTE (
      RuntimeError.UncaughtException(
        $"Stdlib.Test.{helper} requires a running package test",
        []
      )
    )


/// Run the test body. The caller handles conversion to Dark values and decides whether the
/// test passed.
let private executeTestBody
  (state : ExecutionState)
  (test : PT.PackageTest.PackageTest)
  : Ply<ExecutionResult> =
  uply {
    // TODO: Store test instructions alongside PT, as functions do, to avoid
    // converting the body on every run. Verify that runtime upgrades regenerate
    // them and that expected-error tests still behave the same.
    let instructions = PT2RT.Expr.toRT Map.empty 0 None test.body
    return! Execution.executeExpr state instructions
  }


/// Convert a test outcome into a Dark Result. Keep runtime errors structured
/// so expected-error assertions can compare their kind and fields.
let reflectOutcome (result : ExecutionResult) : Dval =
  let dvalKT =
    KTCustomType(
      FQTypeName.fqPackage (PackageRefs.Type.LanguageTools.RuntimeTypes.dval ()),
      []
    )
  let errorKT =
    KTCustomType(
      FQTypeName.fqPackage (
        PackageRefs.Type.LanguageTools.RuntimeTypes.RuntimeError.error ()
      ),
      []
    )
  match result with
  | Ok value -> Dval.resultOk dvalKT errorKT (RT2DT.Dval.toDT value)
  | Error(rte, _) -> Dval.resultError dvalKT errorKT (RT2DT.RuntimeError.toDT rte)

/// Execute the serialized request under the transported permissions. Dark owns
/// worker startup and files; this boundary preserves runtime values and errors.
let private executeWorker
  (pm : PT.PackageManager)
  (caller : ExecutionState)
  (requestBytes : byte[])
  (accessBytes : byte[])
  : Ply<Dval> =
  uply {
    let! request = LibDB.TestProcess.Request.deserialize requestBytes
    use accessStream = new System.IO.MemoryStream(accessBytes)
    use reader = new System.IO.BinaryReader(accessStream)
    let inherited =
      LibSerialization.Binary.Serializers.Permissions.readExecutionAccess reader
    // Set up both request kinds once.
    let guest = LibDB.PolicyStore.testState request.accountID caller
    let workerState =
      { guest with
          access =
            guest.access
            |> Permissions.Access.constrainBy inherited
            |> Permissions.Access.constrainBy caller.access
          test = { guest.test with isPackageTest = true; sideEffectCount = 0 } }
    let! outcome =
      uply {
        match request.target with
        | LibDB.TestProcess.Target.PackageTest hash ->
          let! test = pm.getTest (PT.Hash hash)
          let! result =
            match test with
            | None ->
              Ply(Error(RuntimeError.VariableNotFound $"package test {hash}", []))

            | Some test ->
              // Execute the body directly: dispatching isolation again would recurse.
              executeTestBody workerState test

          return Ok(reflectOutcome result)
        | LibDB.TestProcess.Target.TestCallback(hash, argument) ->
          return!
            Execution.executeFunction
              workerState
              (FQFnName.fqPackage hash)
              []
              (NEList.singleton argument)
      }
    let! value, code =
      uply {
        match outcome with
        | Ok value -> return value, 0
        | Error(error, stack) ->
          let! rendered = Execution.runtimeErrorToString workerState error
          let message =
            match rendered with
            | Ok(DString s) -> s
            | _ -> Execution.runtimeErrorDebugString error
          let! stackText = Execution.callStackString workerState stack
          System.Console.Error.WriteLine($"{message}\n{stackText}")
          return DString message, 1
      }
    let result =
      DEnum(
        Dval.resultType (),
        Dval.resultType (),
        [ ValueType.Unknown; ValueType.Known KTString ],
        (if code = 0 then "Ok" else "Error"),
        [ value ]
      )
    let! bytes = LibDB.TestProcess.WorkerResult.serialize result
    return
      DTuple(LibExecution.Bytes.newEphemeral bytes, Dval.dint64 (int64 code), [])
  }

/// Whether the analysis of <param test>'s body and everything it calls is complete and finds
/// no effect at all (not even the clock), counting a builtin only when <param admits> takes its
/// `previewable` and as unknown otherwise. The one walk behind running a test in-process and
/// reusing its cached pass; the two differ only in which builtins they admit.
let private provablyEffectFree
  (admits : Previewable -> bool)
  (state : ExecutionState)
  (test : PT.PackageTest.PackageTest)
  : Ply<bool> =
  uply {
    let callEffectsFor (name, version) =
      let key : FQFnName.Builtin = { name = name; version = version }
      match state.fns.builtIn.TryGetValue key with
      | true, builtin when admits builtin.previewable -> Some builtin.callEffects
      | _ -> None
    let! result =
      PackagePermissions.cacheSafetyForExpression
        PackagePermissions.Load.fromStore
        callEffectsFor
        test.body
    return result.complete && Set.isEmpty result.requiredEffects
  }

let private inProcessMemo =
  System.Collections.Concurrent.ConcurrentDictionary<PT.Hash, bool>()

/// Whether <param test> starts inside the runner rather than in a worker. Only on positive
/// evidence that it is provably effect free: it writes nothing, prints nothing and asks for no
/// permission. An `Impure` builtin with no declared effect may still be on the way, since many
/// sit only on a failure path (`Stdlib.Test.equal` renders with `toRepr` only when the values
/// differ); if the run actually reaches one it stops before the call and goes to a worker
/// (`executeInProcessTest`), so nothing `Impure` ever runs in the runner. A test's hash covers
/// its body and everything it calls, so the answer is kept for the life of the process, and a
/// test that was stopped is answered `false` from then on.
let private runsInProcess
  (state : ExecutionState)
  (test : PT.PackageTest.PackageTest)
  : Ply<bool> =
  uply {
    match inProcessMemo.TryGetValue test.hash with
    | true, known -> return known
    | false, _ ->
      let! inProcess = provablyEffectFree (fun _ -> true) state test
      inProcessMemo[test.hash] <- inProcess
      return inProcess
  }

/// Whether a passing result of <param test> may be reused: it is provably effect free and every
/// builtin on the way is `Pure`. Stricter than `runsInProcess`, because a cached pass outlives
/// the run and an `Impure` builtin may answer differently next time (`toRepr` once a name
/// moves). Anything unknown reruns.
let private isCacheSafe
  (state : ExecutionState)
  (test : PT.PackageTest.PackageTest)
  : Ply<bool> =
  provablyEffectFree
    (function
    | Pure -> true
    | Impure -> false)
    state
    test

/// Run a test the analysis cleared in this process, under the same test policy a worker
/// gets, with a counter of its own. It shares the runner's warm package caches; a worker
/// starts cold and copies the store first. `None` when the run reached an `Impure` builtin
/// and stopped before calling it: the test then belongs in a worker.
let private executeInProcessTest
  (caller : ExecutionState)
  (vm : VMState)
  (branchId : System.Guid)
  (test : PT.PackageTest.PackageTest)
  : Ply<Option<Dval>> =
  uply {
    let guest = LibDB.PolicyStore.testState caller.accountID caller
    let state =
      { guest with
          branchId = LibExecution.Branching.BranchId.Id branchId
          access = guest.access |> Permissions.Access.constrainBy vm.activeAccess
          test =
            { guest.test with
                isPackageTest = true
                sideEffectCount = 0
                inRunner = true
                stoppedAtImpure = false } }
    let! result = executeTestBody state test
    // Checked whatever the result: a builtin that catches its callee's failure could have
    // swallowed the stop.
    if state.test.stoppedAtImpure then
      inProcessMemo[test.hash] <- false
      return None
    else
      return Some(reflectOutcome result)
  }

/// Every other package test executes its own hash in a fresh worker on the run's read-only
/// snapshot when sharing is available. A refused write triggers a retry on a private copy;
/// only the retry's result is used. If sharing fails, start on a private copy.
let private executeIsolatedTest
  (caller : ExecutionState)
  (vm : VMState)
  (runId : System.Guid)
  (branchId : System.Guid)
  (workerHash : string)
  (hash : string)
  : Ply<Dval> =
  uply {
    let guest = LibDB.PolicyStore.testState caller.accountID caller
    let access = guest.access |> Permissions.Access.constrainBy vm.activeAccess
    let request : LibDB.TestProcess.Request =
      { accountID = caller.accountID
        target = LibDB.TestProcess.Target.PackageTest hash }
    let! outcome =
      LibDB.TestProcess.run
        (LibDB.TestProcess.Shared runId)
        guest
        vm
        access
        branchId
        workerHash
        request
        120000
        80
        24
    let fail message =
      // A broken worker is a failed test, never an expected error from its body.
      let resultType = FQTypeName.fqPackage (PackageRefs.Type.Stdlib.test ())
      Ok(
        DEnum(
          resultType,
          resultType,
          [],
          "Fail",
          [ DList(ValueType.Known KTString, Items([ DString message ])) ]
        )
      )
      |> reflectOutcome
    match outcome with
    | Error message -> return fail message
    | Ok output ->
      match output.exitCode, output.cleanupErrors, output.result with
      | 0, [], Some(DEnum(_, _, _, "Ok", [ result ])) -> return result
      | _ ->
        return
          fail (
            let assertionMessages =
              match output.result with
              | Some(DEnum(_, _, _, "Ok", [ DEnum(_, _, _, "Ok", [ value ]) ])) ->
                match RT2DT.Dval.fromDT value with
                | DEnum(_, _, _, "Fail", [ DList(_, Items(messages)) ]) ->
                  messages
                  |> List.choose (function
                    | DString s -> Some s
                    | _ -> None)
                | _ -> []
              | _ -> []
            String.concat
              "\n"
              (assertionMessages
               @ [ $"Isolated test worker exited {output.exitCode} without a successful result"
                   output.stdout
                   output.stderr ]
               @ output.cleanupErrors)
          )
  }


let private resultKeyType () =
  TCustomType(
    NR.ok (
      FQTypeName.fqPackage (
        PackageRefs.Type.LanguageTools.PackageManager.Test.resultKey ()
      )
    ),
    []
  )

/// A `LanguageTools.PackageManager.Test.ResultKey` record, as the store keys on it.
let private resultKeyFromDT (dv : Dval) : Option<LibDB.TestResults.Key> =
  match dv with
  | DRecord(_, _, _, fields) ->
    match
      RecordFields.tryFind "test" fields,
      RecordFields.tryFind "runtime" fields,
      RecordFields.tryFind "branch" fields,
      RecordFields.tryFind "resultType" fields
    with
    | Some(DString test),
      Some(DString runtime),
      Some(DString branch),
      Some(DString resultType) ->
      Some
        { test = test; runtime = runtime; branch = branch; resultType = resultType }
    | _ -> None
  | _ -> None


let fns (pm : PT.PackageManager) : List<BuiltInFn> =
  [ { name = fn "pmTestWithStoreSnapshot" 0
      typeParams = [ "a" ]
      parameters =
        [ Param.make "branchId" TUuid "The branch the run's tests execute on"
          Param.makeWithArgs
            "run"
            (TFn(NEList.singleton TUuid, TVariable "a"))
            "The test run"
            [ "runId" ] ]
      returnType = TVariable "a"
      description =
        "Shares one starting store snapshot across a test run and cleans it up afterward."
      fn =
        (function
        | state, vm, _, [| DUuid branchId; DApplicable callback |] ->
          uply {
            let guest = LibDB.PolicyStore.testState state.accountID state
            let access =
              guest.access |> Permissions.Access.constrainBy vm.activeAccess
            let! prepared =
              LibExecution.PermissionCheck.performHostWithAccess
                state
                vm
                access
                (LibExecution.HostTypes.Operation.FileSnapshot(
                  LibDB.TestProcess.createRunSnapshot branchId
                ))
            match prepared with
            | Ok(LibExecution.HostTypes.Response.FileSnapshot snapshot) ->
              return!
                LibDB.TestProcess.withStoreSnapshot snapshot (fun id ->
                  uply {
                    // Keep the callback's permissions and per-test counter unchanged.
                    match!
                      Execution.executeApplicable1
                        state
                        vm.activeAccess
                        callback
                        (DUuid id)
                    with
                    | Ok value -> return value
                    | Error(error, stack) ->
                      vm.nestedCallStack <- stack
                      return raiseRTE vm.threadID error
                  })
            | Error error ->
              return
                raiseRTE
                  vm.threadID
                  (RuntimeError.UncaughtException(error.message, []))
            | _ -> return Exception.raiseInternal "Invalid test snapshot response" []
          }
        | _ -> incorrectArgs ())
      previewable = Impure
      // The callback may write. Keep it in program order, rather than letting
      // the interpreter defer this scope as an asynchronous package read.
      callEffects = set [ Effect.Native ]
      deprecated = NotDeprecated }

    { name = fn "pmTestWorkerExecute" 0
      typeParams = []
      parameters = [ Param.make "request" TBytes ""; Param.make "access" TBytes "" ]
      returnType = TTuple(TBytes, TInt64, [])
      description =
        "Executes a serialized test request with its inherited permissions and returns its serialized result and exit code."
      fn =
        (function
        | state, vm, _, [| DBytes request; DBytes access |] ->
          uply {
            let! request = LibExecution.Bytes.readBytes state request
            let! access = LibExecution.Bytes.readBytes state access
            return!
              executeWorker pm { state with access = vm.activeAccess } request access
          }
        | _ -> incorrectArgs ())
      previewable = Impure
      callEffects = set [ Effect.Native ]
      deprecated = NotDeprecated }

    { name = fn "pmTestResultGet" 0
      typeParams = []
      parameters = [ Param.make "key" (resultKeyType ()) "Which stored result" ]
      returnType = TypeReference.option TString
      description =
        "The outcome `dark test run` stored under <param key>, or None if it has none. "
        + "The store does not interpret the outcome."
      fn =
        (function
        | _, _, _, [| keyDval |] ->
          match resultKeyFromDT keyDval with
          | Some key ->
            uply {
              let! outcome = LibDB.TestResults.get key
              return outcome |> Option.map DString |> Dval.option KTString
            }
          | None -> incorrectArgs ()
        | _ -> incorrectArgs ())
      previewable = Impure
      callEffects = set [ Effect.PackageRead ]
      deprecated = NotDeprecated }

    { name = fn "pmTestResultPut" 0
      typeParams = []
      parameters =
        [ Param.make "key" (resultKeyType ()) "Which stored result"
          Param.make
            "outcome"
            TString
            "The outcome to store, replacing any already there" ]
      returnType = TUnit
      description =
        "Stores <param outcome> under <param key> in the `dark test run` result cache."
      fn =
        (function
        | _, _, _, [| keyDval; DString outcome |] ->
          match resultKeyFromDT keyDval with
          | Some key ->
            uply {
              do! LibDB.TestResults.put key outcome
              return DUnit
            }
          | None -> incorrectArgs ()
        | _ -> incorrectArgs ())
      previewable = Impure
      callEffects = set [ Effect.PackageRead; Effect.PackageWrite ]
      deprecated = NotDeprecated }

    { name = fn "pmTestIsCacheSafe" 0
      typeParams = []
      parameters =
        [ Param.make
            "testHash"
            (TCustomType(NR.ok (PT2DT.Hash.typeName ()), []))
            "Hash of the package test to analyze" ]
      returnType = TBool
      description =
        "Checks the test and the functions it calls to decide whether a cached pass "
        + "can be reused. Returns false if it cannot prove reuse is safe."
      fn =
        (function
        | state, _, _, [| hashDval |] ->
          uply {
            let hash = PT2DT.Hash.fromDT hashDval
            match! pm.getTest hash with
            | None -> return DBool false
            | Some test ->
              let! safe = isCacheSafe state test
              return DBool safe
          }
        | _ -> incorrectArgs ())
      previewable = Impure
      callEffects = set [ Effect.PackageRead ]
      deprecated = NotDeprecated }

    { name = fn "testCounter" 0
      typeParams = []
      parameters =
        [ Param.make "increment" TBool "Whether to add one to the counter first" ]
      returnType = TInt64
      description =
        "This test's counter, which starts at zero for each test run, after adding one "
        + "if <param increment>. Use to check how often a callback or expression runs. "
        + "Only available while a package test is running."
      fn =
        (function
        | state, _, _, [| DBool increment |] ->
          requireRunningPackageTest
            state
            (if increment then "incrementCounter" else "counterValue")
          let count =
            if increment then
              System.Threading.Interlocked.Increment(&state.test.sideEffectCount)
            else
              System.Threading.Volatile.Read(&state.test.sideEffectCount)
          count |> int64 |> Dval.int64 |> Ply
        | _ -> incorrectArgs ())
      previewable = Impure
      callEffects = Set.empty
      deprecated = NotDeprecated }

    { name = fn "pmTestRaiseRuntimeError" 0
      typeParams = []
      parameters = [ Param.make "message" TString "The injected exception message" ]
      returnType = TInt64
      description =
        "Raises a runtime error so a package test can check error propagation and messages. "
        + "Only available while a package test is running."
      fn =
        (function
        | state, _, _, [| DString message |] ->
          requireRunningPackageTest state "raiseRuntimeError"
          raiseUntargetedRTE (RuntimeError.UncaughtException(message, []))
        | _ -> incorrectArgs ())
      // Pure: inside a test, the same message always raises the same error, so a test that
      // checks how an error propagates can run in-process and keep its cached pass.
      previewable = Pure
      callEffects = Set.empty
      deprecated = NotDeprecated }

    { name = fn "pmTestRunsInProcess" 0
      typeParams = []
      parameters =
        [ Param.make
            "testHash"
            (TCustomType(NR.ok (PT2DT.Hash.typeName ()), []))
            "Hash of the package test to classify" ]
      returnType = TBool
      description =
        "Whether `pmExecuteTest` runs the test inside this process rather than in a "
        + "fresh process and store: true only when analysis shows it needs no effect, "
        + "and false from then on once a run of it reached an `Impure` builtin."
      fn =
        (function
        | state, _, _, [| hashDval |] ->
          uply {
            match! pm.getTest (PT2DT.Hash.fromDT hashDval) with
            | None -> return DBool false
            | Some test ->
              let! inProcess = runsInProcess state test
              return DBool inProcess
          }
        | _ -> incorrectArgs ())
      previewable = Impure
      callEffects = set [ Effect.PackageRead ]
      deprecated = NotDeprecated }

    { name = fn "pmTestReran" 0
      typeParams = []
      parameters =
        [ Param.make "runId" TUuid "The test run"
          Param.make
            "testHash"
            (TCustomType(NR.ok (PT2DT.Hash.typeName ()), []))
            "Hash of the package test" ]
      returnType = TBool
      description =
        "Whether the test wrote to the run's shared store and so ran again on a copy."
      fn =
        (function
        | _, _, _, [| DUuid runId; hashDval |] ->
          let (PT.Hash hash) = PT2DT.Hash.fromDT hashDval
          LibDB.TestProcess.reranIn runId hash |> DBool |> Ply
        | _ -> incorrectArgs ())
      previewable = Impure
      // Changes as tests finish, so keep it in program order.
      callEffects = set [ Effect.Native ]
      deprecated = NotDeprecated }

    { name = fn "pmExecuteTest" 0
      typeParams = []
      parameters =
        [ Param.make "runId" TUuid "The run whose starting store to use"
          Param.make "branchId" TUuid "The branch to execute on"
          Param.make
            "testHash"
            (TCustomType(NR.ok (PT2DT.Hash.typeName ()), []))
            "Hash of the package test to execute"
          Param.makeWithArgs
            "worker"
            (TFn(NEList.singleton (TList TString), TInt64))
            "The worker entry point"
            [ "args" ] ]
      returnType =
        TypeReference.result
          (TCustomType(
            NR.ok (
              FQTypeName.fqPackage (
                PackageRefs.Type.LanguageTools.RuntimeTypes.dval ()
              )
            ),
            []
          ))
          (TCustomType(
            NR.ok (
              FQTypeName.fqPackage (
                PackageRefs.Type.LanguageTools.RuntimeTypes.RuntimeError.error ()
              )
            ),
            []
          ))
      description =
        "Runs a package test with test permissions: in this process when analysis shows "
        + "it needs no effect and the run calls only `Pure` builtins, otherwise in a fresh "
        + "process and store, cleaned up "
        + "afterwards. Returns the value or runtime error. "
        + "The Dark caller decides whether it passed."
      fn =
        (function
        | exeState,
          vm,
          _,
          [| DUuid runId
             DUuid branchId
             hashDval
             DApplicable(AppNamedFn { name = FQFnName.Package(Hash workerHash)
                                      argsSoFar = []
                                      typeArgs = [] }) |] ->
          uply {
            let (PT.Hash hashStr as hash) = PT2DT.Hash.fromDT hashDval
            match! pm.getTest hash with
            | None ->
              return
                reflectOutcome (
                  Error(RuntimeError.VariableNotFound $"package test {hashStr}", [])
                )
            | Some test ->
              let! inRunner =
                uply {
                  match! runsInProcess exeState test with
                  | true -> return! executeInProcessTest exeState vm branchId test
                  | false -> return None
                }
              match inRunner with
              | Some outcome -> return outcome
              | None ->
                return!
                  executeIsolatedTest exeState vm runId branchId workerHash hashStr
          }
        | _ -> incorrectArgs ())
      previewable = Impure
      // Runs a whole test in a child process. As a package read alone the interpreter would
      // overlap it, so a `List.map` over tests would start one child per element at once.
      callEffects = set [ Effect.PackageRead; Effect.Native ]
      deprecated = NotDeprecated } ]


let builtins ptPM = LibExecution.Builtin.make [] (fns ptPM)
