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


/// Remove only the rows belonging to this execution's private DBs.
let private cleanupTestDBs (dbs : List<PT.DB.T>) : Ply<unit> =
  uply {
    for db in dbs do
      do!
        Sql.query "DELETE FROM user_data_v0 WHERE table_tlid = @tlid"
        |> Sql.parameters [ "tlid", Sql.id db.tlid ]
        |> Sql.executeStatementAsync
  }


/// Give the prepared worker state the test's private DBs, then clean up after execution.
/// The caller handles conversion to Dark values and decides whether the test passed.
let private executeTestBody
  (state : ExecutionState)
  (test : PT.PackageTest.PackageTest)
  : Ply<ExecutionResult> =
  uply {
    // Fresh IDs ensure each run sees only its own rows, including concurrent runs.
    let testDBs : List<PT.DB.T> =
      test.testDBs
      |> List.map (fun (name, typ) ->
        { tlid = gid (); name = name; version = 0; typ = typ })
    let program =
      { state.program with
          dbs =
            testDBs |> List.map (fun db -> db.name, PT2RT.DB.toRT db) |> Map.ofList }
    let state = { state with program = program }
    // TODO: Store test instructions alongside PT, as functions do, to avoid
    // converting the body on every run. Verify that runtime upgrades regenerate
    // them and that expected-error tests still behave the same.
    let instructions = PT2RT.Expr.toRT Map.empty 0 None test.body
    let! result =
      uply {
        try
          return! Execution.executeExpr state instructions
        with e ->
          do! cleanupTestDBs testDBs
          return Exception.reraise e
      }
    do! cleanupTestDBs testDBs
    return result
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
    let request = LibDB.TestProcess.Request.deserialize requestBytes
    use accessStream = new System.IO.MemoryStream(accessBytes)
    use reader = new System.IO.BinaryReader(accessStream)
    let inherited =
      LibSerialization.Binary.Serializers.Permissions.readExecutionAccess reader
    // Set up both request kinds once. The test body only adds its private DBs.
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
            | _ -> string error
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
    let bytes =
      LibSerialization.Binary.Serialization.RT.Dval.serialize "test result" result
    return DTuple(LibExecution.Blob.newEphemeral bytes, Dval.int (bigint code), [])
  }

/// Every package test executes its own hash in a fresh worker and store snapshot.
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
        (Some(LibDB.TestProcess.copyFromRun runId))
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
          [ DList(ValueType.Known KTString, [ DString message ]) ]
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
                | DEnum(_, _, _, "Fail", [ DList(_, messages) ]) ->
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


let fns (pm : PT.PackageManager) : List<BuiltInFn> =
  [ { name = fn "pmTestWithStoreSnapshot" 0
      typeParams = [ "a" ]
      parameters =
        [ Param.makeWithArgs
            "run"
            (TFn(NEList.singleton TUuid, TVariable "a"))
            "The test run"
            [ "runId" ] ]
      returnType = TVariable "a"
      description =
        "Shares one starting store snapshot across a test run and cleans it up afterward."
      fn =
        (function
        | state, vm, _, [| DApplicable callback |] ->
          uply {
            let guest = LibDB.PolicyStore.testState state.accountID state
            let access =
              guest.access |> Permissions.Access.constrainBy vm.activeAccess
            let! prepared =
              LibExecution.PermissionCheck.performHostWithAccess
                state
                vm
                access
                (LibExecution.HostTypes.Operation.FileSnapshot
                  LibDB.Sqlite.Backup.toTestBaseline)
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
      sqlSpec = NotQueryable
      previewable = Impure
      // The callback may write. Keep it in program order, rather than letting
      // the interpreter defer this scope as an asynchronous package read.
      callEffects = set [ Effect.Native ]
      deprecated = NotDeprecated }

    { name = fn "pmTestWorkerExecute" 0
      typeParams = []
      parameters = [ Param.make "request" TBlob ""; Param.make "access" TBlob "" ]
      returnType = TTuple(TBlob, TInt, [])
      description =
        "Executes a serialized test request with its inherited permissions and returns its serialized result and exit code."
      fn =
        (function
        | state, vm, _, [| DBlob request; DBlob access |] ->
          uply {
            let! request = LibExecution.Blob.readBytes state request
            let! access = LibExecution.Blob.readBytes state access
            return!
              executeWorker pm { state with access = vm.activeAccess } request access
          }
        | _ -> incorrectArgs ())
      sqlSpec = NotQueryable
      previewable = Impure
      callEffects = set [ Effect.Native ]
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
            | Some test when not (List.isEmpty test.testDBs) -> return DBool false
            | Some test ->
              let cacheSafeCallEffectsFor (name, version) =
                let key : FQFnName.Builtin = { name = name; version = version }
                // These calls observe branch-dependent rendering or mutable
                // test-local state despite needing no host permissions.
                if
                  name = "toRepr"
                  || name = "testIncrementCounter"
                  || name = "testCounterValue"
                then
                  None
                else
                  match state.fns.builtIn.TryGetValue key with
                  | true, builtin -> Some builtin.callEffects
                  | false, _ -> None
              let! result =
                PackagePermissions.cacheSafetyForExpression
                  PackagePermissions.Load.fromStore
                  cacheSafeCallEffectsFor
                  test.body
              return DBool(result.complete && Set.isEmpty result.requiredEffects)
          }
        | _ -> incorrectArgs ())
      sqlSpec = NotQueryable
      previewable = Impure
      callEffects = set [ Effect.PackageRead ]
      deprecated = NotDeprecated }

    { name = fn "testIncrementCounter" 0
      typeParams = [ "a" ]
      parameters = [ Param.make "value" (TVariable "a") "The value to return" ]
      returnType = TVariable "a"
      description =
        "Increments this test's counter and returns the value unchanged. "
        + "Use to check how often a callback or expression runs. "
        + "Only available while a package test is running."
      fn =
        (function
        | state, _, _, [| value |] ->
          requireRunningPackageTest state "incrementCounter"
          let _ = System.Threading.Interlocked.Increment(&state.test.sideEffectCount)
          Ply value
        | _ -> incorrectArgs ())
      sqlSpec = NotQueryable
      previewable = Impure
      callEffects = Set.empty
      deprecated = NotDeprecated }

    { name = fn "testCounterValue" 0
      typeParams = []
      parameters = [ Param.make "unit" TUnit "" ]
      returnType = TInt64
      description =
        "Returns this test's counter, which starts at zero for each test run. "
        + "Only available while a package test is running."
      fn =
        (function
        | state, _, _, [| DUnit |] ->
          requireRunningPackageTest state "counterValue"
          System.Threading.Volatile.Read(&state.test.sideEffectCount)
          |> int64
          |> Dval.int64
          |> Ply
        | _ -> incorrectArgs ())
      sqlSpec = NotQueryable
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
      sqlSpec = NotQueryable
      previewable = Impure
      callEffects = Set.empty
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
            (TFn(NEList.singleton (TList TString), TInt))
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
        "Runs a package test in a fresh process and store with test permissions. "
        + "Cleans up after execution and returns the value or runtime error. "
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
            | Some _ ->
              return!
                executeIsolatedTest exeState vm runId branchId workerHash hashStr
          }
        | _ -> incorrectArgs ())
      sqlSpec = NotQueryable
      previewable = Impure
      callEffects = set [ Effect.PackageRead ]
      deprecated = NotDeprecated } ]


let builtins ptPM = LibExecution.Builtin.make [] (fns ptPM)
