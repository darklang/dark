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


/// Give the test fresh DBs and a counter, then clean up after execution.
/// The caller handles conversion to Dark values and decides whether the test passed.
let executeTestBody
  (caller : ExecutionState)
  (callerAccess : Permissions.Access)
  (test : PT.PackageTest.PackageTest)
  : Ply<ExecutionResult> =
  uply {
    // Fresh IDs ensure each run sees only its own rows, including concurrent runs.
    let testDBs : List<PT.DB.T> =
      test.testDBs
      |> List.map (fun (name, typ) ->
        { tlid = gid (); name = name; version = 0; typ = typ })
    let program =
      { caller.program with
          dbs =
            testDBs |> List.map (fun db -> db.name, PT2RT.DB.toRT db) |> Map.ofList }
    // Widen only the test instance policy. Caller and callee restrictions still apply.
    let guest = LibDB.PolicyStore.testState caller.accountID caller
    let state =
      { guest with
          access = guest.access |> Permissions.Access.constrainBy callerAccess
          program = program
          test = { guest.test with isPackageTest = true; sideEffectCount = 0 } }
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

/// Every package test executes its own hash in a fresh worker and store snapshot.
let private executeIsolatedTest
  (caller : ExecutionState)
  (vm : VMState)
  (hash : string)
  : Ply<Dval> =
  uply {
    let guest = LibDB.PolicyStore.testState caller.accountID caller
    let access = guest.access |> Permissions.Access.constrainBy vm.activeAccess
    let request =
      DTuple(
        DString hash,
        DUnit,
        [ caller.accountID |> Option.map DUuid |> Dval.option KTUuid; DBool true ]
      )
    let! outcome =
      LibDB.TestProcess.run
        caller.testStoreSnapshot
        guest
        vm
        access
        caller.branchId.Guid
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
  [ { name = fn "pmTestWithSnapshot" 0
      typeParams = [ "a" ]
      parameters =
        [ Param.makeWithArgs
            "run"
            (TFn(NEList.singleton TUnit, TVariable "a"))
            "The test run"
            [ "unit" ] ]
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
                (LibExecution.HostTypes.Operation.TestStoreSnapshot
                  LibDB.Sqlite.Backup.toTestBaseline)
            match prepared with
            | Ok(LibExecution.HostTypes.Response.TestStoreSnapshot snapshot) ->
              use snapshot = snapshot
              let scoped = { state with testStoreSnapshot = Some snapshot }
              // The callback keeps the caller's permissions. Only worker setup
              // uses the test instance policy, just as standalone execution does.
              match!
                Execution.executeApplicable1 scoped vm.activeAccess callback DUnit
              with
              | Ok value -> return value
              | Error(error, stack) ->
                vm.nestedCallStack <- stack
                return raiseRTE vm.threadID error
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
        [ Param.make
            "testHash"
            (TCustomType(NR.ok (PT2DT.Hash.typeName ()), []))
            "Hash of the package test to execute" ]
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
        | exeState, vm, _, [| hashDval |] ->
          uply {
            let (PT.Hash hashStr as hash) = PT2DT.Hash.fromDT hashDval
            match! pm.getTest hash with
            | None ->
              return
                reflectOutcome (
                  Error(RuntimeError.VariableNotFound $"package test {hashStr}", [])
                )
            | Some _ -> return! executeIsolatedTest exeState vm hashStr
          }
        | _ -> incorrectArgs ())
      sqlSpec = NotQueryable
      previewable = Impure
      callEffects = set [ Effect.PackageRead ]
      deprecated = NotDeprecated } ]


let builtins ptPM = LibExecution.Builtin.make [] (fns ptPM)
