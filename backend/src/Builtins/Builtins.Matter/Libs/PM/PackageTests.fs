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
let private executeTestBody
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


let fns (pm : PT.PackageManager) : List<BuiltInFn> =
  [ { name = fn "pmTestIsCacheSafe" 0
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
        "Runs a package test with private DBs and test permissions. "
        + "Cleans up its DB rows and returns the value or runtime error. "
        + "The Dark caller decides whether it passed."
      fn =
        (function
        | exeState, vm, _, [| hashDval |] ->
          uply {
            let (PT.Hash hashStr as hash) = PT2DT.Hash.fromDT hashDval
            let dvalKT =
              KTCustomType(
                FQTypeName.fqPackage (
                  PackageRefs.Type.LanguageTools.RuntimeTypes.dval ()
                ),
                []
              )
            let errorKT =
              KTCustomType(
                FQTypeName.fqPackage (
                  PackageRefs.Type.LanguageTools.RuntimeTypes.RuntimeError.error ()
                ),
                []
              )
            match! pm.getTest hash with
            | None ->
              return
                RuntimeError.VariableNotFound $"package test {hashStr}"
                |> RT2DT.RuntimeError.toDT
                |> Dval.resultError dvalKT errorKT
            | Some test ->
              let! result = executeTestBody exeState vm.activeAccess test
              match result with
              | Ok value ->
                return Dval.resultOk dvalKT errorKT (RT2DT.Dval.toDT value)
              | Error(rte, _) ->
                return Dval.resultError dvalKT errorKT (RT2DT.RuntimeError.toDT rte)
          }
        | _ -> incorrectArgs ())
      sqlSpec = NotQueryable
      previewable = Impure
      callEffects = set [ Effect.PackageRead ]
      deprecated = NotDeprecated } ]


let builtins ptPM = LibExecution.Builtin.make [] (fns ptPM)
