/// Interpreter permission gates, captured access, and deferred execution.
module Tests.PermissionsGate

open Expecto
open Prelude
open TestUtils.TestUtils
open TestUtils.PTShortcuts

module RT = LibExecution.RuntimeTypes
module Effects = LibExecution.Effects
module PT = LibExecution.ProgramTypes
module PT2RT = LibExecution.ProgramTypesToRuntimeTypes
module RTE = RT.RuntimeError

// The gate enforces the active permission policies.
let clockCall () =
  eApply (eBuiltinFn "timeNowMs" 0) [] [ eUnit () ]
  |> PT2RT.Expr.toRT Map.empty 0 None

// The gate denies before this raw descriptor reaches the builtin body.
let nativeCall () =
  eApply (eBuiltinFn "posixFdClose" 0) [] [ PT.EInt(gid (), -1I) ]
  |> PT2RT.Expr.toRT Map.empty 0 None

let private denyAllAccess =
  LibExecution.Permissions.Access.start LibExecution.Permissions.Policy.denyAll

let private denyAll (state : RT.ExecutionState) : RT.ExecutionState =
  { state with access = denyAllAccess }

/// Check that a denial names the permission layer and requested resource.
let private expectDenied (names : List<string>) (actual : RT.ExecutionResult) =
  match actual with
  | Error(RTE.Error.UncaughtException(msg, _), _) ->
    Expect.stringContains msg "permission denied" "the gate denied the operation"
    for name in names do
      Expect.stringContains msg name $"the denial names `{name}`"
  | other -> failtestf "expected a permission denial, got %A" other

/// An allowed call may fail later; it must not fail at the permission gate.
let private expectNotDenied (actual : RT.ExecutionResult) =
  match actual with
  | Error(RTE.Error.UncaughtException(msg, _), _) ->
    Expect.isFalse (msg.Contains "permission denied") "the gate must not deny"
  | _ -> ()

let private dummyVM () =
  RT.VMState.createWithoutTLID
    { registerCount = 1; instructions = [ RT.LoadVal(0, RT.DUnit) ]; resultIn = 0 }

/// Host-only builtins must reject guest calls at the admin boundary.
let private expectHostOnly
  (name : string)
  (args : RT.Dval[])
  : System.Threading.Tasks.Task<unit> =
  task {
    let! (exeState : RT.ExecutionState) =
      executionStateFor TestValues.pm false Map.empty
    let builtin = exeState.fns.builtIn[RT.FQFnName.builtin name 0]
    try
      let! _ = builtin.fn (struct (exeState, dummyVM (), [], args)) |> Ply.toTask
      Expect.equal 1 2 "expected a policy-admin denial"
    with RT.RuntimeErrorException(_, RTE.Error.UncaughtException(message, _)) ->
      Expect.stringContains
        message
        "trusted `dark permissions` command"
        "the admin boundary is explained"
  }

let denied =
  testTask "an effectful builtin is denied by a deny-all policy" {
    let! exeState = executionStateFor TestValues.pm false Map.empty
    let! actual =
      LibExecution.Execution.executeExpr (denyAll exeState) (clockCall ())
    expectDenied [ "clock" ] actual
  }

let allowed =
  testTask "the gate permits an effect under an allow-all policy" {
    let! exeState = executionStateFor TestValues.pm false Map.empty // allow-all access
    let! actual = LibExecution.Execution.executeExpr exeState (clockCall ())
    expectNotDenied actual
  }

let nativeDenied =
  testTask "a native builtin is denied by restricted execution" {
    let! exeState = executionStateFor TestValues.pm false Map.empty
    let! actual =
      LibExecution.Execution.executeExpr (denyAll exeState) (nativeCall ())
    expectDenied [ "posixFdClose" ] actual
  }

let nativeAllowedByAllowAll =
  testTask "allow-all explicitly permits a native builtin" {
    let! exeState = executionStateFor TestValues.pm false Map.empty
    let! actual = LibExecution.Execution.executeExpr exeState (nativeCall ())
    expectNotDenied actual
  }

let newRunPolicyDenies =
  testTask "the run policy attenuates an allow-all instance" {
    let! (exeState : RT.ExecutionState) =
      executionStateFor TestValues.pm false Map.empty
    let access =
      exeState.access
      |> LibExecution.Permissions.Access.restrict
        LibExecution.Permissions.Layer.Run
        LibExecution.Permissions.Policy.denyAll
    let restricted : RT.ExecutionState = { exeState with access = access }
    let! actual = LibExecution.Execution.executeExpr restricted (clockCall ())
    expectDenied [ "run policy" ] actual
  }

let private clockEffects = Set.singleton Effects.Effect.Clock

let private ceilingFn
  (hash : string)
  (ceiling : Option<Set<Effects.Effect>>)
  (body : PT.Expr)
  : PT.PackageFn.PackageFn =
  { hash = PT.Hash hash
    typeParams = []
    parameters = NEList.singleton { name = "unit"; typ = PT.TUnit; description = "" }
    returnType = PT.TInt
    body = body
    description = ""
    permissionCeiling = ceiling }

let private clockBody = eApply (eBuiltinFn "timeNowMs" 0) [] [ eUnit () ]

let private pmWith (fns : List<PT.PackageFn.PackageFn>) : PT.PackageManager =
  let byHash = fns |> List.map (fun fn -> fn.hash, fn) |> Map.ofList
  { TestValues.pm with
      getFn =
        fun hash ->
          match Map.tryFind hash byHash with
          | Some fn -> Ply(Some fn)
          | None -> TestValues.pm.getFn hash }

let private runPackageFnWith
  (configure : RT.ExecutionState -> RT.ExecutionState)
  (pm : PT.PackageManager)
  (hash : string)
  : System.Threading.Tasks.Task<RT.ExecutionResult> =
  task {
    let! exeState = executionStateFor pm false Map.empty // instance grants all
    return!
      LibExecution.Execution.executeFunction
        (configure exeState)
        (RT.FQFnName.fqPackage hash)
        []
        (NEList.singleton RT.DUnit)
  }

let private runPackageFn = runPackageFnWith (fun state -> state)

/// Deny one package by hash while every other package stays allow-all.
let private denyPackage (hash : string) (state : RT.ExecutionState) =
  { state with
      packagePolicy =
        fun id ->
          if id = RT.Hash hash then
            LibExecution.Permissions.Policy.denyAll
          else
            LibExecution.Permissions.Policy.allowAll }

let packagePolicyDenies =
  testTask "a package policy attenuates instance and run access" {
    let hash = "permissions-package-denied"
    let fn = ceilingFn hash None clockBody
    let! actual = runPackageFnWith (denyPackage hash) (pmWith [ fn ]) hash
    expectDenied [ "package policy" ] actual
  }

let escapedLambdaKeepsAccess =
  testTask "a returned lambda keeps its creator's package access" {
    let producerHash = "permissions-lambda-producer"
    let runnerHash = "permissions-lambda-runner"
    let lambdaBody = eLambda (gid ()) [ lpVar "unit" ] clockBody
    let producer =
      { ceilingFn producerHash None lambdaBody with
          returnType = PT.TFn(NEList.singleton PT.TUnit, PT.TInt) }
    let returnedLambda = eApply (ePackageFn producerHash) [] [ eUnit () ]
    let runnerBody = eApply returnedLambda [] [ eUnit () ]
    let runner = ceilingFn runnerHash None runnerBody
    let! actual =
      runPackageFnWith
        (denyPackage producerHash)
        (pmWith [ producer; runner ])
        runnerHash
    // The closure kept its creator's package access.
    expectDenied [ "package policy" ] actual
  }

let functionCeilingDenies =
  testTask "a function ceiling attenuates an otherwise-permitted instance" {
    let hash = "permissions-ceiling-denied"
    let fn = ceilingFn hash (Some Set.empty) clockBody
    let! actual = runPackageFn (pmWith [ fn ]) hash
    expectDenied [ "function policy"; "clock" ] actual
  }

let functionCeilingAllows =
  testTask "a function ceiling can permit an instance-granted effect" {
    let hash = "permissions-ceiling-allowed"
    let fn = ceilingFn hash (Some clockEffects) clockBody
    let! actual = runPackageFn (pmWith [ fn ]) hash
    expectNotDenied actual
  }

let callerCeilingAttenuates =
  testTask "a caller ceiling continues to constrain its callees" {
    let calleeHash = "permissions-ceiling-callee"
    let callerHash = "permissions-ceiling-caller"
    let callee = ceilingFn calleeHash (Some clockEffects) clockBody
    let callerBody = eApply (ePackageFn calleeHash) [] [ eUnit () ]
    let caller = ceilingFn callerHash (Some Set.empty) callerBody
    let! actual = runPackageFn (pmWith [ caller; callee ]) callerHash
    // The caller's ceiling constrains the callee.
    expectDenied [ "function policy" ] actual
  }

let escapedLambdaKeepsCeiling =
  testTask "a returned lambda keeps its creator's function ceiling" {
    let producerHash = "permissions-ceiling-lambda-producer"
    let runnerHash = "permissions-ceiling-lambda-runner"
    let lambdaBody = eLambda (gid ()) [ lpVar "unit" ] clockBody
    let producer =
      { ceilingFn producerHash (Some Set.empty) lambdaBody with
          returnType = PT.TFn(NEList.singleton PT.TUnit, PT.TInt) }
    let returnedLambda = eApply (ePackageFn producerHash) [] [ eUnit () ]
    let runnerBody = eApply returnedLambda [] [ eUnit () ]
    let runner = ceilingFn runnerHash None runnerBody
    let! actual = runPackageFn (pmWith [ producer; runner ]) runnerHash
    // The escaped closure retains its creator's ceiling.
    expectDenied [ "function policy" ] actual
  }

// ── deferred-execution matrix ───────────────────────────────
//
// One rule for every value that runs later: it keeps the restrictions of
// the frame that MADE it, and runs under the intersection of those with the
// frame that RUNS it. Two findings were each one cell of this table: a
// partial application built in a `:{}` function escaping with its
// reference's broader capture, and a stream built broad and drained inside
// a `:{}` function running its callback under the construction access
// alone. Every cell: a producer builds the value, a runner runs it, one of
// them is restricted, and the run must be refused by that restriction.

let private shapes = [ "named fn"; "lambda"; "partial application"; "stream" ]
let private directions =
  [ "restricted maker, broad runner"; "broad maker, restricted runner" ]
let private layers = [ "function ceiling"; "package approval" ]

/// `Int -> Unit -> Int`: reads the clock; the partial application applies the Int.
let private clockTwoArgs (hash : string) : PT.PackageFn.PackageFn =
  { ceilingFn hash None clockBody with
      parameters =
        NEList.ofList
          { name = "n"; typ = PT.TInt; description = "" }
          [ { name = "unit"; typ = PT.TUnit; description = "" } ] }

/// One cell. Three functions, because WHERE a value is made and WHERE it is
/// run must be different frames: a broad `driver` calls the `producer` to
/// make the value and hands it to the `runner` as a parameter. Calling the
/// producer from inside the runner would make it inherit the runner's
/// restriction, and a "broad maker" would never be broad -- the first
/// version of this matrix did exactly that and detected nothing. The partial
/// application likewise applies a reference the driver already stamped in
/// its broad frame, which is the escape: a partial built inside a `:{}`
/// function kept that broader stamp.
let private deferredCase
  (shape : string)
  (direction : string)
  (layer : string)
  : Test =
  let slug (text : string) = text.Replace(" ", "-").Replace(",", "")
  let id = $"{slug shape}-{slug direction}-{slug layer}"
  let clockHash = $"matrix-clock-{id}"
  let clock2Hash = clockHash + "-2"
  let producerHash = $"matrix-producer-{id}"
  let runnerHash = $"matrix-runner-{id}"
  let driverHash = $"matrix-driver-{id}"
  let makerRestricted = direction = "restricted maker, broad runner"
  let byCeiling = layer = "function ceiling"
  let restrictedCeiling (isThisOne : bool) =
    if byCeiling && isThisOne then Some Set.empty else None
  let unitLambdaClock = eLambda (gid ()) [ lpVar "unit" ] clockBody
  let param (name : string) : PT.PackageFn.Parameter =
    { name = name; typ = PT.TVariable "a"; description = "" }
  let generic
    (parameters : NEList<PT.PackageFn.Parameter>)
    (body : PT.Expr)
    (hash : string)
    (ceiling : Option<Set<Effects.Effect>>)
    : PT.PackageFn.PackageFn =
    { ceilingFn hash ceiling body with
        parameters = parameters
        typeParams = [ "a" ]
        returnType = PT.TVariable "a" }
  // The producer makes the value. For the partial application it receives
  // the reference `f` from the driver and applies its first argument.
  let makeBody =
    match shape with
    | "named fn" -> ePackageFn clockHash
    | "lambda" -> unitLambdaClock
    | "partial application" -> eApply (eVar "f") [] [ PT.EInt(gid (), 1I) ]
    | _ ->
      eApply
        (eBuiltinFn "streamMap" 0)
        [ PT.TUnit; PT.TInt ]
        [ eApply (eBuiltinFn "streamFromList" 0) [ PT.TUnit ] [ eList [ eUnit () ] ]
          unitLambdaClock ]
  let isStream = shape = "stream"
  // A stream crossing a function boundary needs its concrete type: the
  // fn-valued shapes unify with a type variable, a `DStream` does not.
  let streamOfInt = PT.TStream PT.TInt
  let producer =
    let fn =
      generic
        (NEList.singleton (
          param (if shape = "partial application" then "f" else "unit")
        ))
        makeBody
        producerHash
        (restrictedCeiling makerRestricted)
    if isStream then { fn with typeParams = []; returnType = streamOfInt } else fn
  // The runner runs the value it is handed.
  let runBody =
    match shape with
    | "stream" -> eApply (eBuiltinFn "streamToList" 0) [ PT.TInt ] [ eVar "v" ]
    | _ -> eApply (eVar "v") [] [ eUnit () ]
  let runner =
    let fn =
      generic
        (NEList.singleton (param "v"))
        runBody
        runnerHash
        (restrictedCeiling (not makerRestricted))
    if isStream then
      { fn with
          parameters =
            NEList.singleton { name = "v"; typ = streamOfInt; description = "" }
          typeParams = []
          returnType = PT.TList PT.TInt }
    else
      fn
  // The driver is broad and outermost: make, then run.
  let driverBody =
    let runIt (made : PT.Expr) = eApply (ePackageFn runnerHash) [] [ made ]
    match shape with
    | "partial application" ->
      // `f` is loaded in value position here, so it is stamped with the
      // driver's broad access before the producer ever sees it.
      eLet
        (lpVar "f")
        (ePackageFn clock2Hash)
        (runIt (eApply (ePackageFn producerHash) [] [ eVar "f" ]))
    | _ -> runIt (eApply (ePackageFn producerHash) [] [ eUnit () ])
  let driver = generic (NEList.singleton (param "unit")) driverBody driverHash None
  let pm =
    pmWith
      [ ceilingFn clockHash None clockBody
        clockTwoArgs clock2Hash
        producer
        runner
        driver ]
  let configure =
    if byCeiling then
      (fun state -> state)
    else
      denyPackage (if makerRestricted then producerHash else runnerHash)
  testTask $"deferred: {shape}, {direction}, {layer}" {
    let! actual = runPackageFnWith configure pm driverHash
    expectDenied
      [ (if byCeiling then "function policy" else "package policy") ]
      actual
  }

let deferredExecutionMatrix : List<Test> =
  [ for shape in shapes do
      for direction in directions do
        for layer in layers do
          deferredCase shape direction layer ]

let guestCannotChangePolicies =
  testTask "guest code cannot change stored policies" {
    do! expectHostOnly "pmPolicySetInstance" [| RT.DUnit |]
    do! expectHostOnly "pmPolicyEditInstance" [| RT.DUnit |]
  }

let guestCannotApprovePackages =
  testTask "guest code cannot change package approvals" {
    do!
      expectHostOnly
        "pmPolicyApproveVersion"
        [| LibExecution.Dval.optionNone RT.KTUuid
           RT.DString "loc"
           RT.DString "unknown"
           RT.DBool false
           RT.DBool false |]
  }

let guestFileApiCannotReachPolicyStore =
  testTask "guest filesystem access cannot reach host policy state" {
    let! exeState = executionStateFor TestValues.pm false Map.empty
    let vm = dummyVM ()
    match LibDB.LocalFile.path "policies.bin" with
    | Error message -> Expect.equal 1 2 message
    | Ok policyPath ->
      let assertDenied (path : string) (explanation : string) : unit =
        try
          LibExecution.PermissionCheck.requireFileWrite exeState vm path
          Expect.equal 1 2 "expected the protected-path denial"
        with RT.RuntimeErrorException(_, RTE.Error.UncaughtException(message, _)) ->
          Expect.stringContains message "protected host policy state" explanation
      assertDenied
        policyPath
        "instance allow-all cannot authorize writing its own policy"
      policyPath
      |> System.IO.Path.GetDirectoryName
      |> System.IO.Path.GetDirectoryName
      |> fun darklangDirectory ->
          assertDenied
            darklangDirectory
            "an ancestor cannot be renamed to edit policy under another spelling"
  }

let evaluatedNamedFnInContainerCannotWidenReentry =
  testTask "a named fn captures access before being nested in a container" {
    let! exeState = executionStateFor TestValues.pm false Map.empty
    let expr = eList [ eBuiltinFn "timeNowMs" 0 ] |> PT2RT.Expr.toRT Map.empty 0 None
    let! produced = LibExecution.Execution.executeExpr (denyAll exeState) expr
    match produced with
    | Ok(RT.DList(_, [ RT.DApplicable(RT.AppNamedFn namedFn) ])) ->
      Expect.isSome
        namedFn.access
        "evaluating the function reference captures before list construction"
      let! actual =
        LibExecution.Execution.executeApplicable
          exeState
          // Host-initiated, as `execute` is: the state's own access.
          exeState.access
          (RT.AppNamedFn namedFn)
          (NEList.singleton RT.DUnit)
        |> Ply.toTask
      // The nested callback kept the restricted creator access.
      expectDenied [] actual
    | other -> Expect.equal 1 2 $"expected a one-element function list, got {other}"
  }

let partiallyAppliedNamedFnCapturesAccess =
  testTask "a partial application captures access before escaping" {
    // Direct calls need no stamp; partial applications retain the caller's.
    let! exeState = executionStateFor TestValues.pm false Map.empty
    let expr =
      eApply (eBuiltinFn "int64Add" 0) [] [ eInt64 1L ]
      |> PT2RT.Expr.toRT Map.empty 0 None
    let! produced = LibExecution.Execution.executeExpr (denyAll exeState) expr
    match produced with
    | Ok(RT.DApplicable(RT.AppNamedFn namedFn)) ->
      match namedFn.access with
      | None -> Expect.equal 1 2 "the partial application must carry a capture"
      | Some access ->
        Expect.isFalse
          (LibExecution.Permissions.Access.allows
            LibExecution.Permissions.Request.clock
            access)
          "the capture is the restricted applying frame's access"
    | other -> Expect.equal 1 2 $"expected a partial application, got {other}"
  }

// ── callbacks applied by a builtin ──────────────────────────
//
// `f ()` inside a `:{}` function was denied; `List.map [()] f` ran the clock.
// A builtin applies a callback in a borrowed VM, and that VM used to start
// from the run's base access, so the ceiling and package approval of the
// function that CALLED the builtin never reached the callback. In each case
// below the lambda is built by an unrestricted producer -- captured broad, as
// in the report -- and handed to a builtin inside a restricted frame.

let private clockLambda = eLambda (gid ()) [ lpVar "unit" ] clockBody

/// An unrestricted fn returning `fun () -> timeNowMs ()`.
let private lambdaProducer (hash : string) : PT.PackageFn.PackageFn =
  { ceilingFn hash None clockLambda with
      returnType = PT.TFn(NEList.singleton PT.TUnit, PT.TInt) }

let private producedLambda (producerHash : string) : PT.Expr =
  eApply (ePackageFn producerHash) [] [ eUnit () ]

/// `Stdlib.List.map [()] <the produced lambda>`
let private mapBody (producerHash : string) : PT.Expr =
  eApply
    (eBuiltinFn "listMap" 0)
    []
    [ eList [ eUnit () ]; producedLambda producerHash ]

let private listOfInt = PT.TList PT.TInt

let builtinCallbackKeepsInvokingCeiling =
  testTask "a callback applied by a builtin runs under the calling fn's ceiling" {
    let producerHash = "permissions-callback-producer"
    let runnerHash = "permissions-callback-map-runner"
    let runner =
      { ceilingFn runnerHash (Some Set.empty) (mapBody producerHash) with
          returnType = listOfInt }
    let! actual =
      runPackageFn (pmWith [ lambdaProducer producerHash; runner ]) runnerHash
    expectDenied [ "function policy"; "clock" ] actual
  }

let builtinCallbackAllowedByPermittingCeiling =
  testTask "the same callback is allowed when the calling fn's ceiling permits it" {
    // The control: it is the ceiling that denies above, not the plumbing.
    let producerHash = "permissions-callback-producer-ok"
    let runnerHash = "permissions-callback-map-runner-ok"
    let runner =
      { ceilingFn runnerHash (Some clockEffects) (mapBody producerHash) with
          returnType = listOfInt }
    let! actual =
      runPackageFn (pmWith [ lambdaProducer producerHash; runner ]) runnerHash
    expectNotDenied actual
  }

let builtinCallbackArity2KeepsCeiling =
  testTask "a two-argument callback (fold) runs under the calling fn's ceiling" {
    // `executeApplicable2` is a separate entry point from the one-argument path.
    let producerHash = "permissions-callback-producer2"
    let runnerHash = "permissions-callback-fold-runner"
    let lambda2 = eLambda (gid ()) [ lpVar "acc"; lpVar "unit" ] clockBody
    let producer =
      { ceilingFn producerHash None lambda2 with
          returnType = PT.TFn(NEList.ofList PT.TInt [ PT.TUnit ], PT.TInt) }
    let foldBody =
      eApply
        (eBuiltinFn "listFold" 0)
        []
        [ eList [ eUnit () ]; PT.EInt(gid (), 0I); producedLambda producerHash ]
    let runner = ceilingFn runnerHash (Some Set.empty) foldBody
    let! actual = runPackageFn (pmWith [ producer; runner ]) runnerHash
    expectDenied [ "function policy"; "clock" ] actual
  }

let builtinCallbackNestedKeepsCeiling =
  testTask "a ceiling two frames above the builtin still bounds the callback" {
    // `{}` runner -> unceilinged inner -> List.map. The restriction lives on
    // a frame that is not the builtin's caller, and it must still arrive.
    let producerHash = "permissions-callback-producer-nested"
    let innerHash = "permissions-callback-inner"
    let runnerHash = "permissions-callback-nested-runner"
    let inner =
      { ceilingFn innerHash None (mapBody producerHash) with returnType = listOfInt }
    let runner =
      { ceilingFn
          runnerHash
          (Some Set.empty)
          (eApply (ePackageFn innerHash) [] [ eUnit () ]) with
          returnType = listOfInt }
    let! actual =
      runPackageFn (pmWith [ lambdaProducer producerHash; inner; runner ]) runnerHash
    expectDenied [ "function policy"; "clock" ] actual
  }

let builtinCallbackKeepsPackagePolicy =
  testTask
    "a callback applied by a builtin stays inside the calling package's approval" {
    // The consumer-facing case: `permissions approve` confines "the function
    // and everything it calls". The callback is captured allow-all by the
    // producer; the runner is the package the consumer denied.
    let producerHash = "permissions-callback-producer-pkg"
    let runnerHash = "permissions-callback-pkg-runner"
    let runner =
      { ceilingFn runnerHash None (mapBody producerHash) with
          returnType = listOfInt }
    let! actual =
      runPackageFnWith
        (denyPackage runnerHash)
        (pmWith [ lambdaProducer producerHash; runner ])
        runnerHash
    expectDenied [ "package policy" ] actual
  }

/// A forwarder of `listMap` with the builtin's own generic signature, which
/// is what `thinWrapperOf` elides: type variables compare by NAME, so the
/// wrapper must say `'a` and `'b` exactly as the builtin does. A wrapper with
/// concrete parameter types is not thin and takes the frame path -- which is
/// what the first version of this test did, and so tested nothing about
/// elision.
let private genericMapWrapper
  (hash : string)
  (ceiling : Option<Set<Effects.Effect>>)
  : PT.PackageFn.PackageFn =
  let a = PT.TVariable "a"
  let b = PT.TVariable "b"
  { hash = PT.Hash hash
    typeParams = [ "a"; "b" ]
    parameters =
      NEList.ofList
        { name = "list"; typ = PT.TList a; description = "" }
        [ { name = "fn"; typ = PT.TFn(NEList.singleton a, b); description = "" } ]
    returnType = PT.TList b
    body = eApply (eBuiltinFn "listMap" 0) [] [ eVar "list"; eVar "fn" ]
    description = ""
    permissionCeiling = ceiling }

/// `wrapper [()] <the produced lambda>`
let private viaWrapper (wrapperHash : string) (producerHash : string) : PT.Expr =
  eApply
    (ePackageFn wrapperHash)
    []
    [ eList [ eUnit () ]; producedLambda producerHash ]

let builtinCallbackThroughThinWrapperKeepsCeiling =
  testTask "a callback through an elided forwarder keeps the CALLER's ceiling" {
    // Elision ran the builtin without writing `vm.activeAccess`, so the
    // callback ran under whatever the previous builtin had left there.
    // Called twice: the first call detects and caches the wrapper (late
    // elision), the second is answered from the cache before the package
    // call path is entered (early elision).
    let producerHash = "permissions-callback-producer-thin"
    let wrapperHash = "permissions-callback-thin-wrapper"
    let runnerHash = "permissions-callback-thin-runner"
    let runner =
      { ceilingFn runnerHash (Some Set.empty) (viaWrapper wrapperHash producerHash) with
          returnType = listOfInt }
    let pm =
      pmWith
        [ lambdaProducer producerHash; genericMapWrapper wrapperHash None; runner ]
    let! cold = runPackageFn pm runnerHash
    expectDenied [ "function policy"; "clock" ] cold
    let! warm = runPackageFn pm runnerHash
    expectDenied [ "function policy"; "clock" ] warm
  }

let elidedWrapperKeepsItsOwnCeiling =
  testTask "an elided forwarder's OWN ceiling applies to the builtin's callback" {
    // The reviewer's case: `let restrictedMap xs f :{} = Builtin.listMap xs f`
    // called from an unrestricted caller. The frame path applied the
    // wrapper's ceiling; the elisions applied nothing of the wrapper's.
    let producerHash = "permissions-callback-producer-own-ceiling"
    let wrapperHash = "permissions-callback-own-ceiling-wrapper"
    let runnerHash = "permissions-callback-own-ceiling-runner"
    let runner =
      { ceilingFn runnerHash None (viaWrapper wrapperHash producerHash) with
          returnType = listOfInt }
    let pm =
      pmWith
        [ lambdaProducer producerHash
          genericMapWrapper wrapperHash (Some Set.empty)
          runner ]
    let! cold = runPackageFn pm runnerHash
    expectDenied [ "function policy"; "clock" ] cold
    let! warm = runPackageFn pm runnerHash
    expectDenied [ "function policy"; "clock" ] warm
  }

let elidedWrapperKeepsItsOwnPackagePolicy =
  testTask
    "an elided forwarder's OWN package approval applies to the builtin's callback" {
    let producerHash = "permissions-callback-producer-own-pkg"
    let wrapperHash = "permissions-callback-own-pkg-wrapper"
    let runnerHash = "permissions-callback-own-pkg-runner"
    let runner =
      { ceilingFn runnerHash None (viaWrapper wrapperHash producerHash) with
          returnType = listOfInt }
    let pm =
      pmWith
        [ lambdaProducer producerHash; genericMapWrapper wrapperHash None; runner ]
    let! cold = runPackageFnWith (denyPackage wrapperHash) pm runnerHash
    expectDenied [ "package policy" ] cold
    let! warm = runPackageFnWith (denyPackage wrapperHash) pm runnerHash
    expectDenied [ "package policy" ] warm
  }

// ── HTTP server child guest state ───────────────────────────
//
// `httpServerServe` builds a child guest state for the router, and
// `guestState` REPLACES the access, so the calling frame's layers were gone
// before the port was checked. `HttpServer` is a scoped effect -- decided at
// the host boundary with the exact port, not by the ambient gate -- so a
// `:{Clock, Stdout}` function passed the gate and reached the OS bind.
//
// The port is held open by a listener for the duration, on purpose: a
// regression would then come back as `Error "in use"` in milliseconds, where
// a successful bind would block the test until SIGINT.

let private servingFn
  (hash : string)
  (ceiling : Set<Effects.Effect>)
  (port : int)
  : PT.PackageFn.PackageFn =
  let handler = eLambda (gid ()) [ lpVar "req" ] (eVar "req")
  let onListening = eLambda (gid ()) [ lpVar "unit" ] (eUnit ())
  let body =
    eApply
      (eBuiltinFn "httpServerServe" 0)
      []
      [ PT.EInt(gid (), bigint port)
        handler
        PT.EInt(gid (), 1000I)
        eBool true
        eBool true
        eBool false
        onListening ]
  { ceilingFn hash (Some ceiling) body with
      typeParams = [ "a" ]
      returnType = PT.TVariable "a" }

let private withOccupiedPort (f : int -> System.Threading.Tasks.Task<unit>) =
  task {
    let listener =
      new System.Net.Sockets.TcpListener(System.Net.IPAddress.Loopback, 0)
    listener.Start()
    let port = (listener.LocalEndpoint :?> System.Net.IPEndPoint).Port
    try
      do! f port
    finally
      listener.Stop()
  }

let private serveEffects =
  Set.ofList
    [ Effects.Effect.Clock; Effects.Effect.Stdout; Effects.Effect.HttpServer ]

/// `serve` builds its child state from the real policy store, so on a machine
/// whose instance policy denies the bind (CI's seeded default) the instance
/// layer would answer before the ceiling is consulted. Run the body against a
/// temporary store holding an allow-all instance policy. The override is
/// process-global, so the tests using it are sequenced.
let private withAllowAllInstanceStore
  (f : unit -> System.Threading.Tasks.Task<unit>)
  =
  task {
    let dir =
      System.IO.Path.Combine(
        System.IO.Path.GetTempPath(),
        $"dark-policy-{System.Guid.NewGuid()}"
      )
    System.IO.Directory.CreateDirectory dir |> ignore<System.IO.DirectoryInfo>
    let restore = LibExecution.HostSecurity.policyDirectoryForTesting dir
    try
      LibDB.PolicyStore.setInstancePolicy LibExecution.Permissions.Policy.allowAll
      do! f ()
    finally
      restore.Dispose()
      System.IO.Directory.Delete(dir, true)
  }

let serveBindKeepsInvokingCeiling =
  testTask "an HTTP bind from a fn whose ceiling excludes HttpServer is denied" {
    do!
      withAllowAllInstanceStore (fun () ->
        withOccupiedPort (fun port ->
          task {
            let hash = "permissions-serve-ceiling-denied"
            let fn =
              servingFn
                hash
                (Set.remove Effects.Effect.HttpServer serveEffects)
                port
            let! actual = runPackageFn (pmWith [ fn ]) hash
            expectDenied [ "function policy" ] actual
          }))
  }

let serveBindAllowedByPermittingCeiling =
  testTask "an HTTP bind is allowed when the ceiling includes HttpServer" {
    // The control: with the port occupied, an allowed bind comes back as
    // `Error "in use"` from the OS -- a value, not a denial -- which is the
    // proof that the permitted case reached the host boundary.
    do!
      withAllowAllInstanceStore (fun () ->
        withOccupiedPort (fun port ->
          task {
            let hash = "permissions-serve-ceiling-allowed"
            let fn = servingFn hash serveEffects port
            let! actual = runPackageFn (pmWith [ fn ]) hash
            expectNotDenied actual
            match actual with
            | Ok(RT.DEnum(_, _, _, "Error", [ RT.DString message ])) ->
              Expect.stringContains message "in use" "the OS answered the bind"
            | other ->
              Expect.equal 1 2 $"expected the OS's in-use error, got {other}"
          }))
  }

let streamCallbackKeepsCeiling =
  testTask "a stream transform runs its callback under the ceiling it was built in" {
    // The transform is stored and only runs when the stream is drained, so
    // the access has to be captured when the callable is handed over.
    let producerHash = "permissions-callback-producer-stream"
    let runnerHash = "permissions-callback-stream-runner"
    let streamBody =
      eApply
        (eBuiltinFn "streamToList" 0)
        [ PT.TInt ]
        [ eApply
            (eBuiltinFn "streamMap" 0)
            [ PT.TUnit; PT.TInt ]
            [ eApply
                (eBuiltinFn "streamFromList" 0)
                [ PT.TUnit ]
                [ eList [ eUnit () ] ]
              producedLambda producerHash ] ]
    let runner =
      { ceilingFn runnerHash (Some Set.empty) streamBody with
          returnType = listOfInt }
    let! actual =
      runPackageFn (pmWith [ lambdaProducer producerHash; runner ]) runnerHash
    expectDenied [ "function policy"; "clock" ] actual
  }

let tests =
  testList
    "PermissionsGate"
    [ denied
      allowed
      nativeDenied
      nativeAllowedByAllowAll
      newRunPolicyDenies
      packagePolicyDenies
      escapedLambdaKeepsAccess
      evaluatedNamedFnInContainerCannotWidenReentry
      partiallyAppliedNamedFnCapturesAccess
      functionCeilingDenies
      functionCeilingAllows
      callerCeilingAttenuates
      escapedLambdaKeepsCeiling
      builtinCallbackKeepsInvokingCeiling
      builtinCallbackAllowedByPermittingCeiling
      builtinCallbackArity2KeepsCeiling
      builtinCallbackNestedKeepsCeiling
      builtinCallbackKeepsPackagePolicy
      builtinCallbackThroughThinWrapperKeepsCeiling
      elidedWrapperKeepsItsOwnCeiling
      elidedWrapperKeepsItsOwnPackagePolicy
      streamCallbackKeepsCeiling
      testSequenced (
        testList
          "http bind"
          [ serveBindKeepsInvokingCeiling; serveBindAllowedByPermittingCeiling ]
      )
      testList "deferred execution matrix" deferredExecutionMatrix
      guestCannotChangePolicies
      guestCannotApprovePackages
      guestFileApiCannotReachPolicyStore ]
