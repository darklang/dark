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
  eApply (eBuiltinFn "syscallGettimeofday" 0) [] [ eUnit () ]
  |> PT2RT.Expr.toRT Map.empty 0 None

// Host derives Native from the raw OS number and denies before the kernel call.
let nativeCall () =
  eApply (eBuiltinFn "syscallClose" 0) [] [ PT.EBigInt(gid (), -1I) ]
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

let denied =
  testTask "an effectful builtin is denied by a deny-all policy" {
    let! exeState = executionStateFor TestValues.pm false
    let! actual =
      LibExecution.Execution.executeExpr (denyAll exeState) (clockCall ())
    expectDenied [ "clock" ] actual
  }

let allowed =
  testTask "the gate permits an effect under an allow-all policy" {
    let! exeState = executionStateFor TestValues.pm false // allow-all access
    let! actual = LibExecution.Execution.executeExpr exeState (clockCall ())
    expectNotDenied actual
  }

let nativeDenied =
  testTask "a native builtin is denied by restricted execution" {
    let! exeState = executionStateFor TestValues.pm false
    let! actual =
      LibExecution.Execution.executeExpr (denyAll exeState) (nativeCall ())
    expectDenied [ "raw POSIX operation" ] actual
  }

let nativeAllowedByAllowAll =
  testTask "allow-all explicitly permits a native builtin" {
    let! exeState = executionStateFor TestValues.pm false
    let! actual = LibExecution.Execution.executeExpr exeState (nativeCall ())
    expectNotDenied actual
  }

let newRunPolicyDenies =
  testTask "the run policy attenuates an allow-all instance" {
    let! (exeState : RT.ExecutionState) = executionStateFor TestValues.pm false
    let access =
      exeState.access
      |> LibExecution.Permissions.Access.restrict
        LibExecution.Permissions.Layer.Run
        LibExecution.Permissions.Policy.denyAll
    let restricted : RT.ExecutionState = { exeState with access = access }
    let! actual = LibExecution.Execution.executeExpr restricted (clockCall ())
    expectDenied [ "run policy" ] actual
  }

module P = LibExecution.Permissions

let private clockBody =
  eLet
    (PT.LPWildcard(gid ()))
    (eApply (eBuiltinFn "syscallGettimeofday" 0) [] [ eUnit () ])
    (PT.EBigInt(gid (), 1I))

let private fn hash body : PT.PackageFn.PackageFn =
  { hash = PT.Hash hash
    typeParams = []
    parameters = NEList.singleton { name = "unit"; typ = PT.TUnit; description = "" }
    returnType = PT.TBigInt
    body = body
    description = ""
    bounds = [] }

let private named name hash =
  PT.EFnName(
    gid (),
    { originalName = [ "Me"; name ]
      resolved =
        Ok
          { name = PT.FQFnName.Package(PT.Hash hash)
            location = Some { owner = "Me"; modules = []; name = name } } },
    [],
    []
  )

let private call name hash = eApply (named name hash) [] [ eUnit () ]

let private withApprovals approvals (state : RT.ExecutionState) =
  { state with
      packagePolicy =
        fun name _ ->
          Some(
            name
            |> Option.bind (fun n -> Map.tryFind n approvals)
            |> Option.defaultValue P.Policy.denyAll
          ) }

let private namedState instance restriction approvals fns =
  task {
    let byHash =
      fns |> List.map (fun (f : PT.PackageFn.PackageFn) -> f.hash, f) |> Map.ofList
    let pm = { TestValues.pm with getFn = fun h -> Ply(Map.tryFind h byHash) }
    let! state = executionStateFor pm false
    return
      withApprovals approvals state
      |> LibExecution.Execution.setInstancePolicy instance
      |> LibExecution.Execution.restrictRun restriction
  }

let private runIn state expr =
  LibExecution.Execution.executeExpr state (PT2RT.Expr.toRT Map.empty 0 None expr)

let private runNamed instance restriction approvals fns expr =
  task {
    let! state = namedState instance restriction approvals fns
    return! runIn state expr
  }

let private clockGrant = P.Policy.allowEffects (Set.singleton Effects.Effect.Clock)

let namedApprovalSuppliesMissingAccess =
  testTask "named approval supplies access without an instance allow" {
    let! actual =
      runNamed
        P.Policy.denyAll
        P.Policy.allowAll
        (Map.ofList [ "Me.reader", clockGrant ])
        [ fn "clock" clockBody ]
        (call "reader" "clock")
    expectNotDenied actual
  }

let identicalAliasDoesNotInheritApproval =
  testTask "an identical alias does not inherit a named approval" {
    let! actual =
      runNamed
        P.Policy.denyAll
        P.Policy.allowAll
        (Map.ofList [ "Me.reader", clockGrant ])
        [ fn "clock" clockBody ]
        (call "alias" "clock")
    expectDenied [ "clock" ] actual
  }

let helpersInheritApproval =
  testTask "helpers inherit the entry approval without dependency approvals" {
    let! actual =
      runNamed
        P.Policy.denyAll
        P.Policy.allowAll
        (Map.ofList [ "Me.reader", clockGrant ])
        [ fn "root" (call "helper" "clock"); fn "clock" clockBody ]
        (call "reader" "root")
    expectNotDenied actual
  }

let nestedApprovalCannotWiden =
  testTask "an unapproved caller cannot acquire a callee's approval" {
    let! actual =
      runNamed
        P.Policy.denyAll
        P.Policy.allowAll
        (Map.ofList [ "Me.reader", clockGrant ])
        [ fn "outer" (call "reader" "clock"); fn "clock" clockBody ]
        (call "outer" "outer")
    expectDenied [ "clock" ] actual
  }

let explicitDenyWins =
  testTask "an explicit instance deny beats the named grant" {
    let policy = P.Policy.create [] [ P.Rule.Effect Effects.Effect.Clock ]
    let! actual =
      runNamed
        policy
        P.Policy.allowAll
        (Map.ofList [ "Me.reader", clockGrant ])
        [ fn "clock" clockBody ]
        (call "reader" "clock")
    expectDenied [ "explicitly denied" ] actual
  }

let runRestrictionWins =
  testTask "a run restriction beats the named grant" {
    let! actual =
      runNamed
        P.Policy.denyAll
        P.Policy.denyAll
        (Map.ofList [ "Me.reader", clockGrant ])
        [ fn "clock" clockBody ]
        (call "reader" "clock")
    expectDenied [ "run policy" ] actual
  }

// These share one state's call data across runs, as a host's later runs do.
let approvalChangeBetweenRuns =
  testTask "a changed approval applies on the next run" {
    let! state =
      namedState
        P.Policy.denyAll
        P.Policy.allowAll
        Map.empty
        [ fn "clock" clockBody ]
    let! before = runIn state (call "reader" "clock")
    expectDenied [ "clock" ] before
    let granted = withApprovals (Map.ofList [ "Me.reader", clockGrant ]) state
    let! after = runIn granted (call "reader" "clock")
    expectNotDenied after
    let! revoked = runIn (withApprovals Map.empty granted) (call "reader" "clock")
    expectDenied [ "clock" ] revoked
  }

let namesSharingCallDataKeepTheirOwnApproval =
  testTask "two names for one fn keep their own approvals" {
    let! state =
      namedState
        P.Policy.denyAll
        P.Policy.allowAll
        (Map.ofList [ "Me.reader", clockGrant ])
        [ fn "clock" clockBody ]
    let! first = runIn state (call "alias" "clock")
    expectDenied [ "clock" ] first
    let! approved = runIn state (call "reader" "clock")
    expectNotDenied approved
    let! again = runIn state (call "alias" "clock")
    expectDenied [ "clock" ] again
    let both =
      eLet (PT.LPWildcard(gid ())) (call "reader" "clock") (call "alias" "clock")
    let! oneRun = runIn state both
    expectDenied [ "clock" ] oneRun
  }

/// Make and consume in separate frames so either side can supply the restriction.
let deferredExecutionMatrix =
  [ for shape in [ "named fn"; "lambda"; "stream" ] do
      for direction in [ "maker"; "runner"; "neither" ] do
        testTask $"deferred {shape}: restricted {direction}" {
          let suffix = shape.Replace(" ", "-") + "-" + direction
          let makerHash = "maker-" + suffix
          let runnerHash = "runner-" + suffix
          let clockHash = "clock-" + suffix
          let parameter name typ : PT.PackageFn.Parameter =
            { name = name; typ = typ; description = "" }
          let callable = PT.TFn(NEList.singleton PT.TUnit, PT.TBigInt)
          let valueType =
            if shape = "stream" then PT.TStream PT.TBigInt else callable
          let lambda = eLambda (gid ()) [ lpVar "unit" ] clockBody
          let makeBody =
            match shape with
            | "named fn" -> named "clock" clockHash
            | "lambda" -> lambda
            | _ ->
              eApply
                (eBuiltinFn "streamMap" 0)
                [ PT.TUnit; PT.TBigInt ]
                [ eApply
                    (eBuiltinFn "streamFromList" 0)
                    [ PT.TUnit ]
                    [ eList [ eUnit () ] ]
                  lambda ]
          let maker = { fn makerHash makeBody with returnType = valueType }
          let runBody =
            if shape = "stream" then
              eApply (eBuiltinFn "streamToList" 0) [ PT.TBigInt ] [ eVar "v" ]
            else
              eApply (eVar "v") [] [ eUnit () ]
          let runner =
            { fn runnerHash runBody with
                parameters = NEList.singleton (parameter "v" valueType)
                returnType =
                  if shape = "stream" then PT.TList PT.TBigInt else PT.TBigInt }
          let approval who = if direction = who then P.Policy.denyAll else clockGrant
          let expr = eApply (named "runner" runnerHash) [] [ call "maker" makerHash ]
          let! actual =
            runNamed
              P.Policy.denyAll
              P.Policy.allowAll
              (Map.ofList
                [ "Me.maker", approval "maker"; "Me.runner", approval "runner" ])
              [ maker; runner; fn clockHash clockBody ]
              expr
          if direction = "neither" then
            match actual with
            | Ok _ -> ()
            | Error e -> failtestf "permitted deferred call failed: %A" e
          else
            expectDenied [ "clock" ] actual
        } ]

let private refusesAsAdmin
  (state : RT.ExecutionState)
  (builtin : RT.BuiltInFn)
  : System.Threading.Tasks.Task<bool> =
  task {
    try
      let! _ = builtin.fn (struct (state, dummyVM (), [], [||])) |> Ply.toTask
      return false
    with
    | RT.RuntimeErrorException(_, RTE.Error.UncaughtException(message, _)) when
      message.Contains "trusted `dark permissions` command"
      ->
      return true
    | _ -> return false
  }

/// The policy builtins guest code may call. Every other one must refuse it.
let private policyReads =
  set
    [ "pmPolicyGetInstance"
      "pmPolicyApprovals"
      "pmFnIsBundled"
      "pmFnPermissionRequirements"
      "pmFnEffectReasons" ]

/// The policy builtins only the trusted `dark permissions` command may call.
let private hostOnlyPolicyBuiltins =
  set
    [ "pmPolicyReviewToken"
      "pmPolicyEditInstance"
      "pmPolicyListPackages"
      "pmPolicyWithdraw"
      "pmPolicyApprovalsStale"
      "pmPolicyApproveVersion" ]

let everyPolicyBuiltinRefusesGuestCode =
  testTask "every policy builtin but the named reads refuses guest code" {
    let! (guest : RT.ExecutionState) = executionStateFor TestValues.pm false
    let trusted = { guest with canManagePolicies = true }
    let all = Builtins.Matter.Libs.PM.Permissions.fns ()
    Expect.equal
      (all |> List.map (fun b -> b.name.name) |> Set.ofList)
      (Set.union policyReads hostOnlyPolicyBuiltins)
      "every policy builtin is classed as a guest read or host-only"
    let hostOnly =
      all |> List.filter (fun b -> not (Set.contains b.name.name policyReads))
    for builtin in hostOnly do
      let! refusesGuest = refusesAsAdmin guest builtin
      Expect.isTrue refusesGuest $"{builtin.name.name} refuses guest code"
      let! refusesTrusted = refusesAsAdmin trusted builtin
      Expect.isFalse
        refusesTrusted
        $"{builtin.name.name} lets the CLI's own state through"
  }

/// The reads guest code may call answer it, rather than refusing it as administration.
let guestCannotGrantRunAccess =
  testTask "guest code cannot supply temporary run grants" {
    let! guest = executionStateFor TestValues.pm false
    let builtin =
      Builtins.CliHost.Libs.Cli.fns ()
      |> List.find (fun b -> b.name.name = "cliParseAndExecuteScript")
      |> Option.defaultWith (fun () -> failtest "missing script builtin")
    let args =
      [| LibExecution.Dval.optionNone RT.KTUuid
         RT.DUuid(System.Guid.NewGuid())
         RT.DString "guest.dark"
         RT.DString "()"
         LibExecution.Dval.list RT.KTString []
         RT.DBool false
         RT.DBool false
         LibExecution.PermissionsToDarkTypes.Policy.toDT P.Policy.allowAll
         LibExecution.Dval.optionNone RT.KTUnit |]
    let mutable refused = false
    try
      let! _ = builtin.fn (struct (guest, dummyVM (), [], args)) |> Ply.toTask
      ()
    with RT.RuntimeErrorException(_, RTE.Error.UncaughtException(message, _)) ->
      refused <-
        message.Contains "run approvals require the trusted dark run command"
    Expect.isTrue refused "only the trusted CLI may supply a grant"
  }

let guestCodeCanCallPolicyReads =
  testTask "guest code can still call the policy reads" {
    let! (guest : RT.ExecutionState) = executionStateFor TestValues.pm false
    Expect.isFalse guest.canManagePolicies "the state is a guest's"
    let byName =
      Builtins.Matter.Libs.PM.Permissions.fns ()
      |> List.map (fun b -> b.name.name, b)
      |> Map.ofList
    for name in policyReads do
      let! refuses = refusesAsAdmin guest byName[name]
      Expect.isFalse refuses $"{name} does not refuse guest code"
    let call name args =
      byName[name].fn (struct (guest, dummyVM (), [], args)) |> Ply.toTask
    let hash = "permissions-guest-read-unapproved"
    let! approvals =
      call
        "pmPolicyApprovals"
        [| LibExecution.Dval.optionNone RT.KTUuid
           LibExecution.Dval.optionSome RT.KTString (RT.DString hash) |]
    match approvals with
    | RT.DList(_, RT.Items([ RT.DRecord(_, _, _, fields) ])) ->
      Expect.equal
        (RT.RecordFields.tryFind "hash" fields)
        (Some(RT.DString hash))
        "the entry is for the hash asked about"
    | other -> failtestf "expected one approval entry, got %A" other
    let! bundled = call "pmFnIsBundled" [| RT.DString hash |]
    Expect.equal bundled (RT.DBool false) "an unknown hash is not bundled code"
  }

/// Every way the host builds a state for guest code clears the flag, whatever the caller had.
let guestStatesCannotManagePolicies =
  testTask "every guest state constructor withholds policy administration" {
    let! (base' : RT.ExecutionState) = executionStateFor TestValues.pm false
    let trusted = { base' with canManagePolicies = true }
    let root =
      RT.Hash "0000000000000000000000000000000000000000000000000000000000000000"
    let states =
      [ "guestState",
        LibDB.PolicyStore.guestState
          None
          LibExecution.Permissions.Policy.allowAll
          []
          trusted
        "testState", LibDB.PolicyStore.testState None trusted
        "rootState with a root",
        LibDB.PolicyStore.rootState trusted trusted.access [ root ] ]
    for (name, state) in states do
      Expect.isFalse state.canManagePolicies $"{name} cannot manage policies"
  }

let guestFileApiCannotReachPolicyStore =
  testTask "guest filesystem access cannot reach host policy state" {
    let! exeState = executionStateFor TestValues.pm false
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
    let! exeState = executionStateFor TestValues.pm false
    let expr =
      eList [ eBuiltinFn "syscallGettimeofday" 0 ]
      |> PT2RT.Expr.toRT Map.empty 0 None
    let! produced = LibExecution.Execution.executeExpr (denyAll exeState) expr
    match produced with
    | Ok(RT.DList(_, RT.Items([ RT.DApplicable(RT.AppNamedFn namedFn) ]))) ->
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


let interactiveReviewIsScopedAndTemporary =
  testTask "interactive grants resume an operation and expire with the run" {
    let! (original : RT.ExecutionState) = executionStateFor TestValues.pm false
    let guest = denyAll original
    let mutable prompts = 0
    let ask rules =
      prompts <- prompts + 1
      Expect.equal rules [ "env read 'DARK_REVIEW_TEST'" ] "exact request shown"
      Ply 1L
    let! (reviewed : RT.ExecutionState), _ =
      LibDB.PermissionReview.configure
        guest
        "Me.review"
        "review-version"
        false
        []
        (Some ask)
      |> Ply.toTask
    let expr =
      eApply
        (eBuiltinFn "runtimeEnvironmentGet" 0)
        []
        [ eStr [ strText "DARK_REVIEW_TEST" ] ]
      |> PT2RT.Expr.toRT Map.empty 0 None
    let! first = LibExecution.Execution.executeExpr reviewed expr
    let! second = LibExecution.Execution.executeExpr reviewed expr
    expectNotDenied first
    expectNotDenied second
    Expect.equal prompts 1 "same access asks only once in a run"
    let! another = LibExecution.Execution.executeExpr guest expr
    expectDenied [ "DARK_REVIEW_TEST" ] another
  }

let interactiveReviewCannotOverrideRestrictions =
  testTask
    "interactive review never overrides explicit denies or mandatory restrictions" {
    let! (original : RT.ExecutionState) = executionStateFor TestValues.pm false
    for layer in [ P.Layer.Instance; P.Layer.Run; P.Layer.Value "saved" ] do
      let policy =
        if layer = P.Layer.Instance then
          P.Policy.create [ P.Rule.All ] [ P.Rule.All ]
        else
          P.Policy.denyAll
      let guest =
        { original with access = P.Access.restrict layer policy original.access }
      let mutable prompts = 0
      let ask _ =
        prompts <- prompts + 1
        Ply 1L
      let! reviewed, _ =
        LibDB.PermissionReview.configure
          guest
          "Me.restricted"
          "version"
          false
          []
          (Some ask)
        |> Ply.toTask
      let! result = LibExecution.Execution.executeExpr reviewed (clockCall ())
      expectDenied [] result
      Expect.equal prompts 0 "an approval prompt cannot remove this restriction"
  }

let preflightReviewIsOneBatch =
  testTask "preflight approves exact known access in one batch before execution" {
    let! (original : RT.ExecutionState) = executionStateFor TestValues.pm false
    let request name =
      P.Request.env P.AccessKind.Read name
      |> Result.defaultWith (fun error -> failtest error)
    let requests = [ request "REVIEW_A"; request "REVIEW_B" ]
    let mutable prompts = 0
    let ask (rules : List<string>) =
      prompts <- prompts + 1
      Expect.equal rules.Length 2 "one batch"
      Ply 1L
    let! (reviewed : RT.ExecutionState), _ =
      LibDB.PermissionReview.configure
        (denyAll original)
        "Me.batch"
        "version"
        false
        (List.map LibDB.PermissionReview.atRoot requests)
        (Some ask)
      |> Ply.toTask
    Expect.equal prompts 1 "before any body execution"
    for request in requests do
      Expect.isTrue
        (P.Access.allows request reviewed.access)
        "captures include preflight grants"
  }

let preflightKeepsNamedEntryApprovals =
  testTask "preflight uses the same first named entry as execution" {
    let request =
      P.Request.env P.AccessKind.Read "REPORT_INPUT" |> Result.defaultWith failtest
    let grant =
      P.Policy.create
        [ P.Rule.Env(P.AccessKind.Read, P.Scope.Only "REPORT_INPUT") ]
        []
    let envBody =
      eApply
        (eBuiltinFn "runtimeEnvironmentGet" 0)
        []
        [ eStr [ strText "REPORT_INPUT" ] ]
    let functions =
      [ fn "report" envBody; fn "outer" (call "report" "report") ]
      |> List.map (fun f -> f.hash, f)
      |> Map.ofList
    let load hash = Ply(Map.tryFind hash functions)
    let policyFor name _ =
      Some(if name = Some "Me.report" then grant else P.Policy.denyAll)
    let! original = executionStateFor TestValues.pm false
    let guest = { denyAll original with packagePolicy = policyFor }
    let collect expr =
      LibDB.PermissionReview.preflight load policyFor [ expr ] |> Ply.toTask
    let! direct = collect (call "report" "report")
    let expected : LibDB.PermissionReview.KnownRequest =
      { request = request; entry = Some(Some "Me.report", RT.Hash "report") }
    Expect.equal direct [ expected ] "preflight retains the approval identity"
    let! _ =
      LibDB.PermissionReview.configure
        guest
        "script:/report"
        "script"
        true
        direct
        None
      |> Ply.toTask
    let! nested = collect (call "outer" "outer")
    Expect.equal
      (nested
       |> List.map (fun (item : LibDB.PermissionReview.KnownRequest) -> item.entry))
      [ Some(Some "Me.outer", RT.Hash "outer") ]
      "a helper cannot replace the unapproved entry"
    let mutable denied = false
    try
      let! _ =
        LibDB.PermissionReview.configure
          guest
          "script:/report"
          "script"
          true
          nested
          None
        |> Ply.toTask
      ()
    with RT.RuntimeErrorException(_, RTE.Error.UncaughtException(message, _)) ->
      denied <- message.Contains "permission denied"
    Expect.isTrue denied "the unapproved outer entry still fails before running"
  }

let pendingReviewStopsLaterEffects =
  testTask
    "a pending permission question stops later effects and cancellation stops the run" {
    let! (original : RT.ExecutionState) = executionStateFor TestValues.pm false
    let builtins = System.Collections.Generic.Dictionary(original.fns.builtIn)
    let envBuiltin =
      builtins.Values |> Seq.find (fun fn -> fn.name.name = "runtimeEnvironmentGet")
    let mutable laterCalls = 0
    builtins[envBuiltin.name] <-
      { envBuiltin with
          fn =
            fun invocation ->
              let struct (_, _, _, args) = invocation
              if args = [| RT.DString "LATER_ALLOWED" |] then
                laterCalls <- laterCalls + 1
              envBuiltin.fn invocation }
    let allowed =
      P.Policy.create
        [ P.Rule.Env(P.AccessKind.Read, P.Scope.Only "LATER_ALLOWED") ]
        []
    let guest =
      { original with
          fns = { original.fns with builtIn = builtins }
          access = P.Access.start allowed }
    let answer = System.Threading.Tasks.TaskCompletionSource<int64>()
    let asked = System.Threading.Tasks.TaskCompletionSource<unit>()
    let ask _ =
      asked.SetResult()
      uply { return! answer.Task }
    let! (reviewed : RT.ExecutionState), _ =
      LibDB.PermissionReview.configure
        guest
        "Me.pending"
        "version"
        false
        []
        (Some ask)
      |> Ply.toTask
    let get name =
      eApply (eBuiltinFn "runtimeEnvironmentGet" 0) [] [ eStr [ strText name ] ]
    let expr =
      eLet
        (lpWildcard ())
        (get "NEEDS_REVIEW")
        (eLet (lpWildcard ()) (get "LATER_ALLOWED") (eUnit ()))
      |> PT2RT.Expr.toRT Map.empty 0 None
    let running = LibExecution.Execution.executeExpr reviewed expr
    do! asked.Task.WaitAsync(System.TimeSpan.FromSeconds 5.)
    Expect.equal laterCalls 0 "later access waits for the question"
    answer.SetResult 0L
    let! result = running
    expectDenied [] result
    Expect.equal laterCalls 0 "cancelling never performs later effects"
  }

let tests =
  testList
    "PermissionsGate"
    [ pendingReviewStopsLaterEffects
      interactiveReviewIsScopedAndTemporary
      interactiveReviewCannotOverrideRestrictions
      preflightKeepsNamedEntryApprovals
      preflightReviewIsOneBatch
      denied
      allowed
      nativeDenied
      nativeAllowedByAllowAll
      newRunPolicyDenies
      namedApprovalSuppliesMissingAccess
      identicalAliasDoesNotInheritApproval
      helpersInheritApproval
      nestedApprovalCannotWiden
      explicitDenyWins
      runRestrictionWins
      approvalChangeBetweenRuns
      namesSharingCallDataKeepTheirOwnApproval
      evaluatedNamedFnInContainerCannotWidenReentry
      everyPolicyBuiltinRefusesGuestCode
      guestCannotGrantRunAccess
      guestCodeCanCallPolicyReads
      guestStatesCannotManagePolicies
      guestFileApiCannotReachPolicyStore
      testList "deferred execution" deferredExecutionMatrix ]
