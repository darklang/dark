/// The policy engine: exact requests, rules, policies, the monotonic
/// `Access` conjunction, and the access captured on function values.
module Tests.Permissions

open Expecto
open Prelude
open TestUtils.PTShortcuts

module Effect = LibExecution.Effects
module Permission = LibExecution.Permissions
module PolicyStore = LibDB.PolicyStore
module RT = LibExecution.RuntimeTypes

let private only (item : 'a) : Permission.Scope<'a> = Permission.Scope.Only item

let private request (method : string) (url : string) : Permission.Request =
  match Permission.Request.http method url with
  | Ok request -> request
  | Error error -> Exception.raiseInternal error []

/// An https:443 rule for one method and host; `None` leaves the path (or the
/// query) unscoped.
let private httpRule
  (method : string)
  (host : string)
  (path : Option<string>)
  (query : Option<string>)
  : Permission.Rule =
  let scope value =
    match value with
    | Some v -> only v
    | None -> Permission.Scope.All
  Permission.Rule.Http
    { method = only method
      scheme = only "https"
      host = Permission.HostRule.Exact host
      port = only 443
      pathPrefix = scope path
      query = scope query }

let private scoped (hash : string) : PolicyStore.ScopedKey = None, hash

/// `file read /data` plus the clock: the consumer's own resource rules.
let private consumerRules : Permission.Policy =
  Permission.Policy.create
    [ Permission.Rule.File(Permission.AccessKind.Read, Permission.Scope.Only "/data")
      Permission.Rule.Effect Effect.Effect.Clock ]
    []

let policiesDenyByDefault =
  test "policies deny by default and allowAll is explicit" {
    let operation = Permission.Request.clock
    Expect.isFalse
      (Permission.Policy.allows operation Permission.Policy.denyAll)
      "missing rules deny"
    Expect.isTrue
      (Permission.Policy.allows operation Permission.Policy.allowAll)
      "the explicit all policy allows"
  }

let denyOverridesAllow =
  test "an explicit deny overrides an allow in the same policy" {
    let policy =
      Permission.Policy.create
        [ Permission.Rule.Effect Effect.Effect.Http ]
        [ httpRule "POST" "api.example.com" None None ]

    Expect.isTrue
      (Permission.Policy.allows (request "GET" "https://api.example.com/v1") policy)
      "GET remains allowed"
    Expect.isFalse
      (Permission.Policy.allows (request "POST" "https://api.example.com/v1") policy)
      "the narrower deny wins"
  }

let accessOnlyNarrows =
  test "adding a run or package policy can only narrow access" {
    let instance = Permission.Access.start Permission.Policy.allowAll
    let stripeOnly =
      Permission.Policy.create
        [ httpRule "POST" "api.stripe.com" (Some "/v1/payment_intents") None ]
        []
    let access =
      instance
      |> Permission.Access.restrict Permission.Layer.Run stripeOnly
      |> Permission.Access.restrict
        (Permission.Layer.Package "Acme.Stripe@hash")
        stripeOnly

    Expect.isTrue
      (Permission.Access.allows
        (request "POST" "https://api.stripe.com/v1/payment_intents")
        access)
      "the exact approved operation passes every layer"
    Expect.isFalse
      (Permission.Access.allows
        (request "POST" "https://evil.example/collect")
        access)
      "the instance's broad authority does not bypass narrower layers"
  }

let instanceDenialWinsOverInnerDenial =
  test "check reports the instance denial even when an inner layer also denies" {
    // The instance layer is the operator's hard maximum. If an inner
    // caller-owned layer also denies, check must still report the *instance*
    // denial, so warn/audit mode (which relaxes only caller-owned layers) can
    // never proceed past it.
    let access =
      Permission.Access.start Permission.Policy.denyAll
      |> Permission.Access.restrict
        (Permission.Layer.Package "pkg")
        Permission.Policy.denyAll
    match Permission.Access.check Permission.Request.clock access with
    | Permission.Decision.Denied denial ->
      Expect.equal
        denial.layer
        Permission.Layer.Instance
        "the instance hard-max is reported, not the inner package denial"
    | Permission.Decision.Allowed -> failtest "should be denied"
  }

let warnModeNeverRelaxesTheAuthorsCeiling =
  test
    "audit mode waives run and package denials but not the instance or a function ceiling" {
    let recorded = ResizeArray<Permission.Layer>()
    let relax = Permission.Relax(fun _ layer _ -> recorded.Add layer)
    let decide layer =
      let access =
        Permission.Access.start Permission.Policy.allowAll
        |> Permission.Access.restrict layer Permission.Policy.denyAll
      Permission.Access.decide
        relax
        (fun () -> "clock")
        Permission.Request.clock
        access
    Expect.isNone (decide Permission.Layer.Run) "a run denial is waived"
    Expect.isNone
      (decide (Permission.Layer.Package "p"))
      "a package denial is waived"
    Expect.equal
      (decide (Permission.Layer.Function "f") |> Option.map (fun d -> d.layer))
      (Some(Permission.Layer.Function "f"))
      "the author's ceiling still denies"
    Expect.equal
      (Permission.Access.decide
        relax
        (fun () -> "clock")
        Permission.Request.clock
        (Permission.Access.start Permission.Policy.denyAll)
       |> Option.map (fun d -> d.layer))
      (Some Permission.Layer.Instance)
      "the instance policy still denies"
    Expect.equal
      (List.ofSeq recorded)
      [ Permission.Layer.Run; Permission.Layer.Package "p" ]
      "only the waived denials were recorded"
  }

let httpRulesKeepDimensionsCoupled =
  test "HTTP method, origin, port, and path remain coupled" {
    let policy =
      Permission.Policy.create
        [ httpRule "POST" "api.stripe.com" (Some "/v1/payment_intents") None ]
        []

    let allows method url = Permission.Policy.allows (request method url) policy

    Expect.isTrue
      (allows "POST" "https://api.stripe.com/v1/payment_intents")
      "exact request"
    Expect.isTrue
      (allows "POST" "https://api.stripe.com/v1/payment_intents/pi_1")
      "child path"
    Expect.isFalse
      (allows "GET" "https://api.stripe.com/v1/payment_intents")
      "method differs"
    Expect.isFalse (allows "POST" "https://api.stripe.com/v1/refunds") "path differs"
    Expect.isFalse
      (allows "POST" "https://api.stripe.com.evil.example/v1/payment_intents")
      "lookalike host differs"
  }

let httpQueryIsPartOfTheOperation =
  test "an HTTP rule scopes the query, not just the path" {
    // A rule written with a query covers only that exact query...
    let exact =
      Permission.Policy.create
        [ httpRule "GET" "example.com" (Some "/download") (Some "?id=public") ]
        []
    Expect.isTrue
      (Permission.Policy.allows
        (request "GET" "https://example.com/download?id=public")
        exact)
      "the exact approved query is allowed"
    Expect.isFalse
      (Permission.Policy.allows
        (request "GET" "https://example.com/download?id=secret")
        exact)
      "a different query on the same path is NOT covered"

    // ...while a rule written without a query covers any query on the path.
    let anyQuery =
      Permission.Policy.create
        [ httpRule "GET" "example.com" (Some "/download") None ]
        []
    Expect.isTrue
      (Permission.Policy.allows
        (request "GET" "https://example.com/download?id=secret")
        anyQuery)
      "an unspecified query keeps the intuitive any-query behavior"
  }

let invalidRequestsFailClosed =
  test "invalid requests never become wildcards" {
    Expect.isError
      (Permission.Request.http "GET" "file:///etc/passwd")
      "non-HTTP URLs are rejected"
    Expect.isError (Permission.Request.httpServer 70000) "invalid ports are rejected"
    Expect.isError
      (Permission.Request.processSpawn "git" [ "status" ])
      "unresolved executables are rejected"
  }

let fileRulesRespectBoundaries =
  test "file roots reject sibling prefixes and parent traversal" {
    let policy =
      Permission.Policy.create
        [ Permission.Rule.File(Permission.AccessKind.Read, only "/srv/app/data") ]
        []

    let allows path =
      match Permission.Request.file Permission.AccessKind.Read path with
      | Ok operation -> Permission.Policy.allows operation policy
      | Error _ -> false

    Expect.isTrue (allows "/srv/app/data/users.json") "child path"
    Expect.isFalse (allows "/srv/app/database/users.json") "prefix sibling"
    Expect.isFalse (allows "/srv/app/data/../../secret") "parent traversal"
  }

let fileRulesSeeThroughSymlinkedRoots =
  test "a file rule and a request name the directory a link resolves to" {
    // macOS: `/tmp` is a link to `/private/tmp`. A rule for either spelling
    // must cover a request written with either spelling.
    let directory =
      System.IO.Path.Combine(
        System.IO.Path.GetTempPath(),
        $"dark-linkroot-{System.Guid.NewGuid()}"
      )
    let real = System.IO.Path.Combine(directory, "real")
    let link = System.IO.Path.Combine(directory, "link")
    System.IO.Directory.CreateDirectory real |> ignore<System.IO.DirectoryInfo>
    try
      System.IO.Directory.CreateSymbolicLink(link, real)
      |> ignore<System.IO.FileSystemInfo>
      let allowsUnder root path =
        let policy =
          Permission.Policy.create
            [ Permission.Rule.File(Permission.AccessKind.Read, only root) ]
            []
        match Permission.Request.file Permission.AccessKind.Read path with
        | Ok request -> Permission.Policy.allows request policy
        | Error _ -> false
      let viaLink = System.IO.Path.Combine(link, "x.txt")
      let viaReal = System.IO.Path.Combine(real, "x.txt")
      Expect.isTrue (allowsUnder link viaLink) "link root covers a path via the link"
      Expect.isTrue (allowsUnder link viaReal) "link root covers the real path"
      Expect.isTrue (allowsUnder real viaLink) "real root covers a path via the link"
      Expect.isFalse
        (allowsUnder link (System.IO.Path.Combine(link, "..", "secret")))
        "traversal out of the link is still outside the root"
      Expect.equal
        (LibExecution.HostSecurity.FilePath.canonicalAncestors viaLink)
        viaReal
        "a request resolves its linked ancestors"
      Expect.equal
        (LibExecution.HostSecurity.FilePath.canonicalAncestors link)
        link
        "the final component is kept as written"
      Expect.equal
        (LibExecution.HostSecurity.FilePath.canonical link)
        real
        "a root resolves fully"
    finally
      System.IO.Directory.Delete(directory, true)
  }

let processRulesPreserveArgumentOrder =
  test "process rules match resolved executables and ordered arguments" {
    let policy =
      Permission.Policy.create
        [ Permission.Rule.Process
            { executable = only "/usr/bin/git"; args = only [ "status"; "--short" ] } ]
        []

    let allows executable args =
      match Permission.Request.processSpawn executable args with
      | Ok operation -> Permission.Policy.allows operation policy
      | Error _ -> false

    Expect.isTrue (allows "/usr/bin/git" [ "status"; "--short" ]) "exact argv"
    Expect.isFalse (allows "/usr/bin/git" [ "--short"; "status" ]) "order differs"
    Expect.isFalse (allows "git" [ "status"; "--short" ]) "unresolved name"
    // A relative rule executable would mean a different program per working
    // directory; it is not a grant.
    let relative =
      Permission.Policy.create
        [ Permission.Rule.Process
            { executable = only "git"; args = Permission.Scope.All } ]
        []
    let resolved = System.IO.Path.GetFullPath "git"
    match Permission.Request.processSpawn resolved [] with
    | Ok request ->
      Expect.isFalse
        (Permission.Policy.allows request relative)
        "a relative rule executable matches nothing, even its cwd resolution"
    | Error e -> failtest e
  }

let coverableEffectsFollowTheRules =
  test "the effects a policy can allow are read off its allow rules" {
    Expect.equal
      (Permission.Policy.coverableEffects consumerRules)
      (Some(Set.ofList [ Effect.Effect.FileRead; Effect.Effect.Clock ]))
      "a file-read root and a clock effect"
    Expect.equal
      (Permission.Policy.coverableEffects Permission.Policy.allowAll)
      None
      "`all` is unbounded"
    Expect.equal
      (Permission.Policy.coverableEffects Permission.Policy.denyAll)
      (Some Set.empty)
      "no allow rules cover nothing"
  }

let suggestRuleIsActionable =
  test "the suggested fix comes structurally from the denied request" {
    let ok =
      function
      | Ok r -> r
      | Error e -> Exception.raiseInternal e []
    let suggest = Permission.Request.suggestRule
    // Read vs write is exact because it comes from the request, not the op.
    Expect.equal
      (suggest (ok (Permission.Request.file Permission.AccessKind.Read "/tmp/x")))
      (Some "file read '/tmp/x'")
      "a read request suggests a read rule"
    Expect.equal
      (suggest (ok (Permission.Request.file Permission.AccessKind.Write "/tmp/x")))
      (Some "file write '/tmp/x'")
      "a write request suggests a write rule"
    Expect.equal
      (suggest (ok (Permission.Request.http "POST" "https://api.x/v1")))
      (Some "http POST 'https://api.x:443/v1'")
      "http names the exact method and normalized origin"
    Expect.equal
      (suggest (ok (Permission.Request.native "cliProcessIO")))
      None
      "the all-or-nothing native boundary has no scoped rule to suggest"
  }

let capturedAccessCannotWidenCaller =
  test "constraining by a capture is a conjunction in both directions" {
    let allowAll = Permission.Access.start Permission.Policy.allowAll
    let denyAll = Permission.Access.start Permission.Policy.denyAll
    Expect.isFalse
      (Permission.Access.allows
        Permission.Request.clock
        (denyAll |> Permission.Access.constrainBy allowAll))
      "a wide capture cannot widen a narrow caller"
    Expect.isFalse
      (Permission.Access.allows
        Permission.Request.clock
        (allowAll |> Permission.Access.constrainBy denyAll))
      "a narrow capture constrains a wide applying frame"
  }

let materializedValuesCaptureCallablesRecursively =
  test "materialized values capture nested callables" {
    let denyAll = Permission.Access.start Permission.Policy.denyAll
    let named : RT.ApplicableNamedFn =
      { name = RT.FQFnName.fqBuiltin "timeNowMs" 0
        typeSymbolTable = RT.TST.empty
        typeArgs = []
        access = None
        argsSoFar = [] }
    let stored =
      RT.DList(RT.ValueType.Unknown, [ RT.DApplicable(RT.AppNamedFn named) ])
    match LibExecution.Dval.captureValueAccess denyAll stored with
    | RT.DList(_, [ RT.DApplicable(RT.AppNamedFn captured) ]) ->
      match captured.access with
      | Some access ->
        Expect.isFalse
          (Permission.Access.allows Permission.Request.clock access)
          "the nested function keeps the loading frame's restriction"
      | None -> failtest "the nested function was left uncaptured"
    | other -> failtest $"unexpected captured value: {other}"

    let lambda : RT.ApplicableLambda =
      { exprId = 1UL
        closedRegisters = []
        typeSymbolTable = RT.TST.empty
        access = Permission.Access.start Permission.Policy.allowAll
        argsSoFar = [] }
    match
      LibExecution.Dval.captureValueAccess
        denyAll
        (RT.DApplicable(RT.AppLambda lambda))
    with
    | RT.DApplicable(RT.AppLambda captured) ->
      Expect.isFalse
        (Permission.Access.allows Permission.Request.clock captured.access)
        "a stored lambda is rebound to the loading frame"
    | other -> failtest $"unexpected captured lambda: {other}"
  }

let tests =
  testList
    "permissions"
    [ policiesDenyByDefault
      denyOverridesAllow
      accessOnlyNarrows
      instanceDenialWinsOverInnerDenial
      warnModeNeverRelaxesTheAuthorsCeiling
      httpRulesKeepDimensionsCoupled
      httpQueryIsPartOfTheOperation
      invalidRequestsFailClosed
      fileRulesRespectBoundaries
      fileRulesSeeThroughSymlinkedRoots
      processRulesPreserveArgumentOrder
      coverableEffectsFollowTheRules
      suggestRuleIsActionable
      capturedAccessCannotWidenCaller
      materializedValuesCaptureCallablesRecursively ]
