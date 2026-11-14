/// The policy engine: exact requests, rules, policies, the monotonic
/// `Access` conjunction, and the access captured on function values.
module Tests.Permissions

open System.Threading.Tasks
open FSharp.Control.Tasks

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
      path =
        match path with
        | Some path -> Permission.HttpPath.Subtree path
        | None -> Permission.HttpPath.All
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
    // Without entering a named approval, missing instance grants still deny
    // access, even when another restriction also denies the operation.
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
        "the missing instance grant is reported"
    | Permission.Decision.Allowed -> failtest "should be denied"
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
    Expect.isError
      (Permission.Request.httpServer "127.0.0.1" 70000)
      "invalid ports are rejected"
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

module Bin = LibSerialization.Binary.Serializers.Common

let retiredDatastoreRulesAreRejected =
  test "retired permission effects and tags are rejected" {
    for retired in [ None; Some "db-read"; Some "db-write" ] do
      use stream = new System.IO.MemoryStream()
      use writer = new System.IO.BinaryWriter(stream)
      Bin.Varint.write writer 1
      match retired with
      | None -> writer.Write 6uy
      | Some name ->
        writer.Write 1uy
        Bin.String.write writer name
      writer.Flush()
      stream.Position <- 0L
      use reader = new System.IO.BinaryReader(stream)
      Expect.throws
        (fun () ->
          LibSerialization.Binary.Serializers.Permissions.read reader
          |> ignore<Permission.Policy>)
        "unsupported rules never silently disappear"
  }

let bulkEnvironmentHonorsDenies =
  test "an environment enumeration cannot bypass a denied variable" {
    let deny = Permission.Rule.Env(Permission.AccessKind.Read, only "SECRET")
    let policy = Permission.Policy.create [ Permission.Rule.All ] [ deny ]
    Expect.isFalse
      (Permission.Policy.allows Permission.Request.envList policy)
      "bulk access includes the denied variable"
    let allowed =
      Permission.Request.env Permission.AccessKind.Read "PUBLIC"
      |> Result.defaultWith (fun e -> failtest e)
    Expect.isTrue
      (Permission.Policy.allows allowed policy)
      "unrelated individual reads remain allowed"
  }

let httpRulesNormalizeLikeRequests =
  test "HTTP allows and denies use the same canonical URLs as requests" {
    let pairs =
      [ "example.com", "/café", "https://example.com/caf%C3%A9"
        "bücher.example", "/", "https://xn--bcher-kva.example/"
        "example.com", "/a/../b", "https://example.com/b"
        "example.com", "/caf%c3%a9", "https://example.com/café" ]
    for host, path, url in pairs do
      let rule =
        Permission.Rule.Http
          { method = only "GET"
            scheme = only "https"
            host = Permission.HostRule.Exact host
            port = only 443
            path = Permission.HttpPath.Exact path
            query = Permission.Scope.All }
      let operation = request "GET" url
      Expect.isTrue
        (Permission.Policy.allows operation (Permission.Policy.create [ rule ] []))
        "equivalent URL spelling matches an allow"
      Expect.isFalse
        (Permission.Policy.allows
          operation
          (Permission.Policy.create [ Permission.Rule.All ] [ rule ]))
        "equivalent URL spelling matches a deny"
  }

let suggestRuleIsActionable =
  test "the suggested fix comes structurally from the denied request" {
    let ok =
      function
      | Ok r -> r
      | Error e -> Exception.raiseInternal e []
    let suggest = Permission.Request.suggestRule
    let rule (text : string) = Permission.Suggestion.Paste text
    let file (one : string) (folder : string) (profile : string) =
      Permission.Suggestion.File(one, folder, profile)
    // Read vs write is exact because it comes from the request, not the op.
    Expect.equal
      (suggest (ok (Permission.Request.file Permission.AccessKind.Read "/tmp/x")))
      (file "file read '/tmp/x'" "file read '/tmp'" "read-only '/tmp'")
      "a read request suggests a read rule, its folder's, and the read-only profile"
    Expect.equal
      (suggest (ok (Permission.Request.file Permission.AccessKind.Write "/tmp/x")))
      (file "file write '/tmp/x'" "file write '/tmp'" "local-dev '/tmp'")
      "a write request suggests a write rule, its folder's, and the local-dev profile"
    Expect.equal
      (suggest (ok (Permission.Request.file Permission.AccessKind.Read "/x")))
      (rule "file read '/x'")
      "a file at the root offers no folder: that would be every file"
    Expect.equal
      (suggest (ok (Permission.Request.http "POST" "https://api.x/v1")))
      (rule "http POST 'https://api.x:443/v1'")
      "http names the exact method and normalized origin"
    Expect.equal
      (suggest (ok (Permission.Request.processSpawn "/bin/bash" [ "-c"; "echo hi" ])))
      (Permission.Suggestion.Process(
        "process '/bin/bash' '-c' 'echo hi'",
        "process '/bin/bash'"
      ))
      "a process rule carries the exact argument list, so a shell is not handed out"
    Expect.equal
      (suggest (ok (Permission.Request.processSpawn "/bin/date" [])))
      (Permission.Suggestion.Process(
        "process-exact '/bin/date'",
        "process '/bin/date'"
      ))
      "no-argument processes have an exact, pasteable rule"
    Expect.equal
      (suggest (ok (Permission.Request.native "cliProcessIO")))
      Permission.Suggestion.NoRule
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
      { name = RT.FQFnName.fqBuiltin "syscallGettimeofday" 0
        typeSymbolTable = RT.TST.empty
        typeArgs = []
        approvalName = None
        access = None
        argsSoFar = []
        boundImpls = [] }
    let stored =
      RT.DList(
        RT.ValueType.Unknown,
        RT.Items([ RT.DApplicable(RT.AppNamedFn named) ])
      )
    match LibExecution.Dval.captureValueAccess denyAll stored with
    | RT.DList(_, RT.Items([ RT.DApplicable(RT.AppNamedFn captured) ])) ->
      match captured.access with
      | Some access ->
        Expect.isFalse
          (Permission.Access.allows Permission.Request.clock access)
          "the nested function keeps the loading frame's restriction"
      | None -> failtest "the nested function was left uncaptured"
    | other -> failtest $"unexpected captured value: {other}"

    let lambda : RT.ApplicableLambda =
      { exprId = 1UL
        closedRegisters = [||]
        typeSymbolTable = RT.TST.empty
        access = Permission.Access.start Permission.Policy.allowAll
        expectedReturn = ValueNone
        writtenIn = ValueNone }
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

/// The words a POSIX shell makes of a pasted `permissions allow` line: single quotes
/// are literal, a backslash outside them escapes one character. That is all a
/// suggestion uses, since `quoteRuleToken` single-quotes every guest-controlled field.
let private shellWords (line : string) : List<string> =
  let words = ResizeArray<string>()
  let current = System.Text.StringBuilder()
  let mutable inWord = false
  let mutable quoted = false
  let mutable i = 0
  while i < line.Length do
    let c = line[i]
    if quoted then
      if c = '\'' then
        quoted <- false
      else
        current.Append c |> ignore<System.Text.StringBuilder>
    elif c = '\'' then
      quoted <- true
      inWord <- true
    elif c = '\\' && i + 1 < line.Length then
      i <- i + 1
      current.Append line[i] |> ignore<System.Text.StringBuilder>
      inWord <- true
    elif c = ' ' then
      if inWord then words.Add(current.ToString())
      current.Clear() |> ignore<System.Text.StringBuilder>
      inWord <- false
    else
      current.Append c |> ignore<System.Text.StringBuilder>
      inWord <- true
    i <- i + 1
  if inWord then words.Add(current.ToString())
  List.ofSeq words

/// Read the arguments from a suggested command in the error message.
let private readSuggestedCommandArguments
  (marker : string)
  (output : string)
  : Option<string> =
  match output.IndexOf marker with
  | -1 -> None
  | start ->
    let from = start + marker.Length
    let line = output.Substring(from).Split('\n')[0]
    Some(line.TrimEnd())

/// Read the rule from the suggested permissions command.
let private suggestedRule (output : string) : Option<string> =
  readSuggestedCommandArguments "To allow:\n  dark permissions allow " output

let private denied (output : string) : bool = output.Contains "permission denied"

/// Refuse <param refused> under a fresh install's policy, paste the rule its denial
/// suggests the way a shell would hand it to `dark`, then run <param refused> again and
/// each of <param neighbours>. The rule has to let the one through and none of the others:
/// a suggestion that only passed the first half would be one that hands out more than
/// was asked for, which is the defect this exists to catch.
let private pastedRuleAllowsOnlyTheRefusedCall
  (i : CliInstance.T)
  (refused : string)
  (neighbours : List<string>)
  : Task<string> =
  task {
    let! before = CliInstance.run i [ "eval"; refused ]
    Expect.isTrue (denied before) $"a fresh install refuses it first:\n{before}"
    let rule =
      match suggestedRule before with
      | Some rule -> rule
      | None -> failtest $"the denial suggests no rule to paste:\n{before}"

    let! saved = CliInstance.run i ("permissions" :: "allow" :: shellWords rule)
    Expect.isFalse (denied saved) $"pasting `{rule}` is accepted:\n{saved}"

    let! after = CliInstance.run i [ "eval"; refused ]
    Expect.isFalse (denied after) $"`{rule}` allows the refused call:\n{after}"

    for neighbour in neighbours do
      let! other = CliInstance.run i [ "eval"; neighbour ]
      Expect.isTrue
        (denied other)
        $"`{rule}` must not allow `{neighbour}` as well:\n{other}"
    return rule
  }

let pastedShellRuleRunsOnlyThatCommand =
  CliInstance.instanceTest
    "a pasted process suggestion for a shell command allows that command and no other"
    (fun i ->
      task {
        let! before =
          CliInstance.run i [ "eval"; "Stdlib.Cli.execute(\"echo hi\")" ]
        Expect.stringContains
          before
          "this exact command only"
          "the narrow choice is labelled"
        Expect.stringContains
          before
          "with any arguments"
          "the executable choice is offered"
        Expect.stringContains
          before
          "this allows any command run through that shell"
          "the shell grant's scope is explained"
        let! rule =
          pastedRuleAllowsOnlyTheRefusedCall
            i
            "Stdlib.Cli.execute(\"echo hi\")"
            [ "Stdlib.Cli.execute(\"echo bye\")"
              "Stdlib.Cli.execute(\"echo hi; id\")" ]
        Expect.stringEnds rule "'-c' 'echo hi'" "the shell's whole argument list"
        let executable = (shellWords rule)[1]
        let! _ = CliInstance.run i [ "permissions"; "allow"; "process"; executable ]
        let! other =
          CliInstance.run i [ "eval"; "Stdlib.Cli.execute(\"echo bye\")" ]
        Expect.isFalse
          (denied other)
          "the executable grant allows different commands"
      })

let pastedProcessRuleKeepsItsArguments =
  CliInstance.instanceTest
    "a pasted process suggestion keeps its arguments, quotes and spaces included"
    (fun i ->
      task {
        let! _ =
          pastedRuleAllowsOnlyTheRefusedCall
            i
            "Stdlib.Cli.Process.run(\"/bin/echo\", [\"it's\", \"a b\"])"
            [ "Stdlib.Cli.Process.run(\"/bin/echo\", [\"it's\", \"a c\"])"
              "Stdlib.Cli.Process.run(\"/bin/echo\", [\"it's\"])"
              "Stdlib.Cli.Process.run(\"/bin/echo\", [])" ]
        ()
      })

let pastedFileRuleReadsOnlyThatFile =
  CliInstance.instanceTest
    "a pasted file suggestion allows that file and not its sibling"
    (fun i ->
      task {
        let path name =
          let p = System.IO.Path.Combine(i.dir, name)
          System.IO.File.WriteAllText(p, name)
          p
        let mine = path "mine.txt"
        let sibling = path "sibling.txt"
        let! _ =
          pastedRuleAllowsOnlyTheRefusedCall
            i
            $"Stdlib.Cli.FileSystem.readFile(\"{mine}\")"
            [ $"Stdlib.Cli.FileSystem.readFile(\"{sibling}\")" ]
        ()
      })

let fileDenialOffersTheFolderAndTheProfile =
  CliInstance.instanceTest
    "a file denial also offers its folder's rule and the profile for that folder"
    (fun i ->
      task {
        let dir name =
          let d =
            System.IO.Path.Combine(System.IO.Path.GetDirectoryName i.dir, name)
          System.IO.Directory.CreateDirectory d |> ignore<System.IO.DirectoryInfo>
          d
        let write (d : string) name =
          let p = System.IO.Path.Combine(d, name)
          System.IO.File.WriteAllText(p, name)
          $"Stdlib.Cli.FileSystem.readFile(\"{p}\")"
        let project = dir "project"
        let first = write project "a.csv"
        let second = write project "b.csv"
        let outside = write (dir "elsewhere") "c.csv"

        let! code, stdout, before = CliInstance.runRaw i [ "eval"; first ]
        Expect.equal code 1 "a permission refusal exits nonzero"
        Expect.equal stdout "" "errors go to stderr"
        Expect.stringStarts
          before
          "Error: permission denied by instance policy:\n"
          "permission errors have a heading without an exception prefix"
        Expect.isFalse
          (before.Contains '\u001b')
          "redirected output has no ANSI escapes"
        Expect.stringContains
          before
          "\n\nAgents:\n  "
          "agent guidance has its own block"
        Expect.isTrue (denied before) $"a fresh install refuses it first:\n{before}"
        let folderRule =
          match
            readSuggestedCommandArguments
              "For everything in its folder:\n  dark permissions allow "
              before
          with
          | Some rule -> rule
          | None -> failtest $"no folder rule offered:\n{before}"
        let profile =
          match
            readSuggestedCommandArguments "  dark permissions profile " before
          with
          | Some profile -> profile
          | None -> failtest $"no profile offered:\n{before}"
        Expect.equal folderRule $"file read '{project}'" "the folder the file is in"
        Expect.equal
          profile
          $"read-only '{project}'"
          "a read offers the read-only profile"

        let! saved =
          CliInstance.run i ("permissions" :: "allow" :: shellWords folderRule)
        Expect.isFalse
          (denied saved)
          $"pasting `{folderRule}` is accepted:\n{saved}"
        for call in [ first; second ] do
          let! out = CliInstance.run i [ "eval"; call ]
          Expect.isFalse (denied out) $"the folder rule allows `{call}`:\n{out}"
        let! other = CliInstance.run i [ "eval"; outside ]
        Expect.isTrue (denied other) $"and nothing outside the folder:\n{other}"

        // The profile, from a policy that allows none of it.
        let! _ = CliInstance.run i [ "permissions"; "clear" ]
        let! applied =
          CliInstance.run
            i
            (("permissions" :: "profile" :: shellWords profile) @ [ "--yes" ])
        Expect.stringContains
          applied
          "applied profile"
          $"the profile applies:\n{applied}"
        let! afterProfile = CliInstance.run i [ "eval"; second ]
        Expect.isFalse
          (denied afterProfile)
          $"the profile covers the folder:\n{afterProfile}"
        let! otherAfterProfile = CliInstance.run i [ "eval"; outside ]
        Expect.isTrue
          (denied otherAfterProfile)
          $"and only it:\n{otherAfterProfile}"
      })

/// GET <param port>'s root, waiting up to a minute for the server to come up.
let private getWhenUp (port : int) : Task<int * string> =
  task {
    use client = new System.Net.Http.HttpClient()
    let deadline = System.DateTime.UtcNow.AddMinutes 1.0
    let rec attempt () =
      task {
        try
          let! response = client.GetAsync($"http://127.0.0.1:{port}/")
          let! body = response.Content.ReadAsStringAsync()
          return (int response.StatusCode, body)
        with :? System.Net.Http.HttpRequestException when
            System.DateTime.UtcNow < deadline ->
          do! Task.Delay 250
          return! attempt ()
      }
    return! attempt ()
  }

/// `serve <param flags>` picks up a policy change and an approval made while it runs.
let private serveFollowsApprovalsAndPolicy (flags : List<string>) =
  CliInstance.instanceTest
    ("a running serve"
     + (flags |> List.map (fun f -> " " + f) |> String.concat "")
     + " uses an approval or a policy change from the next request")
    (fun i ->
      task {
        // An environment read, which is one host operation: the server's HOME is the instance's.
        let home = System.IO.Path.GetDirectoryName i.dir
        let author (name : string) (body : string) =
          task {
            let! (out, code) = CliInstance.runWithExit i [ "fn"; name; body ]
            Expect.equal code 0L $"`fn {name}` saved:\n{out}"
          }
        do!
          author
            "Tests.Follow.load"
            "(): String = Stdlib.Option.withDefault(Stdlib.Env.get(\"HOME\"), \"unset\")"
        do!
          author
            "Tests.Follow.router"
            "(req: Stdlib.Http.Request): Stdlib.Http.Response =\n  Stdlib.Http.responseWithText(Stdlib.Http.Status.ok, Tests.Follow.load())"
        let! (committed, code) =
          CliInstance.runWithExit i [ "commit"; "Tests.Follow"; "-y" ]
        Expect.equal code 0L $"committed:\n{committed}"

        let port = Tests.HttpServer.allocateFreePort ()
        let! granted, grantCode =
          CliInstance.runWithExit
            i
            [ "permissions"; "allow"; "http-server"; "127.0.0.1"; string port ]
        Expect.equal grantCode 0L granted
        let server =
          CliInstance.start
            i
            ([ "serve"; "Tests.Follow.router"; "--port"; string port ] @ flags)
        try
          let! (status, body) = getWhenUp port
          Expect.equal
            status
            500
            $"neither the approval nor the read is allowed yet:\n{body}"
          Expect.stringContains body "permission denied" "and the handler says why"
          Expect.stringContains body "the access of" "missing entry approval"

          // Approving a dependency cannot give the router additional access.
          let! _ =
            CliInstance.run
              i
              [ "permissions"
                "approve"
                "Tests.Follow.load"
                "env read HOME"
                "--yes" ]
          let! (status, _) = getWhenUp port
          Expect.equal status 500 "a helper's approval cannot widen its caller"

          let! (approved, code) =
            CliInstance.runWithExit
              i
              [ "permissions"
                "approve"
                "Tests.Follow.router"
                "env read HOME"
                "--yes" ]
          Expect.equal code 0L $"approved:\n{approved}"
          let! (status, body) = getWhenUp port
          Expect.equal (status, body) (200, home) "entry approval supplies access"

          let! _ =
            CliInstance.run i [ "permissions"; "deny"; "env"; "read"; "HOME" ]
          let! (status, body) = getWhenUp port
          Expect.equal status 500 "a new explicit deny reaches the running server"
          Expect.stringContains body "explicitly denied" "the deny wins"
        finally
          CliInstance.stop server
      })

let noArgumentsHasExactRule =
  CliInstance.instanceTest
    "a process run with no arguments suggests an exact rule"
    (fun i ->
      task {
        let! out =
          CliInstance.run i [ "eval"; "Stdlib.Cli.Process.run(\"/bin/true\", [])" ]
        Expect.isTrue (denied out) $"refused:\n{out}"
        Expect.stringContains
          out
          "process-exact"
          "the suggested rule permits no arguments"
      })

let effectsCommand =
  CliInstance.instanceTest "effects are usable without executing code" (fun i ->
    task {
      let! pureOutput, pureCode =
        CliInstance.runWithExit i [ "effects"; "Darklang.Stdlib.Int64.toFloat" ]
      Expect.equal pureCode 0L pureOutput
      Expect.stringContains
        pureOutput
        "effect-free"
        "pure functions have a clear answer"
      let! effectOutput, effectCode =
        CliInstance.runWithExit i [ "effects"; "Darklang.Stdlib.Env.get" ]
      Expect.equal effectCode 0L effectOutput
      Expect.stringContains effectOutput "env-read" "effect shown"
      Expect.stringContains
        effectOutput
        "Builtin.runtimeEnvironmentGet"
        "cause shown"
    })

/// Approvals remember access, not which code a name should resolve to.
let private approvalsDoNotPinCode =
  CliInstance.instanceTest
    "approvals do not pin edited or branch-local code"
    (fun i ->
      task {
        let name = "Tests.CurrentCode.value"
        let run args =
          task {
            let! output, code = CliInstance.runWithExit i args
            Expect.equal code 0L output
            return output
          }
        let! _ = run [ "fn"; name; "(): Int64 = 1" ]
        let! _ = run [ "permissions"; "approve"; name; "--yes" ]
        let! _ = run [ "fn"; name; "(): Int64 = 2" ]
        let! edited = run [ "eval"; name + "()" ]
        Expect.equal (edited.Trim()) "2" "the edit runs without reapproval"
        let! _ = run [ "commit"; "current code"; "-y" ]
        let! _ = run [ "branch"; "create"; "approval-branch" ]
        let! _ = run [ "fn"; name; "(): Int64 = 3" ]
        let! branched = run [ "eval"; name + "()" ]
        Expect.equal (branched.Trim()) "3" "approval cannot override branch code"
        let! _ = run [ "permissions"; "unapprove"; name ]
        let! withdrawn = run [ "eval"; name + "()" ]
        Expect.equal (withdrawn.Trim()) "3" "withdrawal does not change code"
      })

/// Editing a helper changes its caller's hash too. Neither edit inherits access.
let private editedCodeNeedsFreshApproval =
  CliInstance.instanceTest
    "edited functions and dependencies run current code with fresh approval"
    (fun i ->
      task {
        let name = "Tests.CurrentAccess.report"
        let helper = "Tests.CurrentAccess.message"
        let run args =
          task {
            let! output, code = CliInstance.runWithExit i args
            Expect.equal code 0L output
            return output
          }
        let! _ = run [ "fn"; helper; "(): String = \"before\"" ]
        let body =
          "(): String =\n  let _ = Stdlib.Env.get(\"HOME\")\n  " + helper + "()"
        let! _ = run [ "fn"; name; body ]
        let! _ = run [ "permissions"; "approve"; name; "env read HOME"; "--yes" ]
        let! before = run [ "eval"; name + "()" ]
        Expect.equal (before.Trim()) "before" "reviewed code has access"

        let! _ = run [ "fn"; helper; "(): String = \"after\"" ]
        let! denied, code = CliInstance.runWithExit i [ "eval"; name + "()" ]
        Expect.notEqual code 0L denied
        Expect.stringContains
          denied
          "permission denied"
          "helper edit loses approval"

        let script = System.IO.Path.Combine(i.dir, "current.dark")
        System.IO.File.WriteAllText(script, "Stdlib.printLine(" + name + "())\n")
        let! temporary = run [ "run"; "--allow"; "env read HOME"; script ]
        Expect.equal
          (temporary.Trim())
          "after"
          "script calls use the edited dependency"
        let! stillDenied, code = CliInstance.runWithExit i [ "eval"; name + "()" ]
        Expect.notEqual code 0L stillDenied
        Expect.stringContains
          stillDenied
          "permission denied"
          "run grant stays temporary"

        let! _ = run [ "permissions"; "update"; name; "--yes" ]
        let! after = run [ "eval"; name + "()" ]
        Expect.equal (after.Trim()) "after" "update approves current code"

        let editedBody =
          "(): String =\n  let _ = Stdlib.Env.get(\"HOME\")\n  \"edited\""
        let! _ = run [ "fn"; name; editedBody ]
        let! denied, code = CliInstance.runWithExit i [ "eval"; name + "()" ]
        Expect.notEqual code 0L denied
        Expect.stringContains denied "permission denied" "root edit loses approval"
        let! _ = run [ "permissions"; "approve"; name; "env read HOME"; "--yes" ]
        let! edited = run [ "eval"; name + "()" ]
        Expect.equal (edited.Trim()) "edited" "approve also reviews current code"
      })

/// A trait implementation the caller picks runs with the caller's access, not the approval's.
let private callerChosenImplsKeepCallerAccess =
  CliInstance.instanceTest
    "a trait implementation the caller chose runs with the caller's access"
    (fun i ->
      task {
        let run args =
          task {
            let! output, code = CliInstance.runWithExit i args
            Expect.equal code 0L output
            return output
          }
        let readHome =
          "Stdlib.Option.withDefault(Stdlib.Env.get(\"HOME\"), \"none\")"
        let! _ =
          run
            [ "fn"
              "Tests.Approved.describe"
              "<'a: Display>(v: 'a): String = \"value: \" + Stdlib.Display.display(v)" ]
        let! _ = run [ "type"; "Tests.Approved.Own"; "{ x: Int64 }" ]
        let! _ =
          run
            [ "impl"
              "Tests.Approved"
              $"Display for Own = let display(o: Own): String = {readHome}" ]
        let! _ =
          run
            [ "fn"
              "Tests.Approved.own"
              "(): String = Stdlib.Display.display(Tests.Approved.Own { x = 1 })" ]
        let! _ = run [ "type"; "Tests.Caller.Zork"; "{ x: Int64 }" ]
        let! _ =
          run
            [ "impl"
              "Tests.Caller"
              $"Display for Zork = let display(z: Zork): String = {readHome}" ]
        let! _ =
          run
            [ "fn"
              "Tests.Caller.go"
              "(): String = Tests.Approved.describe(Tests.Caller.Zork { x = 1 })" ]
        for name in [ "Tests.Approved.describe"; "Tests.Approved.own" ] do
          let! _ = run [ "permissions"; "approve"; name; "env read HOME"; "--yes" ]
          ()

        for call in
          [ "Tests.Approved.describe(Tests.Caller.Zork { x = 1 })"
            "Tests.Caller.go()" ] do
          let! denied, code = CliInstance.runWithExit i [ "eval"; call ]
          Expect.notEqual code 0L denied
          Expect.stringContains denied "permission denied" call

        let! plain = run [ "eval"; "Tests.Approved.describe(5)" ]
        Expect.equal (plain.Trim()) "value: 5" "a pure implementation still runs"
        let! own = run [ "eval"; "Tests.Approved.own()" ]
        Expect.stringContains own "/" "the approved fn's own choice keeps access"
      })

let private httpPathsAreExactUnlessSubtree =
  test "HTTP exact paths and explicit subtrees stay distinct" {
    let operation = request "GET" "https://api.example.com/orders"
    let exact = Permission.Rule.forRequest operation
    Expect.isTrue (Permission.Rule.covers exact operation) "the reviewed path"
    for path in [ "/orders/1"; "/orders/"; "/orders-old"; "/orders/../private" ] do
      Expect.isFalse
        (Permission.Rule.covers
          exact
          (request "GET" ("https://api.example.com" + path)))
        $"exact approval excludes {path}"
    let subtree = httpRule "GET" "api.example.com" (Some "/orders") None
    Expect.isTrue
      (Permission.Rule.covers
        subtree
        (request "GET" "https://api.example.com/orders/1"))
      "an explicit subtree includes descendants"
    Expect.isFalse
      (Permission.Rule.covers
        subtree
        (request "GET" "https://api.example.com/orders-old"))
      "subtrees respect segment boundaries"
    let policy = Permission.Policy.create [ subtree ] [ exact ]
    Expect.isFalse (Permission.Policy.allows operation policy) "an exact deny wins"
    Expect.isTrue
      (Permission.Policy.allows
        (request "GET" "https://api.example.com/orders/1")
        policy)
      "an exact deny does not become a subtree deny"
  }

let private serverRulesCheckAddressAndPort =
  test "server rules check addresses and ports including wildcard deny overlap" {
    let server address port =
      match Permission.Request.httpServer address port with
      | Ok request -> request
      | Error message -> failtest message
    let local = Permission.Rule.HttpServer(only "127.0.0.1", only 9090)
    let allowed = Permission.Policy.create [ local ] []
    Expect.isTrue
      (Permission.Policy.allows (server "localhost" 9090) allowed)
      "localhost"
    for address, port in [ "0.0.0.0", 9090; "192.0.2.1", 9090; "127.0.0.1", 9091 ] do
      Expect.isFalse
        (Permission.Policy.allows (server address port) allowed)
        "another address or port needs separate access"
    let denied = Permission.Policy.create [ Permission.Rule.All ] [ local ]
    Expect.isFalse
      (Permission.Policy.allows (server "0.0.0.0" 9090) denied)
      "binding every interface cannot bypass a loopback deny"
    Expect.isTrue
      (Permission.Policy.allows (server "192.0.2.1" 9090) denied)
      "the deny leaves a different explicit address alone"
    for invalid in [ "example.com"; "127.1"; "*"; "127.0.0.1/path" ] do
      Expect.isError
        (Permission.Request.httpServer invalid 9090)
        "never resolve DNS or widen input"
  }

let tests =
  testList
    "permissions"
    [ httpPathsAreExactUnlessSubtree
      serverRulesCheckAddressAndPort
      approvalsDoNotPinCode
      editedCodeNeedsFreshApproval
      callerChosenImplsKeepCallerAccess
      effectsCommand
      policiesDenyByDefault
      denyOverridesAllow
      accessOnlyNarrows
      instanceDenialWinsOverInnerDenial
      httpRulesKeepDimensionsCoupled
      httpQueryIsPartOfTheOperation
      invalidRequestsFailClosed
      fileRulesRespectBoundaries
      fileRulesSeeThroughSymlinkedRoots
      processRulesPreserveArgumentOrder
      coverableEffectsFollowTheRules
      retiredDatastoreRulesAreRejected
      bulkEnvironmentHonorsDenies
      httpRulesNormalizeLikeRequests
      suggestRuleIsActionable
      pastedShellRuleRunsOnlyThatCommand
      pastedProcessRuleKeepsItsArguments
      pastedFileRuleReadsOnlyThatFile
      fileDenialOffersTheFolderAndTheProfile
      serveFollowsApprovalsAndPolicy []
      serveFollowsApprovalsAndPolicy [ "--live" ]
      noArgumentsHasExactRule
      capturedAccessCannotWidenCaller
      materializedValuesCaptureCallablesRecursively ]
