/// Runtime permissions.
///
/// A Request is one exact host operation. A Rule may cover requests. A Policy
/// is an allowlist with optional explicit denies. Access is the immutable
/// conjunction of the invocation grants and its mandatory restrictions.
module LibExecution.Permissions

open Prelude

module Effect = LibExecution.Effects

[<RequireQualifiedAccess>]
type Scope<'a> =
  | All
  | Only of 'a

module Scope =
  let covers (matches : 'a -> 'b -> bool) (scope : Scope<'a>) (value : 'b) : bool =
    match scope with
    | Scope.All -> true
    | Scope.Only allowed -> matches allowed value

/// Read or write, for every resource family that distinguishes the two.
[<RequireQualifiedAccess>]
type AccessKind =
  | Read
  | Write

type private HttpRequest =
  {
    method : string
    scheme : string
    host : string
    port : int
    path : string
    /// Exact query string, including the leading `?`; an absent query is empty.
    /// Query values can select different resources, so they are checked too.
    query : string
  }

/// Exact, normalized operations checked immediately before a host effect.
[<RequireQualifiedAccess>]
type Request =
  private
  | Http of HttpRequest
  | HttpServer of address : string * port : int
  | File of access : AccessKind * path : string
  | Env of access : AccessKind * name : string
  | EnvList
  | Stdin
  | Stdout
  | Clock
  | Random
  | Process of executable : string * args : List<string>
  /// The package and trace stores are host-owned wholes: there is no
  /// per-resource scoping, so these are ambient effects like `Stdout`.
  | Package of access : AccessKind
  | Trace of access : AccessKind
  /// Spawning a process, `Exec.spawn`. Ambient, like `Stdout`.
  | Concurrency
  /// The runtime's own in-memory state (`Effect.Runtime`). Ambient.
  | Runtime
  | Native of operation : string

/// What a denial offers to paste.
[<RequireQualifiedAccess>]
type Suggestion =
  /// `permissions allow` text that covers the refused request and no other.
  | Paste of string
  /// The effect has no safely scoped rule to suggest (`Native`).
  | NoRule
  /// A file: the rule for that one file, as `Paste`, and also the rule for the folder it is in
  /// and the profile for working in that folder (`<name> <root>`), so a person can answer once
  /// for a task rather than once per file.
  | File of rule : string * folderRule : string * profile : string

module Suggestion =
  /// The rule to record and to offer at an interactive prompt, if any.
  let rule (suggestion : Suggestion) : Option<string> =
    match suggestion with
    | Suggestion.Paste rule
    | Suggestion.File(rule, _, _) -> Some rule
    | Suggestion.NoRule -> None

/// URI normalization shared by requests and literal components of HTTP rules.
module HttpUrl =
  let escaped (value : string) : string =
    System.Text.RegularExpressions.Regex.Replace(
      value,
      "%[0-9a-fA-F]{2}",
      System.Text.RegularExpressions.MatchEvaluator(fun m ->
        m.Value.ToUpperInvariant())
    )

  let host (uri : System.Uri) = uri.IdnHost.ToLowerInvariant().TrimEnd('.')
  let path (uri : System.Uri) = escaped uri.AbsolutePath
  let query (uri : System.Uri) = escaped uri.Query

module Request =
  /// One shell-safe token for an actionable `permissions allow` command.
  /// Always quote guest-controlled text: paths and URLs may contain spaces or
  /// shell metacharacters, and a denial must never print an injectable command.
  let private quoteRuleToken (value : string) : string =
    "'" + value.Replace("'", "'\\''") + "'"

  let effect (request : Request) : Effect.Effect =
    match request with
    | Request.Http _ -> Effect.Effect.Http
    | Request.HttpServer _ -> Effect.Effect.HttpServer
    | Request.File(AccessKind.Read, _) -> Effect.Effect.FileRead
    | Request.File(AccessKind.Write, _) -> Effect.Effect.FileWrite
    | Request.Env(AccessKind.Read, _) -> Effect.Effect.EnvRead
    | Request.Env(AccessKind.Write, _) -> Effect.Effect.EnvWrite
    | Request.EnvList -> Effect.Effect.EnvRead
    | Request.Stdin -> Effect.Effect.Stdin
    | Request.Stdout -> Effect.Effect.Stdout
    | Request.Clock -> Effect.Effect.Clock
    | Request.Random -> Effect.Effect.Random
    | Request.Process _ -> Effect.Effect.Process
    | Request.Package AccessKind.Read -> Effect.Effect.PackageRead
    | Request.Package AccessKind.Write -> Effect.Effect.PackageWrite
    | Request.Trace AccessKind.Read -> Effect.Effect.TraceRead
    | Request.Trace AccessKind.Write -> Effect.Effect.TraceWrite
    | Request.Concurrency -> Effect.Effect.Concurrency
    | Request.Runtime -> Effect.Effect.Runtime
    | Request.Native _ -> Effect.Effect.Native

  /// Suggested HTTP rules name one exact path; file rules include children.
  /// A URL with no query allows any query on that path.
  let private suggestTargetRule (request : Request) : Option<string> =
    match request with
    | Request.Http r ->
      let host =
        if r.host.Contains ':' && not (r.host.StartsWith "[") then
          $"[{r.host}]"
        else
          r.host
      let url = $"{r.scheme}://{host}:{r.port}{r.path}{r.query}"
      Some $"http {r.method} {quoteRuleToken url}"
    | Request.HttpServer(address, port) -> Some $"http-server {address} {port}"
    | Request.File(AccessKind.Read, path) -> Some $"file read {quoteRuleToken path}"
    | Request.File(AccessKind.Write, path) ->
      Some $"file write {quoteRuleToken path}"
    | Request.Env(AccessKind.Read, name) -> Some $"env read {quoteRuleToken name}"
    | Request.Env(AccessKind.Write, name) -> Some $"env write {quoteRuleToken name}"
    | Request.EnvList -> Some "env read"
    | Request.Stdin -> Some "stdin"
    | Request.Stdout -> Some "stdout"
    | Request.Clock -> Some "clock"
    | Request.Random -> Some "random"
    | Request.Package AccessKind.Read -> Some "package-read"
    | Request.Package AccessKind.Write -> Some "package-write"
    | Request.Trace AccessKind.Read -> Some "trace-read"
    | Request.Trace AccessKind.Write -> Some "trace-write"
    | Request.Concurrency -> Some "concurrency"
    | Request.Runtime -> Some "runtime"
    // Their narrowest rule covers more than the request; `suggestRule` says why.
    | Request.Process _
    | Request.Native _ -> None

  /// The `permissions allow <rule>` text that covers this request and no other
  /// program or argument list, or why there is none to offer.
  ///
  /// A denial prints this, and an interactive `run` offers to save it, so a
  /// suggestion broader than the refused call hands out access nobody asked
  /// for. A process rule therefore carries the exact argument list: a bare
  /// `process '/bin/bash'` would let any code run any shell command, and a
  /// shell-mediated call (`Cli.execute "echo hi"`) is still `/bin/bash -c ...`
  /// to the check, since the shell decides what the string runs.
  let suggestRule (request : Request) : Suggestion =
    match request with
    | Request.File(access, path) ->
      // A read is offered the read-only profile, a write local-dev, which also writes.
      let kind, profile =
        match access with
        | AccessKind.Read -> "read", "read-only"
        | AccessKind.Write -> "write", "local-dev"
      let rule = $"file {kind} {quoteRuleToken path}"
      // The folder, unless it is the root: a rule for `/` is every file there is.
      match System.IO.Path.GetDirectoryName path with
      | null
      | ""
      | "/" -> Suggestion.Paste rule
      | folder ->
        let folder = quoteRuleToken folder
        Suggestion.File(rule, $"file {kind} {folder}", $"{profile} {folder}")
    | Request.Process(executable, []) ->
      Suggestion.Paste $"process-exact {quoteRuleToken executable}"
    | Request.Process(executable, args) ->
      let tokens = executable :: args |> List.map quoteRuleToken |> String.concat " "
      Suggestion.Paste $"process {tokens}"
    | request ->
      match suggestTargetRule request with
      | Some rule -> Suggestion.Paste rule
      | None -> Suggestion.NoRule


  /// Accept literal IPv4 addresses only; localhost has one fixed meaning.
  /// Never resolve a permission's address through DNS.
  let serverAddress (address : string) : Result<string, string> =
    let address = if address = "localhost" then "127.0.0.1" else address
    match System.Net.IPAddress.TryParse address with
    | true, ip when
      ip.AddressFamily = System.Net.Sockets.AddressFamily.InterNetwork
      && ip.ToString() = address
      ->
      Ok address
    | _ -> Error $"Invalid bind address: {address}; use an IPv4 address or localhost"

  let httpServer (address : string) (port : int) : Result<Request, string> =
    if port < 0 || port > 65535 then
      Error $"Invalid HTTP server port: {port}"
    else
      serverAddress address
      |> Result.map (fun address -> Request.HttpServer(address, port))

  /// Parse and normalize an absolute HTTP(S) URL. Invalid input is an error,
  /// never an unrestricted request.
  let http (method : string) (url : string) : Result<Request, string> =
    match System.Uri.TryCreate(url, System.UriKind.Absolute) with
    | true, uri when
      (uri.Scheme = System.Uri.UriSchemeHttp
       || uri.Scheme = System.Uri.UriSchemeHttps)
      && not (System.String.IsNullOrWhiteSpace uri.Host)
      && not (System.String.IsNullOrWhiteSpace method)
      ->
      Ok(
        Request.Http
          { method = method.ToUpperInvariant()
            scheme = uri.Scheme.ToLowerInvariant()
            host = HttpUrl.host uri
            port = uri.Port
            path = HttpUrl.path uri
            query = HttpUrl.query uri }
      )
    | _ -> Error $"Invalid HTTP request: {method} {url}"

  /// Normalize a path before creating its request. The host operation performs
  /// final symlink checks when it opens or changes the path.
  let file (access : AccessKind) (path : string) : Result<Request, string> =
    try
      Ok(Request.File(access, HostSecurity.FilePath.canonicalAncestors path))
    with _ ->
      Error $"Invalid file path: {path}"

  let env (access : AccessKind) (name : string) : Result<Request, string> =
    if System.String.IsNullOrWhiteSpace name || name.Contains "=" then
      Error $"Invalid environment variable name: {name}"
    else
      Ok(Request.Env(access, name))

  let envList : Request = Request.EnvList

  let stdin : Request = Request.Stdin
  let stdout : Request = Request.Stdout
  let clock : Request = Request.Clock
  let random : Request = Request.Random

  /// Process requests contain an already-resolved executable. Name/PATH
  /// resolution belongs inside the checked host boundary, before this call.
  let processSpawn
    (executable : string)
    (args : List<string>)
    : Result<Request, string> =
    try
      if not (System.IO.Path.IsPathFullyQualified executable) then
        Error $"Executable is not an absolute path: {executable}"
      else
        Ok(Request.Process(System.IO.Path.GetFullPath executable, args))
    with _ ->
      Error $"Invalid executable path: {executable}"

  let package (access : AccessKind) : Request = Request.Package access

  let trace (access : AccessKind) : Request = Request.Trace access

  let native (operation : string) : Result<Request, string> =
    if System.String.IsNullOrWhiteSpace operation then
      Error "Native operation cannot be empty"
    else
      Ok(Request.Native operation)

  /// The request an ambient effect stands for, checked at the interpreter
  /// gate before a builtin body runs. `Native` is keyed by the builtin's name.
  /// Scoped effects name a resource and never reach the gate.
  let ofAmbientEffect (effect : Effect.Effect) (builtinName : string) : Request =
    match effect with
    | Effect.Effect.Stdout -> Request.Stdout
    | Effect.Effect.Stdin -> Request.Stdin
    | Effect.Effect.Random -> Request.Random
    | Effect.Effect.Clock -> Request.Clock
    | Effect.Effect.PackageRead -> Request.Package AccessKind.Read
    | Effect.Effect.PackageWrite -> Request.Package AccessKind.Write
    | Effect.Effect.TraceRead -> Request.Trace AccessKind.Read
    | Effect.Effect.TraceWrite -> Request.Trace AccessKind.Write
    | Effect.Effect.Concurrency -> Request.Concurrency
    | Effect.Effect.Runtime -> Request.Runtime
    | Effect.Effect.Native -> Request.Native builtinName
    | scoped ->
      Exception.raiseInternal
        "scoped effect reached the ambient gate"
        [ "effect", Effect.name scoped ]

[<RequireQualifiedAccess>]
type HostRule =
  | Any
  | Exact of string
  /// The domain itself and its subdomains.
  | SubdomainsOf of string

[<RequireQualifiedAccess>]
type HttpPath =
  | All
  | Exact of string
  | Subtree of string

type HttpRule =
  {
    method : Scope<string>
    scheme : Scope<string>
    host : HostRule
    port : Scope<int>
    path : HttpPath
    /// Exact query strings this rule covers. `Scope.All` (the default when a
    /// rule is written without a query) matches any query, so an unspecified
    /// rule keeps the intuitive "any query on this path" behavior; a rule
    /// written with a query covers only that exact query.
    query : Scope<string>
  }

module HttpRule =
  /// Normalize literal URL components while retaining explicitly typed wildcards.
  /// Fragments and credentials cannot scope a network resource and are rejected.
  let normalize (rule : HttpRule) : Result<HttpRule, string> =
    try
      let method =
        match rule.method with
        | Scope.All -> Scope.All
        | Scope.Only method ->
          let parsed = System.Net.Http.HttpMethod(method)
          Scope.Only(parsed.Method.ToUpperInvariant())
      let scheme =
        match rule.scheme with
        | Scope.All -> Scope.All
        | Scope.Only scheme ->
          match scheme.ToLowerInvariant() with
          | "http"
          | "https" as scheme -> Scope.Only scheme
          | _ -> invalidArg "scheme" "Expected HTTP or HTTPS"
      let canonicalHost (host : string) =
        let literal = if host.Contains(":") then $"[{host.Trim('[', ']')}]" else host
        let uri = System.Uri($"https://{literal}/")
        if
          uri.UserInfo <> ""
          || uri.AbsolutePath <> "/"
          || uri.Query <> ""
          || uri.Fragment <> ""
          || not uri.IsDefaultPort
        then
          invalidArg "host" "Invalid permission host"
        HttpUrl.host uri
      let host =
        match rule.host with
        | HostRule.Any -> HostRule.Any
        | HostRule.Exact host -> HostRule.Exact(canonicalHost host)
        | HostRule.SubdomainsOf host ->
          if host.Contains(":") || host.Contains("*") then
            invalidArg "host" "Invalid domain wildcard"
          HostRule.SubdomainsOf(canonicalHost host)
      match rule.port with
      | Scope.Only port when port < 1 || port > 65535 ->
        invalidArg "port" "Invalid HTTP port"
      | _ -> ()
      let pathText =
        match rule.path with
        | HttpPath.All -> "/"
        | HttpPath.Exact path
        | HttpPath.Subtree path ->
          if
            not (path.StartsWith "/")
            || path.Contains("?")
            || path.Contains("#")
            || path.Contains("*")
          then
            invalidArg "path" "Invalid HTTP permission path"
          path
      let queryText =
        match rule.query with
        | Scope.All -> ""
        | Scope.Only query ->
          if (query <> "" && not (query.StartsWith "?")) || query.Contains("#") then
            invalidArg "query" "Invalid HTTP permission query"
          query
      // The fixed origin is only a base for normalizing the path/query reference.
      let uri = System.Uri("https://permission.invalid" + pathText + queryText)
      let path =
        match rule.path with
        | HttpPath.All -> HttpPath.All
        | HttpPath.Exact _ -> HttpPath.Exact(HttpUrl.path uri)
        | HttpPath.Subtree _ -> HttpPath.Subtree(HttpUrl.path uri)
      let query =
        match rule.query with
        | Scope.All -> Scope.All
        | Scope.Only _ -> Scope.Only(HttpUrl.query uri)
      Ok
        { method = method
          scheme = scheme
          host = host
          port = rule.port
          path = path
          query = query }
    with _ ->
      Error "Invalid HTTP permission rule"

/// `args` is the exact argument list, in order, or any.
type ProcessRule = { executable : Scope<string>; args : Scope<List<string>> }

/// Patterns used by both allow and deny lists.
[<RequireQualifiedAccess>]
type Rule =
  | All
  | Effect of Effect.Effect
  | Http of HttpRule
  | HttpServer of address : Scope<string> * port : Scope<int>
  | File of access : AccessKind * roots : Scope<string>
  | Env of access : AccessKind * names : Scope<string>
  | Process of ProcessRule

module Rule =
  /// Resource scopes shown in the approval prompt. File paths include children and
  /// an unspecified HTTP query allows any query, just like written rules.
  /// Uses typed host requests, never a description supplied by guest code.
  let forRequest (request : Request) : Rule =
    match request with
    | Request.Http r ->
      Rule.Http
        { method = Scope.Only r.method
          scheme = Scope.Only r.scheme
          host = HostRule.Exact r.host
          port = Scope.Only r.port
          path = HttpPath.Exact r.path
          query = if r.query = "" then Scope.All else Scope.Only r.query }
    | Request.HttpServer(address, port) ->
      Rule.HttpServer(Scope.Only address, Scope.Only port)
    | Request.File(access, path) -> Rule.File(access, Scope.Only path)
    | Request.Env(access, name) -> Rule.Env(access, Scope.Only name)
    | Request.EnvList -> Rule.Env(AccessKind.Read, Scope.All)
    | Request.Process(executable, args) ->
      Rule.Process { executable = Scope.Only executable; args = Scope.Only args }
    | _ -> Rule.Effect(Request.effect request)

  let private same (left : 'a) (right : 'a) : bool = left = right

  // Windows environment-variable names are case-insensitive; POSIX names are
  // case-sensitive. A deny on `PATH` must still catch a `path` request on
  // Windows.
  let private envNameMatches (expected : string) (actual : string) : bool =
    if System.OperatingSystem.IsWindows() then
      System.String.Equals(
        expected,
        actual,
        System.StringComparison.OrdinalIgnoreCase
      )
    else
      expected = actual

  // Windows and macOS normally ignore case in filesystem paths. Use the
  // platform comparison so alternate-case spellings refer to the same path.
  let private pathComparison : System.StringComparison =
    if System.OperatingSystem.IsWindows() || System.OperatingSystem.IsMacOS() then
      System.StringComparison.OrdinalIgnoreCase
    else
      System.StringComparison.Ordinal

  let private underRoot (root : string) (path : string) : bool =
    let root = System.IO.Path.TrimEndingDirectorySeparator root
    if System.String.Equals(path, root, pathComparison) then
      true
    else
      let separator = string System.IO.Path.DirectorySeparatorChar
      let boundary = if root.EndsWith separator then root else root + separator
      path.StartsWith(boundary, pathComparison)

  // Compare the resolved root and its ancestor-resolved spelling. This handles
  // aliases such as macOS `/tmp` and operations on a symlink entry.
  let private pathUnder (root : string) (path : string) : bool =
    try
      let path = System.IO.Path.GetFullPath path
      underRoot (HostSecurity.FilePath.canonical root) path
      || underRoot (HostSecurity.FilePath.canonicalAncestors root) path
    with _ ->
      false

  let private urlPathUnder (prefix : string) (path : string) : bool =
    let prefix = if prefix = "" then "/" else prefix
    let path = if path = "" then "/" else path
    if path = prefix then
      true
    else
      let boundary = if prefix.EndsWith "/" then prefix else prefix + "/"
      path.StartsWith(boundary, System.StringComparison.Ordinal)

  // Executable rules must be absolute so they identify the same program in
  // every working directory.
  let private samePath (allowed : string) (actual : string) : bool =
    try
      System.IO.Path.IsPathFullyQualified allowed
      && System.String.Equals(
        System.IO.Path.GetFullPath allowed,
        System.IO.Path.GetFullPath actual,
        pathComparison
      )
    with _ ->
      false

  let private hostMatches (rule : HostRule) (host : string) : bool =
    let host = host.ToLowerInvariant().TrimEnd('.')
    match rule with
    | HostRule.Any -> true
    | HostRule.Exact expected -> host = expected.ToLowerInvariant().TrimEnd('.')
    | HostRule.SubdomainsOf domain ->
      let domain = domain.ToLowerInvariant().TrimEnd('.')
      host = domain || host.EndsWith("." + domain, System.StringComparison.Ordinal)

  let private serverAddressMatches expected actual =
    match Request.serverAddress expected with
    | Ok address -> address = actual
    | Error _ -> false

  let private httpPathMatches rule actual =
    match rule with
    | HttpPath.All -> true
    | HttpPath.Exact expected -> expected = actual
    | HttpPath.Subtree root -> urlPathUnder root actual

  let private effectMatches (expected : Effect.Effect) (request : Request) : bool =
    Request.effect request = expected

  let covers (rule : Rule) (request : Request) : bool =
    match rule, request with
    | Rule.All, _ -> true
    | Rule.Effect effect, request -> effectMatches effect request
    | Rule.Http rule, Request.Http request ->
      Scope.covers
        (fun (allowed : string) actual -> allowed.ToUpperInvariant() = actual)
        rule.method
        request.method
      && Scope.covers
        (fun (allowed : string) actual -> allowed.ToLowerInvariant() = actual)
        rule.scheme
        request.scheme
      && hostMatches rule.host request.host
      && Scope.covers same rule.port request.port
      && httpPathMatches rule.path request.path
      && Scope.covers same rule.query request.query
    | Rule.HttpServer(addresses, ports), Request.HttpServer(address, port) ->
      Scope.covers serverAddressMatches addresses address
      && Scope.covers same ports port
    | Rule.File(expected, roots), Request.File(actual, path) when expected = actual ->
      Scope.covers pathUnder roots path
    | Rule.Env(expected, names), Request.Env(actual, name) when expected = actual ->
      Scope.covers envNameMatches names name
    | Rule.Env(AccessKind.Read, Scope.All), Request.EnvList -> true
    | Rule.Process rule, Request.Process(executable, args) ->
      Scope.covers samePath rule.executable executable
      && Scope.covers same rule.args args
    | _ -> false

  /// A wildcard bind includes each local address, so an address-specific deny
  /// must also block it. Checking only equal bind strings would bypass the deny.
  let denies (rule : Rule) (request : Request) : bool =
    match rule, request with
    | Rule.Env(AccessKind.Read, _), Request.EnvList -> true
    | Rule.HttpServer(_, ports), Request.HttpServer("0.0.0.0", port) ->
      Scope.covers same ports port
    | _ -> covers rule request

  /// Return the effect kinds a rule can allow, or `None` for `All`. Approval
  /// checks this before storing an explicit consumer policy.
  let allowableEffects (rule : Rule) : Option<Set<Effect.Effect>> =
    match rule with
    | Rule.All -> None
    | Rule.Effect effect -> Some(Set.singleton effect)
    | Rule.Http _ -> Some(Set.singleton Effect.Effect.Http)
    | Rule.HttpServer _ -> Some(Set.singleton Effect.Effect.HttpServer)
    | Rule.File(AccessKind.Read, _) -> Some(Set.singleton Effect.Effect.FileRead)
    | Rule.File(AccessKind.Write, _) -> Some(Set.singleton Effect.Effect.FileWrite)
    | Rule.Env(AccessKind.Read, _) -> Some(Set.singleton Effect.Effect.EnvRead)
    | Rule.Env(AccessKind.Write, _) -> Some(Set.singleton Effect.Effect.EnvWrite)
    | Rule.Process _ -> Some(Set.singleton Effect.Effect.Process)

type Policy = private { allow : List<Rule>; deny : List<Rule> }

[<RequireQualifiedAccess>]
type PolicyDenial =
  | ExplicitlyDenied
  | NotAllowed

module Policy =
  let denyAll : Policy = { allow = []; deny = [] }
  let allowAll : Policy = { allow = [ Rule.All ]; deny = [] }

  let create (allow : List<Rule>) (deny : List<Rule>) : Policy =
    let normalize =
      function
      | Rule.Http rule ->
        match HttpRule.normalize rule with
        | Ok rule -> Rule.Http rule
        | Error message -> invalidArg "rule" message
      | rule -> rule
    { allow = allow |> List.map normalize |> List.distinct
      deny = deny |> List.map normalize |> List.distinct }

  /// Grant effect families. Actual operations still check every restriction.
  let allowEffects (effects : Set<Effect.Effect>) : Policy =
    effects |> Set.toList |> List.map Rule.Effect |> (fun allow -> create allow [])

  let rules (policy : Policy) : List<Rule> * List<Rule> = policy.allow, policy.deny

  /// Return the effect kinds covered by the allow rules, or `None` when an
  /// `All` rule covers every effect. Deny rules only narrow those effects.
  let coverableEffects (policy : Policy) : Option<Set<Effect.Effect>> =
    policy.allow
    |> List.fold
      (fun acc rule ->
        match acc, Rule.allowableEffects rule with
        | None, _
        | _, None -> None
        | Some effects, Some more -> Some(Set.union effects more))
      (Some Set.empty)

  /// Default instance policy: package/local-store access, clock, randomness,
  /// terminal I/O and spawning are allowed; filesystem, network, processes,
  /// native, and environment access require an explicit grant. Spawning is
  /// allowed because a spawned process runs under the spawner's own access
  /// and can do nothing the spawner could not.
  let defaultInstance : Policy =
    allowEffects (
      Set.ofList
        [ Effect.Effect.PackageRead
          Effect.Effect.TraceRead
          Effect.Effect.TraceWrite
          Effect.Effect.Clock
          Effect.Effect.Random
          Effect.Effect.Stdout
          Effect.Effect.Stdin
          Effect.Effect.Concurrency ]
    )

  /// True for the canonical allow-all policy. It changes no decision, so
  /// callers can skip adding it as a restriction.
  let isAllowAll (policy : Policy) : bool =
    List.isEmpty policy.deny
    && (match policy.allow with
        | [ Rule.All ] -> true
        | _ -> false)

  let denial (request : Request) (policy : Policy) : Option<PolicyDenial> =
    if policy.deny |> List.exists (fun rule -> Rule.denies rule request) then
      Some PolicyDenial.ExplicitlyDenied
    elif policy.allow |> List.exists (fun rule -> Rule.covers rule request) then
      None
    else
      Some PolicyDenial.NotAllowed

  let allows (request : Request) (policy : Policy) : bool =
    denial request policy |> Option.isNone

[<RequireQualifiedAccess>]
type Layer =
  | Instance
  | Run
  | Package of id : string
  /// The ceiling every package `val` is evaluated under: no effects at all. A value is
  /// evaluated once, when it is saved (`Execution.evaluateValue`), so an effect would freeze
  /// one machine's answer into the store. `name` is the value's, for the message.
  | Value of name : string

type Denial = { layer : Layer; reason : PolicyDenial }

[<RequireQualifiedAccess>]
type Decision =
  | Allowed
  | Denied of Denial

type private Restriction = { layer : Layer; policy : Policy }

/// Immutable grants and restrictions for one invocation.
type Access = private Access of List<Restriction>

module Access =
  /// Host transport preserves every existing restriction across a process boundary.
  let layers (Access restrictions) : List<Layer * Policy> =
    restrictions
    |> List.map (fun restriction -> restriction.layer, restriction.policy)

  let ofLayers (layers : List<Layer * Policy>) : Access =
    match layers with
    | [] -> Access [ { layer = Layer.Instance; policy = Policy.denyAll } ]
    | _ ->
      Access(
        List.map (fun (layer, policy) -> { layer = layer; policy = policy }) layers
      )

  let start (instancePolicy : Policy) : Access =
    Access [ { layer = Layer.Instance; policy = instancePolicy } ]

  /// The fail-safe starting point, shared because `Access` is immutable.
  /// Pooled VMs reset their root frame to this value on every reuse, avoiding
  /// a new restriction list for each execution.
  let denyAll : Access =
    Access
      [ { layer = Layer.Instance; policy = Policy.denyAll }
        { layer = Layer.Run; policy = Policy.denyAll } ]

  let restrict
    (layer : Layer)
    (policy : Policy)
    (Access restrictions as access)
    : Access =
    // Appending allow-all cannot change any decision (and an allow-all layer
    // can never be the denying layer in diagnostics), so skip the allocation.
    if Policy.isAllowAll policy then
      access
    else
      Access({ layer = layer; policy = policy } :: restrictions)

  /// Host-supplied access for this run only. Callers must authorize the user
  /// gesture before calling this; guest code never receives this operation.
  let grantRun (grant : Policy) (Access restrictions) : Access =
    let allows, denies = Policy.rules grant
    Access(
      restrictions
      |> List.map (fun r ->
        match r.layer with
        | Layer.Instance ->
          let defaults, bans = Policy.rules r.policy
          { r with policy = Policy.create (defaults @ allows) (bans @ denies) }
        | _ -> r)
    )

  /// Establish one named invocation. Defaults and the approval supply grants;
  /// explicit instance denies and run restrictions still win. A nested call
  /// cannot introduce another approval.
  let enter
    (name : string)
    (approval : Policy)
    (Access restrictions as access)
    : Access =
    let entered =
      restrictions
      |> List.exists (fun r ->
        match r.layer with
        | Layer.Package _ -> true
        | _ -> false)
    if entered then
      access
    else
      let grants, denied = Policy.rules approval
      let restrictions =
        restrictions
        |> List.collect (fun r ->
          match r.layer with
          | Layer.Instance ->
            let defaults, bans = Policy.rules r.policy
            [ { layer = Layer.Instance; policy = Policy.create [ Rule.All ] bans }
              { layer = Layer.Package name
                policy = Policy.create (defaults @ grants) denied } ]
          | _ -> [ r ])
      Access restrictions

  /// Apply every restriction from another Access. This is used when invoking
  /// a closure: both the caller and the captured access must allow the request.
  let constrainBy (Access constraints) (Access restrictions as current) : Access =
    // A closure applied in the frame that created it carries the identical
    // Access; the conjunction is then the value itself — skip the append.
    if System.Object.ReferenceEquals(constraints, restrictions) then
      current
    else
      Access(constraints @ restrictions)

  /// Interactive approval may fill missing grants, never lift a deny or a
  /// mandatory run/value restriction (including serialized captured access).
  let canApprove request (Access restrictions) : bool =
    restrictions
    |> List.forall (fun r ->
      match Policy.denial request r.policy with
      | None -> true
      | Some PolicyDenial.ExplicitlyDenied -> false
      | Some PolicyDenial.NotAllowed ->
        match r.layer with
        | Layer.Instance
        | Layer.Package _ -> true
        | Layer.Run
        | Layer.Value _ -> false)

  let approve (grant : Policy) (Access restrictions) : Access =
    let added, _ = Policy.rules grant
    Access(
      restrictions
      |> List.map (fun r ->
        match r.layer with
        | Layer.Instance
        | Layer.Package _ ->
          let allow, deny = Policy.rules r.policy
          { r with policy = Policy.create (allow @ added) deny }
        | Layer.Run
        | Layer.Value _ -> r)
    )

  /// Every active restriction must allow the operation. An instance denial
  /// takes precedence in diagnostics.
  let decide (request : Request) (Access restrictions) : Option<Denial> =
    let rec walk first remaining =
      match remaining with
      | [] -> first
      | restriction :: rest ->
        match Policy.denial request restriction.policy with
        | None -> walk first rest
        | Some reason ->
          let denial = { layer = restriction.layer; reason = reason }
          if restriction.layer = Layer.Instance then
            Some denial
          else
            walk (first |> Option.orElse (Some denial)) rest
    walk None restrictions

  let check (request : Request) (access : Access) : Decision =
    match decide request access with
    | None -> Decision.Allowed
    | Some denial -> Decision.Denied denial

  let allows (request : Request) (access : Access) : bool =
    check request access = Decision.Allowed
