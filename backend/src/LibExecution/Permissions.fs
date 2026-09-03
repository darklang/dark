/// Runtime permissions.
///
/// A Request is one exact host operation. A Rule may cover requests. A Policy
/// is an allowlist with optional explicit denies. Access is the immutable
/// conjunction of the instance, run, package, and function policies currently
/// constraining execution.
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
  | HttpServer of port : int
  | File of access : AccessKind * path : string
  | Env of access : AccessKind * name : string
  | EnvList
  | Db of access : AccessKind * name : string
  | DbList
  | Stdin
  | Stdout
  | Clock
  | Random
  | Process of executable : string * args : List<string>
  /// The package and trace stores are host-owned wholes: there is no
  /// per-resource scoping, so these are ambient effects like `Stdout`.
  | Package of access : AccessKind
  | Trace of access : AccessKind
  | Native of operation : string

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
    | Request.Db(AccessKind.Read, _) -> Effect.Effect.DbRead
    | Request.Db(AccessKind.Write, _) -> Effect.Effect.DbWrite
    | Request.DbList -> Effect.Effect.DbRead
    | Request.Stdin -> Effect.Effect.Stdin
    | Request.Stdout -> Effect.Effect.Stdout
    | Request.Clock -> Effect.Effect.Clock
    | Request.Random -> Effect.Effect.Random
    | Request.Process _ -> Effect.Effect.Process
    | Request.Package AccessKind.Read -> Effect.Effect.PackageRead
    | Request.Package AccessKind.Write -> Effect.Effect.PackageWrite
    | Request.Trace AccessKind.Read -> Effect.Effect.TraceRead
    | Request.Trace AccessKind.Write -> Effect.Effect.TraceWrite
    | Request.Native _ -> Effect.Effect.Native

  /// Return the narrow `permissions allow <rule>` text that covers this
  /// request, or `None` when the effect has no scoped rule (such as Native).
  let suggestRule (request : Request) : Option<string> =
    match request with
    | Request.Http r ->
      let host =
        if r.host.Contains ':' && not (r.host.StartsWith "[") then
          $"[{r.host}]"
        else
          r.host
      let url = $"{r.scheme}://{host}:{r.port}{r.path}{r.query}"
      Some $"http {r.method} {quoteRuleToken url}"
    | Request.HttpServer port -> Some $"http-server {port}"
    | Request.File(AccessKind.Read, path) -> Some $"file read {quoteRuleToken path}"
    | Request.File(AccessKind.Write, path) ->
      Some $"file write {quoteRuleToken path}"
    | Request.Env(AccessKind.Read, name) -> Some $"env read {quoteRuleToken name}"
    | Request.Env(AccessKind.Write, name) -> Some $"env write {quoteRuleToken name}"
    | Request.EnvList -> Some "env read"
    | Request.Db(AccessKind.Read, name) -> Some $"db read {quoteRuleToken name}"
    | Request.Db(AccessKind.Write, name) -> Some $"db write {quoteRuleToken name}"
    | Request.DbList -> Some "db read"
    | Request.Stdin -> Some "stdin"
    | Request.Stdout -> Some "stdout"
    | Request.Clock -> Some "clock"
    | Request.Random -> Some "random"
    | Request.Process(executable, _) -> Some $"process {quoteRuleToken executable}"
    | Request.Package AccessKind.Read -> Some "package-read"
    | Request.Package AccessKind.Write -> Some "package-write"
    | Request.Trace AccessKind.Read -> Some "trace-read"
    | Request.Trace AccessKind.Write -> Some "trace-write"
    | Request.Native _ -> None

  let httpServer (port : int) : Result<Request, string> =
    if port >= 0 && port <= 65535 then
      Ok(Request.HttpServer port)
    else
      Error $"Invalid HTTP server port: {port}"

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
            host = uri.IdnHost.ToLowerInvariant().TrimEnd('.')
            port = uri.Port
            path = if uri.AbsolutePath = "" then "/" else uri.AbsolutePath
            query = uri.Query }
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

  let db (access : AccessKind) (name : string) : Result<Request, string> =
    if System.String.IsNullOrWhiteSpace name then
      Error "Database name cannot be empty"
    else
      Ok(Request.Db(access, name))

  let dbList : Request = Request.DbList

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

type HttpRule =
  {
    method : Scope<string>
    scheme : Scope<string>
    host : HostRule
    port : Scope<int>
    pathPrefix : Scope<string>
    /// Exact query strings this rule covers. `Scope.All` (the default when a
    /// rule is written without a query) matches any query, so an unspecified
    /// rule keeps the intuitive "any query on this path" behavior; a rule
    /// written with a query covers only that exact query.
    query : Scope<string>
  }

/// `args` is the exact argument list, in order, or any.
type ProcessRule = { executable : Scope<string>; args : Scope<List<string>> }

/// Patterns used by both allow and deny lists.
[<RequireQualifiedAccess>]
type Rule =
  | All
  | Effect of Effect.Effect
  | Http of HttpRule
  | HttpServer of Scope<int>
  | File of access : AccessKind * roots : Scope<string>
  | Env of access : AccessKind * names : Scope<string>
  | Db of access : AccessKind * names : Scope<string>
  | Process of ProcessRule

module Rule =
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
      && Scope.covers urlPathUnder rule.pathPrefix request.path
      && Scope.covers same rule.query request.query
    | Rule.HttpServer ports, Request.HttpServer port -> Scope.covers same ports port
    | Rule.File(expected, roots), Request.File(actual, path) when expected = actual ->
      Scope.covers pathUnder roots path
    | Rule.Env(expected, names), Request.Env(actual, name) when expected = actual ->
      Scope.covers envNameMatches names name
    | Rule.Env(AccessKind.Read, Scope.All), Request.EnvList -> true
    | Rule.Db(expected, names), Request.Db(actual, name) when expected = actual ->
      Scope.covers same names name
    | Rule.Db(AccessKind.Read, Scope.All), Request.DbList -> true
    | Rule.Process rule, Request.Process(executable, args) ->
      Scope.covers samePath rule.executable executable
      && Scope.covers same rule.args args
    | _ -> false

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
    | Rule.Db(AccessKind.Read, _) -> Some(Set.singleton Effect.Effect.DbRead)
    | Rule.Db(AccessKind.Write, _) -> Some(Set.singleton Effect.Effect.DbWrite)
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
    { allow = List.distinct allow; deny = List.distinct deny }

  /// A broad static-effect ceiling. Exact resource policies are still
  /// intersected at the instance/run/package layers.
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
  /// and terminal I/O are allowed; filesystem, network, processes, native,
  /// and environment access require an explicit grant.
  let defaultInstance : Policy =
    allowEffects (
      Set.ofList
        [ Effect.Effect.PackageRead
          Effect.Effect.DbRead
          Effect.Effect.DbWrite
          Effect.Effect.TraceRead
          Effect.Effect.TraceWrite
          Effect.Effect.Clock
          Effect.Effect.Random
          Effect.Effect.Stdout
          Effect.Effect.Stdin ]
    )

  /// True for the canonical allow-all policy. It changes no decision, so
  /// callers can skip adding it as a restriction.
  let isAllowAll (policy : Policy) : bool =
    List.isEmpty policy.deny
    && (match policy.allow with
        | [ Rule.All ] -> true
        | _ -> false)

  let denial (request : Request) (policy : Policy) : Option<PolicyDenial> =
    if policy.deny |> List.exists (fun rule -> Rule.covers rule request) then
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
  | Function of id : string

type Denial = { layer : Layer; reason : PolicyDenial }

[<RequireQualifiedAccess>]
type Decision =
  | Allowed
  | Denied of Denial

/// Controls whether caller-owned run/package denials are recorded and allowed
/// (`--warn-permissions`) or enforced. Instance and function policies never
/// relax.
type Relax =
  | NoRelax
  | Relax of record : (string -> Layer -> PolicyDenial -> unit)

type private Restriction = { layer : Layer; policy : Policy }

/// Immutable conjunction of policies; restrictions can only be added.
type Access = private Access of List<Restriction>

module Access =
  let start (instancePolicy : Policy) : Access =
    Access [ { layer = Layer.Instance; policy = instancePolicy } ]

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

  /// Apply every restriction from another Access. This is used when invoking
  /// a closure: both the caller and the captured access must allow the request.
  let constrainBy (Access constraints) (Access restrictions as current) : Access =
    // A closure applied in the frame that created it carries the identical
    // Access; the conjunction is then the value itself — skip the append.
    if System.Object.ReferenceEquals(constraints, restrictions) then
      current
    else
      Access(constraints @ restrictions)

  /// The instance layer is the operator's hard maximum. If *any* layer that
  /// denies is the instance, report that denial — never an inner one — so
  /// audit/warn mode (which may relax only caller-owned layers) can never
  /// proceed past an instance denial that an inner caller-owned denial would
  /// otherwise mask. Otherwise report the first (innermost) denial. Runs on
  /// every gated builtin call, so it allocates nothing on the allowed path.
  let check (request : Request) (Access restrictions) : Decision =
    let restrictions : List<Restriction> = restrictions
    match restrictions with
    | [ only ] when Policy.isAllowAll only.policy -> Decision.Allowed
    | _ ->
      let mutable first = ValueNone
      let mutable remaining = restrictions
      while not (List.isEmpty remaining) do
        match remaining with
        | [] -> ()
        | restriction :: rest ->
          remaining <- rest
          match Policy.denial request restriction.policy with
          | None -> ()
          | Some reason ->
            let denial : Denial = { layer = restriction.layer; reason = reason }
            if restriction.layer = Layer.Instance then
              first <- ValueSome denial
              remaining <- []
            elif first.IsNone then
              first <- ValueSome denial
      match first with
      | ValueNone -> Decision.Allowed
      | ValueSome denial -> Decision.Denied denial

  let allows (request : Request) (access : Access) : bool =
    check request access = Decision.Allowed

  /// Which layers `Relax` may waive: only the ones the invoker owns.
  let private isCallerOwned (layer : Layer) : bool =
    match layer with
    | Layer.Run
    | Layer.Package _ -> true
    | Layer.Instance
    | Layer.Function _ -> false

  /// Check and apply audit mode in one place: a denial from a caller-owned
  /// layer is recorded (with the resource description, rendered only then)
  /// and waived; the instance policy and function ceilings are never relaxed.
  /// Returns the denial that stands, if any.
  let decide
    (relax : Relax)
    (resource : unit -> string)
    (request : Request)
    (access : Access)
    : Option<Denial> =
    match check request access with
    | Decision.Allowed -> None
    | Decision.Denied denial ->
      match relax with
      | Relax record when isCallerOwned denial.layer ->
        record (resource ()) denial.layer denial.reason
        None
      | _ -> Some denial
