/// Capabilities — the resource-domain axis used to GATE effectful builtins.
///
/// "This code may do exactly these effects." Both a GRANT (the instance's configured allowances) and a
/// NEED (what a fn requires) are ONE `Capabilities` record — a field per domain, each holding that
/// domain's nuance (the http method/host scopes, the file read-scope × write-scope, …). A grant covers
/// a need iff it covers it field-by-field. Modelling it as a record (not a list of cases) means: a
/// single well-defined configured state, natural JSON, and per-direction scopes for free — "read
/// anywhere, write only in ~/.darklang" is just `file = { read = Any; write = Only {…} }`.
///
/// This is the resource-domain axis (for gating) — ORTHOGONAL to the concurrency-character axis (a
/// separate `effects` field: Pure / AsyncRead / …, used for scheduling).
///
/// RT-independent (only Prelude) so it compiles before RuntimeTypes.fs.
///
/// TODO: `Capabilities` is arguably a LANGUAGE-LEVEL type — low-level, defined in the context of
/// builtins, and meant to be stable — so it should probably live in ProgramTypes (or RuntimeTypes)
/// rather than its own module here. Moving it would let it share the PT/RT binary-serialization +
/// hashing machinery (its disk serializer in `LibSerialization/Binary/Serializers/Capabilities.fs`
/// would move under the matching PT/RT folder). Deferred — revisit when shuffling these namespaces.
module LibExecution.Capabilities

open Prelude

/// A scope over targets: `Any` (the ⊤ — "anywhere") or a specific allow-list. `Only Set.empty` is the
/// bottom (nothing). A GRANT field says what's allowed; a NEED field says what's required.
type Scope<'a when 'a : comparison> =
  | Any
  | Only of Set<'a>

/// A read-scope and a write-scope for one domain (files, env, db) — they can differ.
type RW<'a when 'a : comparison> = { read : Scope<'a>; write : Scope<'a> }

/// A structured host matcher — deliberately NOT regex (auditability + no host-bypass/ReDoS footguns).
type HostMatch =
  | AnyHost
  | ExactHost of string // "api.github.com"
  | Subdomain of string // "github.com" matches "github.com" and any "*.github.com"

/// A URL capability: every dimension scoped INDEPENDENTLY and EXPLICITLY. There is no "empty implies
/// any" — an undetermined dimension is a deliberate value (`Any` / `[ AnyHost ]`), never an accident.
type UrlScope =
  { schemes : Scope<string> // Only {"https"} | Any
    hosts : List<HostMatch> // OR-list; [ AnyHost ] = any host; [] = no host (bottom)
    ports : Scope<int64> // Only {443L} | Any
    paths : Scope<string> } // prefix-matched (pathUnder); Any = any path

/// One http-client rule: these methods to this URL scope. The http grant/need is a LIST of rules, so
/// method and URL COUPLE per rule — "any method to localhost AND GET to everywhere else" is two rules.
/// A request (method, scheme, host, port, path) is allowed iff some rule covers it.
type HttpRule = { methods : Scope<string>; url : UrlScope }

/// One exec rule: these programs with these args. Like httpClient, exec is a LIST of rules so program
/// and args COUPLE per rule — "git with any args, but rm only with --dry-run" is two rules. A launch
/// (program, args) is allowed iff some rule covers it: the program is in scope AND every arg is in scope.
type ExecRule = { programs : Scope<string>; args : Scope<string> }

/// The full per-instance capability shape — a field per domain.
type Capabilities =
  { httpClient : List<HttpRule>
    httpServer : Scope<int64> // which ports may bind
    file : RW<string> // read paths × write paths (prefix-scoped)
    env : RW<string> // read vars × write vars
    db : RW<string> // read tables × write tables
    exec : List<ExecRule> // (program × args) rules — a launch matching SOME rule is allowed
    stdout : bool
    stdin : bool
    random : bool
    clock : bool
    llm : bool } // the AI opt-in — denied by default


// ───────────────────────────────────────────────────────────────────────────
// Construction
// ───────────────────────────────────────────────────────────────────────────

let private noneScope<'a when 'a : comparison> : Scope<'a> = Only Set.empty
let private noRW<'a when 'a : comparison> : RW<'a> =
  { read = noneScope; write = noneScope }

/// The fully-permissive URL scope (any scheme/host/port/path) — used by `allCaps` and as the maximal
/// NEED for a name-based http need (only an unrestricted http grant covers it).
let anyUrl : UrlScope =
  { schemes = Any; hosts = [ AnyHost ]; ports = Any; paths = Any }

/// The strict default — grants nothing. Pure code (a need that's all-empty) always passes.
let noCaps : Capabilities =
  { httpClient = [] // no rules ⇒ no http allowed
    httpServer = noneScope
    file = noRW
    env = noRW
    db = noRW
    exec = [] // no rules ⇒ no external programs may run
    stdout = false
    stdin = false
    random = false
    clock = false
    llm = false }

/// A permissive grant — everything, unscoped. The rollout default, so wiring the gate changes no
/// existing behavior; a real instance narrows it.
let allCaps : Capabilities =
  { httpClient = [ { methods = Any; url = anyUrl } ]
    httpServer = Any
    file = { read = Any; write = Any }
    env = { read = Any; write = Any }
    db = { read = Any; write = Any }
    exec = [ { programs = Any; args = Any } ]
    stdout = true
    stdin = true
    random = true
    clock = true
    llm = true }

/// Builtin need constructors — what a builtin declares as its `capabilities` (pure ⇒ `noCaps`). The
/// rich-domain needs (http/file/exec/…) are coarse: the gate checks presence, the builtin body narrows.
module Needs =
  let http : Capabilities =
    { noCaps with httpClient = [ { methods = Any; url = anyUrl } ] }
  let httpServer : Capabilities = { noCaps with httpServer = Any }
  let fileRead : Capabilities =
    { noCaps with file = { read = Any; write = Only Set.empty } }
  let fileWrite : Capabilities =
    { noCaps with file = { read = Only Set.empty; write = Any } }
  let fileReadWrite : Capabilities =
    { noCaps with file = { read = Any; write = Any } }
  let envRead : Capabilities =
    { noCaps with env = { read = Any; write = Only Set.empty } }
  let envWrite : Capabilities =
    { noCaps with env = { read = Only Set.empty; write = Any } }
  let dbRead : Capabilities =
    { noCaps with db = { read = Any; write = Only Set.empty } }
  let dbWrite : Capabilities =
    { noCaps with db = { read = Only Set.empty; write = Any } }
  let exec : Capabilities = { noCaps with exec = [ { programs = Any; args = Any } ] }
  let clock : Capabilities = { noCaps with clock = true }
  let random : Capabilities = { noCaps with random = true }
  let stdout : Capabilities = { noCaps with stdout = true }
  let stdin : Capabilities = { noCaps with stdin = true }
  let llm : Capabilities = { noCaps with llm = true }


// ───────────────────────────────────────────────────────────────────────────
// Matchers
// ───────────────────────────────────────────────────────────────────────────

/// Does a granted host matcher cover a needed one? Needs from the gate are always concrete
/// (`ExactHost`); `AnyHost` on the need side (an undetermined host) is covered only by an `AnyHost`
/// grant — fail-closed, but EXPLICIT (never an empty-string accident).
let hostMatchCovers (grant : HostMatch) (need : HostMatch) : bool =
  match grant, need with
  | AnyHost, _ -> true
  | _, AnyHost -> false // needing "any host" requires an AnyHost grant
  | ExactHost g, ExactHost n -> g = n
  | Subdomain g, ExactHost n -> n = g || n.EndsWith("." + g)
  | Subdomain g, Subdomain n -> n = g || n.EndsWith("." + g)
  | ExactHost _, Subdomain _ -> false

/// Every needed host matched by SOME granted matcher.
let hostsCover (grant : List<HostMatch>) (need : List<HostMatch>) : bool =
  need
  |> List.forall (fun n -> grant |> List.exists (fun g -> hostMatchCovers g n))

/// Path scope match: a grant prefix covers a path only at or BELOW a directory boundary — `/a` covers
/// `/a` and `/a/b`, but NOT the sibling `/ab` or `/a-evil`. A trailing `/` on the grant is fine
/// (`~/.darklang/` covers `~/.darklang/x`).
let pathUnder (prefix : string) (path : string) : bool =
  if path = prefix then
    true
  else
    let boundary = if prefix.EndsWith "/" then prefix else prefix + "/"
    path.StartsWith boundary

let private exact (a : 'a) (b : 'a) : bool = a = b


// ───────────────────────────────────────────────────────────────────────────
// Subsumption — does the grant cover the need?
// ───────────────────────────────────────────────────────────────────────────

/// Scope subsumption (`need ⊑ grant`) under a per-element matcher.
let scopeCovers
  (matcher : 'a -> 'a -> bool)
  (grant : Scope<'a>)
  (need : Scope<'a>)
  : bool =
  match grant, need with
  | Any, _ -> true
  | Only _, Any -> false // "needs any" is covered only by "grants any"
  | Only g, Only n ->
    n |> Set.forall (fun x -> g |> Set.exists (fun p -> matcher p x))

/// The gate verdict. `Denied` names the first unmet aspect so the error is actionable.
type CapDecision =
  | Allowed
  | Denied of string

/// Does a granted URL scope cover a needed one, dimension by dimension (scheme AND host AND port AND
/// path)? Needs from the gate carry concrete singletons.
let urlCovers (grant : UrlScope) (need : UrlScope) : bool =
  scopeCovers exact grant.schemes need.schemes
  && hostsCover grant.hosts need.hosts
  && scopeCovers exact grant.ports need.ports
  && scopeCovers pathUnder grant.paths need.paths

/// http: every NEEDED rule must be covered by SOME granted rule (method AND the whole URL scope). This
/// is the "list of rules, OR'd" coupling — so "any method to localhost" + "GET everywhere" composes.
let private httpCovers (grant : List<HttpRule>) (need : List<HttpRule>) : bool =
  need
  |> List.forall (fun n ->
    grant
    |> List.exists (fun g ->
      scopeCovers exact g.methods n.methods && urlCovers g.url n.url))

/// exec: same coupling as http — every NEEDED rule must be covered by SOME granted rule (program AND
/// args both covered). So "git any-args" + "rm --dry-run-only" composes as two rules.
let private execCovers (grant : List<ExecRule>) (need : List<ExecRule>) : bool =
  need
  |> List.forall (fun n ->
    grant
    |> List.exists (fun g ->
      scopeCovers exact g.programs n.programs && scopeCovers exact g.args n.args))

/// Does `grant` cover `need`, field by field? Returns the first unmet aspect as `Denied "<desc>"`.
let covers (grant : Capabilities) (need : Capabilities) : CapDecision =
  let rw matcher (g : RW<'a>) (n : RW<'a>) =
    scopeCovers matcher g.read n.read && scopeCovers matcher g.write n.write
  let needsBool g n = (not n) || g // if needed, must be granted
  if not (httpCovers grant.httpClient need.httpClient) then
    Denied "http-client (a method/scheme/host/port/path it doesn't allow)"
  elif not (scopeCovers exact grant.httpServer need.httpServer) then
    Denied "http-server (a port)"
  elif not (rw pathUnder grant.file need.file) then
    Denied "file (a path/access it doesn't allow)"
  elif not (rw exact grant.env need.env) then
    Denied "env"
  elif not (rw exact grant.db need.db) then
    Denied "db"
  elif not (execCovers grant.exec need.exec) then
    Denied "exec (a program/args it doesn't allow)"
  elif not (needsBool grant.stdout need.stdout) then
    Denied "stdout"
  elif not (needsBool grant.stdin need.stdin) then
    Denied "stdin"
  elif not (needsBool grant.random need.random) then
    Denied "random"
  elif not (needsBool grant.clock need.clock) then
    Denied "clock"
  elif not (needsBool grant.llm need.llm) then
    Denied "llm"
  else
    Allowed


/// The GATE check: does the grant allow this builtin's domain AT ALL? Presence-only for the rich domains
/// (http/file/env/db/exec) — the builtin's body then enforces the specific URL/path/args via `covers`.
/// Bools are exact. So a scoped grant (e.g. "http GET api.x") passes the gate and the body decides the
/// concrete call, while a grant with no http at all is denied here before the body runs.
let coversStructurally (grant : Capabilities) (need : Capabilities) : CapDecision =
  let present (s : Scope<'a>) =
    match s with
    | Any -> true
    | Only xs -> not (Set.isEmpty xs)
  // (does the need touch this domain?, does the grant provide any presence?, label)
  let checks : List<bool * bool * string> =
    [ not (List.isEmpty need.httpClient),
      not (List.isEmpty grant.httpClient),
      "http-client"
      present need.httpServer, present grant.httpServer, "http-server"
      present need.file.read, present grant.file.read, "file (read)"
      present need.file.write, present grant.file.write, "file (write)"
      present need.env.read, present grant.env.read, "env (read)"
      present need.env.write, present grant.env.write, "env (write)"
      present need.db.read, present grant.db.read, "db (read)"
      present need.db.write, present grant.db.write, "db (write)"
      not (List.isEmpty need.exec), not (List.isEmpty grant.exec), "exec"
      need.stdout, grant.stdout, "stdout"
      need.stdin, grant.stdin, "stdin"
      need.random, grant.random, "random"
      need.clock, grant.clock, "clock"
      need.llm, grant.llm, "llm" ]
  match checks |> List.tryFind (fun (n, g, _) -> n && not g) with
  | Some(_, _, label) -> Denied label
  | None -> Allowed


// ───────────────────────────────────────────────────────────────────────────
// Join — union two capability records field-wise (for the analysis fold)
// ───────────────────────────────────────────────────────────────────────────

let joinScope (a : Scope<'x>) (b : Scope<'x>) : Scope<'x> =
  match a, b with
  | Any, _
  | _, Any -> Any
  | Only x, Only y -> Only(Set.union x y)

let private joinRW (a : RW<'x>) (b : RW<'x>) : RW<'x> =
  { read = joinScope a.read b.read; write = joinScope a.write b.write }

/// Field-wise union of two capability records (used to fold a fn's effective needs over its call graph).
let join (a : Capabilities) (b : Capabilities) : Capabilities =
  { httpClient = List.append a.httpClient b.httpClient |> List.distinct
    httpServer = joinScope a.httpServer b.httpServer
    file = joinRW a.file b.file
    env = joinRW a.env b.env
    db = joinRW a.db b.db
    exec = List.append a.exec b.exec |> List.distinct
    stdout = a.stdout || b.stdout
    stdin = a.stdin || b.stdin
    random = a.random || b.random
    clock = a.clock || b.clock
    llm = a.llm || b.llm }

// The grant-spec LANGUAGE (parse/render of `http-client GET`, `file write ~/x`, …) lives entirely in
// `.dark` (`LanguageTools.Capabilities`) — F# only ever deals with this structured model. The F#/Dark
// boundary is `CapabilitiesToDarkTypes`.
