/// Tests for the nuanced capability model.
///
/// Pure — no ExecutionState. Covers the field-wise `covers` gate (subsumption), the structural presence
/// gate (`coversStructurally`), the host/path matchers + scope subsumption, the `join` fold, the binary
/// blob round-trip, and the `fnRefs` PT-walk. (The grant-spec LANGUAGE — parse/render — now lives in
/// `.dark`'s `LanguageTools.Capabilities`, tested there.)
module Tests.Capabilities

open Expecto

open Prelude

module Cap = LibExecution.Capabilities
module PT = LibExecution.ProgramTypes
module Analysis = LibExecution.CapabilityAnalysis
module Grants = LibDB.CapabilityGrants

let private only (xs : string list) : Cap.Scope<string> = Cap.Only(Set.ofList xs)
let private isAllowed (g : Cap.Capabilities) (n : Cap.Capabilities) =
  Cap.covers g n = Cap.Allowed

let private isDenied (g : Cap.Capabilities) (n : Cap.Capabilities) =
  match Cap.covers g n with
  | Cap.Denied _ -> true
  | _ -> false

/// A concrete http-request NEED: `METHOD scheme://host:port/path` (what the gate builds from a call).
let private req
  (method : string)
  (scheme : string)
  (host : string)
  (port : int64)
  (path : string)
  : Cap.Capabilities =
  { Cap.noCaps with
      httpClient =
        [ { methods = Cap.Only(Set.singleton method)
            url =
              { schemes = Cap.Only(Set.singleton scheme)
                hosts = [ Cap.ExactHost host ]
                ports = Cap.Only(Set.singleton port)
                paths = Cap.Only(Set.singleton path) } } ] }

/// Build grants STRUCTURALLY — the grant-spec LANGUAGE now lives in `.dark`
/// (`LanguageTools.Capabilities.parse`), so F# tests construct the model directly.
let private port (n : int64) : Cap.Scope<int64> = Cap.Only(Set.singleton n)

let private urlS
  (schemes : Cap.Scope<string>)
  (hosts : List<Cap.HostMatch>)
  (ports : Cap.Scope<int64>)
  (paths : Cap.Scope<string>)
  : Cap.UrlScope =
  { schemes = schemes; hosts = hosts; ports = ports; paths = paths }

let private httpGrant
  (methods : Cap.Scope<string>)
  (url : Cap.UrlScope)
  : Cap.Capabilities =
  { Cap.noCaps with httpClient = [ { methods = methods; url = url } ] }

/// Build a exec grant/need from an explicit list of (programs, args) rules.
let private execRules
  (rules : List<Cap.Scope<string> * Cap.Scope<string>>)
  : Cap.Capabilities =
  { Cap.noCaps with
      exec = rules |> List.map (fun (p, a) -> { programs = p; args = a }) }

/// A exec NEED for one concrete launch: program `p` with args `argv`.
let private needExec (p : string) (argv : List<string>) : Cap.Capabilities =
  execRules [ (only [ p ], Cap.Only(Set.ofList argv)) ]


// ─────────────────────────────────────────────────────────────────────
// covers — field-wise subsumption (the gate)
// ─────────────────────────────────────────────────────────────────────

let noCapsDeniesEffectful =
  test "covers: noCaps denies any effectful need (the strict default blocks)" {
    Expect.isTrue
      (isDenied Cap.noCaps { Cap.noCaps with clock = true })
      "clock denied under noCaps"
    Expect.equal
      (Cap.covers Cap.noCaps Cap.noCaps)
      Cap.Allowed
      "a pure need (noCaps) always passes — the fast-path"
  }

let allCapsGrantsEverything =
  test "covers: allCaps grants every need" {
    Expect.isTrue
      (isAllowed Cap.allCaps (req "POST" "https" "api.x" 443L "/"))
      "http allowed"
    Expect.isTrue
      (isAllowed Cap.allCaps { Cap.noCaps with random = true })
      "random allowed"
  }

let getOnlyCoversGetDeniesPost =
  test "covers: a `GET-only` grant covers a GET need but denies a POST" {
    let grant = httpGrant (only [ "GET" ]) Cap.anyUrl // any URL, GET only
    Expect.isTrue
      (isAllowed grant (req "GET" "https" "api.github.com" 443L "/x"))
      "GET passes"
    Expect.isTrue
      (isDenied grant (req "POST" "https" "api.x" 443L "/"))
      "POST denied"
  }

let hostScopedGrant =
  test "covers: a host-scoped grant covers that host, denies others" {
    let grant =
      httpGrant
        Cap.Any
        (urlS
          (only [ "https" ])
          [ Cap.ExactHost "api.github.com" ]
          (port 443L)
          Cap.Any)
    Expect.isTrue
      (isAllowed grant (req "GET" "https" "api.github.com" 443L "/x"))
      "in-scope host"
    Expect.isTrue
      (isDenied grant (req "GET" "https" "evil.com" 443L "/"))
      "out-of-scope host"
  }

let schemePortPathScoping =
  test "covers: scheme/port/path are each enforced" {
    // https://api.x/v3  (omitted scheme ⇒ https, omitted port ⇒ 443, path-prefix /v3)
    let grant =
      httpGrant
        (only [ "GET" ])
        (urlS
          (only [ "https" ])
          [ Cap.ExactHost "api.x" ]
          (port 443L)
          (only [ "/v3" ]))
    Expect.isTrue
      (isAllowed grant (req "GET" "https" "api.x" 443L "/v3/users"))
      "in-scope path ok"
    Expect.isTrue
      (isDenied grant (req "GET" "https" "api.x" 443L "/admin"))
      "out-of-scope path denied"
    Expect.isTrue
      (isDenied grant (req "GET" "http" "api.x" 80L "/v3"))
      "plain http denied (https only)"
    Expect.isTrue
      (isDenied grant (req "GET" "https" "api.x" 8443L "/v3"))
      "non-default port denied"
    // explicit any-scheme + any-port
    let loose =
      httpGrant
        (only [ "GET" ])
        (urlS Cap.Any [ Cap.ExactHost "api.x" ] Cap.Any Cap.Any)
    Expect.isTrue
      (isAllowed loose (req "GET" "http" "api.x" 8080L "/"))
      "any scheme + any port ok"
  }

let subdomainScoping =
  test
    "covers: a `*.github.com` grant covers subdomains + the bare domain, not look-alikes" {
    let grant =
      httpGrant
        (only [ "GET" ])
        (urlS (only [ "https" ]) [ Cap.Subdomain "github.com" ] (port 443L) Cap.Any)
    Expect.isTrue
      (isAllowed grant (req "GET" "https" "api.github.com" 443L "/"))
      "subdomain ok"
    Expect.isTrue
      (isAllowed grant (req "GET" "https" "github.com" 443L "/"))
      "bare domain ok"
    Expect.isTrue
      (isDenied grant (req "GET" "https" "github.com.evil.com" 443L "/"))
      "look-alike suffix denied"
  }

let perHostCoupling =
  // the headline for the rule LIST: "any method to localhost, AND GET-only everywhere else".
  // method and URL couple per-rule, and the request must match SOME rule.
  test
    "covers: coupled rules — any-method@localhost + GET-anywhere passes/denies correctly" {
    let grant =
      Cap.join
        (httpGrant
          Cap.Any
          (urlS (only [ "https" ]) [ Cap.ExactHost "localhost" ] (port 443L) Cap.Any)) // any method, https://localhost
        (httpGrant (only [ "GET" ]) Cap.anyUrl) // GET anywhere
    // POST to localhost — covered by rule 1
    Expect.isTrue
      (isAllowed grant (req "POST" "https" "localhost" 443L "/"))
      "POST→localhost ok"
    // GET to an external host — covered by rule 2
    Expect.isTrue
      (isAllowed grant (req "GET" "https" "api.github.com" 443L "/x"))
      "GET→external ok"
    // POST to an external host — covered by NEITHER rule
    Expect.isTrue
      (isDenied grant (req "POST" "https" "api.github.com" 443L "/"))
      "POST→external denied"
  }

let execCoupling =
  // the same coupled-rule pattern for exec: "git with any args, but rm only with --dry-run".
  test "covers: coupled exec rules — program × args couple per rule" {
    let grant =
      execRules
        [ (only [ "git" ], Cap.Any) // git with any args
          (only [ "rm" ], only [ "--dry-run" ]) ] // rm only with --dry-run
    Expect.isTrue
      (isAllowed grant (needExec "git" [ "commit"; "-m"; "x" ]))
      "git any-args ok"
    Expect.isTrue (isAllowed grant (needExec "rm" [ "--dry-run" ])) "rm --dry-run ok"
    Expect.isTrue
      (isAllowed grant (needExec "rm" []))
      "rm no-args ok (empty ⊑ allowlist)"
    Expect.isTrue
      (isDenied grant (needExec "rm" [ "-rf"; "/" ]))
      "rm -rf denied (arg not in allowlist)"
    Expect.isTrue
      (isDenied grant (needExec "curl" []))
      "an ungranted program is denied"
  }

let fileReadAnyWriteHere =
  // the headline: read anywhere, write only inside ~/.darklang  — one record, two scopes
  test
    "covers: file read=any, write=only-dir — reads pass anywhere, writes only inside the dir" {
    let grant =
      { Cap.noCaps with file = { read = Cap.Any; write = only [ "~/.darklang/" ] } }
    let needReadAnywhere =
      { Cap.noCaps with file = { read = Cap.Any; write = Cap.Only Set.empty } }
    let needWriteInside =
      { Cap.noCaps with
          file = { read = Cap.Only Set.empty; write = only [ "~/.darklang/x" ] } }
    let needWriteOutside =
      { Cap.noCaps with
          file = { read = Cap.Only Set.empty; write = only [ "/etc/passwd" ] } }
    Expect.isTrue (isAllowed grant needReadAnywhere) "read anywhere allowed"
    Expect.isTrue
      (isAllowed grant needWriteInside)
      "write inside the dir allowed (prefix)"
    Expect.isTrue (isDenied grant needWriteOutside) "write outside the dir denied"
  }


// ─────────────────────────────────────────────────────────────────────
// matchers + scope subsumption
// ─────────────────────────────────────────────────────────────────────

let hostMatching =
  test "hostMatchCovers: AnyHost, ExactHost, Subdomain (subdomain + bare)" {
    let exact = Cap.ExactHost "api.github.com"
    Expect.isTrue
      (Cap.hostMatchCovers exact (Cap.ExactHost "api.github.com"))
      "exact match"
    Expect.isFalse
      (Cap.hostMatchCovers exact (Cap.ExactHost "evil.com"))
      "exact non-match"
    let sub = Cap.Subdomain "github.com"
    Expect.isTrue
      (Cap.hostMatchCovers sub (Cap.ExactHost "api.github.com"))
      "subdomain covered"
    Expect.isTrue
      (Cap.hostMatchCovers sub (Cap.ExactHost "github.com"))
      "bare domain covered"
    Expect.isFalse
      (Cap.hostMatchCovers sub (Cap.ExactHost "github.com.evil.com"))
      "look-alike suffix not covered"
    Expect.isTrue
      (Cap.hostMatchCovers Cap.AnyHost (Cap.ExactHost "anything.example"))
      "AnyHost covers all"
    Expect.isFalse
      (Cap.hostMatchCovers exact Cap.AnyHost)
      "needing AnyHost requires AnyHost grant"
  }

let pathMatching =
  test "pathUnder: a grant prefix covers paths at/under it, on a directory boundary" {
    Expect.isTrue (Cap.pathUnder "/a" "/a") "exact prefix"
    Expect.isTrue (Cap.pathUnder "/a" "/a/b/c") "under the prefix"
    Expect.isFalse (Cap.pathUnder "/a" "/b") "outside the prefix"
    // boundary: a prefix must NOT cover a sibling that merely shares the string prefix
    Expect.isFalse
      (Cap.pathUnder "/a" "/ab")
      "sibling sharing the string prefix is NOT covered"
    Expect.isFalse
      (Cap.pathUnder "/home/safe" "/home/safe-evil/x")
      "no partial-segment match"
    // a trailing slash on the grant is fine
    Expect.isTrue
      (Cap.pathUnder "~/.darklang/" "~/.darklang/x")
      "trailing-slash prefix covers under it"
  }

let scopeSubsumption =
  test "scopeCovers: Any ⊒ anything; Only ⊒ subset; needing Any needs Any" {
    let m = (=)
    Expect.isTrue (Cap.scopeCovers m Cap.Any (only [ "x" ])) "Any covers a specific"
    Expect.isTrue
      (Cap.scopeCovers m (only [ "x"; "y" ]) (only [ "x" ]))
      "superset covers subset"
    Expect.isFalse
      (Cap.scopeCovers m (only [ "x" ]) (only [ "y" ]))
      "missing element denied"
    Expect.isFalse
      (Cap.scopeCovers m (only [ "x" ]) Cap.Any)
      "needing Any is only covered by Any"
  }


// ─────────────────────────────────────────────────────────────────────
// join — the analysis fold's field-wise union
// ─────────────────────────────────────────────────────────────────────

let joinUnions =
  test "join: unions scopes + ORs the flags, field by field" {
    let a = { httpGrant (only [ "GET" ]) Cap.anyUrl with random = true }
    let b = { httpGrant (only [ "POST" ]) Cap.anyUrl with clock = true }
    let j = Cap.join a b
    Expect.isTrue j.random "random kept"
    Expect.isTrue j.clock "clock added"
    // both a GET need and a POST need are now covered (the two rules are appended)
    Expect.isTrue (isAllowed j (req "GET" "https" "api.x" 443L "/")) "GET covered"
    Expect.isTrue (isAllowed j (req "POST" "https" "api.x" 443L "/")) "POST covered"
  }


// ─────────────────────────────────────────────────────────────────────
// binary blob — toBytes / fromBytes round-trip (the on-disk grant store)
// (the grant-spec LANGUAGE round-trip lives in `.dark`'s `LanguageTools.Capabilities` tests now)
// ─────────────────────────────────────────────────────────────────────

let binaryRoundTrip =
  test "toBytes ↔ fromBytes round-trips the grant (the on-disk binary blob)" {
    let caps =
      { Cap.noCaps with
          httpClient =
            [ { methods = only [ "GET" ]
                url =
                  urlS
                    (only [ "https" ])
                    [ Cap.Subdomain "github.com" ]
                    (port 443L)
                    (only [ "/api" ]) }
              { methods = Cap.Any
                url = urlS Cap.Any [ Cap.ExactHost "localhost" ] Cap.Any Cap.Any } ]
          httpServer = Cap.Any
          file = { read = Cap.Any; write = only [ "~/.darklang/" ] }
          env = { read = only [ "PATH" ]; write = Cap.Only Set.empty }
          db = { read = only [ "users" ]; write = only [ "users" ] }
          exec =
            [ { programs = only [ "git" ]; args = Cap.Any }
              { programs = only [ "rm" ]; args = only [ "--dry-run" ] } ]
          random = true
          clock = true
          stdout = true }
    Expect.equal
      (Grants.fromBytes (Grants.toBytes caps))
      caps
      "binary blob round-trips the record"
    Expect.equal
      (Grants.fromBytes (Grants.toBytes Cap.noCaps))
      Cap.noCaps
      "noCaps round-trips"
  }


// ─────────────────────────────────────────────────────────────────────
// gate — the structural PRESENCE check (`coversStructurally`)
// ─────────────────────────────────────────────────────────────────────

let private isPresent (g : Cap.Capabilities) (n : Cap.Capabilities) =
  Cap.coversStructurally g n = Cap.Allowed

let private isAbsent (g : Cap.Capabilities) (n : Cap.Capabilities) =
  match Cap.coversStructurally g n with
  | Cap.Denied _ -> true
  | _ -> false

let structuralGatePresence =
  // the gate is presence-only for rich domains: a SCOPED grant passes (the body refines the specifics),
  // but a grant with NO http at all is denied here before the body runs. Bools are exact.
  test "coversStructurally: presence for rich domains, exact for bools" {
    // a GET-only grant passes the http presence gate for any http need (body enforces the method/host)
    let getOnly = httpGrant (only [ "GET" ]) Cap.anyUrl
    Expect.isTrue
      (isPresent getOnly (req "POST" "https" "api.x" 443L "/"))
      "a POST need passes the http presence gate under a GET grant (body decides the method)"
    Expect.isTrue
      (isAbsent Cap.noCaps (Cap.Needs.http))
      "no http grant ⇒ http need denied at the gate"
    // file read presence is per-direction: a write-only grant doesn't satisfy a read need
    Expect.isTrue
      (isAbsent Cap.Needs.fileWrite Cap.Needs.fileRead)
      "a write-only grant doesn't satisfy a read need at the gate"
    Expect.isTrue
      (isPresent Cap.Needs.fileRead Cap.Needs.fileRead)
      "a read grant satisfies a read need"
    // bools are exact
    Expect.isTrue
      (isAbsent Cap.noCaps Cap.Needs.clock)
      "clock need denied without grant"
    Expect.isTrue
      (isPresent Cap.Needs.clock Cap.Needs.clock)
      "clock granted ⇒ allowed"
  }

let scopedGrantsEnforced =
  // the headline: `file write ~/.darklang/` must enforce at runtime. The builtin body builds the PRECISE
  // need (a singleton path) and `covers` decides — exactly what `CapabilityCheck.requireFileWrite` does.
  test "covers: a scoped file/db grant covers in-scope targets and denies others" {
    let fileGrant =
      { Cap.noCaps with
          file = { read = Cap.Only Set.empty; write = only [ "~/.darklang/" ] } }
    let writeNeed (p : string) : Cap.Capabilities =
      { Cap.noCaps with
          file = { read = Cap.Only Set.empty; write = Cap.Only(Set.singleton p) } }
    Expect.isTrue
      (isAllowed fileGrant (writeNeed "~/.darklang/x"))
      "write inside ~/.darklang/ allowed"
    Expect.isTrue
      (isDenied fileGrant (writeNeed "/etc/passwd"))
      "write outside the dir denied"
    // an unscoped `file write` (Any) still covers any concrete path
    Expect.isTrue
      (isAllowed
        ({ Cap.noCaps with file = { read = Cap.Only Set.empty; write = Cap.Any } })
        (writeNeed "/etc/passwd"))
      "an unscoped write grant covers any path"
    // db: the table is the concrete reference the body holds
    let dbGrant =
      { Cap.noCaps with
          db = { read = only [ "users" ]; write = Cap.Only Set.empty } }
    let dbReadNeed (t : string) : Cap.Capabilities =
      { Cap.noCaps with
          db = { read = Cap.Only(Set.singleton t); write = Cap.Only Set.empty } }
    Expect.isTrue
      (isAllowed dbGrant (dbReadNeed "users"))
      "db read of the granted table allowed"
    Expect.isTrue
      (isDenied dbGrant (dbReadNeed "secrets"))
      "db read of a different table denied"
  }

// revoke + clear now live in `.dark` as spec-list filtering over get/set (no F# domain-clear).


// ─────────────────────────────────────────────────────────────────────
// fnRefs — the PT-walk that feeds the analysis
// ─────────────────────────────────────────────────────────────────────

let private bi (name : string) : PT.FQFnName.FQFnName =
  PT.FQFnName.Builtin { name = name; version = 0 }

let private fnName (fq : PT.FQFnName.FQFnName) : PT.Expr =
  PT.EFnName(0UL, PT.NameResolution.ok fq)

let fnRefsDirectName =
  test "fnRefs: an EFnName yields its resolved name" {
    Expect.equal
      (Analysis.fnRefs (fnName (bi "httpClientRequest")))
      [ bi "httpClientRequest" ]
      "the single referenced builtin"
  }

let fnRefsRecursesSubexprs =
  test "fnRefs: recurses into every subexpression (both sides of a let)" {
    let e =
      PT.ELet(
        0UL,
        PT.LPWildcard 0UL,
        fnName (bi "httpClientRequest"),
        fnName (bi "randomUuid")
      )
    Expect.equal
      (Set.ofList (Analysis.fnRefs e))
      (Set.ofList [ bi "httpClientRequest"; bi "randomUuid" ])
      "names collected from value AND body"
  }

let fnRefsSkipsUnresolved =
  test "fnRefs: an unresolved name contributes nothing" {
    let e =
      PT.EFnName(
        0UL,
        { originalName = [ "Nope" ]
          resolved = Error PT.NameResolutionError.NotFound }
      )
    Expect.equal (Analysis.fnRefs e) [] "an Error resolution yields no fn ref"
  }

let fnRefsCollectsPipedCalls =
  test "fnRefs: a piped fn-call (EPipeFnCall) contributes its name" {
    let pipe = PT.EPipeFnCall(0UL, PT.NameResolution.ok (bi "jsonSerialize"), [], [])
    let e = PT.EPipe(0UL, PT.EVariable(0UL, "x"), [ pipe ])
    Expect.equal (Analysis.fnRefs e) [ bi "jsonSerialize" ] "the piped fn name"
  }

let fnRefsLiteralsAreEmpty =
  test "fnRefs: a literal references no fns (the pure leaf)" {
    Expect.equal (Analysis.fnRefs (PT.EInt64(0UL, 5L))) [] "no fn refs in a literal"
  }


let tests =
  testList
    "capabilities"
    [ noCapsDeniesEffectful
      allCapsGrantsEverything
      getOnlyCoversGetDeniesPost
      hostScopedGrant
      schemePortPathScoping
      subdomainScoping
      perHostCoupling
      execCoupling
      fileReadAnyWriteHere
      hostMatching
      pathMatching
      scopeSubsumption
      joinUnions
      binaryRoundTrip
      structuralGatePresence
      scopedGrantsEnforced
      fnRefsDirectName
      fnRefsRecursesSubexprs
      fnRefsSkipsUnresolved
      fnRefsCollectsPipedCalls
      fnRefsLiteralsAreEmpty ]
