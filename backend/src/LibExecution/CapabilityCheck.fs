/// CapabilityCheck — the PRECISE, per-call capability check that a nuanced builtin runs in its own body.
///
/// The call-site gate (Interpreter) already did the BROAD check — `coversStructurally grant fn.capabilities`
/// — so the instance allows this builtin's domain at all. But a scoped grant (`file read ~/x`,
/// `http-client GET api.x`) only means something if the SPECIFIC target is enforced, and only the builtin
/// knows its own target (the path is fileRead's first arg, the URL is httpClient's). So each such builtin
/// calls the matching `require*` here with the concrete value it holds — no name-matching, no central
/// side-table. On denial we name the resource and the exact `dark caps grant …` command that would allow it.
module LibExecution.CapabilityCheck

open Prelude
open LibExecution.RuntimeTypes
module Cap = LibExecution.Capabilities

/// Enforce a precise need against the grant; raise a denial (resource + grant hint) if uncovered.
let private check
  (grant : Cap.Capabilities)
  (need : Cap.Capabilities)
  (resource : string)
  : unit =
  match Cap.covers grant need with
  | Cap.Allowed -> ()
  | Cap.Denied what ->
    RuntimeError.UncaughtException(
      $"capability denied: {resource} needs {what}, which this instance doesn't grant. Grant it with `dark caps`.",
      []
    )
    |> raiseUntargetedRTE

let private only1 (x : string) : Cap.Scope<string> = Cap.Only(Set.singleton x)
let private noScope : Cap.Scope<string> = Cap.Only Set.empty

// ── http ────────────────────────────────────────────────────────────────────

/// Decompose the request URL into the structured need and enforce it. An unparseable URL ⇒ the MAXIMAL
/// `anyUrl` need (only an unrestricted http grant covers it) — fail-closed, never a silent "empty ⇒ any".
let requireHttp (grant : Cap.Capabilities) (method : string) (uri : string) : unit =
  let urlNeed : Cap.UrlScope =
    try
      let u = System.Uri uri
      { schemes = only1 (u.Scheme.ToLowerInvariant())
        hosts = [ Cap.ExactHost(u.Host.ToLowerInvariant()) ]
        ports = Cap.Only(Set.singleton (int64 u.Port))
        paths = only1 u.AbsolutePath }
    with _ ->
      Cap.anyUrl
  let need =
    { Cap.noCaps with
        httpClient =
          [ { methods = only1 (method.ToUpperInvariant()); url = urlNeed } ] }
  check grant need $"{method.ToUpperInvariant()} {uri}"

// ── exec ──────────────────────────────────────────────────────────────────────

/// Enforce the CONCRETE program + arg tokens at a spawn call site.
let requireExec
  (grant : Cap.Capabilities)
  (program : string)
  (args : List<string>)
  : unit =
  let need =
    { Cap.noCaps with
        exec = [ { programs = only1 program; args = Cap.Only(Set.ofList args) } ] }
  check grant need $"running `{program}`"

// ── filesystem ────────────────────────────────────────────────────────────────

let private fileNeed (r, w) : Cap.Capabilities =
  { Cap.noCaps with file = { read = r; write = w } }

let requireFileRead (grant : Cap.Capabilities) (path : string) : unit =
  check grant (fileNeed (only1 path, noScope)) $"reading `{path}`"

let requireFileWrite (grant : Cap.Capabilities) (path : string) : unit =
  check grant (fileNeed (noScope, only1 path)) $"writing `{path}`"

let requireFileReadWrite (grant : Cap.Capabilities) (path : string) : unit =
  check grant (fileNeed (only1 path, only1 path)) $"accessing `{path}`"

// ── environment ───────────────────────────────────────────────────────────────

let private envNeed (r, w) : Cap.Capabilities =
  { Cap.noCaps with env = { read = r; write = w } }

let requireEnvRead (grant : Cap.Capabilities) (var : string) : unit =
  check grant (envNeed (only1 var, noScope)) $"reading env `{var}`"

/// Reading ALL of the environment (no single var) — needs an unscoped env-read grant.
let requireEnvReadAll (grant : Cap.Capabilities) : unit =
  check grant (envNeed (Cap.Any, noScope)) "reading the environment"

let requireEnvWrite (grant : Cap.Capabilities) (var : string) : unit =
  check grant (envNeed (noScope, only1 var)) $"setting env `{var}`"

// ── datastore ─────────────────────────────────────────────────────────────────

let private dbNeed (r, w) : Cap.Capabilities =
  { Cap.noCaps with db = { read = r; write = w } }

let requireDbRead (grant : Cap.Capabilities) (table : string) : unit =
  check grant (dbNeed (only1 table, noScope)) $"reading datastore `{table}`"

let requireDbWrite (grant : Cap.Capabilities) (table : string) : unit =
  check grant (dbNeed (noScope, only1 table)) $"writing datastore `{table}`"
