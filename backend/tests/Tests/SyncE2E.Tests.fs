/// HEAVY, flag-gated end-to-end sync tests. Unlike `MultiInstance.Tests.fs` (which drives the store +
/// CLI in-process), this spins up REAL `dark` processes, each with its own rundir + data.db, and syncs
/// them over REAL HTTP — the one layer the in-process tests can't reach: the actual `/sync/events` server,
/// the `pull` loop + cursors, and process isolation.
///
/// It's slow (a fresh process per command, ~1.3s cold-start each) and needs the Cli exe built, so it's
/// OFF by default. Enable it with the `DARK_E2E=1` env var:
///
///   DARK_E2E=1 ./scripts/run-backend-tests --filter-test-list "SyncE2E"
///
/// When the flag is absent, the suite collapses to a single skipped test that says how to turn it on — so
/// a normal `run-backend-tests` neither builds the exe nor pays the cost.
module Tests.SyncE2E

open Expecto
open System.Threading.Tasks
open FSharp.Control.Tasks
open Prelude

module Config = LibConfig.Config

// ── flag gate ─────────────────────────────────────────────────────────────────────────────────────────
let private enabled : bool =
  match System.Environment.GetEnvironmentVariable "DARK_E2E" with
  | null
  | ""
  | "0" -> false
  | _ -> true

// ── locating the built Cli exe + a loaded seed store ────────────────────────────────────────────────
// The exe lands under <repoRoot>/backend/Build/out/Cli/Debug/net<ver>/Cli. run-backend-tests runs the
// Tests exe with CWD = <repoRoot>/backend, so try both that and the repo root; glob for the file so a
// .NET version bump doesn't break the path.
let private cliExe : string =
  [ "Build/out/Cli/Debug"; "backend/Build/out/Cli/Debug" ]
  |> List.tryPick (fun dir ->
    if System.IO.Directory.Exists dir then
      System.IO.Directory.EnumerateFiles(
        dir,
        "Cli",
        System.IO.SearchOption.AllDirectories
      )
      |> Seq.tryHead
    else
      None)
  |> Option.map System.IO.Path.GetFullPath
  |> Option.defaultValue ""

/// The live test store (run-backend-tests reloads packages into it before the suite). It is MUTATED by
/// other tests running in parallel, so we never seed directly from it.
let private liveStore : string = System.IO.Path.GetFullPath Config.dbPath

/// Shell `sqlite3 <db> <sql>` (matches the bash harness; sqlite3 CLI is in the devcontainer).
let private sqlite (db : string) (sql : string) : string =
  let psi = System.Diagnostics.ProcessStartInfo()
  psi.FileName <- "sqlite3"
  psi.ArgumentList.Add db
  psi.ArgumentList.Add sql
  psi.RedirectStandardOutput <- true
  psi.UseShellExecute <- false
  use p = System.Diagnostics.Process.Start(psi)
  let out = p.StandardOutput.ReadToEnd()
  p.WaitForExit(10_000) |> ignore<bool>
  out.Trim()

/// A frozen, consistent snapshot of the live store, captured ONCE. Every instance is a copy of this, and
/// baselines are read from this — so `mk` (copy) and `presync` (baseline read) always agree, even while
/// the live store keeps moving under concurrent tests. We copy the WAL/SHM sidecars for a consistent point,
/// then `wal_checkpoint(TRUNCATE)` collapses the WAL into the main file — so a plain `File.Copy` of the main
/// file alone is a COMPLETE store (an instance that copied only the main file would otherwise be missing
/// rows the WAL still held, leaving it behind its own sync cursor → "already up to date").
let private seedDb : Lazy<string> =
  lazy
    (let dst =
      System.IO.Path.Combine("/tmp", $"dark-e2e-seed-{System.Guid.NewGuid()}.db")
     for suffix in [ ""; "-wal"; "-shm" ] do
       if System.IO.File.Exists(liveStore + suffix) then
         System.IO.File.Copy(liveStore + suffix, dst + suffix, overwrite = true)
     sqlite dst "PRAGMA wal_checkpoint(TRUNCATE);" |> ignore<string>
     dst)

let private ansi = System.Text.RegularExpressions.Regex(@"\x1b\[[0-9;]*m")
let private strip (s : string) : string = ansi.Replace(s, "")

// ── process helpers ─────────────────────────────────────────────────────────────────────────────────
let private startInfo
  (dir : string)
  (args : string list)
  : System.Diagnostics.ProcessStartInfo =
  let psi = System.Diagnostics.ProcessStartInfo()
  psi.FileName <- cliExe
  args |> List.iter psi.ArgumentList.Add
  psi.RedirectStandardOutput <- true
  psi.RedirectStandardError <- true
  psi.UseShellExecute <- false
  psi.WorkingDirectory <- dir
  psi.Environment["DARK_CONFIG_RUNDIR"] <- dir
  psi.Environment["DARK_CONFIG_DB_NAME"] <- "data.db"
  // These children read trace detail from their own environment (LibDB.Tracing.TraceDetail.readEnv, once at
  // startup), and config/dev sets it on. Left inherited, a served sync writes tens of thousands of trace rows
  // plus the package_blobs rows trace prep promotes with them, and the disk-safety assertion below fails for
  // the environment rather than for the bug it guards.
  psi.Environment["DARK_CONFIG_TRACE_DETAIL"] <- "off"
  psi

/// Run `dark <args>` against instance <dir> to completion; return combined, color-stripped stdout+stderr.
let private darkIn (dir : string) (args : string list) : string =
  use p = System.Diagnostics.Process.Start(startInfo dir args)
  let out = p.StandardOutput.ReadToEnd()
  let err = p.StandardError.ReadToEnd()
  p.WaitForExit(60_000) |> ignore<bool>
  strip (out + err)

// ── instance lifecycle ──────────────────────────────────────────────────────────────────────────────
type Instance = { name : string; dir : string }

let private mk (root : string) (name : string) : Instance =
  let dir = System.IO.Path.Combine(root, name)
  System.IO.Directory.CreateDirectory(System.IO.Path.Combine(dir, "logs"))
  |> ignore<System.IO.DirectoryInfo>
  // The CLI stores its login/session config next to this instance's store (`<dir>/cli-config.json`), so
  // `login` persists here and a later `commit` process sees the logged-in user — each instance isolated.
  System.IO.File.Copy(
    seedDb.Force(),
    System.IO.Path.Combine(dir, "data.db"),
    overwrite = true
  )
  { name = name; dir = dir }

let private dbOf (inst : Instance) : string =
  System.IO.Path.Combine(inst.dir, "data.db")

/// Serve <inst> on <port> in the background; poll `/sync/health` until it answers. Returns the process
/// (kill it to stop). Health-check via HttpClient — real HTTP, no curl dependency.
let private serve (inst : Instance) (port : int) : Task<System.Diagnostics.Process> =
  task {
    let p =
      System.Diagnostics.Process.Start(
        startInfo inst.dir [ "sync"; "serve"; "--port"; string port ]
      )
    use http = new System.Net.Http.HttpClient()
    http.Timeout <- System.TimeSpan.FromSeconds 2.0
    let mutable healthy = false
    let mutable tries = 0
    while not healthy && tries < 40 do
      try
        let! resp = http.GetAsync($"http://127.0.0.1:{port}/sync/health")
        healthy <- resp.IsSuccessStatusCode
      with _ ->
        ()
      if not healthy then
        do! Task.Delay 250
        tries <- tries + 1
    if not healthy then
      // kill the started process before bailing, or it keeps holding the port for the next run
      let killed =
        try
          p.Kill(entireProcessTree = true)
          true
        with _ ->
          false
      ignore<bool> killed
      Exception.raiseInternal
        $"serve {inst.name} never became healthy on port {port}"
        []
    return p
  }

let private stop (p : System.Diagnostics.Process) : unit =
  try
    p.Kill(entireProcessTree = true)
  with _ ->
    ()

/// Connect <inst> to <peerUrl> and set the per-log cursors to the UNCHANGING seed baseline — the
/// "already synced up to the common seed" point, so only the ops a scenario authored (rowid > seed)
/// transfer (matches the bash harness; keeps the batch tiny + avoids re-shipping the whole seed).
let private presync (inst : Instance) (peerUrl : string) : unit =
  darkIn inst.dir [ "sync"; "connect"; peerUrl ] |> ignore<string>
  // The seed-baseline cursor per log — "already synced up to the common seed" so only a scenario's authored
  // ops transfer. Each log's cursor is in the units that log PAGES by: package_ops now pages by `committed_seq`
  // (a commit-order stamp; seed ops are NULL → MAX is 0, so authored ops with committed_seq > 0 still ship, and
  // the seed itself — NULL committed_seq — never does), while branch_ops/resolutions still page by `rowid`.
  let baseline (table : string) (col : string) =
    let v = sqlite (seedDb.Force()) $"SELECT COALESCE(MAX({col}),0) FROM {table}"
    if v = "" then "0" else v
  let pkg = baseline "package_ops" "committed_seq"
  let br = baseline "branch_ops" "rowid"
  let res = baseline "resolutions" "rowid"
  sqlite
    (dbOf inst)
    $"INSERT OR REPLACE INTO sync_cursors_v1(peer,kind,cursor) VALUES('{peerUrl}','package_ops',{pkg}),('{peerUrl}','branch_ops',{br}),('{peerUrl}','resolutions',{res})"
  |> ignore<string>

let private author (inst : Instance) (loc : string) (expr : string) : unit =
  darkIn inst.dir [ "val"; loc; "="; expr ] |> ignore<string>

let private commit (inst : Instance) (msg : string) : unit =
  darkIn inst.dir [ "commit"; msg; "--yes" ] |> ignore<string>

let private hashOf (inst : Instance) (loc : string) : string =
  (darkIn inst.dir [ "hash"; loc ]).Trim().Split('\n') |> Array.head

let private viewOf (inst : Instance) (loc : string) : string =
  (darkIn inst.dir [ "view"; loc ]).Trim().Split('\n') |> Array.head

/// Assert a `dark hash` line is a REAL hash, not an empty/error line — so a convergence `Expect.equal` on two
/// instances' hashes can't pass vacuously (both sides printing an identical "not found" banner).
let private assertRealHash (label : string) (h : string) : unit =
  Expect.isTrue
    (h.Length >= 8)
    $"{label}: expected a real (non-empty) hash line, got '{h}'"
  let low = h.ToLower()
  Expect.isFalse
    (low.Contains "error" || low.Contains "not found" || low.Contains "no such")
    $"{label}: hash line looks like an error, not a hash: '{h}'"

// Real-process sync is eventually-consistent: a single `dark sync` transfers the ops, but the peer's
// fold + value-eval + blob fetch-on-miss can still be settling when a read fires immediately after (the DIAG
// was "Pulled 1 change" yet `view` → "Not found"). So the convergence assertions RE-PULL and re-check up to a
// deadline instead of asserting once — the logic is proven deterministically by the in-process MultiInstance
// suite; this only absorbs cross-process timing. (A re-`sync` also re-triggers blob fetch-on-miss.)

/// Re-pull on <inst> until <loc> hashes to <target>, up to ~8s. Returns whether it converged.
let private pullUntilHash (inst : Instance) (loc : string) (target : string) : bool =
  let mutable h = hashOf inst loc
  let mutable waited = 0
  while h <> target && waited < 8000 do
    System.Threading.Thread.Sleep 300
    darkIn inst.dir [ "sync" ] |> ignore<string>
    h <- hashOf inst loc
    waited <- waited + 300
  h = target

/// Re-pull on <inst> until `<produce>()` contains <needle>, up to ~8s. Returns the last output (so a failed
/// assertion still shows a real value). Used for "the conflict was recorded/shown" after a cross-sync.
let private syncUntilShows
  (inst : Instance)
  (produce : unit -> string)
  (needle : string)
  : string =
  let mutable out = produce ()
  let mutable waited = 0
  while not (out.Contains needle) && waited < 8000 do
    System.Threading.Thread.Sleep 300
    darkIn inst.dir [ "sync" ] |> ignore<string>
    out <- produce ()
    waited <- waited + 300
  out

// ── scenarios ───────────────────────────────────────────────────────────────────────────────────────
let private withRoot (name : string) (body : string -> Task<unit>) : Task<unit> =
  task {
    let root =
      System.IO.Path.Combine("/tmp", $"dark-e2e-{name}-{System.Guid.NewGuid()}")
    System.IO.Directory.CreateDirectory root |> ignore<System.IO.DirectoryInfo>
    try
      do! body root
    finally
      try
        System.IO.Directory.Delete(root, recursive = true)
      with _ ->
        ()
  }

// testSequenced: each scenario spins up real `dark` processes on fixed ports; running them serially avoids
// port contention + the cold-start pile-up a parallel run would cause on a loaded machine.
let private realTests =
  testSequenced
  <| testList
    "SyncE2E"
    [ testTask
        "basic sync: a committed change pulls over real HTTP and converges (hash + value)" {
        do!
          withRoot "basic" (fun root ->
            task {
              let a = mk root "A"
              let b = mk root "B"
              darkIn a.dir [ "login"; "Feriel" ] |> ignore<string>
              darkIn b.dir [ "login"; "Stachu" ] |> ignore<string>
              author a "SyncTest.Basic.x" "42"
              commit a "add x"
              let port = 9310
              let! server = serve a port
              try
                presync b $"http://127.0.0.1:{port}"
                let out = darkIn b.dir [ "sync" ]
                Expect.stringContains out "Pulled" "B pulled A's change"
                let target = hashOf a "SyncTest.Basic.x"
                assertRealHash "A's hash" target
                Expect.isTrue
                  (pullUntilHash b "SyncTest.Basic.x" target)
                  "the hash converged (re-pull until settled)"
                Expect.equal
                  (viewOf b "SyncTest.Basic.x")
                  (viewOf a "SyncTest.Basic.x")
                  "the value converged"
                let resync = darkIn b.dir [ "sync" ]
                Expect.stringContains
                  resync
                  "up to date"
                  "a second pull is idempotent"
              finally
                stop server
            })
      }

      testTask
        "conflict race: concurrent binds record a conflict; keep-mine propagates and converges" {
        do!
          withRoot "conflict" (fun root ->
            task {
              let a = mk root "A"
              let b = mk root "B"
              darkIn a.dir [ "login"; "Feriel" ] |> ignore<string>
              darkIn b.dir [ "login"; "Stachu" ] |> ignore<string>
              author a "SyncTest.Race.n" "1"
              commit a "A=1"
              author b "SyncTest.Race.n" "2"
              commit b "B=2"
              let pA = 9320
              let pB = 9321
              let! serverA = serve a pA
              let! serverB = serve b pB
              try
                presync a $"http://127.0.0.1:{pB}"
                presync b $"http://127.0.0.1:{pA}"
                darkIn a.dir [ "sync" ] |> ignore<string>
                darkIn b.dir [ "sync" ] |> ignore<string>
                // A records the conflict when it pulls B's diverging op; re-pull until it shows.
                Expect.stringContains
                  (syncUntilShows
                    a
                    (fun () -> darkIn a.dir [ "conflicts" ])
                    "SyncTest.Race.n")
                  "SyncTest.Race.n"
                  "the divergence was recorded + shown (never silent)"
                darkIn a.dir [ "conflicts"; "keep-mine"; "SyncTest.Race.n" ]
                |> ignore<string>
                let target = hashOf a "SyncTest.Race.n"
                assertRealHash "A's post-resolution hash" target
                Expect.isTrue
                  (pullUntilHash b "SyncTest.Race.n" target)
                  "B adopts A's resolution → both converge on A's pick"
              finally
                stop serverA
                stop serverB
            })
      }

      testTask
        "conflict race: keep-theirs adopts the PEER's value (overriding the local LWW winner) + converges" {
        do!
          withRoot "conflictT" (fun root ->
            task {
              let a = mk root "A"
              let b = mk root "B"
              darkIn a.dir [ "login"; "Feriel" ] |> ignore<string>
              darkIn b.dir [ "login"; "Stachu" ] |> ignore<string>
              // B commits FIRST (earlier stamp = LWW loser), A SECOND (later = LWW winner). So A holds the
              // conflict with its own value winning; keep-theirs must flip A to B's losing value — a real
              // override, not a no-op that just re-picks the LWW winner.
              author b "SyncTest.RaceT.n" "20"
              commit b "B=20"
              author a "SyncTest.RaceT.n" "10"
              commit a "A=10"
              let pA = 9322
              let pB = 9323
              let! serverA = serve a pA
              let! serverB = serve b pB
              try
                presync a $"http://127.0.0.1:{pB}"
                presync b $"http://127.0.0.1:{pA}"
                darkIn a.dir [ "sync" ] |> ignore<string>
                darkIn b.dir [ "sync" ] |> ignore<string>
                // A must hold the conflict before it can keep-theirs; re-pull until it does.
                syncUntilShows
                  a
                  (fun () -> darkIn a.dir [ "conflicts" ])
                  "SyncTest.RaceT.n"
                |> ignore<string>
                darkIn a.dir [ "conflicts"; "keep-theirs"; "SyncTest.RaceT.n" ]
                |> ignore<string>
                let target = hashOf a "SyncTest.RaceT.n"
                assertRealHash "A's post-keep-theirs hash" target
                Expect.isTrue
                  (pullUntilHash b "SyncTest.RaceT.n" target)
                  "both converge on the kept-theirs pick"
                Expect.stringContains
                  (viewOf a "SyncTest.RaceT.n")
                  "20"
                  "A adopted B's value (20 = theirs), overriding its own LWW-winning 10"
              finally
                stop serverA
                stop serverB
            })
      }

      testTask
        "branch sync: a branch created on A appears on B over real HTTP (exercises the branch-op wire)" {
        // The other E2E scenarios stay on `main`, so the branch-op JSON wire (BranchOpEvent) isn't otherwise
        // exercised over real HTTP. Create a branch on A, sync, and confirm B learns it — end to end through
        // the native read builtin → JSON → HTTP → receiveBranchOps.
        do!
          withRoot "branch" (fun root ->
            task {
              let a = mk root "A"
              let b = mk root "B"
              darkIn a.dir [ "login"; "Feriel" ] |> ignore<string>
              darkIn b.dir [ "login"; "Stachu" ] |> ignore<string>
              darkIn a.dir [ "branch"; "create"; "syncfeature" ] |> ignore<string>
              let port = 9330
              let! server = serve a port
              try
                presync b $"http://127.0.0.1:{port}"
                darkIn b.dir [ "sync" ] |> ignore<string>
                let branches = darkIn b.dir [ "branch"; "list" ]
                Expect.stringContains
                  branches
                  "syncfeature"
                  "the branch A created synced to B over HTTP (the branch-op wire converges)"
              finally
                stop server
            })
      }

      testTask
        "offline peer: syncing against an unreachable peer fails honestly + leaves the store intact" {
        // Only a real-process/HTTP test can exercise this: connect to a port nobody is serving, then sync.
        // The CLI must report the peer is unreachable (never claim success), exit cleanly, and leave the
        // local store fully usable (a subsequent `hash` still works).
        do!
          withRoot "offline" (fun root ->
            task {
              let b = mk root "B"
              darkIn b.dir [ "login"; "Stachu" ] |> ignore<string>
              // No server on this port — the peer is "down". `sync fetch <url>` (single peer) surfaces the
              // error, unlike bare `sync` which pulls-all and stays quiet about offline peers.
              let deadPort = 9339
              let deadUrl = $"http://127.0.0.1:{deadPort}"
              presync b deadUrl
              let out = (darkIn b.dir [ "sync"; "fetch"; deadUrl ]).ToLower()
              Expect.isFalse
                (out.Contains "pulled")
                "it must not claim to have pulled anything from a peer that isn't there"
              Expect.isFalse
                (out.Contains "up to date")
                "it must NOT falsely report 'up to date' when it couldn't even reach the peer"
              Expect.isTrue
                (out.Contains "reach"
                 || out.Contains "couldn't"
                 || out.Contains "unreachable")
                "it surfaces that the peer was unreachable"
              // The store is unharmed: a normal read-only command still works afterward.
              // `--local`, because bare `version` checks GitHub for a newer release and
              // waits ~11s to give up where there is no egress. What is being asked here
              // is whether the CLI still answers at all.
              let ver = darkIn b.dir [ "version"; "--local" ]
              Expect.isFalse
                (ver = "")
                "the CLI + store are still usable after the failed sync"
            })
      }

      testTask
        "daemon disk-safety: serving pulls accumulates NO traces + NO oversized blobs (trace-off is a true no-op)" {
        // A served instance, hit by repeated pulls, must not grow its own store: trace storage defaults off in
        // the shipped binary, and off is a true no-op. (Otherwise the serve promotes every request's captured
        // ephemeral blobs into package_blobs even with trace rows suppressed, so the blob store grows per pull.)
        // Asserts on the SERVER's store — the instance that only serves, never pulls.
        do!
          withRoot "disksafety" (fun root ->
            task {
              let a = mk root "A"
              let b = mk root "B"
              darkIn a.dir [ "login"; "Feriel" ] |> ignore<string>
              darkIn b.dir [ "login"; "Stachu" ] |> ignore<string>
              author a "SyncTest.Disk.v" "12345"
              commit a "add v"
              // Baselines read AFTER A's own authoring and commit and BEFORE it starts serving. This test is
              // about what SERVING accumulates; baselining off the seed snapshot folded in whatever A's own
              // commands did, which is a different claim.
              let beforeTraces =
                (sqlite (dbOf a) "SELECT COUNT(*) FROM trace_fn_calls").Trim()
              let beforeBlobs =
                (sqlite (dbOf a) "SELECT COUNT(*) FROM package_blobs").Trim()
              let port = 9340
              let! server = serve a port
              try
                presync b $"http://127.0.0.1:{port}"
                // Several pulls — each hits A's /sync/events + blob channel, the path that used to trace + promote.
                for _ in 1..6 do
                  darkIn b.dir [ "sync" ] |> ignore<string>
                // A is the SERVER (it only serves, never pulls), so with trace storage off serving must add
                // NOTHING to its store: both counts stay exactly where they were before it started.
                // Comparing the COUNT (not just >1MB rows) catches ANY promotion — the bug persisted a fresh
                // blob per request, small at first, so a size threshold would miss it while the store still
                // grew unbounded.
                let traceRows =
                  (sqlite (dbOf a) "SELECT COUNT(*) FROM trace_fn_calls").Trim()
                let servedBlobs =
                  (sqlite (dbOf a) "SELECT COUNT(*) FROM package_blobs").Trim()
                Expect.equal
                  traceRows
                  beforeTraces
                  "the serve wrote NO NEW trace rows (serving must not accumulate)"
                Expect.equal
                  servedBlobs
                  beforeBlobs
                  "serving promoted NO new blobs into package_blobs — any growth here is the disk-fill bug"
              finally
                stop server
            })
      } ]

/// A single skipped test carrying `msg` — keeps the "SyncE2E" name in the run without paying any cost.
let private skippedWith (msg : string) =
  testList "SyncE2E" [ testCase $"skipped ({msg})" (fun () -> skiptest msg) ]

let tests =
  if not enabled then
    skippedWith "set DARK_E2E=1 to run the heavy real-process sync tests"
  elif cliExe = "" then
    // Enabled but nothing to drive — fail loudly-but-skipped, not with a cryptic Process error.
    skippedWith
      "DARK_E2E=1 but no Cli exe found under backend/Build/out/Cli/Debug — build it first (dotnet build backend/src/Cli/Cli.fsproj)"
  else
    realTests
