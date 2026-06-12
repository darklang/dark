/// The Release migrator — moves a store forward one Release at a time, and (the part that matters
/// today) REFUSES to open a store from a NEWER Release with older code.
///
/// A **Release** is the single version coordinate spanning {language/`ProgramTypes`, op-serialization
/// format, SQL schema, content-hashing} — the same integer (`Sync.wireFormatVersion`) that gates
/// cross-instance sync also gates whether this binary may open this store. One coordinate, one upgrade.
///
/// A `Release N` step bundles:
///   1. a forward, **copy-and-swap** canonical `.sql` — never `DROP`; the op log is preserved,
///   2. an optional **op-format remap** — re-serialize the whole log once, in one transaction,
///   3. a **projection refold** — mark ops unapplied; startup regenerates the projections.
/// Projections are dropped+refolded, never migrated. Forward-only; the undo is "restore from a peer"
/// (every peer holds the whole log, so the tailnet is the backup).
///
/// The current baseline is **Release 3** — a fresh store is born here. The lone registry entry records
/// why 3 is a clean break (meaning-stable hashing redefined every content hash), but because no older
/// store exists, no store ever replays it; the machinery is in place for the first real format change,
/// which appends the next entry. See `releases`.
module LibDB.Releases

open Prelude

open Fumble
open LibDB.Sqlite

/// One forward step that ARRIVES at Release `n` (apply it to move a store from `n-1` to `n`).
type Release =
  {
    /// the Release this step lands on
    n : int
    /// canonical-table forward migration — copy-and-swap, NEVER drop. "" = no canonical-shape change.
    sql : string
    /// op-format remap (old `op_blob` bytes → new), only when the serialization format changed.
    /// `None` = the op format is unchanged at this step (the common case).
    reserialize : (byte[] -> byte[]) option
    /// CLEAN-BREAK boundary: when `true`, pre-this-Release data is treated as disposable and the package
    /// dataset is CLEARED so the store rebuilds from source (dev) or re-pulls from a same-Release peer
    /// (tailnet). Use this only when the change can't be cheaply migrated AND the old data is disposable
    /// — e.g. a content-hash redefinition. The default (`false`) is the durable path (sql/reserialize +
    /// re-fold). The PRINCIPLE: PT (the op log) is the source of truth and migrates forward; RT-derived
    /// data (rt_dval, traces) is never migrated — it's dropped and recomputed from PT.
    clearForRebuild : bool
  }


/// The ordered registry of forward steps. Add an entry when you bump `Sync.wireFormatVersion`; the
/// migrator does the rest. Each `n` must be exactly one greater than the previous (see
/// `registryIsWellFormed`).
///
/// **Release 3 — meaning-stable hashing.** Content hashes are now over the alpha-normalized canonical
/// form, so older `op_blob`s would embed stale hashes and can't be cheaply migrated — hence the
/// CLEAN-BREAK marker (`clearForRebuild`). In practice every store is BORN at Release 3 and never runs
/// this step; it stands as the worked example of the clean-break path and the registry's upper bound.
let releases : Release list =
  [ { n = 3; sql = ""; reserialize = None; clearForRebuild = true } ]


// ── The pure planning half (unit-tested; takes the registry explicitly so tests inject their own) ──

/// The steps to move a store from `storeN` up to `codeN`: the entries with `storeN < n <= codeN`, in
/// ascending order. Pure.
let pendingReleases
  (registry : Release list)
  (storeN : int)
  (codeN : int)
  : Release list =
  registry
  |> List.filter (fun r -> r.n > storeN && r.n <= codeN)
  |> List.sortBy (fun r -> r.n)

/// Is the registry well-formed: strictly ascending, **contiguous** (no gaps), no duplicates, and none
/// above `codeRelease`? A gap would silently skip a migration; a dup would double-apply; an entry above
/// the code's Release is unreachable. Pure guard — unit-tested and asserted at boot.
let registryIsWellFormed (registry : Release list) (codeRelease : int) : bool =
  let ns = registry |> List.map (fun r -> r.n)
  let contiguous = ns |> List.pairwise |> List.forall (fun (a, b) -> b = a + 1)
  let distinct = (List.distinct ns) = ns
  let noneAboveCode = ns |> List.forall (fun n -> n <= codeRelease)
  contiguous && distinct && noneAboveCode


// ── The store's Release stamp (a tiny local table, separate from the schema-hash stamp) ──

let private releaseTable = "release_state_v0"

/// The Release this store was last stamped at, or `None` if it predates Release tracking (or is fresh).
let storedRelease () : int option =
  let exists =
    Sql.query "SELECT 1 FROM sqlite_master WHERE type = 'table' AND name = @n"
    |> Sql.parameters [ "n", Sql.string releaseTable ]
    |> Sql.executeExistsSync
  if not exists then
    None
  else
    match
      Sql.query $"SELECT release FROM {releaseTable} WHERE id = 0"
      |> Sql.execute (fun read -> read.int64 "release")
    with
    | Ok [ r ] -> Some(int r)
    | _ -> None

/// Stamp the store at Release `n`. `release_state_v0` is defined in schema.sql and created when it's
/// applied (the schema bootstrap runs before the Release migrator), so this just writes the row.
let writeRelease (n : int) : unit =
  Sql.query $"INSERT OR REPLACE INTO {releaseTable} (id, release) VALUES (0, @r)"
  |> Sql.parameters [ "r", Sql.int64 (int64 n) ]
  |> Sql.executeStatementSync


// ── Applying a step ──

/// Re-serialize the WHOLE op log once through `remap` (old `op_blob` → new), in a single transaction.
/// The op id is content-addressed over the op's MEANING (a normalized canonical form), not its raw
/// bytes — so a pure *format* change keeps the same id, and we update `op_blob` in place. (A remap that
/// changes an op's meaning/hash is a different, louder operation — the hash-remap path — not this.)
let reserializeLog (remap : byte[] -> byte[]) : unit =
  let rows =
    Sql.query "SELECT id, branch_id, op_blob FROM package_ops"
    |> Sql.execute (fun read ->
      (read.string "id", read.string "branch_id", read.bytes "op_blob"))
    |> Result.unwrap
  let updates =
    rows
    |> List.map (fun (id, branchId, blob) ->
      ("UPDATE package_ops SET op_blob = @blob WHERE id = @id AND branch_id = @branch_id",
       [ [ "blob", Sql.bytes (remap blob)
           "id", Sql.string id
           "branch_id", Sql.string branchId ] ]))
  Sql.executeTransactionSync updates |> ignore<List<int>>

/// The package dataset cleared by a `clearForRebuild` boundary: the PT op log + blobs, the branch
/// structure, the regenerable projections, the RT-derived caches (traces), and local sync cursors/
/// conflicts. Reload-from-source (dev) or a re-pull from a same-Release peer (tailnet) repopulates it.
/// We KEEP accounts, user data, and the peer list (`sync_remotes`) — only the package world is reset.
/// (`rt_dval` lives in `package_values`, so it's cleared with the projections — RT recomputed from PT.)
let private rebuildClearTables : List<string> =
  [ "package_ops"; "package_blobs"; "branches"; "commits"; "branch_ops" ]
  @ LibDB.Seed.projectionTables
  @ [ "traces"; "trace_fn_calls"; "sync_cursors"; "sync_conflicts" ]

/// Clear the package dataset for a clean-break Release (FK off; the rows go, the tables stay so the
/// next reload/sync refills them).
let clearForRebuildData () : unit =
  Sql.query "PRAGMA foreign_keys = OFF" |> Sql.executeStatementSync
  for t in rebuildClearTables do
    Sql.query (sprintf "DELETE FROM \"%s\"" t) |> Sql.executeStatementSync

/// Apply one Release step. A CLEAN-BREAK (`clearForRebuild`) clears the package dataset so it rebuilds
/// from source/peer (disposable pre-Release data). Otherwise it's the durable path: the canonical
/// copy-swap `.sql` (if any), then the op-format remap (if any, which marks the log unapplied so startup
/// refolds projections from the new bytes).
let applyRelease (r : Release) : unit =
  if r.clearForRebuild then
    print
      $"Release {r.n}: clean-break boundary — clearing the package dataset; it rebuilds from source / re-pulls from a same-Release peer."
    clearForRebuildData ()
  else
    if r.sql <> "" then Sql.query r.sql |> Sql.executeStatementSync
    match r.reserialize with
    | Some remap ->
      reserializeLog remap
      Sql.query "UPDATE package_ops SET applied = 0" |> Sql.executeStatementSync
    | None -> ()


// ── The boot guard + forward migrator ──

/// Reconcile the store's Release with this binary's (`codeRelease = Sync.wireFormatVersion`):
///  - **store == code** → nothing;
///  - **store > code**  → REFUSE (a newer store; older code would misread the op format) — raise;
///  - **store < code**  → apply each pending step in order, then stamp `code`;
///  - **store absent**  → stamp `code` (a fresh store is born at the current Release; a pre-tracking
///    store is, by construction, at the current format — no format bumps happened before tracking).
let applyPending (codeRelease : int) : unit =
  if not (registryIsWellFormed releases codeRelease) then
    Exception.raiseInternal
      "Release registry is not well-formed (a gap, duplicate, or an entry above the code Release)"
      [ "codeRelease", codeRelease ]
  match storedRelease () with
  | None -> writeRelease codeRelease
  | Some s when s = codeRelease -> ()
  | Some s when s > codeRelease ->
    Exception.raiseInternal
      $"This store is on Release {s}; this Dark speaks Release {codeRelease}. Upgrade Dark to open it — never open a newer store with older code."
      []
  | Some s ->
    for r in pendingReleases releases s codeRelease do
      let extra = if r.reserialize.IsSome then " + op re-serialize" else ""
      print $"Applying Release {r.n} (schema{extra})…"
      applyRelease r
    writeRelease codeRelease
