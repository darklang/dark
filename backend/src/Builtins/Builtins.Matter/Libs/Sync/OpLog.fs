/// The op-log read/append wire: native read/append builtins over the three logs — `package_ops`,
/// `branch_ops`, and `resolutions`. Internal machinery under `Darklang.Sync.*` (NOT stdlib); records are
/// assembled natively (like `Traces`) so a 1000-op batch stays milliseconds.
module Builtins.Matter.Libs.Sync.OpLog

open FSharp.Control.Tasks

open Prelude
open LibExecution.RuntimeTypes
open LibExecution.Builtin.Shortcuts

module Dval = LibExecution.Dval
module PackageRefs = LibExecution.PackageRefs
module NR = LibExecution.RuntimeTypes.NameResolution

module EventLogRefs = LibExecution.PackageRefs.Type.Sync.EventLog
let private eventLogEventType () = FQTypeName.fqPackage (EventLogRefs.event ())
let private eventLogCommitType () = FQTypeName.fqPackage (EventLogRefs.commit ())
let private eventLogChangeType () = FQTypeName.fqPackage (EventLogRefs.change ())


let private eventLogAppendResultType () =
  FQTypeName.fqPackage (EventLogRefs.appendResult ())


/// Build the `Darklang.Sync.EventLog.Change` record for one classified change.
let private changeRecord (c : LibDB.ChangeSummary.Change) : Dval =
  let t = eventLogChangeType ()
  DRecord(
    t,
    t,
    [],
    Map
      [ "action", DString c.action
        "itemKind", DString c.itemKind
        "location", DString c.location ]
  )


/// Build the `Darklang.Sync.EventLog.AppendResult` — ops folded + what they mean.
let private appendResultRecord
  (applied : int64)
  (changes : List<LibDB.ChangeSummary.Change>)
  : Dval =
  let t = eventLogAppendResultType ()
  let kt = KTCustomType(eventLogChangeType (), [])
  DRecord(
    t,
    t,
    [],
    Map
      [ "applied", Dval.int (bigint applied)
        "changes", changes |> List.map changeRecord |> Dval.list kt ]
  )


/// Build the `Darklang.Sync.EventLog.Event` record for one op row.
let private eventRecord
  ((id, op, br, ch, ts) : string * string * string * string * string)
  : Dval =
  let t = eventLogEventType ()
  DRecord(
    t,
    t,
    [],
    Map
      [ "id", DString id
        "op", DString op
        "branchId", DString br
        "commitHash", DString ch
        "originTs", DString ts ]
  )


let private commitRecord
  ((hash, msg, br, acct, at) : string * string * string * string * string)
  : Dval =
  let t = eventLogCommitType ()
  DRecord(
    t,
    t,
    [],
    Map
      [ "hash", DString hash
        "message", DString msg
        "branchId", DString br
        "accountId", DString acct
        "createdAt", DString at ]
  )


let private branchOpEventType () =
  FQTypeName.fqPackage (EventLogRefs.branchOpEvent ())


/// Build the `Darklang.Sync.EventLog.BranchOpEvent` record for one branch_ops row.
let private branchOpEventRecord
  ((id, op, originTs) : string * string * string)
  : Dval =
  let t = branchOpEventType ()
  DRecord(
    t,
    t,
    [],
    Map [ "id", DString id; "op", DString op; "originTs", DString originTs ]
  )


let private resolutionEventType () =
  FQTypeName.fqPackage (EventLogRefs.resolutionEvent ())


/// Build the `Darklang.Sync.EventLog.ResolutionEvent` record for one resolutions row.
let private resolutionEventRecord
  ((id, branchId, location, itemKind, chosenHash, resolvedBy, at) :
    string * string * string * string * string * string * string)
  : Dval =
  let t = resolutionEventType ()

  DRecord(
    t,
    t,
    [],
    Map
      [ "id", DString id
        "branchId", DString branchId
        "location", DString location
        "itemKind", DString itemKind
        "chosenHash", DString chosenHash
        "resolvedBy", DString resolvedBy
        "at", DString at ]
  )


/// Read a string field out of an EventLog Event/Commit record (built by the peer + parsed from JSON).
let private recField (name : string) (fields : Map<string, Dval>) : string =
  match Map.tryFind name fields with
  | Some(DString s) -> s
  | _ ->
    Exception.raiseInternal
      "eventLog record missing expected string field"
      [ "field", name ]


/// A peer sent a `kind` row (op / commit / resolution) we can't parse. Peers are fully trusted for now, so
/// the append skips it rather than crash the pull — but that must be VISIBLE, not silent: a skipped row is
/// dropped while the pull cursor still advances past it, so on divergence the operator needs to see it.
/// `idOpt` names the exact row when its id is still legible (the usual case — a corrupt op-blob or branchId
/// with an intact id), so chasing a divergence lands on one op instead of "some package op".
/// (The proper hardening — quarantine + retry per-op instead of skip — is tracked in CLEANUP(sync-security).)
let private warnSkippedSyncRow (kind : string) (idOpt : Option<string>) : unit =
  let which =
    match idOpt with
    | Some id -> $" (id {id})"
    | None -> ""

  System.Console.Error.WriteLine(
    $"sync: skipped an unparseable {kind}{which} from a peer (dropped, not applied). This can diverge the "
    + "store; re-pull once the peer is fixed."
  )


/// Parse wire `Event` records into the receive path's tuples. Total against a malformed/hostile peer: a row
/// that won't parse is surfaced + skipped rather than crashing the pull. Shared by append + summarize so the
/// two can't disagree about what a batch contains.
let private parseEvents
  (events : List<Dval>)
  : List<System.Guid * byte[] * System.Guid * string * string> =
  events
  |> List.choose (fun ev ->
    match ev with
    | DRecord(_, _, _, f) ->
      let idOpt =
        try
          Some(recField "id" f)
        with _ ->
          None

      try
        Some(
          System.Guid.Parse(recField "id" f),
          System.Convert.FromHexString(recField "op" f),
          System.Guid.Parse(recField "branchId" f),
          recField "commitHash" f,
          recField "originTs" f
        )
      with _ ->
        warnSkippedSyncRow "package op" idOpt
        None
    | _ ->
      warnSkippedSyncRow "package op" None
      None)


let fns () : List<BuiltInFn> =
  [
    // ── native op-log read/append + blob-channel builtins (the ops-queue seam sync rides on) ──
    // CLEANUP(sync-builtins): these operate on the FIXED local store (not an arbitrary path) and still declare
    // noCaps, so untrusted `dark run` can read/append the op log. Lower-risk than the arbitrary-path sqlite* above
    // (can't touch other files), but when these are reorganized around a first-class ops-queue they should carry
    // a real store capability too.
    //
    // The TYPED, stream-shaped read of the package op log: Event/Commit records + the resume cursor, all built
    // NATIVELY so a 1000-op batch never pays the per-row Dark interpreter cost. `EventLog.readSince` wraps this;
    // the events Stream is drained (native loop) for the wire, and is filterable for branch-scoped reads.
    { name = fn "packageOpsReadNative" 0
      typeParams = [ "c"; "e" ]
      parameters =
        [ Param.make
            "cursor"
            TInt64
            "read events after this cursor (0 = from the start)"
          Param.make "limit" TInt64 "at most this many events — one bounded batch" ]
      returnType = TTuple(TList(TVariable "c"), TStream(TVariable "e"), [ TInt64 ])
      description =
        "This instance's committed events after <param cursor> (at most <param "
        + "limit>) as a Stream of Event records, the Commit records they reference, "
        + "and the resume cursor. Built natively — the fast typed read half of the "
        + "op-log seam."
      fn =
        (function
        | struct (_, _, _, [| DInt64 cursor; DInt64 limit |]) ->
          uply {
            let! (commits, events, newCursor) = LibDB.Seed.eventsSince cursor limit

            let commitsList =
              List.map commitRecord commits
              |> Dval.list (KTCustomType(eventLogCommitType (), []))

            let remaining = ref (List.map eventRecord events)

            let nextFn () : Ply<Option<Dval>> =
              uply {
                match remaining.Value with
                | head :: tail ->
                  remaining.Value <- tail
                  return Some head
                | [] -> return None
              }

            let eventStream =
              LibExecution.Stream.newFromIO
                (ValueType.Known(KTCustomType(eventLogEventType (), [])))
                nextFn
                None

            return DTuple(commitsList, eventStream, [ DInt64 newCursor ])
          }
        | _ -> incorrectArgs ())
      sqlSpec = NotQueryable
      previewable = Impure
      capabilities = LibExecution.Capabilities.noCaps
      deprecated = NotDeprecated }

    // The TYPED append: Commit + Event records received from a peer (parsed from JSON natively) folded into the
    // package op log. Field extraction is native — a 1000-op pull never pays the per-row Dark cost. Returns the
    // count newly applied (idempotent). `EventLog.append` wraps this.
    { name = fn "packageOpsAppendNative" 0
      typeParams = [ "c"; "e" ]
      parameters =
        [ Param.make
            "commits"
            (TList(TVariable "c"))
            "Commit records the events reference"
          Param.make
            "events"
            (TList(TVariable "e"))
            "Event records received from a peer" ]
      returnType = TCustomType(NR.ok (eventLogAppendResultType ()), [])
      description =
        "Append received Commit + Event records to the op log + fold, reconciling "
        + "origin_ts to the MIN stamp so LWW converges. Idempotent; extracts fields "
        + "natively. Returns an AppendResult: `applied` = ops that folded (0 = "
        + "nothing new, -1 = DB error, so don't advance the cursor), and `changes` "
        + "= what those ops mean to a human, classified against the pre-fold bindings."
      fn =
        (function
        | struct (_, _, _, [| DList(_, commits); DList(_, events) |]) ->
          uply {
            // Receive must be TOTAL against a malformed/hostile peer: skip any event/commit that won't parse
            // (bad guid/hex), and catch a batch-level DB error (e.g. an FK to a row not in this batch) so the
            // pull loop + background daemon survive instead of crashing. CLEANUP(sync-security): peers are
            // FULLY TRUSTED for now — a future hardening should verify op ids against content + reject bad ops
            // per-op (not skip the whole batch) + not trust the peer's account_id/commit_hash.
            let parsedCommits =
              commits
              |> List.choose (fun c ->
                match c with
                | DRecord(_, _, _, f) ->
                  let idOpt =
                    try
                      Some(recField "hash" f)
                    with _ ->
                      None

                  try
                    Some(
                      recField "hash" f,
                      recField "message" f,
                      System.Guid.Parse(recField "branchId" f),
                      System.Guid.Parse(recField "accountId" f),
                      recField "createdAt" f
                    )
                  with _ ->
                    warnSkippedSyncRow "commit" idOpt
                    None
                | _ ->
                  warnSkippedSyncRow "commit" None
                  None)

            let parsedEvents = parseEvents events

            try
              // Classify BEFORE the fold: a change is "new" vs "updated" relative to what's bound right now,
              // and after the fold that's gone. Also self-filters a re-pull (same hash already bound = no
              // change), so an idempotent pull reports nothing rather than re-announcing old edits.
              let! changes = LibDB.ChangeSummary.ofIncoming parsedEvents
              let! applied = LibDB.Seed.receiveOps parsedCommits parsedEvents
              return appendResultRecord applied changes
            with _ ->
              // -1 = a DB error applying the batch (distinct from 0 = idempotent/nothing new). The puller
              // must NOT advance the cursor on this, or the ops are silently skipped (divergence). Any
              // individually unparseable ops were skipped above and surfaced via warnSkippedSyncRow — they are
              // dropped (not applied) while the cursor still advances, so they're logged, not silently lost.
              return appendResultRecord -1L []
          }
        | _ -> incorrectArgs ())
      sqlSpec = NotQueryable
      previewable = Impure
      capabilities = LibExecution.Capabilities.noCaps
      deprecated = NotDeprecated }


    // The branch-structure log (CreateBranch/CreateCommit/Rebase/Merge/Archive), read as a Stream of
    // BranchOpEvent records + the resume cursor. Branch ops are self-contained (no side commits). Peers apply
    // these to LEARN branches — the structure that `packageOps` events reference by branch_id.
    { name = fn "branchOpsReadNative" 0
      typeParams = [ "e" ]
      parameters =
        [ Param.make
            "cursor"
            TInt64
            "read branch ops after this cursor (0 = from the start)"
          Param.make "limit" TInt64 "at most this many — one bounded batch" ]
      returnType = TTuple(TStream(TVariable "e"), TInt64, [])
      description =
        "This instance's branch ops after <param cursor> as a Stream of "
        + "BranchOpEvent records + the resume cursor. Built natively."
      fn =
        (function
        | struct (_, _, _, [| DInt64 cursor; DInt64 limit |]) ->
          uply {
            let! (events, newCursor) = LibDB.Seed.branchOpsSince cursor limit

            let remaining = ref (List.map branchOpEventRecord events)

            let nextFn () : Ply<Option<Dval>> =
              uply {
                match remaining.Value with
                | head :: tail ->
                  remaining.Value <- tail
                  return Some head
                | [] -> return None
              }

            let stream =
              LibExecution.Stream.newFromIO
                (ValueType.Known(KTCustomType(branchOpEventType (), [])))
                nextFn
                None

            return DTuple(stream, DInt64 newCursor, [])
          }
        | _ -> incorrectArgs ())
      sqlSpec = NotQueryable
      previewable = Impure
      capabilities = LibExecution.Capabilities.noCaps
      deprecated = NotDeprecated }

    // Apply branch ops RECEIVED from a peer (BranchOpEvent records) — fold into branches/commits. Idempotent.
    { name = fn "branchOpsAppendNative" 0
      typeParams = [ "e" ]
      parameters =
        [ Param.make
            "events"
            (TList(TVariable "e"))
            "BranchOpEvent records received from a peer" ]
      returnType = TInt
      description =
        "Apply received BranchOpEvent records to the branch-ops log (fold into "
        + "branches/commits). Returns the count processed. Idempotent."
      fn =
        (function
        | struct (_, _, _, [| DList(_, events) |]) ->
          uply {
            // Total against a bad peer (see packageOpsAppendNative): skip unparseable events, catch batch errors.
            let parsed =
              events
              |> List.choose (fun ev ->
                match ev with
                | DRecord(_, _, _, f) ->
                  let idOpt =
                    try
                      Some(recField "id" f)
                    with _ ->
                      None

                  try
                    Some(
                      recField "id" f,
                      System.Convert.FromHexString(recField "op" f),
                      recField "originTs" f
                    )
                  with _ ->
                    warnSkippedSyncRow "branch op" idOpt
                    None
                | _ ->
                  warnSkippedSyncRow "branch op" None
                  None)

            try
              let! applied = LibDB.Seed.receiveBranchOps parsed
              return Dval.int (bigint applied)
            with _ ->
              // -1 = DB error (vs 0 = idempotent) so the puller stops without advancing this log's cursor.
              return Dval.int (bigint -1L)
          }
        | _ -> incorrectArgs ())
      sqlSpec = NotQueryable
      previewable = Impure
      capabilities = LibExecution.Capabilities.noCaps
      deprecated = NotDeprecated }

    // The resolutions log — synced override overlays. Peers apply these AFTER package ops so a human's
    // "keep mine" decision converges everywhere. Read as a Stream of ResolutionEvent records + the cursor.
    { name = fn "resolutionsReadNative" 0
      typeParams = [ "e" ]
      parameters =
        [ Param.make
            "cursor"
            TInt64
            "read resolutions after this cursor (0 = from the start)"
          Param.make "limit" TInt64 "at most this many — one bounded batch" ]
      returnType = TTuple(TStream(TVariable "e"), TInt64, [])
      description =
        "This instance's resolutions after <param cursor> as a Stream of "
        + "ResolutionEvent records + the resume cursor. Built natively."
      fn =
        (function
        | struct (_, _, _, [| DInt64 cursor; DInt64 limit |]) ->
          uply {
            let! (events, newCursor) =
              LibDB.Resolutions.resolutionsSince cursor limit

            let remaining = ref (List.map resolutionEventRecord events)

            let nextFn () : Ply<Option<Dval>> =
              uply {
                match remaining.Value with
                | head :: tail ->
                  remaining.Value <- tail
                  return Some head
                | [] -> return None
              }

            let stream =
              LibExecution.Stream.newFromIO
                (ValueType.Known(KTCustomType(resolutionEventType (), [])))
                nextFn
                None

            return DTuple(stream, DInt64 newCursor, [])
          }
        | _ -> incorrectArgs ())
      sqlSpec = NotQueryable
      previewable = Impure
      capabilities = LibExecution.Capabilities.noCaps
      deprecated = NotDeprecated }

    // Apply resolutions RECEIVED from a peer (ResolutionEvent records) — record + overlay onto locations.
    // Idempotent (id-keyed) + LWW-gated by each resolution's `at`.
    { name = fn "resolutionsAppendNative" 0
      typeParams = [ "e" ]
      parameters =
        [ Param.make
            "events"
            (TList(TVariable "e"))
            "ResolutionEvent records received from a peer" ]
      returnType = TInt
      description =
        "Apply received ResolutionEvent records (record + overlay onto locations). "
        + "Returns the count processed. Idempotent."
      fn =
        (function
        | struct (_, _, _, [| DList(_, events) |]) ->
          uply {
            let parsed =
              events
              |> List.choose (fun ev ->
                match ev with
                | DRecord(_, _, _, f) ->
                  let idOpt =
                    try
                      Some(recField "id" f)
                    with _ ->
                      None

                  try
                    Some(
                      recField "id" f,
                      recField "branchId" f,
                      recField "location" f,
                      recField "itemKind" f,
                      recField "chosenHash" f,
                      recField "resolvedBy" f,
                      recField "at" f
                    )
                  with _ ->
                    warnSkippedSyncRow "resolution" idOpt
                    None
                | _ ->
                  warnSkippedSyncRow "resolution" None
                  None)

            try
              let! applied = LibDB.Resolutions.receiveResolutions parsed
              return Dval.int (bigint applied)
            with _ ->
              // -1 = DB error (vs 0 = idempotent) so the puller stops without advancing this log's cursor.
              return Dval.int (bigint -1L)
          }
        | _ -> incorrectArgs ())
      sqlSpec = NotQueryable
      previewable = Impure
      capabilities = LibExecution.Capabilities.noCaps
      deprecated = NotDeprecated }


    ]


let builtins () = LibExecution.Builtin.make [] (fns ())
