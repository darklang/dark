/// Sync surface — pull another instance's package ops into this one.
module Builtins.Matter.Libs.PM.Sync

open Prelude
open LibExecution.RuntimeTypes

module Builtin = LibExecution.Builtin
module PT = LibExecution.ProgramTypes
module Dval = LibExecution.Dval
module VT = LibExecution.ValueType

open Builtin.Shortcuts


let fns () : List<BuiltInFn> =
  [ { name = fn "pmSyncPull" 0
      typeParams = []
      parameters =
        [ Param.make "sourcePath" TString "Path to a peer's data.db to pull ops from" ]
      returnType = TTuple(TInt64, TInt64, [])
      description =
        "Pull a peer's package ops from a local `data.db` file into this instance: resume from
         the stored per-peer cursor, apply the peer's new ops (op log + projections, idempotent),
         and persist the advanced cursor. Returns `(newCursor, divergenceCount)` — the peer's
         last applied rowid, and how many name→hash divergences were surfaced (never blocks)."
      fn =
        (function
        | exeState, vm, _, [ DString sourcePath ] ->
          uply {
            let! (newCursor, divergences) = LibDB.Sync.pullFromFile sourcePath
            // Route each surfaced divergence through the sync policy. The default keeps today's
            // behavior (surface-as-data, LWW stands); a policy can keep-local. branchId = the
            // puller's current branch.
            let callCtx : CallContext =
              { branchId = exeState.branchId; threadID = vm.threadID }
            let! _reconciled =
              LibDB.Sync.routeDivergences
                LibDB.Sync.defaultSyncPolicy
                callCtx
                sourcePath
                exeState.branchId
                divergences
            return
              DTuple(DInt64 newCursor, DInt64(int64 (List.length divergences)), [])
          }
        | _ -> incorrectArgs ())
      sqlSpec = NotQueryable
      previewable = Impure
      capabilities = LibExecution.Capabilities.noCaps
      deprecated = NotDeprecated }


    // ── HTTP transport — server read + client apply, base64 over the wire ──
    { name = fn "pmSyncOpsSince" 0
      typeParams = []
      parameters =
        [ Param.make
            "cursor"
            TInt64
            "Resume point — the last rowid the puller already has" ]
      returnType = TString
      description =
        "The sync SERVER read (the `GET /sync/events?since=cursor` body): the ops the puller
         hasn't seen, encoded as a base64 wire batch. A Darklang HTTP router returns this string;
         the puller decodes + applies it via `pmSyncApplyWire`. Committed-only today (so equivalent to
         `pmSyncOpsSinceCommitted`); kept as the general entry point for when WIP/own-device sync returns."
      fn =
        (function
        | _, _, _, [ DInt64 cursor ] ->
          uply {
            let! ops = LibDB.Sync.opsToSend cursor
            return DString(System.Convert.ToBase64String(LibDB.Sync.encodeBatch ops))
          }
        | _ -> incorrectArgs ())
      sqlSpec = NotQueryable
      previewable = Impure
      capabilities = LibExecution.Capabilities.noCaps
      deprecated = NotDeprecated }


    { name = fn "pmSyncOpsSinceCommitted" 0
      typeParams = []
      parameters =
        [ Param.make
            "cursor"
            TInt64
            "Resume point — the last rowid the puller already has" ]
      returnType = TString
      description =
        "The COMMITTED-ONLY server read (`GET /sync/events?since=cursor&committed=1`): like
         `pmSyncOpsSince` but ships only ops belonging to a commit (excludes WIP) — the multi-author
         scope, so a coworker syncs your committed history, not your uncommitted mid-edits."
      fn =
        (function
        | _, _, _, [ DInt64 cursor ] ->
          uply {
            let! ops = LibDB.Inserts.opsSinceCommitted cursor
            return DString(System.Convert.ToBase64String(LibDB.Sync.encodeBatch ops))
          }
        | _ -> incorrectArgs ())
      sqlSpec = NotQueryable
      previewable = Impure
      capabilities = LibExecution.Capabilities.noCaps
      deprecated = NotDeprecated }


    { name = fn "pmSyncApplyWire" 0
      typeParams = []
      parameters =
        [ Param.make
            "remote"
            TString
            "Peer identity (e.g. its URL) — keys the resume cursor"
          Param.make
            "wireB64"
            TString
            "A base64 wire batch from the peer's `pmSyncOpsSince`" ]
      returnType = TTuple(TInt64, TInt64, [])
      description =
        "The sync CLIENT apply (after `httpRequest`-ing a peer's `/sync/events`): decode the
         base64 wire batch and apply it into this instance (op log + projections, idempotent),
         advancing this peer's cursor. Returns `(newCursor, divergenceCount)` — divergences
         surfaced (never blocked), same as the file pull."
      fn =
        (function
        | exeState, vm, _, [ DString remote; DString wireB64 ] ->
          uply {
            let bytes = System.Convert.FromBase64String wireB64
            let! (newCursor, divergences) =
              LibDB.Sync.applyWireBatch remote PT.mainBranchId None bytes
            // Same policy routing as the file pull (the wire batch applies on main).
            let callCtx : CallContext =
              { branchId = exeState.branchId; threadID = vm.threadID }
            let! _reconciled =
              LibDB.Sync.routeDivergences
                LibDB.Sync.defaultSyncPolicy
                callCtx
                remote
                PT.mainBranchId
                divergences
            return
              DTuple(DInt64 newCursor, DInt64(int64 (List.length divergences)), [])
          }
        | _ -> incorrectArgs ())
      sqlSpec = NotQueryable
      previewable = Impure
      capabilities = LibExecution.Capabilities.noCaps
      deprecated = NotDeprecated }


    { name = fn "pmSyncCursorFor" 0
      typeParams = []
      parameters =
        [ Param.make
            "remote"
            TString
            "Peer identity (path or URL) — the resume cursor key" ]
      returnType = TInt64
      description =
        "The stored resume cursor for this peer — the last rowid we've applied from it, or 0 if
         never synced. The HTTP client passes this as `?since=` so the server returns only the
         ops we don't yet have (incremental pull instead of the whole log every time)."
      fn =
        (function
        | _, _, _, [ DString remote ] ->
          uply {
            let! cursor = LibDB.SyncCursors.cursorFor remote
            return DInt64 cursor
          }
        | _ -> incorrectArgs ())
      sqlSpec = NotQueryable
      previewable = Impure
      capabilities = LibExecution.Capabilities.noCaps
      deprecated = NotDeprecated }


    // ── the RESOLUTION channel over HTTP — override decisions ride alongside the op log ──
    { name = fn "pmSyncResolutionsSince" 0
      typeParams = []
      parameters =
        [ Param.make
            "cursor"
            TInt64
            "Resume point — the last resolution rowid the puller already has" ]
      returnType = TString
      description =
        "The resolution-channel server read (`GET /sync/resolutions?since=cursor`): the override
         decisions the puller hasn't seen, encoded as a base64 wire batch (the client decodes + folds
         them via `pmSyncApplyResolutions`). Resolutions sync immediately — a decision is published
         when made, not gated by commit."
      fn =
        (function
        | _, _, _, [ DInt64 cursor ] ->
          uply {
            let! rs = LibDB.Resolutions.since cursor
            return
              DString(System.Convert.ToBase64String(LibDB.Sync.encodeResolutions rs))
          }
        | _ -> incorrectArgs ())
      sqlSpec = NotQueryable
      previewable = Impure
      capabilities = LibExecution.Capabilities.noCaps
      deprecated = NotDeprecated }


    { name = fn "pmSyncResolutionCursorFor" 0
      typeParams = []
      parameters =
        [ Param.make
            "remote"
            TString
            "Peer identity (path or URL) — the resolution-cursor key" ]
      returnType = TInt64
      description =
        "The stored RESOLUTION cursor for this peer — the last resolution rowid we've applied from it,
         or 0 if never synced. The HTTP client passes this as `?since=` so the server returns only the
         resolutions we don't yet have (a separate cursor from the op cursor)."
      fn =
        (function
        | _, _, _, [ DString remote ] ->
          uply {
            let! cursor = LibDB.SyncCursors.resolutionCursorFor remote
            return DInt64 cursor
          }
        | _ -> incorrectArgs ())
      sqlSpec = NotQueryable
      previewable = Impure
      capabilities = LibExecution.Capabilities.noCaps
      deprecated = NotDeprecated }


    { name = fn "pmSyncApplyResolutions" 0
      typeParams = []
      parameters =
        [ Param.make
            "remote"
            TString
            "Peer identity (e.g. its URL) — keys the resolution cursor"
          Param.make
            "wireB64"
            TString
            "A base64 resolution wire batch from the peer's `pmSyncResolutionsSince`" ]
      returnType = TInt64
      description =
        "The resolution-channel client apply (after `httpRequest`-ing a peer's `/sync/resolutions`):
         decode the base64 batch and fold each resolution into this instance's bindings (the overlay,
         idempotent), advancing this peer's resolution cursor. Returns the new cursor."
      fn =
        (function
        | _, _, _, [ DString remote; DString wireB64 ] ->
          uply {
            let bytes = System.Convert.FromBase64String wireB64
            let decoded = LibDB.Sync.decodeResolutions bytes
            let! cursor = LibDB.Sync.applyRemoteResolutions remote decoded
            return DInt64 cursor
          }
        | _ -> incorrectArgs ())
      sqlSpec = NotQueryable
      previewable = Impure
      capabilities = LibExecution.Capabilities.noCaps
      deprecated = NotDeprecated }


    { name = fn "pmSyncOpKindsSince" 0
      typeParams = []
      parameters =
        [ Param.make "sourcePath" TString "A peer's data.db path"
          Param.make "sinceCursor" TInt64 "Only ops with rowid above this" ]
      returnType = TList TString
      description =
        "The item kinds ('fn'/'type'/'value') of the naming ops a file peer has above sinceCursor —
         for `dark sync pull`'s breakdown. One entry per SetName (each item named once, no
         double-count). File peers only."
      fn =
        (function
        | _, _, _, [ DString sourcePath; DInt64 sinceCursor ] ->
          uply {
            let! kinds = LibDB.Sync.opKindsSince sourcePath sinceCursor
            return Dval.list KTString (kinds |> List.map DString)
          }
        | _ -> incorrectArgs ())
      sqlSpec = NotQueryable
      previewable = Impure
      capabilities = LibExecution.Capabilities.noCaps
      deprecated = NotDeprecated }


    { name = fn "pmSyncStatus" 0
      typeParams = []
      parameters = [ Param.make "unit" TUnit "" ]
      returnType = TList TString
      description =
        "Every peer this instance has synced with and how far (the last applied rowid) — one
         pre-formatted line per peer, for `dark sync status`. Empty if nothing's been synced."
      fn =
        (function
        | _, _, _, [ DUnit ] ->
          uply {
            let! cursors = LibDB.SyncCursors.listCursors ()
            let peerLines =
              cursors
              |> List.map (fun (remote, c) ->
                DString $"{remote} — synced through op {string c}")
            // registered remotes (`dark remote add`) that have NO cursor yet — show them as pending
            // so a freshly-added peer appears in status (the daemon already polls it) instead of
            // silently absent until its first pull.
            let! registered = LibDB.Remotes.list ()
            let syncedSet = cursors |> List.map fst |> Set.ofList
            let registeredLines =
              registered
              |> List.filter (fun (_name, url) -> not (Set.contains url syncedSet))
              |> List.map (fun (name, url) ->
                DString $"{url} — registered as '{name}', not yet synced")
            // proactively RAISE unacknowledged conflicts here so `dark sync status` flags them
            // without the user having to remember to run `dark conflicts` — the common path is a
            // glance at status, see the pending count, then go ack.
            let! conflicts = LibDB.Conflicts.list ()
            let pending =
              conflicts
              |> List.filter (fun (c : LibDB.Conflicts.Conflict) ->
                not c.acknowledged)
            let conflictLines =
              if List.isEmpty pending then
                []
              else
                [ DString
                    $"⚠ {List.length pending} unacknowledged sync conflict(s) — run `dark conflicts`" ]
            return Dval.list KTString (peerLines @ registeredLines @ conflictLines)
          }
        | _ -> incorrectArgs ())
      sqlSpec = NotQueryable
      previewable = Impure
      capabilities = LibExecution.Capabilities.noCaps
      deprecated = NotDeprecated }


    { name = fn "pmSyncRemotes" 0
      typeParams = []
      parameters = [ Param.make "unit" TUnit "" ]
      returnType = TList TString
      description =
        "Every peer a tailnet-wide daemon polls: the UNION of remotes we have a sync cursor for
         (pulled at least once) and remotes explicitly registered via `dark remote add`. De-duped;
         empty if nothing's been synced or registered yet."
      fn =
        (function
        | _, _, _, [ DUnit ] ->
          uply {
            let! cursors = LibDB.SyncCursors.listCursors ()
            let! registered = LibDB.Remotes.urls ()
            // union: cursor-peers (implicit, from pulling) + registered remotes (explicit) — so a
            // `remote add`ed peer is polled even before its first manual pull, and neither path drops.
            let remotes =
              (cursors |> List.map fst) @ registered
              |> List.distinct
              |> List.map DString
            return Dval.list KTString remotes
          }
        | _ -> incorrectArgs ())
      sqlSpec = NotQueryable
      previewable = Impure
      capabilities = LibExecution.Capabilities.noCaps
      deprecated = NotDeprecated }


    { name = fn "pmSyncHealth" 0
      typeParams = []
      parameters = [ Param.make "unit" TUnit "" ]
      returnType = TString
      description =
        "Liveness + state probe for the sync SERVER (the `GET /sync/health` body): confirms the
         server is up, its RELEASE (the op-format/wire version — the single coordinate that gates
         cross-instance sync), and how many ops its canonical log holds. So a puller (or a person)
         can check readiness, see convergence (server ops vs local ops), AND detect a version skew
         before pulling. `release` comes first so the older `ops=` parser stays valid."
      fn =
        (function
        | _, _, _, [ DUnit ] ->
          uply {
            let! (opsCount, _folded) = LibDB.Seed.projectionStatus ()
            return
              DString
                $"sync-server ok; release={string LibDB.Sync.wireFormatVersion}; ops={string opsCount}"
          }
        | _ -> incorrectArgs ())
      sqlSpec = NotQueryable
      previewable = Impure
      capabilities = LibExecution.Capabilities.noCaps
      deprecated = NotDeprecated }


    { name = fn "pmSyncReleaseVersion" 0
      typeParams = []
      parameters = [ Param.make "unit" TUnit "" ]
      returnType = TInt64
      description =
        "THIS instance's sync RELEASE (the op-format/wire version). The single coordinate that must
         match a peer's for sync to proceed — the wire gate refuses a mismatched batch (fail-closed,
         no corruption). Compare against a peer's `/sync/health` `release=` to detect a skew and tell
         the user to upgrade, rather than surfacing a raw decode failure."
      fn =
        (function
        | _, _, _, [ DUnit ] ->
          uply { return DInt64(int64 LibDB.Sync.wireFormatVersion) }
        | _ -> incorrectArgs ())
      sqlSpec = NotQueryable
      previewable = Impure
      capabilities = LibExecution.Capabilities.noCaps
      deprecated = NotDeprecated }


    // ── HTTP blob channel (sender) — content addressed, mirrors the file pull's blob fetch ──
    { name = fn "pmSyncBlobHashes" 0
      typeParams = []
      parameters = [ Param.make "unit" TUnit "" ]
      returnType = TString
      description =
        "The blob MANIFEST (the `GET /sync/blobs` body): every content hash this instance holds,
         newline-joined. A receiver keeps the ones it lacks (`Blob.missing`) and pulls each one's
         bytes via `pmSyncBlobBytes` — the HTTP blob channel, mirroring the file pull's blob fetch."
      fn =
        (function
        | _, _, _, [ DUnit ] ->
          uply {
            let! hashes = LibDB.RuntimeTypes.Blob.allHashes ()
            return DString(String.concat "\n" hashes)
          }
        | _ -> incorrectArgs ())
      sqlSpec = NotQueryable
      previewable = Impure
      capabilities = LibExecution.Capabilities.noCaps
      deprecated = NotDeprecated }


    { name = fn "pmSyncBlobBytes" 0
      typeParams = []
      parameters = [ Param.make "hash" TString "The content hash to fetch" ]
      returnType = TString
      description =
        "The bytes for one content hash, base64-encoded (the `GET /sync/blob?hash=` body), or empty
         if this instance lacks it. The receiver base64-decodes + `Blob.insert`s — content-addressed,
         so the insert dedups exactly like ops (the blob counterpart to `pmSyncOpsSince`)."
      fn =
        (function
        | _, _, _, [ DString hash ] ->
          uply {
            match! LibDB.RuntimeTypes.Blob.get hash with
            | Some bytes -> return DString(System.Convert.ToBase64String bytes)
            | None -> return DString ""
          }
        | _ -> incorrectArgs ())
      sqlSpec = NotQueryable
      previewable = Impure
      capabilities = LibExecution.Capabilities.noCaps
      deprecated = NotDeprecated }


    // ── HTTP blob channel (receiver) — which of a peer's blobs we lack, + store a fetched one ──
    { name = fn "pmBlobMissing" 0
      typeParams = []
      parameters =
        [ Param.make
            "hashes"
            (TList TString)
            "A peer's offered content hashes (its manifest)" ]
      returnType = TList TString
      description =
        "Of the peer's offered content hashes, which this instance LACKS — exactly the blobs to
         fetch (`pmSyncBlobBytes` each). Over `Blob.missing`; content-addressed, so it's a pure
         set-difference (no cursor, unlike the op stream)."
      fn =
        (function
        | _, _, _, [ DList(_, hashDvals) ] ->
          uply {
            let hashes =
              hashDvals
              |> List.choose (fun d ->
                match d with
                | DString s -> Some s
                | _ -> None)
            let! missing = LibDB.RuntimeTypes.Blob.missing hashes
            return Dval.list KTString (missing |> List.map DString)
          }
        | _ -> incorrectArgs ())
      sqlSpec = NotQueryable
      previewable = Impure
      capabilities = LibExecution.Capabilities.noCaps
      deprecated = NotDeprecated }


    { name = fn "pmBlobInsert" 0
      typeParams = []
      parameters =
        [ Param.make "hash" TString "The content hash"
          Param.make
            "base64Bytes"
            TString
            "The blob's bytes, base64-encoded (empty = skip)" ]
      returnType = TBool
      description =
        "Store a fetched blob: base64-decode the bytes and insert under its content hash. Idempotent
         (content-addressed dedup, same guarantee as ops). Returns true if non-empty bytes were
         inserted, false if the peer's body was empty (it lacked the blob)."
      fn =
        (function
        | _, _, _, [ DString hash; DString b64 ] ->
          uply {
            if b64 = "" then
              return DBool false
            else
              let bytes = System.Convert.FromBase64String b64
              do! LibDB.RuntimeTypes.Blob.insert hash bytes
              return DBool true
          }
        | _ -> incorrectArgs ())
      sqlSpec = NotQueryable
      previewable = Impure
      capabilities = LibExecution.Capabilities.noCaps
      deprecated = NotDeprecated }


    // ── conflicts surface — the recorded, reviewable auto-resolutions (dark conflicts) ──
    { name = fn "pmConflictsList" 0
      typeParams = []
      parameters =
        [ Param.make
            "includeResolved"
            TBool
            "true = include acked/overridden (history); false = only pending" ]
      returnType =
        TList(
          TTuple(
            TString,
            TString,
            [ TString; TString; TString; TString; TString; TString ]
          )
        )
      description =
        "Recorded sync conflicts (auto-resolved name-binding divergences), one STRUCTURED tuple each
         for `dark conflicts` to format in Dark (so the display is package-testable + iterable):
         `(id, location, status, chosenHash, resolvedBy, localHash, incomingHash, remote)`. `status`
         is NEW/acked/overridden; `chosenHash` is which content won and `resolvedBy` the policy that
         picked it (e.g. `auto:last-writer-wins`); hashes are full (the Dark formatter shortens).
         `includeResolved=false` shows only pending (acked/overridden drop out, the ack-to-dismiss
         model). Empty if none match."
      fn =
        (function
        | _, _, _, [ DBool includeResolved ] ->
          uply {
            let! all = LibDB.Conflicts.list ()
            let conflicts =
              if includeResolved then
                all
              else
                all
                |> List.filter (fun (c : LibDB.Conflicts.Conflict) ->
                  not (c.acknowledged || c.overridden))
            let rows =
              conflicts
              |> List.map (fun c ->
                let status =
                  if c.overridden then "overridden"
                  elif c.acknowledged then "acked"
                  else "NEW"
                DTuple(
                  DString c.id,
                  DString c.location,
                  [ DString status
                    DString c.chosenHash
                    DString c.resolvedBy
                    DString c.localHash
                    DString c.incomingHash
                    DString c.remote ]
                ))
            return
              Dval.list
                (KTTuple(
                  VT.string,
                  VT.string,
                  [ VT.string
                    VT.string
                    VT.string
                    VT.string
                    VT.string
                    VT.string ]
                ))
                rows
          }
        | _ -> incorrectArgs ())
      sqlSpec = NotQueryable
      previewable = Impure
      capabilities = LibExecution.Capabilities.noCaps
      deprecated = NotDeprecated }


    { name = fn "pmSyncRecordDaemonEvent" 0
      typeParams = []
      parameters =
        [ Param.make "peersPolled" TInt64 "How many peers this cycle polled"
          Param.make "changed" TBool "Did any peer advance this cycle?"
          Param.make "conflicts" TInt64 "Divergences auto-resolved this cycle"
          Param.make "skews" TInt64 "Peers paused on a Release skew this cycle" ]
      returnType = TUnit
      description =
        "Record one autosync cycle's outcome to the local `sync_daemon_events` telemetry (trimmed to
         the most recent rows). Lets `sync events` and a dashboard view show daemon activity as data,
         not scraped log text."
      fn =
        (function
        | _, _, _, [ DInt64 peers; DBool changed; DInt64 conflicts; DInt64 skews ] ->
          uply {
            do!
              LibDB.Sync.recordDaemonEvent
                (int peers)
                changed
                (int conflicts)
                (int skews)
            return DUnit
          }
        | _ -> incorrectArgs ())
      sqlSpec = NotQueryable
      previewable = Impure
      capabilities = LibExecution.Capabilities.noCaps
      deprecated = NotDeprecated }


    { name = fn "pmSyncRecentDaemonEvents" 0
      typeParams = []
      parameters =
        [ Param.make "limit" TInt64 "How many recent cycles to return (newest first)" ]
      returnType = TList(TTuple(TString, TInt64, [ TBool; TInt64; TInt64 ]))
      description =
        "The autosync daemon's most recent poll cycles as STRUCTURED rows for `sync events` to format
         in Dark: `(at, peersPolled, changed, conflicts, skews)`, newest first. Empty if it never ran."
      fn =
        (function
        | _, _, _, [ DInt64 limit ] ->
          uply {
            let! rows = LibDB.Sync.recentDaemonEvents (int limit)
            let dvals =
              rows
              |> List.map (fun (at, peers, changed, conflicts, skews) ->
                DTuple(
                  DString at,
                  DInt64(int64 peers),
                  [ DBool(changed <> 0)
                    DInt64(int64 conflicts)
                    DInt64(int64 skews) ]
                ))
            return
              Dval.list
                (KTTuple(VT.string, VT.int64, [ VT.bool; VT.int64; VT.int64 ]))
                dvals
          }
        | _ -> incorrectArgs ())
      sqlSpec = NotQueryable
      previewable = Impure
      capabilities = LibExecution.Capabilities.noCaps
      deprecated = NotDeprecated }


    { name = fn "pmConflictAck" 0
      typeParams = []
      parameters =
        [ Param.make
            "idPrefix"
            TString
            "A recorded conflict's id (or a unique prefix of it)" ]
      returnType = TBool
      description =
        "Acknowledge a recorded conflict — 'the auto-resolution was right' (the common case). Matches
         by id or a unique id-prefix; returns true if exactly one matched and was acknowledged,
         false if none or ambiguous."
      fn =
        (function
        | _, _, _, [ DString idPrefix ] ->
          uply {
            let! conflicts = LibDB.Conflicts.list ()
            match conflicts |> List.filter (fun c -> c.id.StartsWith idPrefix) with
            | [ c ] ->
              do! LibDB.Conflicts.acknowledge c.id
              return DBool true
            | _ -> return DBool false
          }
        | _ -> incorrectArgs ())
      sqlSpec = NotQueryable
      previewable = Impure
      capabilities = LibExecution.Capabilities.noCaps
      deprecated = NotDeprecated }


    { name = fn "pmConflictAckAll" 0
      typeParams = []
      parameters = [ Param.make "unit" TUnit "" ]
      returnType = TInt64
      description =
        "Acknowledge ALL unacknowledged sync conflicts at once (the bulk 'the auto thing was right'
         path). Returns how many were newly acknowledged."
      fn =
        (function
        | _, _, _, [ DUnit ] ->
          uply {
            let! n = LibDB.Conflicts.acknowledgeAll ()
            return DInt64(int64 n)
          }
        | _ -> incorrectArgs ())
      sqlSpec = NotQueryable
      previewable = Impure
      capabilities = LibExecution.Capabilities.noCaps
      deprecated = NotDeprecated }


    { name = fn "pmConflictResolve" 0
      typeParams = []
      parameters =
        [ Param.make
            "idPrefix"
            TString
            "A recorded conflict's id (or a unique prefix)"
          Param.make
            "keepMine"
            TBool
            "true = re-bind to YOUR hash; false = keep the incoming" ]
      returnType = TBool
      description =
        "Override a recorded conflict's auto-resolution (by id or unique prefix). `keepMine` re-binds
         the location to your hash via a new WIP `SetName` op (op-log clean); false keeps the incoming
         (which already won). Marks it overridden. True if exactly one matched and was resolved."
      fn =
        (function
        | _, _, _, [ DString idPrefix; DBool keepMine ] ->
          uply {
            let! conflicts = LibDB.Conflicts.list ()
            match conflicts |> List.filter (fun c -> c.id.StartsWith idPrefix) with
            | [ c ] ->
              let! ok = LibDB.Sync.resolveConflict c.id keepMine
              return DBool ok
            | _ -> return DBool false
          }
        | _ -> incorrectArgs ())
      sqlSpec = NotQueryable
      previewable = Impure
      capabilities = LibExecution.Capabilities.noCaps
      deprecated = NotDeprecated } ]


let builtins () : Builtins = LibExecution.Builtin.make [] (fns ())
