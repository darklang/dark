/// THE authoring path: store ops on a branch's draft and fold them, in one transaction. Main is a
/// branch like any other here; the only difference is that a branch also records, per name it
/// touches, what its parent held there (`branch_name_bases`).
module LibDB.Inserts

open System.Threading.Tasks
open FSharp.Control.Tasks

open Microsoft.Data.Sqlite

open Prelude
open LibExecution.ProgramTypes

open Fumble
open LibDB.Sqlite
open LibDB.PreparedBatch

module PT = LibExecution.ProgramTypes
module BS = LibSerialization.Binary.Serialization
open LibSerialization.Hashing


/// The content-addressed id for a PackageOp. See `Hashing.computeOpRowId`, shared by every path
/// that mints or looks up an op id.
let computeOpHash (op : PT.PackageOp) : System.Guid = Hashing.computeOpRowId op


/// The authoring stamp. Defined once in `LibDB.OriginTs` -- see there for why it's monotonic and
/// why it must not be duplicated.
let nextOriginTs () : string = OriginTs.next ()

/// Main's draft, as a condition on `package_ops`: main's uncommitted ops.
let mainDraftWhere =
  $"branch_id = '{string PT.BranchId.Main}' AND commit_hash IS NULL"

/// Read one text column per row.
let private texts
  (ctx : Ctx)
  (sql : string)
  (bind : SqliteCommand -> unit)
  : Task<List<string * string>> =
  task {
    let cmd = command ctx sql
    cmd.Parameters.Clear()
    bind cmd
    use! reader = cmd.ExecuteReaderAsync()
    return
      reader
      |> Sqlite.readRows (fun r ->
        (r.GetString 0, (if r.IsDBNull 1 then "" else r.GetString 1)))
  }


// ------------------------------------------------------------------
// Restating
// ------------------------------------------------------------------

/// Which of <param ids> the log holds, on any branch.
let private heldOn (ctx : Ctx) (ids : List<System.Guid>) : Task<Set<System.Guid>> =
  task {
    if List.isEmpty ids then
      return Set.empty
    else
      let! rows =
        texts
          ctx
          "SELECT DISTINCT id, '' FROM package_ops WHERE id IN (SELECT value FROM json_each($ids))"
          (fun cmd -> p cmd "$ids" (jsonStrings (ids |> List.map string)))
      return rows |> List.map (fst >> System.Guid.Parse) |> Set.ofList
  }

/// What <param branch> sees at a name, or "" for nothing.
let private boundIn
  (ctx : Ctx)
  (branch : PT.BranchId)
  (loc : PT.PackageLocation)
  : Task<string> =
  task {
    let! found =
      textOption
        ctx
        (Layers.view "$" Layers.locations
         + " SELECT item_hash FROM visible
             WHERE owner = $owner AND modules = $modules AND name = $name")
        (fun cmd ->
          pLoc cmd loc
          Layers.bind cmd branch true)
    return Option.defaultValue "" found
  }

/// The text <param branch> sees for a name's doc part, if a name says one of its own.
let private docIn
  (ctx : Ctx)
  (branch : PT.BranchId)
  (loc : PT.PackageLocation)
  (part : PT.DocPart)
  : Task<Option<string>> =
  textOption
    ctx
    (Layers.view "$" Layers.docs
     + " SELECT text FROM visible
         WHERE owner = $owner AND modules = $modules AND name = $name
           AND kind = $kind AND within = $within")
    (fun cmd ->
      pLoc cmd loc
      p cmd "$kind" (Docs.kind part)
      p cmd "$within" (Docs.within part)
      Layers.bind cmd branch true)

/// The deprecation <param branch> sees standing on an item.
let private deprecationIn
  (ctx : Ctx)
  (branch : PT.BranchId)
  (target : PT.Reference)
  : Task<Option<PT.DeprecationKind * string>> =
  task {
    let (Hash h) = target.hash
    let! found =
      bytesOption
        ctx
        (Layers.view "$" Layers.deprecations
         + " SELECT CASE WHEN state = 'deprecated' THEN annotation_blob ELSE NULL END
             FROM visible WHERE item_hash = $item_hash AND item_kind = $item_kind")
        (fun cmd ->
          p cmd "$item_hash" h
          p cmd "$item_kind" (target.kind.toString ())
          Layers.bind cmd branch true)
    return found |> Option.bind Deprecations.deserializeAnnotation
  }

/// An op the log already holds can only be said again by a DISTINCT op: ids are content hashes, so
/// putting a name back to an earlier version, a doc back to earlier words, or a deprecation back
/// after an undeprecate, produces the op that already exists. Such an op is re-said with a fresh
/// stamp (`PT.restating`) when what it says is not what <param branch> currently sees, and dropped
/// when it is, which is a re-run of the same command.
let private restateOrDrop
  (ctx : Ctx)
  (branch : PT.BranchId)
  (ops : List<PT.PackageOp>)
  : Task<List<PT.PackageOp>> =
  task {
    let restatable =
      ops |> List.filter (fun op -> Option.isSome (PT.restating "" op))
    let! held = heldOn ctx (restatable |> List.map computeOpHash)
    let out = ResizeArray()
    for op in ops do
      if not (Set.contains (computeOpHash op) held) then
        out.Add op
      else
        let! saysSomethingNew =
          task {
            match op with
            | PT.PackageOp.SetName(loc, target, _) ->
              let! bound = boundIn ctx branch loc
              let (Hash h) = target.hash
              return bound <> h
            | PT.PackageOp.UpdateDoc(loc, part, text, _, _) ->
              let! said = docIn ctx branch loc part
              return said <> Some text
            | PT.PackageOp.Deprecate(target, kind, message, _) ->
              let! standing = deprecationIn ctx branch target
              return standing <> Some(kind, message)
            | PT.PackageOp.Undeprecate(target, _) ->
              let! standing = deprecationIn ctx branch target
              return standing <> None
            | _ -> return false
          }
        if saysSomethingNew then
          out.Add(PT.restating (nextOriginTs ()) op |> Option.defaultValue op)
    return List.ofSeq out
  }


// ------------------------------------------------------------------
// Name bases
// ------------------------------------------------------------------

/// The locations a set of ops rebinds or removes. An override counts: a name bound solely by
/// resolving a conflict still needs its base, or it could never conflict again.
let private rebound (ops : List<PT.PackageOp>) : List<PT.PackageLocation> =
  ops
  |> List.collect PT.packageOpBindingEffects
  |> List.choose (fun op ->
    match op with
    | PT.PackageOp.SetName(loc, _, _)
    | PT.PackageOp.Unbind(loc, _)
    | PT.PackageOp.Decision(_, loc, _, PT.DecisionKind.Override _) -> Some loc
    | _ -> None)
  |> List.distinct

/// Record, for each name <param ops> touches on <param branch>, what the branch's parent held there
/// ('' when nothing), once: the first touch is the fork point a later merge compares against.
let private recordNameBases
  (ctx : Ctx)
  (branch : PT.BranchId)
  (ops : List<PT.PackageOp>)
  : Task<unit> =
  task {
    for loc in rebound ops do
      do!
        exec
          ctx
          (Layers.view "$" Layers.locations
           + " INSERT OR IGNORE INTO branch_name_bases (branch_id, owner, modules, name, base_hash)
               SELECT $vb, $owner, $modules, $name,
                 COALESCE((SELECT item_hash FROM visible
                           WHERE owner = $owner AND modules = $modules AND name = $name), '')")
          (fun cmd ->
            pLoc cmd loc
            Layers.bind cmd branch false)
  }


// ------------------------------------------------------------------
// Authoring
// ------------------------------------------------------------------

/// Store <param ops> on <param branch>, on the caller's connection and inside its transaction, and
/// fold the ones that are new. Returns them. An op the branch already holds is skipped, except for
/// the names its body reached its callees through in THIS parse, which are recorded.
let storeAndFoldOn
  (ctx : Ctx)
  (branch : PT.BranchId)
  (source : string)
  (tsFor : System.Guid -> string)
  (commitFor : System.Guid -> Option<string>)
  (ops : List<PT.PackageOp>)
  : Task<List<PT.PackageOp>> =
  task {
    let inserted = ResizeArray<PT.PackageOp>()
    let ignored = ResizeArray<PT.PackageOp>()
    if not (List.isEmpty ops) then
      do! exec ctx AppliedOps.holdPending ignore<SqliteCommand>
    for op in ops do
      let opId = computeOpHash op
      let! n =
        execRows
          ctx
          "INSERT OR IGNORE INTO package_ops
             (id, branch_id, authored_on, op_blob, op_kind, origin_ts, commit_hash, source)
           VALUES ($id, $branch, $branch, $blob, $kind, $ts, $commit, $source)"
          (fun cmd ->
            pUuid cmd "$id" opId
            p cmd "$branch" (string branch)
            p cmd "$blob" (BS.PT.PackageOp.serialize opId op)
            p cmd "$kind" (BS.PT.PackageOp.kind op)
            p cmd "$ts" (tsFor opId)
            pOpt cmd "$commit" (commitFor opId)
            p cmd "$source" source)
      if n > 0 then inserted.Add op else ignored.Add op

    if not branch.IsMain then do! recordNameBases ctx branch (List.ofSeq inserted)

    do!
      Telemetry.timeTask "author.fold" [] (fun () ->
        PackageOpPlayback.applyOpsOnConnection ctx.conn branch (List.ofSeq inserted))
    if inserted.Count > 0 then
      do! exec ctx AppliedOps.coverAll ignore<SqliteCommand>
    do!
      PackageOpPlayback.recordDependenciesOnConnection ctx.conn (List.ofSeq ignored)
    return List.ofSeq inserted
  }

/// Run <param f> in one transaction on a connection of its own.
let inTransaction (f : Ctx -> Task<'a>) : Task<'a> =
  task {
    use conn = new SqliteConnection(LibDB.Sqlite.connString)
    do! conn.OpenAsync()
    do
      use pragma = conn.CreateCommand()
      pragma.CommandText <- "PRAGMA busy_timeout=5000;"
      Telemetry.count "sql.init"
      pragma.ExecuteNonQuery() |> ignore<int>
    use tx = conn.BeginTransaction()
    let ctx = newCtx conn
    try
      let! r = f ctx
      tx.Commit()
      Caching.invalidateAll ()
      return r
    finally
      disposeCtx ctx
  }

/// Record each repointed item's callees under every name its previous version reached them through.
///
/// Two names can hold one body, and a reload parses each and records the callees under both. A
/// repoint rewrites the one stored body, so the rows written from it have only that body's names;
/// this copies the rest from the previous version. <param edited> is the edit that started the
/// repoint, as (location, kind, previous hash, hash), one per previous hash.
let carryDependencyNames
  (edited : List<PT.PackageLocation * PT.ItemKind * Hash * Hash>)
  (repoints : List<PT.PropagateRepoint>)
  : Task<unit> =
  task {
    let moved =
      edited
      @ (repoints
         |> List.map (fun r ->
           (r.location, r.toRef.kind, r.fromRef.hash, r.toRef.hash)))
    let byLocation =
      moved
      |> List.fold
        (fun m (loc : PT.PackageLocation, kind : PT.ItemKind, _, Hash toHash) ->
          Map.add
            (kind.toString (), loc.owner, String.concat "." loc.modules, loc.name)
            toHash
            m)
        Map.empty
    let byHash =
      moved |> List.fold (fun m (_, _, Hash f, Hash t) -> Map.add f t m) Map.empty
    // A callee recorded without a name moves by hash, through every step: the edit, then the
    // re-hash of a cycle it is in.
    let rec latest (steps : int) (h : string) =
      match Map.tryFind h byHash with
      | Some next when next <> h && steps < List.length moved ->
        latest (steps + 1) next
      | _ -> h
    let froms =
      repoints
      |> List.map (fun r -> let (Hash h) = r.fromRef.hash in [ h ])
      |> List.distinct
    let! previous =
      Sql.query
        "SELECT d.item_hash, d.depends_on_hash, d.depends_on_item_type, d.depends_on_owner,
                d.depends_on_modules, d.depends_on_name
         FROM json_each(@froms) j JOIN package_dependencies d
           ON d.item_hash = json_extract(j.value, '$[0]')"
      |> Sql.parameters [ "froms", Sql.string (jsonRows froms) ]
      |> Sql.executeAsync (fun read ->
        ({ itemHash = read.string "item_hash"
           hash = read.string "depends_on_hash"
           kind = read.string "depends_on_item_type"
           owner = read.string "depends_on_owner"
           modules = read.string "depends_on_modules"
           name = read.string "depends_on_name" }
        : PackageOpPlayback.DependencyRow))
    let previousOf =
      previous
      |> List.groupBy (fun (r : PackageOpPlayback.DependencyRow) -> r.itemHash)
    let rows =
      repoints
      |> List.collect (fun r ->
        let (Hash fromHash) = r.fromRef.hash
        let (Hash toHash) = r.toRef.hash
        Map.tryFind fromHash previousOf
        |> Option.defaultValue []
        |> List.map (fun (row : PackageOpPlayback.DependencyRow) ->
          let callee =
            match
              Map.tryFind (row.kind, row.owner, row.modules, row.name) byLocation
            with
            | Some h -> h
            | None when row.owner = "" -> latest 0 row.hash
            | None -> row.hash
          { row with itemHash = toHash; hash = callee }))
    // A name the new body already reaches its callee through keeps what the body says. Copying the
    // previous version's row for that name too would leave a second row on the callee's old hash,
    // and a caller repointed twice in one save (once for each of two edited names it reaches) then
    // reads as still calling the old version.
    let tos =
      repoints
      |> List.map (fun r -> let (Hash h) = r.toRef.hash in [ h ])
      |> List.distinct
    let! own =
      Sql.query
        "SELECT d.item_hash, d.depends_on_item_type, d.depends_on_owner, d.depends_on_modules,
                d.depends_on_name
         FROM json_each(@tos) j JOIN package_dependencies d
           ON d.item_hash = json_extract(j.value, '$[0]')
         WHERE d.depends_on_owner <> ''"
      |> Sql.parameters [ "tos", Sql.string (jsonRows tos) ]
      |> Sql.executeAsync (fun read ->
        (read.string "item_hash",
         read.string "depends_on_item_type",
         read.string "depends_on_owner",
         read.string "depends_on_modules",
         read.string "depends_on_name"))
    let ownNames = Set.ofList own
    let rows =
      rows
      |> List.filter (fun (row : PackageOpPlayback.DependencyRow) ->
        row.owner = ""
        || not (
          Set.contains
            (row.itemHash, row.kind, row.owner, row.modules, row.name)
            ownNames
        ))
    if not (List.isEmpty rows) then
      do!
        inTransaction (fun ctx ->
          task { PackageOpPlayback.writeDependencies ctx.conn rows })
  }

/// Store <param ops> on <param branch>'s draft exactly as given, each stamped by <param tsFor>, and
/// fold them, in one transaction. No restating: for a caller replaying ops that already have their
/// identities and stamps. Returns how many were newly stored.
let authorWith
  (branch : PT.BranchId)
  (source : string)
  (tsFor : System.Guid -> string)
  (ops : List<PT.PackageOp>)
  : Task<int64> =
  inTransaction (fun ctx ->
    task {
      let! inserted = storeAndFoldOn ctx branch source tsFor (fun _ -> None) ops
      return int64 (List.length inserted)
    })

/// Author <param ops> onto <param branch>'s draft, stamped fresh, and fold them. Returns how many
/// were newly stored: an op the branch already holds counts 0. Saved reference names that do not
/// describe their declaration are refused here, whoever wrote them.
let authorOn
  (branch : PT.BranchId)
  (source : string)
  (ops : List<PT.PackageOp>)
  : Task<int64> =
  if List.isEmpty ops then
    Task.FromResult 0L
  else
    task {
      match! ReferenceNamesContent.validate ops with
      | Some reason -> return Exception.raiseInternal reason []
      | None ->
        return!
          inTransaction (fun ctx ->
            task {
              let! ops = restateOrDrop ctx branch ops
              // Stamps in list order, so sequential edits within one millisecond keep their order.
              let stamps =
                ops
                |> List.map (fun op -> computeOpHash op, nextOriginTs ())
                |> Map.ofList
              let! inserted =
                storeAndFoldOn
                  ctx
                  branch
                  source
                  (fun id ->
                    Map.tryFind id stamps |> Option.defaultWith nextOriginTs)
                  (fun _ -> None)
                  ops
              return int64 (List.length inserted)
            })
    }


let reservedOwnerViolation = OpValidation.reservedOwnerViolation

/// Detect a parser placeholder instead of a real content hash. Placeholders
/// are empty or contain the package location; real hashes contain only hex
/// digits and are produced during stabilization.
let private isPlaceholderHash (PT.Hash hash : PT.Hash) : bool =
  hash = "" || hash |> Seq.exists (fun c -> not (System.Uri.IsHexDigit c))

/// A rejection reason when a package operation still has a placeholder hash.
/// `None` means all hashes are content hashes.
let placeholderHashViolation (ops : List<PT.PackageOp>) : Option<string> =
  let hashesOf (op : PT.PackageOp) : List<PT.Hash> =
    match op with
    | PT.PackageOp.AddType t -> [ t.hash ]
    | PT.PackageOp.AddValue v -> [ v.hash ]
    | PT.PackageOp.AddFn f -> [ f.hash ]
    | PT.PackageOp.AddTrait t -> [ t.hash ]
    | PT.PackageOp.AddTraitImpl i -> [ i.hash ]
    | PT.PackageOp.AddTest test -> [ test.hash ]
    | PT.PackageOp.SetName(_, target, _)
    | PT.PackageOp.Rename(_, _, target, _, _) -> [ target.hash ]
    | _ -> []
  ops
  |> List.collect hashesOf
  |> List.tryFind isPlaceholderHash
  |> Option.map (fun (PT.Hash h) ->
    $"cannot store package ops with the placeholder hash \"{h}\"; stabilize them first (WrittenTypesToProgramTypes.stabilizePackageOps)")

let insertAndApplyOps (ops : List<PT.PackageOp>) : Task<int64> =
  authorOn PT.BranchId.Main "op" ops

let private insertCheckedOps
  (reservedOwnersAllowed : bool)
  (branch : PT.BranchId)
  (ops : List<PT.PackageOp>)
  : Task<Result<int64, string>> =
  task {
    // Reserved names on every branch: bundled code changes only by propagation, which does not come
    // through here, and by `insertBundledOps`. Placeholder hashes are main's question; a branch
    // repairs them before a merge.
    let reserved = if reservedOwnersAllowed then None else reservedOwnerViolation ops
    let violation =
      if branch.IsMain then
        Option.orElse (placeholderHashViolation ops) reserved
      else
        reserved
    match violation with
    | Some reason -> return Error reason
    | None ->
      match! ReferenceNamesContent.validate ops with
      | Some reason -> return Error reason
      | None ->
        let! count = authorOn branch "op" ops
        return Ok count
  }

/// Safely insert package operations submitted by RUNNING Dark code. On main, rejects protected
/// `Darklang` bindings and unstabilized hashes before insertion; trusted seeding does not come through
/// here.
let insertUntrustedOps (branch : PT.BranchId) (ops : List<PT.PackageOp>) =
  insertCheckedOps false branch ops

/// `insertUntrustedOps` that lets `Darklang` names change, for `LocalExec apply-package-edits`
/// alone: the dev build landing edits to `packages/`, the source a reload loads them from. No
/// `dark` process can reach it.
let insertBundledOps (branch : PT.BranchId) (ops : List<PT.PackageOp>) =
  insertCheckedOps true branch ops


/// Replace the ops <param ops0> of main's draft with <param ops>, but only if the draft still holds
/// exactly the rows `draftAsRead` lists, checked under the write lock. A caller that read the draft, worked on it, and then deleted
/// it would otherwise delete whatever another author added in between. `None` means the draft moved
/// and nothing was written; read it again.
let rewriteDraftIfUnchanged
  (draftAsRead : List<System.Guid>)
  (ops0 : List<PT.PackageOp>)
  (tsFor : System.Guid -> string)
  (ops : List<PT.PackageOp>)
  : Task<Option<int64>> =
  inTransaction (fun ctx ->
    task {
      let! current =
        texts
          ctx
          $"SELECT id, '' FROM package_ops WHERE {mainDraftWhere} ORDER BY rowid"
          (fun _ -> ())
      if (current |> List.map (fst >> System.Guid.Parse)) <> draftAsRead then
        return None
      else
        // Only what was read as an op: an op this build cannot decode is spared, not skipped, so a
        // build that can read it still will.
        let readable = ops0 |> List.map (computeOpHash >> string) |> jsonStrings
        let bindIds (cmd : SqliteCommand) = p cmd "$ids" readable
        do!
          Layers.dropRowsOf
            ctx
            PT.BranchId.Main
            "op_id IN (SELECT value FROM json_each($ids))"
            bindIds
        do!
          exec
            ctx
            $"DELETE FROM package_ops WHERE {mainDraftWhere}
                AND id IN (SELECT value FROM json_each($ids))"
            bindIds
        let! inserted =
          storeAndFoldOn ctx PT.BranchId.Main "op" tsFor (fun _ -> None) ops
        return Some(int64 (List.length inserted))
    })


/// Commit every currently-uncommitted MAIN op into one commit. The package RELOAD path uses this:
/// the `.dark` files on disk are the shipped baseline, not your uncommitted draft, so leaving them
/// uncommitted would open every `dark status` on "5,000 items changed".
///
/// The hash is derived from what it commits (message + count + newest stamp), so two instances that
/// reload the same packages compute the same id rather than inventing divergent ones.
///
/// DEV CAVEAT: a reload sweeps a genuine un-committed local draft into the baseline commit too.
/// That's tolerable only because reload is a dev-loop tool.
let commitAllAsBaseline (message : string) : Task<string> =
  inTransaction (fun ctx ->
    task {
      let! count =
        scalarInt
          ctx
          $"SELECT COUNT(*) FROM package_ops WHERE {mainDraftWhere}"
          (fun _ -> ())
      let! latest =
        textOption
          ctx
          $"SELECT MAX(origin_ts) FROM package_ops WHERE {mainDraftWhere}"
          (fun _ -> ())

      if count = 0L then
        return ""
      else
        let material = $"""{message}|{count}|{Option.defaultValue "" latest}"""
        let hash =
          material
          |> System.Text.Encoding.UTF8.GetBytes
          |> System.Security.Cryptography.SHA256.HashData
          |> System.Convert.ToHexString
          |> fun h -> h.Substring(0, 16).ToLowerInvariant()

        do!
          exec ctx "INSERT OR REPLACE INTO commits (hash, branch_id, message, origin_ts)
             VALUES ($hash, $branch, $message, $ts)" (fun cmd ->
            p cmd "$branch" (string PT.BranchId.Main)
            p cmd "$hash" hash
            p cmd "$message" message
            p cmd "$ts" (nextOriginTs ()))
        do!
          exec
            ctx
            $"UPDATE package_ops SET commit_hash = $hash WHERE {mainDraftWhere}"
            (fun cmd -> p cmd "$hash" hash)
        do!
          Layers.commitRowsOf
            ctx
            PT.BranchId.Main
            "op_id IN (SELECT id FROM package_ops WHERE branch_id = $branch AND commit_hash = $hash)"
            (fun cmd -> p cmd "$hash" hash)
        return hash
    })
