module LibDB.RuntimeTypes

open Prelude
open LibExecution.RuntimeTypes

open Microsoft.Data.Sqlite
open Fumble
open LibDB.Sqlite

module RT = LibExecution.RuntimeTypes
module BS = LibSerialization.Binary.Serialization


/// Bracket a package-item load, splitting "the query" from "turning bytes into a runtime value": different
/// problems with different fixes, and an aggregate can't tell them apart.
///
/// Gated: `Stopwatch.GetTimestamp()` costs ~1.27us on an HPET clocksource, so this is not something to
/// pay when nobody is measuring.
module private LoadTiming =
  let inline enabled () = Telemetry.isEnabled ()

  let inline now (on : bool) =
    if on then System.Diagnostics.Stopwatch.GetTimestamp() else 0L

  let record (on : bool) (kind : string) (t0 : int64) (t1 : int64) (t2 : int64) =
    if on then
      let toUs (a : int64) (b : int64) =
        (b - a) * 1_000_000L / System.Diagnostics.Stopwatch.Frequency
      Telemetry.addUs $"pkg.{kind}.sql" (toUs t0 t1)
      Telemetry.addUs $"pkg.{kind}.deserialize" (toUs t1 t2)


module Type =
  let get (hash : Hash) : Ply<Option<RT.PackageType.PackageType>> =
    uply {
      Telemetry.count "pkg.type.get"
      let (Hash hashStr) = hash
      let on = LoadTiming.enabled ()
      let t0 = LoadTiming.now on
      let! bytes =
        Sql.query
          """
          SELECT rt_def
          FROM package_types
          WHERE hash = @hash
          """
        |> Sql.parameters [ "hash", Sql.string hashStr ]
        |> Sql.executeRowOptionAsync (fun read -> read.bytes "rt_def")
      let t1 = LoadTiming.now on
      let result = bytes |> Option.map (BS.RT.PackageType.deserialize hash)
      let t2 = LoadTiming.now on
      LoadTiming.record on "type" t0 t1 t2
      return result
    }


module Value =
  /// The evaluated runtime value at <param hash>, or None if there's no such value OR it exists but hasn't
  /// been evaluated yet. Unlike types/functions (rt_def/rt_instrs are NOT NULL), `rt_dval` is NULL until a
  /// value is evaluated — and a value arrives unevaluated whenever it's folded from an op (e.g. pulled from a
  /// peer) before the next grow pass, or if its evaluation errored. The `IS NOT NULL` guard makes that read
  /// return None instead of throwing "data is NULL at ordinal 0" on the NULL blob; the caller treats a
  /// not-yet-evaluated value the same as an absent one (grow populates it before it's needed in the happy path).
  let get (hash : Hash) : Ply<Option<RT.PackageValue.PackageValue>> =
    uply {
      Telemetry.count "pkg.value.get"
      let (Hash hashStr) = hash
      let on = LoadTiming.enabled ()
      let t0 = LoadTiming.now on
      let! bytes =
        Sql.query
          """
          SELECT rt_dval
          FROM package_values
          WHERE hash = @hash AND rt_dval IS NOT NULL
          """
        |> Sql.parameters [ "hash", Sql.string hashStr ]
        |> Sql.executeRowOptionAsync (fun read -> read.bytes "rt_dval")
      let t1 = LoadTiming.now on
      let result = bytes |> Option.map (BS.RT.PackageValue.deserialize hash)
      let t2 = LoadTiming.now on
      LoadTiming.record on "value" t0 t1 t2
      return result
    }

  /// Find all value hashes that have the given ValueType (exact match)
  let findByValueType (vt : RT.ValueType) : Ply<List<Hash>> =
    uply {
      let vtBytes = BS.RT.ValueType.serialize vt
      return!
        Sql.query
          """
          SELECT hash
          FROM package_values
          WHERE value_type = @value_type
          """
        |> Sql.parameters [ "value_type", Sql.bytes vtBytes ]
        |> Sql.executeAsync (fun read -> Hash(read.string "hash"))
    }


module Fn =
  let get (hash : Hash) : Ply<Option<RT.PackageFn.PackageFn>> =
    uply {
      let (Hash hashStr) = hash
      // One query and one deserialize PER FUNCTION, demand-driven. Counted because the per-item cost only
      // matters once you know the item count -- see `Telemetry.count`.
      Telemetry.count "pkg.fn.get"
      // Gated: on an HPET clocksource a timestamp costs ~1.27us, so three per load is not something to
      // pay when nobody is measuring.
      let on = LoadTiming.enabled ()
      let t0 = LoadTiming.now on
      let! bytes =
        Sql.query
          """
          SELECT rt_instrs
          FROM package_functions
          WHERE hash = @hash
          """
        |> Sql.parameters [ "hash", Sql.string hashStr ]
        |> Sql.executeRowOptionAsync (fun read -> read.bytes "rt_instrs")
      let t1 = LoadTiming.now on
      let result = bytes |> Option.map (BS.RT.PackageFn.deserialize hash)
      let t2 = LoadTiming.now on
      LoadTiming.record on "fn" t0 t1 t2
      return result
    }


/// Content-addressed blob storage — bytes keyed by SHA-256 hash.
module Blob =
  /// Look up bytes by hash. Returns [None] when the row doesn't exist.
  let get (hash : string) : Ply<Option<byte[]>> =
    uply {
      Telemetry.count "pkg.blob.get"
      return!
        Sql.query
          """
          SELECT bytes
          FROM package_blobs
          WHERE hash = @hash
          """
        |> Sql.parameters [ "hash", Sql.string hash ]
        |> Sql.executeRowOptionAsync (fun read -> read.bytes "bytes")
    }

  /// Insert bytes under [hash]. If the row already exists (same hash
  /// = same content, by content-addressing invariant), this is a no-op
  /// — `INSERT OR IGNORE` handles dedup.
  let insert (hash : string) (bytes : byte[]) : Ply<unit> =
    uply {
      let! _ =
        Sql.query
          """
          INSERT OR IGNORE INTO package_blobs (hash, length, bytes)
          VALUES (@hash, @length, @bytes)
          """
        |> Sql.parameters
          [ "hash", Sql.string hash
            "length", Sql.int64 (int64 bytes.Length)
            "bytes", Sql.bytes bytes ]
        |> Sql.executeNonQueryAsync
      return ()
    }

  /// Base64-decode [b64], verify the bytes actually hash to [hash], and store under it. Returns whether it
  /// stored. False on empty input, un-decodable base64, or a hash mismatch — the last is the integrity core
  /// of a content-addressed store: without it a peer could serve arbitrary bytes for a legitimate hash and
  /// poison the store (a value silently becomes different code) for every branch that references it. Total:
  /// a hostile/garbled peer body must not throw.
  let insertVerified (hash : string) (b64 : string) : Ply<bool> =
    uply {
      if b64 = "" then
        return false
      else
        match
          (try
            Some(System.Convert.FromBase64String b64)
           with _ ->
             None)
        with
        | None -> return false
        | Some bytes ->
          if LibExecution.Blob.sha256Hex bytes = hash then
            do! insert hash bytes
            return true
          else
            return false
    }

  /// Of some offered content hashes, which this store LACKS — sync's fetch-on-miss (blobs don't ride the
  /// op stream). Content-addressed, so a hash we have is identical content; only genuinely-absent ones need
  /// fetching.
  let missing (hashes : List<string>) : Ply<List<string>> =
    uply {
      let! present =
        Sql.query "SELECT hash FROM package_blobs"
        |> Sql.executeAsync (fun read -> read.string "hash")
      let presentSet = Set.ofList present
      return hashes |> List.filter (fun h -> not (Set.contains h presentSet))
    }

  /// Every content hash this store holds — the sync blob MANIFEST (sender side of fetch-on-miss).
  let allHashes () : Ply<List<string>> =
    uply {
      return!
        Sql.query "SELECT hash FROM package_blobs"
        |> Sql.executeAsync (fun read -> read.string "hash")
    }


  /// Walk a Dval tree and collect every `Persistent` blob hash it
  /// references. Ephemeral blobs aren't rows in `package_blobs` — they
  /// live in the per-ExecutionState byte-store and don't need sweeping.
  let private collectBlobHashes (dv : RT.Dval) : Set<string> =
    let rec go (acc : Set<string>) (dv : RT.Dval) : Set<string> =
      match dv with
      | RT.DBlob(RT.Persistent(hash, _)) -> Set.add hash acc
      | RT.DBlob(RT.Ephemeral _) -> acc
      | RT.DStream _
      | RT.DUnit
      | RT.DBool _
      | RT.DInt8 _
      | RT.DUInt8 _
      | RT.DInt16 _
      | RT.DUInt16 _
      | RT.DInt32 _
      | RT.DUInt32 _
      | RT.DInt64 _
      | RT.DUInt64 _
      | RT.DInt128 _
      | RT.DUInt128 _
      | RT.DInt _
      | RT.DFloat _
      | RT.DChar _
      | RT.DString _
      | RT.DDateTime _
      | RT.DUuid _
      | RT.DApplicable _
      | RT.DDB _ -> acc
      | RT.DList(_, items) -> items |> List.fold go acc
      | RT.DTuple(a, b, rest) ->
        let acc = go acc a
        let acc = go acc b
        rest |> List.fold go acc
      | RT.DDict(_, entries) -> entries |> Map.values |> Seq.fold go acc
      | RT.DRecord(_, _, _, fields) -> fields |> Map.values |> Seq.fold go acc
      | RT.DEnum(_, _, _, _, fields) -> fields |> List.fold go acc
    go Set.empty dv


  /// Delete `package_blobs` rows whose hashes aren't referenced by any
  /// materialised Dval in `package_values.rt_dval`. Returns the count
  /// of rows deleted.
  ///
  /// Intentionally narrow: only scans `package_values`. Other tables
  /// that might later hold Dvals (User DB rows, `trace_data`) will
  /// need their own reference-collection pass.
  ///
  /// Idempotent: re-running after a clean sweep deletes nothing. Safe
  /// to run while the system is live — worst-case race is a concurrent
  /// promote racing the delete, which the foreign-key-style orphan
  /// check prevents (content-addressed re-insert is cheap).
  ///
  /// For a package set with N values and M blobs, cost is O(N+M)
  /// deserialise passes plus one DELETE per orphan. Good enough for
  /// CLI-triggered sweeps at current scale; a reverse-index table
  /// is the natural next step when the DB grows past it.
  let sweepOrphans () : Ply<int64> =
    uply {
      // Pull every materialised rt_dval — deserialise and collect
      // hashes referenced anywhere in the tree.
      let! valueRows =
        Sql.query
          """
          SELECT hash, rt_dval
          FROM package_values
          WHERE rt_dval IS NOT NULL
          """
        |> Sql.executeAsync (fun r -> (r.string "hash", r.bytes "rt_dval"))

      let referenced : Set<string> =
        valueRows
        |> List.fold
          (fun acc (valueHash, rtDvalBytes) ->
            try
              let pv = BS.RT.PackageValue.deserialize (Hash valueHash) rtDvalBytes
              Set.union acc (collectBlobHashes pv.body)
            with _ ->
              // Corrupt / stale row — don't let one bad row block the
              // sweep; skip and carry on.
              acc)
          Set.empty

      // List of candidate hashes in storage.
      let! allHashes =
        Sql.query "SELECT hash FROM package_blobs"
        |> Sql.executeAsync (fun r -> r.string "hash")

      let orphans =
        allHashes |> List.filter (fun h -> not (Set.contains h referenced))

      for h in orphans do
        do!
          Sql.query "DELETE FROM package_blobs WHERE hash = @hash"
          |> Sql.parameters [ "hash", Sql.string h ]
          |> Sql.executeStatementAsync

      return int64 (List.length orphans)
    }
