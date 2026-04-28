module LibPackageManager.RuntimeTypes

open Prelude
open LibExecution.RuntimeTypes

open Microsoft.Data.Sqlite
open Fumble
open LibDB.Db

module RT = LibExecution.RuntimeTypes
module BS = LibSerialization.Binary.Serialization


module Type =
  let get (hash : Hash) : Ply<Option<RT.PackageType.PackageType>> =
    uply {
      let (Hash hashStr) = hash
      return!
        Sql.query
          """
          SELECT rt_def
          FROM package_types
          WHERE hash = @hash
          """
        |> Sql.parameters [ "hash", Sql.string hashStr ]
        |> Sql.executeRowOptionAsync (fun read -> read.bytes "rt_def")
        |> Task.map (Option.map (BS.RT.PackageType.deserialize hash))
    }


module Value =
  let get (hash : Hash) : Ply<Option<RT.PackageValue.PackageValue>> =
    uply {
      let (Hash hashStr) = hash
      return!
        Sql.query
          """
          SELECT rt_dval
          FROM package_values
          WHERE hash = @hash
          """
        |> Sql.parameters [ "hash", Sql.string hashStr ]
        |> Sql.executeRowOptionAsync (fun read -> read.bytes "rt_dval")
        |> Task.map (Option.map (BS.RT.PackageValue.deserialize hash))
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
      return!
        Sql.query
          """
          SELECT rt_instrs
          FROM package_functions
          WHERE hash = @hash
          """
        |> Sql.parameters [ "hash", Sql.string hashStr ]
        |> Sql.executeRowOptionAsync (fun read -> read.bytes "rt_instrs")
        |> Task.map (Option.map (BS.RT.PackageFn.deserialize hash))
    }


/// Content-addressed blob storage — bytes keyed by SHA-256 hash.
module Blob =
  /// Look up bytes by hash. Returns [None] when the row doesn't exist.
  let get (hash : string) : Ply<Option<byte[]>> =
    uply {
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
  /// For a canvas with N package values and M blobs, cost is O(N+M)
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
