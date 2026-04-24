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
/// See thinking/blobs-and-streams/00-design.md.
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
