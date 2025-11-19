/// Content-addressed hashing utilities for Darklang
///
/// Provides deterministic Guid generation by hashing serialized content.
/// Used for stable package item IDs based on location or content.
module LibSerialization.Hashing.ContentHash

open System
open System.IO
open System.Security.Cryptography

/// Compute SHA256 hash of bytes
let hashBytes (bytes : byte[]) : byte[] = SHA256.HashData(ReadOnlySpan(bytes))

/// Convert hash bytes to Guid (using first 16 bytes of hash)
let hashToGuid (hashBytes : byte[]) : Guid =
  if hashBytes.Length < 16 then
    Exception.raiseInternal
      "Hash too short for Guid conversion"
      [ "length", hashBytes.Length ]

  Guid(hashBytes[0..15])


/// Serialize a value using a binary writer function, then hash it to a Guid
///
/// This is the core pattern: whatever can be written to binary can be hashed.
/// The binary format is the source of truth for what makes something "the same".
let hashWithWriter<'T> (writer : BinaryWriter -> 'T -> unit) (value : 'T) : Guid =
  use ms = new MemoryStream()
  use bw = new BinaryWriter(ms)

  writer bw value

  let bytes = ms.ToArray()
  let hash = hashBytes bytes
  hashToGuid hash


/// Hash PackageLocations to stable Guids
module PackageLocation =
  open LibSerialization.Binary

  /// Compute a stable Guid for a PackageLocation
  ///
  /// This is used for definition IDs - the identity of a package item
  /// is determined by where it lives (owner/modules/name), not its content.
  let hash (loc : LibExecution.ProgramTypes.PackageLocation) : Guid =
    hashWithWriter Serializers.PT.Common.PackageLocation.write loc


/// Hash PackageOps to stable Guids (for deduplication)
module PackageOp =
  open LibSerialization.Binary

  /// Compute a content-addressed ID for a PackageOp
  ///
  /// This is used for deduplication - the same op content gets the same ID.
  /// Originally from PT/SQL/OpPlayback.fs, centralized here.
  let hash (op : LibExecution.ProgramTypes.PackageOp) : Guid =
    hashWithWriter Serializers.PT.PackageOp.write op
