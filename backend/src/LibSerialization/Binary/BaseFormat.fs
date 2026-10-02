/// Core binary format definitions and constants
module LibSerialization.Binary.BaseFormat

open System

/// The format this build WRITES. v1 is the format the op-log substrate ships with; nothing older
/// ever existed in the wild (pre-v1 stores were rebuilt from `.dark` source each build). Bump on
/// every wire-layout change, keeping a readV1 beside the new writer: from here, stores cannot be
/// rebuilt from text.
///
/// `DARK_FORMAT_VERSION` overrides it, and exists for one reason: there is only one format so far,
/// so the store migrator has nothing to migrate between and no way to be exercised. With it set,
/// this build writes the version named and reads every version up to it -- a SYNTHETIC bump whose
/// layout happens to be identical, which makes a migration across it a pure blob rewrite. That is
/// the format-only case, and having it mechanised and tested before the first real bump is the
/// point. Never set in production; `LibDB.StoreUpgrade` is what uses it.
///
/// A plain `let` rather than a `[<Literal>]` for that reason. Nothing pattern-matches on it.
let currentVersion : uint32 =
  match System.Environment.GetEnvironmentVariable "DARK_FORMAT_VERSION" with
  | null
  | "" -> 1u
  | s ->
    match System.UInt32.TryParse s with
    | true, n when n >= 1u -> n
    | _ -> 1u

/// Binary file header structure (8 bytes)
type BinaryHeader =
  {
    // The blob's format version. Passed to version-dispatched readers (makeDeserializerV) so a new
    // binary can decode an OLD layout by branching on it: the keystone of any future format
    // migration. Bump `currentVersion` on the next wire-layout change and add the matching readVN.
    Version : uint32 // 4 bytes - format version
    DataLength : uint32 } // 4 bytes - payload size

/// Validation errors for binary format
type BinaryFormatError =
  | UnsupportedVersion of version : uint32
  | CorruptedData of message : string
  | UnexpectedEndOfStream
  | DataLengthMismatch of expected : uint32 * actual : uint32

exception BinaryFormatException of BinaryFormatError


/// Varint: small values in fewer bytes; high bit marks continuation.
module Varint =
  [<Literal>]
  let MaxSingleByteValue = 127

  [<Literal>]
  let ContinuationBit = 0x80uy

  [<Literal>]
  let ValueMask = 0x7Fuy


module Validation =
  let validateVersion (version : uint32) =
    // OLDER is fine, NEWER is not, and the asymmetry is the whole point of a versioned header.
    //
    // A blob from an older format can be read: every historical reader stays in the binary, and
    // `makeDeserializerV` hands the version to the reader so it can branch. That is how a store
    // moves forward without being rebuilt from text, which after the flip is the only way it can
    // move at all.
    //
    // A blob from a NEWER format cannot be read by trying harder -- the layout is one this binary
    // has never seen -- so it is refused rather than guessed at. `LibDB.Releases` refuses the whole
    // STORE for the same reason, before anything gets as far as a blob.
    if version = 0u || version > currentVersion then
      raise (BinaryFormatException(UnsupportedVersion version))

  let validateDataLength (expected : uint32) (actual : uint32) =
    if expected <> actual then
      raise (BinaryFormatException(DataLengthMismatch(expected, actual)))
