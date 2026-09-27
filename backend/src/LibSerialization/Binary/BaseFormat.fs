/// Core binary format definitions and constants
module LibSerialization.Binary.BaseFormat

open System

/// v1 is the format the op-log substrate ships with; nothing older ever existed in
/// the wild (pre-v1 stores were rebuilt from `.dark` source each build). Bump on every
/// wire-layout change, keeping a readV1 beside the new writer: from here, stores
/// cannot be rebuilt from text.
/// v2 (traits, 2026-09): `bounds` on PackageFn and TypeDeclaration, `FQFnName.TraitMethod`
/// tag 2. Readers dispatch on the blob version; v1 blobs read with `bounds = []`.
/// v3 (traits, 2026-09): `Trait` and `TraitImpl` items, `AddTrait`/`AddTraitImpl` op tags 15
/// and 16, `Reference` tags 3 and 4. Nothing existing changed layout; a v2 blob reads as is.
[<Literal>]
let CurrentVersion = 3u

/// The oldest version this binary still reads.
[<Literal>]
let OldestReadableVersion = 1u

/// Binary file header structure (8 bytes)
type BinaryHeader =
  {
    // The blob's format version. Passed to version-dispatched readers (makeDeserializerV) so a new
    // binary can decode an OLD layout by branching on it: the keystone of any future format
    // migration. Bump `CurrentVersion` on the next wire-layout change and add the matching readVN.
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
    // Reject formats we cannot read rather than guessing how to parse them. Anything from
    // OldestReadableVersion up to CurrentVersion has a reader.
    if version < OldestReadableVersion || version > CurrentVersion then
      raise (BinaryFormatException(UnsupportedVersion version))

  let validateDataLength (expected : uint32) (actual : uint32) =
    if expected <> actual then
      raise (BinaryFormatException(DataLengthMismatch(expected, actual)))
