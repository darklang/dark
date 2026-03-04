/// Core binary format definitions and constants
module LibSerialization.Binary.BaseFormat

open System

[<Literal>]
let CurrentVersion = 1u

/// Binary file header structure (8 bytes)
type BinaryHeader =
  {
    // TODO: this seems useless? at least until we start shipping non-alpha versions.
    Version : uint32 // 4 bytes - format version
    DataLength : uint32 } // 4 bytes - payload size

/// Validation errors for binary format
type BinaryFormatError =
  | UnsupportedVersion of version : uint32
  | CorruptedData of message : string
  | UnexpectedEndOfStream
  | DataLengthMismatch of expected : uint32 * actual : uint32

exception BinaryFormatException of BinaryFormatError


/// Constants for varint encoding
///
/// "varint encoding" ~=
///   "when serializing integers that could be of a large size,
///     try to save some space if it's a small #"
///   e.g. storing `7` for a uint64 sholdn't take up a whole uint64's worth of bits...
module Varint =
  [<Literal>]
  let MaxSingleByteValue = 127

  [<Literal>]
  let ContinuationBit = 0x80uy

  [<Literal>]
  let ValueMask = 0x7Fuy


module Validation =
  let validateVersion (version : uint32) =
    if version > CurrentVersion then
      raise (BinaryFormatException(UnsupportedVersion version))

  let validateDataLength (expected : uint32) (actual : uint32) =
    if expected <> actual then
      raise (BinaryFormatException(DataLengthMismatch(expected, actual)))
