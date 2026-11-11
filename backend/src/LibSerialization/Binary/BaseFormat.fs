/// Core binary format definitions and constants
module LibSerialization.Binary.BaseFormat

open System

/// Only this binary format is supported. Bump it when the wire layout changes.
[<Literal>]
let CurrentVersion = 11u

/// Binary file header structure (8 bytes)
type BinaryHeader =
  {
    /// The format expected by this build. Older and newer formats are rejected.
    Version : uint32 // 4 bytes - format version
    DataLength : uint32
  } // 4 bytes - payload size

/// Validation errors for binary format
type BinaryFormatError =
  | UnsupportedVersion of version : uint32
  | CorruptedData of message : string
  | UnexpectedEndOfStream
  | DataLengthMismatch of expected : uint32 * actual : uint32

exception BinaryFormatException of BinaryFormatError with
  /// Spelled out: the generated message is `%A`, which a release build cannot print
  /// properly, and this one reaches people through every `ex.Message` that reports a
  /// blob it could not read.
  override this.Message =
    match this.Data0 with
    | UnsupportedVersion version -> $"unsupported binary format version {version}"
    | CorruptedData message -> $"corrupted binary data: {message}"
    | UnexpectedEndOfStream -> "binary data ended early"
    | DataLengthMismatch(expected, actual) ->
      $"binary data length mismatch: expected {expected} bytes, found {actual}"


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
    if version <> CurrentVersion then
      raise (BinaryFormatException(UnsupportedVersion version))

  let validateDataLength (expected : uint32) (actual : uint32) =
    if expected <> actual then
      raise (BinaryFormatException(DataLengthMismatch(expected, actual)))
