/// Core binary format definitions and constants
module LibBinarySerialization.Binary.BinaryFormat

open System

/// Magic header for Dark binary files
[<Literal>]
let MagicHeader = 0x4441524Bu // "DARK" in little-endian

/// Current binary format version
[<Literal>]
let CurrentVersion = 1u

/// Type discriminators for serialized data
type TypeId =
  | Expr = 1uy
  | PackageType = 2uy
  | PackageConstant = 3uy
  | PackageFn = 4uy
  | Toplevel = 5uy
  | Toplevels = 6uy

/// Flags for format options
[<Flags>]
type FormatFlags =
  | None = 0uy
  | Compressed = 1uy
  | Reserved1 = 2uy
  | Reserved2 = 4uy

/// Binary file header structure (16 bytes)
type BinaryHeader =
  { Magic : uint32 // 4 bytes - magic "DARK"
    Version : uint32 // 4 bytes - format version
    DataLength : uint32 // 4 bytes - payload size
    TypeId : TypeId // 1 byte  - type discriminator
    Flags : FormatFlags // 1 byte  - format flags
    Reserved : uint16 } // 2 bytes - reserved for future use

/// Validation errors for binary format
type BinaryFormatError =
  | InvalidMagicHeader of actual : uint32
  | UnsupportedVersion of version : uint32
  | InvalidTypeId of typeId : byte
  | CorruptedData of message : string
  | UnexpectedEndOfStream
  | DataLengthMismatch of expected : uint32 * actual : uint32

exception BinaryFormatException of BinaryFormatError

/// Constants for varint encoding
module Varint =
  [<Literal>]
  let MaxSingleByteValue = 127

  [<Literal>]
  let ContinuationBit = 0x80uy

  [<Literal>]
  let ValueMask = 0x7Fuy

/// Helper functions for format validation
module Validation =
  let validateMagicHeader (magic : uint32) =
    if magic <> MagicHeader then
      raise (BinaryFormatException(InvalidMagicHeader magic))

  let validateVersion (version : uint32) =
    if version > CurrentVersion then
      raise (BinaryFormatException(UnsupportedVersion version))

  let validateTypeId (typeId : byte) =
    if not (Enum.IsDefined(typeof<TypeId>, typeId)) then
      raise (BinaryFormatException(InvalidTypeId typeId))

  let validateDataLength (expected : uint32) (actual : uint32) =
    if expected <> actual then
      raise (BinaryFormatException(DataLengthMismatch(expected, actual)))
