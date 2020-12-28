module LibExecution.SharedTypes

// Some fundamental types that we want to use everywhere.

// DO NOT define any serialization on these types. If you want to serialize
// them, you should move these to the files with specific formats and serialize
// them there.

// fsharplint:disable FL0039

open Prelude

type pos = { x : int; y : int }

// We use an explicit sign for Floats, instead of making it implicit in the
// first digit, because otherwise we lose the sign on 0, and can't represent
// things like -0.5
type Sign =
  | Positive
  | Negative

type tlid = uint64
type id = uint64
type CanvasID = System.Guid
type UserID = System.Guid

let id (x : int) : id = uint64 x
