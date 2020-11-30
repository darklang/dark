module LibExecution.SharedTypes

// Some fundamental types that we want to use everywhere.

// DO NOT define any serialization on these types. If you want to serialize
// them, you should move these to the files with specific formats and serialize
// them there.

// fsharplint:disable FL0039

open Prelude

type pos = { x : int; y : int }

type tlid = int64
type id = int64
type CanvasID = System.Guid
type UserID = System.Guid

let id (x : int) : id = int64 x
