module LibSerialization.Binary.Serializers.PT.SyncConflict

open System.IO
open Prelude

open LibExecution.ProgramTypes

open LibSerialization.Binary.Serializers.Common
open LibSerialization.Binary.Serializers.PT.Common

// `Reference` (hash + kind) already has a serializer next to `PackageOp` — reuse it so a Reference
// has ONE wire shape everywhere (an op's target and a conflict's candidate serialize identically).
module Reference = LibSerialization.Binary.Serializers.PT.PackageOp.Reference


// -- SyncConflict --
// (only `SyncConflict` is serialized — it backs the recorded `conflict_blob`. A resolution is stored
//  flattened to columns, `chosen_hash` + `resolved_by`, so `DivergenceResolution` has no wire form.)

let write (w : BinaryWriter) (conflict : SyncConflict) : unit =
  match conflict with
  | Divergence(location, candidates) ->
    w.Write(0uy)
    PackageLocation.write w location
    List.write w Reference.write candidates

let read (r : BinaryReader) : SyncConflict =
  match r.ReadByte() with
  | 0uy ->
    let location = PackageLocation.read r
    let candidates = List.read r Reference.read
    Divergence(location, candidates)
  | b -> raiseFormatError $"Invalid SyncConflict tag: {b}"
