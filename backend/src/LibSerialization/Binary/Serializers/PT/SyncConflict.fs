module LibSerialization.Binary.Serializers.PT.SyncConflict

open System.IO
open Prelude

open LibExecution.ProgramTypes

open LibSerialization.Binary.Serializers.Common
open LibSerialization.Binary.Serializers.PT.Common

// `Reference` (hash + kind) already has a serializer next to `PackageOp` — reuse it so a Reference
// has ONE wire shape everywhere (an op's target and a conflict's candidate serialize identically).
module Reference = LibSerialization.Binary.Serializers.PT.PackageOp.Reference


// -- ResolvedBy --

module ResolvedBy =
  let write (w : BinaryWriter) (by : ResolvedBy) : unit =
    match by with
    | Auto policy ->
      w.Write(0uy)
      String.write w policy
    | Human -> w.Write(1uy)

  let read (r : BinaryReader) : ResolvedBy =
    match r.ReadByte() with
    | 0uy -> Auto(String.read r)
    | 1uy -> Human
    | b -> raiseFormatError $"Invalid ResolvedBy tag: {b}"


// -- DivergenceResolution --

module DivergenceResolution =
  let write (w : BinaryWriter) (res : DivergenceResolution) : unit =
    Reference.write w res.chosen
    ResolvedBy.write w res.by

  let read (r : BinaryReader) : DivergenceResolution =
    let chosen = Reference.read r
    let by = ResolvedBy.read r
    { chosen = chosen; by = by }


// -- SyncConflict --

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
