/// Getting BYTES across the platform wire.
///
/// The at-rest `Dval` format refuses an ephemeral blob, and the reason is a good one: at rest,
/// bytes have to be addressable, and an ephemeral blob's bytes live inline with nowhere to point.
/// A frame is not at rest. It is consumed the moment it arrives and the receiver mints fresh
/// bytes, so the frame carries the bytes itself.
///
/// The envelope, ahead of every frame in both directions: a varint count, then that many entries of
/// a hash (length-prefixed string), a varint byte count, and the bytes. After it comes the payload,
/// in exactly the encoding it had before this existed, with every blob appearing as a PERSISTENT
/// reference. A receiver rehydrates any reference whose hash is in the table.
///
/// Keyed by content hash rather than by position, which buys two things for free. The same blob
/// passed twice is one entry. And a hash that is NOT in the table is still meaningful: it is a
/// reference to bytes the receiver is expected to already hold, which is what the host does when it
/// reads its own store.
///
/// Persistent blobs are inlined too, which is a real cost worth naming: sending a large stored blob
/// to a platform copies it through a pipe. That is inherent to another process, not to this
/// encoding, and the alternative is handing a plugin access to the store.
module LibDB.PlatformWire

open System.IO
open Prelude

module RT = LibExecution.RuntimeTypes
module Blob = LibExecution.Blob
module Varint = LibSerialization.Binary.Serializers.Common.Varint
module WireString = LibSerialization.Binary.Serializers.Common.String

/// Bytes that travel beside a frame, by hash.
type Table = Map<string, byte[]>

/// Replace every blob in a value with a persistent reference, collecting the bytes to send beside
/// it. Ephemeral blobs are hashed here; persistent ones are read from the store, because the far
/// side has no store to read them from.
let collect
  (state : RT.ExecutionState)
  (values : List<RT.Dval>)
  : Ply.Ply<Table * List<RT.Dval>> =
  uply {
    let mutable table : Table = Map.empty

    let substitute (dv : RT.Dval) : Ply.Ply<Option<RT.Dval>> =
      uply {
        match dv with
        | RT.DBlob ref ->
          let! bytes = Blob.readBytes state ref
          let hash =
            match ref with
            | RT.Persistent(hash, _) -> hash
            | RT.Ephemeral _ -> Blob.sha256Hex bytes
          table <- Map.add hash bytes table
          return Some(RT.DBlob(RT.Persistent(hash, int64 bytes.Length)))
        | _ -> return None
      }

    let! rewritten = values |> Ply.List.mapSequentially (RT.Dval.rewriteWith substitute)
    return (table, rewritten)
  }

/// Turn references back into bytes, for every hash the sender sent along.
///
/// A reference the table does not mention is left alone rather than treated as an error: it names
/// bytes the receiver was expected to already have, and whether it does is the store's question,
/// asked later and answered with the store's own message.
let rehydrate (table : Table) (dv : RT.Dval) : Ply.Ply<RT.Dval> =
  if Map.isEmpty table then
    Ply.Ply dv
  else
    dv
    |> RT.Dval.rewriteWith (fun d ->
      match d with
      | RT.DBlob(RT.Persistent(hash, _)) ->
        match Map.tryFind hash table with
        | Some bytes -> Ply.Ply(Some(Blob.newEphemeral bytes))
        | None -> Ply.Ply None
      | _ -> Ply.Ply None)

let writeTable (w : BinaryWriter) (table : Table) : unit =
  Varint.write w (Map.count table)
  for KeyValue(hash, bytes) in table do
    WireString.write w hash
    Varint.write w bytes.Length
    w.Write bytes

let readTable (r : BinaryReader) : Table =
  let count = Varint.read r
  let mutable table : Table = Map.empty
  for _ in 1..count do
    let hash = WireString.read r
    let length = Varint.read r
    table <- Map.add hash (r.ReadBytes length) table
  table
