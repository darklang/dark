/// Getting BYTES across the platform wire.
///
/// The at-rest `Dval` format refuses an ephemeral blob, and the reason is a good one: at rest,
/// bytes have to be addressable, and an ephemeral blob's bytes live inline with nowhere to point.
/// A frame is not at rest. It is consumed the moment it arrives and the receiver mints fresh
/// bytes, so the frame carries the bytes itself.
///
/// The envelope, ahead of every frame in both directions: a varint count, then that many entries of
/// a hash (length-prefixed string), a varint byte count, and the bytes. After it comes the payload,
/// in the ordinary at-rest encoding, with every blob appearing as a PERSISTENT reference. A
/// receiver rehydrates any reference whose hash is in the table.
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

/// Values a platform may not MINT, however well formed the frame is.
///
/// The wire can express every `Dval`, but a platform is not this runtime and some values are not
/// data: they are handles whose meaning is a resource here. A `DDB` names a user database; a
/// `DApplicable` names a function to call. Neither was handed to the platform and neither can be,
/// so either one arriving is a forged handle rather than an answer.
///
/// The type checker does not catch this. `Dval.toValueType` maps `DDB` to `ValueType.Unknown`,
/// which is honest, since the element type cannot be recovered from a table name, and which
/// unifies with everything. So a manifest promising `String` and a platform returning a `DDB` type
/// checks, and the confusion surfaces much later as a .NET exception from deep inside a builtin.
///
/// Same rule as `DStream`, and the same reason: what cannot honestly cross does not cross. Applied
/// only on the way IN, because a signature that could carry one out is already refused at install
/// by `Manifest.travels`.
let rec private forgedHandle (dv : RT.Dval) : Option<string> =
  match dv with
  | RT.DDB name ->
    Some
      $"named the database '{name}', which is a handle this runtime hands out and a platform cannot be given"
  | RT.DApplicable _ ->
    Some "returned a function, which names code in this runtime rather than data"
  | RT.DList(_, items) -> items |> List.tryPick forgedHandle
  | RT.DTuple(first, second, rest) ->
    (first :: second :: rest) |> List.tryPick forgedHandle
  | RT.DDict(_, _, entries) ->
    entries |> Map.toList |> List.tryPick (snd >> forgedHandle)
  | RT.DRecord(_, _, _, fields) ->
    fields |> Map.toList |> List.tryPick (snd >> forgedHandle)
  | RT.DEnum(_, _, _, _, fields) -> fields |> List.tryPick forgedHandle
  | _ -> None

/// Refuse a forged handle anywhere in a value a platform sent back.
///
/// Recursive, because a `DDB` inside a list or a record field is the same forgery with one more
/// step: checking only the top level would be a check somebody could walk around by wrapping.
let refuseForgedHandles (platformName : string) (dv : RT.Dval) : Result<unit, string> =
  match forgedHandle dv with
  | None -> Ok()
  | Some what -> Error $"the {platformName} platform {what}"

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
