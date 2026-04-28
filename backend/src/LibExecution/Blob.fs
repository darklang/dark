/// Runtime helpers for the `Blob` Dval type.
///
/// `Blob`s carry a `BlobRef` pointing at byte content held either
/// in-process (Ephemeral) or in the content-addressed `package_blobs`
/// table (Persistent). This module owns the byte-store mechanics
/// (mint, read, scope-based reclaim) and the
/// ephemeral-to-persistent promotion that runs at every persistence
/// boundary.
///
/// The walk implementation (`promoteBlobs`) recurses through every
/// Dval shape because a blob can be nested arbitrarily deep —
/// rebuilding containers along the way. The traversal lives here
/// rather than in `Dval.fs` because it's specifically a blob
/// operation; Dval shape-walking is just the means.
module LibExecution.Blob

open Prelude

open LibExecution.RuntimeTypes


/// SHA-256 of zero bytes. The well-known canonical hash backing
/// `Builtin.blobEmpty`; short-circuited in [readBlobBytes] because
/// Microsoft.Data.Sqlite/Fumble's `read.bytes` on a zero-length BLOB
/// column returns None (not Some [||]) even when the row exists.
/// We could seed the row via migration or LocalExec, but the read
/// would still fail — empty-blob handling has to live here. Future
/// non-empty blob constants don't have this problem and could be
/// seeded via LocalExec at startup.
let emptyHash = "e3b0c44298fc1c149afbf4c8996fb92427ae41e4649b934ca495991b7852b855"


/// SHA-256 hex digest of the given bytes — content-addressing for
/// blob promotion.
let sha256Hex (bytes : byte[]) : string =
  use sha = System.Security.Cryptography.SHA256.Create()
  let digest = sha.ComputeHash(bytes)
  System.Convert.ToHexStringLower(digest)


/// Mint a fresh ephemeral blob: register the bytes in the exeState's
/// blob store and return a DBlob that references them. Caller retains
/// no direct handle on the byte[] past this point.
///
/// If a blob-scope is active (see [pushScope]), the new UUID is
/// recorded in the top scope so [popScope] can reclaim the bytes
/// when the scope exits. Runs without an active scope (CLI, tests)
/// leave the bytes in the ExecutionState for its full lifetime.
let newEphemeral (exeState : ExecutionState) (bytes : byte[]) : Dval =
  let id = System.Guid.NewGuid()
  exeState.blobStore[id] <- bytes
  if exeState.blobScopes.Count > 0 then
    exeState.blobScopes.Peek().Add(id) |> ignore<bool>
  DBlob(Ephemeral id)


/// Push a fresh, empty blob-scope onto the stack. Each subsequent
/// [newEphemeral] records its UUID in this scope until it's popped.
/// Used by long-lived VMs (http-server) around per-handler work so
/// ephemeral blobs don't accumulate across requests.
let pushScope (exeState : ExecutionState) : unit =
  exeState.blobScopes.Push(System.Collections.Generic.HashSet<System.Guid>())


/// Pop the top blob-scope: drop every UUID it tracked from
/// `blobStore`. Safe to call without a push (no-op on empty stack).
/// Caller should wrap `pushScope` / `popScope` in a `try/finally` so
/// failures don't leak blob bytes.
///
/// Blobs promoted to `Persistent` inside this scope are unaffected —
/// promotion writes to `package_blobs` (a separate durable table)
/// and the DBlob reference swaps to `Persistent`. We only drop the
/// in-memory ephemeral byte cache.
let popScope (exeState : ExecutionState) : unit =
  if exeState.blobScopes.Count > 0 then
    let scope = exeState.blobScopes.Pop()
    for id in scope do
      exeState.blobStore.TryRemove(id) |> ignore<bool * byte[]>


/// Resolve a BlobRef to its bytes. Ephemerals read from the VM store;
/// persistent refs hit package_blobs via the ExecutionState's blob
/// accessor. Shared by every builtin that dereferences a DBlob.
let readBytes (state : ExecutionState) (ref : BlobRef) : Ply.Ply<byte[]> =
  uply {
    match ref with
    | Ephemeral id ->
      let mutable bs : byte[] = null
      if state.blobStore.TryGetValue(id, &bs) then
        return bs
      else
        return Exception.raiseInternal "ephemeral blob not found" [ "id", id ]
    | Persistent(hash, _length) when hash = emptyHash -> return [||]
    | Persistent(hash, _length) ->
      let! got = state.blobs.get hash
      match got with
      | Some bs -> return bs
      | None ->
        return
          Exception.raiseInternal
            "persistent blob missing in package_blobs"
            [ "hash", hash ]
  }


/// Promote any ephemeral blobs reachable from [dv] to persistent:
/// hash the bytes (SHA-256), write them to the content-addressed
/// store via [insert], and swap the ref. Idempotent by design — the
/// insert path uses `INSERT OR IGNORE`, so promoting the same bytes
/// twice writes to `package_blobs` once.
///
/// Called before serializing a Dval through any persistence boundary
/// (val commit, User DB write, trace capture). Plain `DBlob` writers
/// (binary, JSON) still raise on ephemeral — promote first, serialize
/// second.
///
/// TODO LATENT BUG: `Tracing.fs` (storeTraceInput / storeFnResult) does
/// NOT call this before serializing through DvalReprInternalRoundtrippable.
/// A captured trace holding a `DBlob(Ephemeral _)` deserialises in a
/// fresh VM with the UUID intact but no bytes in that VM's blobStore,
/// so the next `readBytes` raises "ephemeral blob not found".
/// Fix: thread a `promoteForCapture : Dval -> Ply<Dval>` through the
/// Tracing.T record, wrap each storeXXX in promote-then-serialize.
///
/// CLEANUP rebuilds container Dvals (Map.toList → walk → Map.ofList)
/// even when no descendant blob promoted. Alloc-cheap in practice
/// (~75KB regardless of input size), but a "did anything change"
/// short-circuit would skip the round-trip in the common case.
let promote
  (exeState : ExecutionState)
  (insert : string -> byte[] -> Ply.Ply<unit>)
  (dv : Dval)
  : Ply.Ply<Dval> =
  uply {
    let rec go (dv : Dval) : Ply.Ply<Dval> =
      uply {
        match dv with
        | DBlob(Ephemeral id) ->
          let mutable bs : byte[] = null
          if exeState.blobStore.TryGetValue(id, &bs) then
            let h = sha256Hex bs
            let n : int64 = System.Convert.ToInt64 bs.Length
            do! insert h bs
            return DBlob(Persistent(h, n))
          else
            return
              Exception.raiseInternal
                "Ephemeral blob not found in store during promotion"
                [ "id", id ]
        | DBlob(Persistent _)
        | DUnit
        | DBool _
        | DInt8 _
        | DUInt8 _
        | DInt16 _
        | DUInt16 _
        | DInt32 _
        | DUInt32 _
        | DInt64 _
        | DUInt64 _
        | DInt128 _
        | DUInt128 _
        | DFloat _
        | DChar _
        | DString _
        | DDateTime _
        | DUuid _
        | DApplicable _
        | DDB _
        | DStream _ -> return dv
        | DList(vt, items) ->
          let! items' = items |> Ply.List.mapSequentially go
          return DList(vt, items')
        | DDict(vt, entries) ->
          let! entries' =
            entries
            |> Map.toList
            |> Ply.List.mapSequentially (fun (k, v) ->
              uply {
                let! v' = go v
                return (k, v')
              })
          return DDict(vt, Map.ofList entries')
        | DTuple(a, b, rest) ->
          let! a' = go a
          let! b' = go b
          let! rest' = rest |> Ply.List.mapSequentially go
          return DTuple(a', b', rest')
        | DRecord(src, rt, typeArgs, fields) ->
          let! fields' =
            fields
            |> Map.toList
            |> Ply.List.mapSequentially (fun (k, v) ->
              uply {
                let! v' = go v
                return (k, v')
              })
          return DRecord(src, rt, typeArgs, Map.ofList fields')
        | DEnum(src, rt, typeArgs, caseName, fields) ->
          let! fields' = fields |> Ply.List.mapSequentially go
          return DEnum(src, rt, typeArgs, caseName, fields')
      }
    return! go dv
  }
