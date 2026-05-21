/// Runtime helpers for the `Blob` Dval type.
///
/// `Blob`s carry a `BlobRef`: ephemeral blobs hold their bytes inline
/// (lifetime is GC), persistent blobs hold a content hash backed by the
/// content-addressed `package_blobs` table. This module owns minting,
/// reading, and the leaf logic for ephemeral-to-persistent promotion
/// ([promoteEphemeralLeaf]).
///
/// Promotion runs at every persistence boundary; structural recursion
/// over the surrounding Dval shape is delegated to [Dval.rewriteWith] in
/// `RuntimeTypes.fs`. `Blob.promote` wires the leaf handler into
/// `rewriteWith` for the val-commit / User-DB-write path; the trace path
/// (`LibDB.Tracing.prepareDvalForStorage`) reuses the same leaf
/// handler alongside its own stream-stub rewrite.
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


/// Mint a fresh ephemeral blob holding [bytes] inline. Each mint gets a
/// fresh identity (`id`), so two ephemerals with the same bytes are
/// distinct values; the bytes are reachable only through the returned
/// Dval, so GC reclaims them when it's collected. No store, no scope,
/// and so no ExecutionState — minting needs no execution context.
///
/// Takes ownership of [bytes]: the array is stored as-is (not copied),
/// so the caller must not mutate it afterwards.
let newEphemeral (bytes : byte[]) : Dval =
  DBlob(Ephemeral { id = System.Guid.NewGuid(); bytes = bytes })


/// Resolve a BlobRef to its bytes. Ephemerals carry their bytes inline;
/// persistent refs hit package_blobs via the ExecutionState's blob
/// accessor. Shared by every builtin that dereferences a DBlob.
///
/// For ephemerals this returns the blob's own array, not a copy — treat
/// the result as read-only; mutating it mutates the blob.
let readBytes (state : ExecutionState) (ref : BlobRef) : Ply.Ply<byte[]> =
  uply {
    match ref with
    | Ephemeral eph -> return eph.bytes
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


/// [Dval.rewriteWith] leaf handler that promotes a `DBlob(Ephemeral _)`:
/// hash the inline bytes, persist them via [insert], and return
/// `Some(DBlob(Persistent _))`. Returns `None` for anything else so the
/// walker keeps descending. Can't fail to find bytes — they travel with
/// the value.
let promoteEphemeralLeaf
  (insert : string -> byte[] -> Ply.Ply<unit>)
  (dv : Dval)
  : Ply.Ply<Dval option> =
  uply {
    match dv with
    | DBlob(Ephemeral eph) ->
      let bs = eph.bytes
      let h = sha256Hex bs
      let n : int64 = System.Convert.ToInt64 bs.Length
      do! insert h bs
      return Some(DBlob(Persistent(h, n)))
    | _ -> return None
  }


/// Promote any ephemeral blobs reachable from [dv] to persistent:
/// hash the bytes (SHA-256), write them to the content-addressed
/// store via [insert], and swap the ref. Idempotent by design — the
/// insert path uses `INSERT OR IGNORE`, so promoting the same bytes
/// twice writes to `package_blobs` once.
///
/// Called before serializing a Dval through a non-trace persistence
/// boundary (val commit, User DB write). Plain `DBlob` writers
/// (binary, JSON) still raise on ephemeral — promote first, serialize
/// second. The trace path has its own walker in
/// `LibDB.Tracing.prepareDvalForStorage` that also stubs streams; it
/// reuses [promoteEphemeralLeaf] but does not call this fn.
///
/// Structural recursion (including into DApplicable closures) is
/// delegated to [Dval.rewriteWith] — we only describe the leaf substitution
/// here. A lambda closing over an ephemeral blob has its capture
/// environment promoted along with the rest of the value graph.
let promote
  (insert : string -> byte[] -> Ply.Ply<unit>)
  (dv : Dval)
  : Ply.Ply<Dval> =
  dv |> Dval.rewriteWith (promoteEphemeralLeaf insert)
