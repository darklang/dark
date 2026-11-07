/// Runtime helpers for the `Bytes` Dval type.
///
/// `Bytes` values carry a `BytesRef`: ephemeral bytes hold their bytes inline
/// (lifetime is GC), persistent blobs hold a content hash backed by the
/// content-addressed `package_blobs` table. This module owns minting,
/// reading, and the leaf logic for ephemeral-to-persistent promotion
/// ([promoteEphemeralLeaf]).
///
/// Promotion runs at every persistence boundary; structural recursion
/// over the surrounding Dval shape is delegated to [Dval.rewriteWith] in
/// `RuntimeTypes.fs`. `Bytes.promote` wires the leaf handler into
/// `rewriteWith` for the val-commit path; the trace path
/// (`LibDB.Tracing.prepareDvalForStorage`) reuses the same leaf
/// handler alongside its own stream-stub rewrite.
module LibExecution.Bytes

open Prelude

open LibExecution.RuntimeTypes


/// SHA-256 of zero bytes. The well-known canonical hash backing
/// `Builtin.bytesEmpty`; short-circuited in [readBytes] because
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
  System.Convert.ToHexStringLower(System.Security.Cryptography.SHA256.HashData bytes)


/// Keep bytes in memory without hashing or saving them yet. The content hash
/// is computed once if equality, ordering, hashing, or persistence needs it.
/// The inspection id does not affect value equality. GC reclaims the bytes
/// when the value is no longer reachable; no execution context is needed.
///
/// Takes ownership of [bytes]: the array is stored as-is (not copied),
/// so the caller must not mutate it afterwards.
let newEphemeral (bytes : byte[]) : Dval =
  DBytes(
    Ephemeral
      { id = System.Guid.NewGuid()
        buffer = bytes
        offset = 0
        count = bytes.Length
        hash = null }
  )


/// Slices of this many bytes or fewer are copied rather than shared. A copy this
/// small costs about what a view does, and keeps nothing else alive.
let sliceCopyAtMost = 64

/// A slice is also copied when it is under 1/[sliceShareRatio] of the array it would
/// share, so a small piece cannot keep a much bigger one alive: a view never holds
/// more than this many times its own size. Repeated slicing still copies at most
/// once per factor of this ratio the value shrinks by, so the work stays linear.
let sliceShareRatio = 8


/// [count] bytes of [parent] from [start], which the caller has already clamped to
/// the parent. Shares the parent's array unless the piece is small, absolutely or
/// next to that array (see [sliceCopyAtMost] and [sliceShareRatio]).
let sliceEphemeral (parent : EphemeralBytes) (start : int) (count : int) : Dval =
  if start = 0 && count = parent.count then
    DBytes(Ephemeral parent)
  elif
    count <= sliceCopyAtMost
    || int64 count * int64 sliceShareRatio < int64 parent.buffer.Length
  then
    newEphemeral (parent.Span.Slice(start, count).ToArray())
  else
    DBytes(
      Ephemeral
        { id = System.Guid.NewGuid()
          buffer = parent.buffer
          offset = parent.offset + start
          count = count
          hash = null }
    )


/// Resolve a BytesRef to its bytes. Ephemerals carry their bytes inline;
/// persistent refs hit package_blobs via the ExecutionState's blob
/// accessor. Shared by every builtin that dereferences a DBytes.
///
/// For ephemerals this returns the blob's own array, not a copy — treat
/// the result as read-only; mutating it mutates the blob. A slice that shares a
/// bigger array gets a copy of its own bytes instead (see
/// [EphemeralBytes.ToArray]); builtins that only read a span match `Ephemeral`
/// first and skip that.
let readBytes (state : ExecutionState) (ref : BytesRef) : Ply.Ply<byte[]> =
  uply {
    match ref with
    | Ephemeral eph -> return eph.ToArray()
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


/// `readBytes`, then `k`, without a builder when the bytes are in hand. An ephemeral blob
/// (nearly every blob a builtin sees: a string just encoded, a body just received) carries
/// its bytes, so `k` runs at once and what it returns is the answer; only a persistent blob
/// enters `uply` to wait for the store. For a builtin body, so the common path is a value.
let withBytes
  (state : ExecutionState)
  (ref : BytesRef)
  (k : byte[] -> Ply.Ply<Dval>)
  : Ply.Ply<Dval> =
  match ref with
  | Ephemeral eph -> k (eph.ToArray())
  | Persistent(hash, _) when hash = emptyHash -> k [||]
  | Persistent _ ->
    uply {
      let! bytes = readBytes state ref
      return! k bytes
    }


/// [Dval.rewriteWith] leaf handler that promotes a `DBytes(Ephemeral _)`:
/// hash the inline bytes, persist them via [insert], and return
/// `Some(DBytes(Persistent _))`. Returns `None` for anything else so the
/// walker keeps descending. Can't fail to find bytes — they travel with
/// the value.
let promoteEphemeralLeaf
  (insert : string -> byte[] -> Ply.Ply<unit>)
  (dv : Dval)
  : Ply.Ply<Dval option> =
  uply {
    match dv with
    | DBytes(Ephemeral eph) ->
      // A view saves its own bytes, not the array it shares.
      let bs = eph.ToArray()
      let h = eph.ContentHash
      let n : int64 = System.Convert.ToInt64 bs.Length
      do! insert h bs
      return Some(DBytes(Persistent(h, n)))
    | _ -> return None
  }


/// Promote any ephemeral blobs reachable from [dv] to persistent:
/// hash the bytes (SHA-256), write them to the content-addressed
/// store via [insert], and swap the ref. Idempotent by design — the
/// insert path uses `INSERT OR IGNORE`, so promoting the same bytes
/// twice writes to `package_blobs` once.
///
/// Called before serializing a Dval through a non-trace persistence
/// boundary (val commit). Plain `DBytes` writers
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
