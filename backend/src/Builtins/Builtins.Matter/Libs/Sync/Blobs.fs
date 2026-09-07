/// The content-addressed blob channel: `package_blobs` (a value's large content) don't ride the op stream,
/// so after applying a peer's ops the puller fetches the blobs it lacks. Internal machinery under `Darklang.Sync.*`.
///
/// PER-BRANCH SYNC — the factors, for whoever scopes this later (B5). Today everything is wholesale: the op
/// wire sends every branch's ops and the manifest offers every hash. Scoping content to a subset of branches
/// is not one filter; these all move together:
///   1. The blob manifest is the hard one, and the reason wholesale is the honest default. A blob is
///      CONTENT-ADDRESSED, so "which branch does this hash belong to" is NOT a property of the blob — the same
///      bytes are the same hash on every branch that references them. To scope blobs you have to compute the
///      set transitively: the ops on the wanted branches -> the value hashes they bind -> the blobs those
///      values reference. Offer the whole manifest and a "private" branch's large values leak by hash (this is
///      exactly why #5684's `private` control was pulled).
///   2. The op wire (`wireSince`) filters `package_ops`/`resolutions` by branch_id — a request parameter for
///      "which branches", and the server trusting or checking it.
///   3. Branch structure (`branch_ops`) still syncs WHOLESALE even when content is scoped: a puller that takes
///      branch X's ops must be able to resolve the branch_ids those ops reference, or the FK aborts the fold.
///   4. Cursors become per-(peer, branch, log) instead of per-(peer, log), or paging changes shape.
/// The `CLEANUP(per-branch-sync)` markers in sync.dark + server.dark point back here.
module Builtins.Matter.Libs.Sync.Blobs

open FSharp.Control.Tasks

open Prelude
open LibExecution.RuntimeTypes
open LibExecution.Builtin.Shortcuts
open LibExecution.Effects

module Dval = LibExecution.Dval


let fns () : List<BuiltInFn> =
  [
    // Sender: the blob MANIFEST — every content hash this instance holds, newline-joined (GET /sync/blobs).
    { name = fn "packageBlobManifest" 0
      typeParams = []
      parameters = [ Param.make "unit" TUnit "" ]
      returnType = TString
      description =
        "The blob manifest (the GET /sync/blobs body): every content hash this "
        + "instance holds, newline-joined."
      fn =
        (function
        | _, _, _, [| DUnit |] ->
          uply {
            let! hashes = LibDB.RuntimeTypes.Blob.allHashes ()
            return DString(String.concat "\n" hashes)
          }
        | _ -> incorrectArgs ())
      sqlSpec = NotQueryable
      previewable = Impure
      callEffects = set [ Effect.PackageRead ]
      deprecated = NotDeprecated }

    // Sender: the bytes for one hash, base64 (GET /sync/blob?hash=), or empty if this instance lacks it.
    { name = fn "packageBlobBytes" 0
      typeParams = []
      parameters = [ Param.make "hash" TString "The content hash to fetch" ]
      returnType = TString
      description =
        "The bytes for one content hash, base64-encoded (the GET /sync/blob?hash= "
        + "body), or empty if this instance lacks it."
      fn =
        (function
        | _, _, _, [| DString hash |] ->
          uply {
            match! LibDB.RuntimeTypes.Blob.get hash with
            | Some bytes -> return DString(System.Convert.ToBase64String bytes)
            | None -> return DString ""
          }
        | _ -> incorrectArgs ())
      sqlSpec = NotQueryable
      previewable = Impure
      callEffects = set [ Effect.PackageRead ]
      deprecated = NotDeprecated }

    // Receiver: of a peer's offered hashes, which this instance LACKS — exactly the blobs to fetch.
    { name = fn "packageBlobMissing" 0
      typeParams = []
      parameters =
        [ Param.make
            "hashes"
            (TList TString)
            "A peer's offered content hashes (its manifest)" ]
      returnType = TList TString
      description =
        "Of the peer's offered content hashes, which this instance lacks — a pure "
        + "content-addressed set-difference (no cursor)."
      fn =
        (function
        | _, _, _, [| DList(_, hashDvals) |] ->
          uply {
            let hashes =
              hashDvals
              |> List.choose (fun d ->
                match d with
                | DString s -> Some s
                | _ -> None)
            let! missing = LibDB.RuntimeTypes.Blob.missing hashes
            return Dval.list KTString (missing |> List.map DString)
          }
        | _ -> incorrectArgs ())
      sqlSpec = NotQueryable
      previewable = Impure
      callEffects = set [ Effect.PackageRead ]
      deprecated = NotDeprecated }

    // Receiver: store a fetched blob — base64-decode + insert under its content hash. Idempotent (dedup).
    { name = fn "packageBlobInsert" 0
      typeParams = []
      parameters =
        [ Param.make "hash" TString "The content hash"
          Param.make
            "base64Bytes"
            TString
            "The blob's bytes, base64-encoded (empty = skip)" ]
      returnType = TBool
      description =
        "Store a fetched blob: base64-decode + insert under its content hash. "
        + "Idempotent. Returns true if non-empty bytes were inserted, false if the "
        + "peer's body was empty."
      fn =
        (function
        | _, _, _, [| DString hash; DString b64 |] ->
          uply {
            // The integrity check (bytes must hash to the claimed hash) lives with the store in
            // LibDB.Blob.insertVerified — the builtin just bridges Dval -> call -> Dval.
            let! stored = LibDB.RuntimeTypes.Blob.insertVerified hash b64
            return DBool stored
          }
        | _ -> incorrectArgs ())
      sqlSpec = NotQueryable
      previewable = Impure
      callEffects = set [ Effect.PackageWrite ]
      deprecated = NotDeprecated } ]


let builtins () = LibExecution.Builtin.make [] (fns ())
