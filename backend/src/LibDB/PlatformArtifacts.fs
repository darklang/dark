/// Where a platform's executable lives on this machine, and what is checked before it runs.
///
/// The cache is keyed by the artifact's SHA-256, which is also the name the manifest gave it, so
/// two platforms shipping the same bytes share one file and a changed artifact cannot land on top
/// of the one it replaces. Nobody names a file: the hash is the name and the runtime owns the path.
module LibDB.PlatformArtifacts

open System
open Prelude

module Blob = LibExecution.Blob

/// The cache lives beside the policy rather than in the package database.
///
/// An executable on disk is host state, not package content: it is per machine, it is per target,
/// and an instance restored from a backup should not arrive with a directory of foreign binaries.
/// The package database holds the NAME (the hash, in the manifest); the bytes are a local cache of
/// something re-fetchable.
let private directoryName = "platforms"

let private cacheDirectory () : Result<string, string> =
  LibExecution.HostSecurity.policyDirectory ()
  |> Result.map (fun policy -> IO.Path.Combine(policy, directoryName))

/// Where the bytes with this hash would live. Not a promise that they are there.
let path (hash : string) : Result<string, string> =
  // The hash is the whole filename, so a caller cannot pass a path fragment and walk out of the
  // cache. Checked rather than trusted, since the hash reaches here from a manifest.
  if hash.Length = 64 && hash |> Seq.forall (fun c -> Char.IsAsciiDigit c || (c >= 'a' && c <= 'f')) then
    cacheDirectory () |> Result.map (fun dir -> IO.Path.Combine(dir, hash))
  else
    Error $"'{hash}' is not a SHA-256, so it cannot name an artifact"

/// Is the artifact with this hash already on disk AND still the bytes it claims to be?
///
/// Verified on every materialization rather than only on install. The cheap version of this trusts
/// the filename, which means anything that can write the cache directory can swap the executable
/// that a hash approved. Hashing a file we are about to EXECUTE is not the place to save the read.
let verified (hash : string) : Result<bool, string> =
  match path hash with
  | Error e -> Error e
  | Ok file ->
    if not (IO.File.Exists file) then
      Ok false
    else
      try
        Ok(Blob.sha256Hex (IO.File.ReadAllBytes file) = hash)
      with e ->
        Error $"could not read the cached artifact: {e.Message}"

/// Put bytes in the cache under their own hash, refusing any that are not what they claim.
///
/// Returns the path, so the caller never constructs one. The bytes are checked BEFORE the write,
/// so a mismatched artifact never reaches the disk under a name that would later be trusted.
let materialize (hash : string) (bytes : byte[]) : Result<string, string> =
  let actual = Blob.sha256Hex bytes
  if actual <> hash then
    Error $"artifact does not match its hash: expected {hash}, got {actual}"
  else
    match path hash with
    | Error e -> Error e
    | Ok file ->
      try
        IO.Directory.CreateDirectory(IO.Path.GetDirectoryName file)
        |> ignore<IO.DirectoryInfo>
        // Written next to its destination and renamed, so a crash mid-write cannot leave a
        // truncated file under a hash that says it is complete.
        let temp = file + ".partial"
        IO.File.WriteAllBytes(temp, bytes)
        if OperatingSystem.IsLinux() || OperatingSystem.IsMacOS() then
          IO.File.SetUnixFileMode(
            temp,
            IO.UnixFileMode.UserRead ||| IO.UnixFileMode.UserWrite
            ||| IO.UnixFileMode.UserExecute
          )
        IO.File.Move(temp, file, true)
        Ok file
      with e ->
        Error $"could not write the artifact: {e.Message}"


/// Where artifact bytes can be found, given a hash.
///
/// A function rather than a concrete source, for the same reason `External.Invoke` and the manifest
/// type lookup are: this module should not have an opinion about transport. The local blob store is
/// one source; a relay fetch would be another, and neither needs this code to change.
type Source = string -> Ply.Ply<Option<byte[]>>

/// The local package store as a source. Bytes somebody already has.
let fromStore (getBlob : string -> Ply.Ply<Option<byte[]>>) : Source = getBlob

/// Make sure the artifact with this hash is on disk, fetching it if it is not.
///
/// The cache is consulted first and VERIFIED, not merely tested for existence, so a swapped file is
/// refetched rather than trusted. Then the source, then a check of what the source returned, then
/// the write. At no point does anything unverified reach a path that gets executed.
let ensure (source : Source) (hash : string) : Ply.Ply<Result<string, string>> =
  uply {
    match verified hash with
    | Error e -> return Error e
    | Ok true ->
      // Already here and still itself.
      return path hash
    | Ok false ->
      match! source hash with
      | None ->
        return
          Error
            $"no artifact with hash {hash} is available here. It may not have been fetched yet, or this platform may not build for this machine."
      | Some bytes -> return materialize hash bytes
  }
