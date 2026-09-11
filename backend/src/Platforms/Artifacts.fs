/// Where a platform's executable lives on this machine, and what is checked before it runs.
///
/// The cache is keyed by the artifact's SHA-256, which is also the name the manifest gave it, so
/// two platforms shipping the same bytes share one file and a changed artifact cannot land on top
/// of the one it replaces. Nobody names a file: the hash is the name and the runtime owns the path.
module Platforms.Artifacts

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
