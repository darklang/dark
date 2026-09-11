/// Which external platforms this instance has installed, and how to turn them into `Platform`s.
///
/// Host-owned and not synced, beside the policy and the activation file, for the same reason those
/// are: installing somebody else's executable is a decision about THIS machine. The manifest it
/// points at is content and travels; the decision to have it does not.
///
/// The record is a name and a manifest hash. Everything else is in the manifest, and the manifest
/// is in the artifact cache under that hash, so one line is enough to reconstruct a platform.
module Platforms.Installed

open System
open Prelude

module PT = LibExecution.ProgramTypes
module Platform = LibExecution.Platform
module Blob = LibExecution.Blob
module LocalFile = LibDB.LocalFile

let private fileName = "installed"
let private header = "DARK-PLATFORM-INSTALLS 1"

/// Platform name to the hash of the manifest it was installed from.
///
/// The hash is what makes this a pin rather than a note. A manifest changing underneath an install
/// is a different platform claiming the same name, and recording the hash is what lets anything
/// downstream notice.
type Installs = Map<string, string>

let parse (text : string) : Installs =
  let lines =
    text.Split('\n')
    |> Array.map (fun line -> line.Trim())
    |> Array.filter (fun line -> line <> "" && not (line.StartsWith "#"))
    |> List.ofArray
  match lines with
  | first :: rest when first = header ->
    rest
    |> List.choose (fun line ->
      match line.Split(' ') |> Array.filter (fun p -> p <> "") |> List.ofArray with
      | [ name; hash ] -> Some(name, hash)
      | _ -> None)
    |> Map.ofList
  // Same fail-closed posture as the activation file: a header we cannot read means no installs
  // rather than every install, because failing open here would run somebody's binary on the
  // strength of a corrupted byte.
  | _ -> Map.empty

let render (installs : Installs) : string =
  let lines =
    installs |> Map.toList |> List.map (fun (name, hash) -> $"{name} {hash}")
  String.concat "\n" (header :: lines) + "\n"

let get () : Installs =
  match LocalFile.read fileName with
  | LocalFile.Missing -> Map.empty
  | LocalFile.Read bytes ->
    try
      parse (Text.Encoding.UTF8.GetString bytes)
    with _ ->
      Map.empty
  | LocalFile.Unreadable _ -> Map.empty

let private write (installs : Installs) : unit =
  LocalFile.writeAtomic fileName (Text.Encoding.UTF8.GetBytes(render installs))

/// One locked read-modify-write, the shape `PolicyStore.update` and `Activation.modify` use, and
/// for the same reason: reading outside the lock loses one of two concurrent installs.
let private modify (change : Installs -> Installs) : unit =
  LocalFile.withExclusiveLock (fun () -> write (change (get ())))

let add (name : string) (manifestHash : string) : unit =
  modify (Map.add name manifestHash)

let remove (name : string) : unit = modify (Map.remove name)

/// Install from manifest TEXT: cache the manifest under its own hash, then record it.
///
/// The manifest is cached the same way an artifact is, because it is the same kind of thing: bytes
/// addressed by their hash. That means one cache, one verification path, and an install that can be
/// reconstructed from the recorded hash alone.
let install
  (pm : PT.PackageManager)
  (manifestText : string)
  : Ply.Ply<Result<string * Platform.External.Manifest, Platform.External.Rejection>> =
  uply {
    let bytes = Text.Encoding.UTF8.GetBytes manifestText
    let hash = Blob.sha256Hex bytes

    match Platform.Written.parse manifestText with
    | Error problems ->
      return Error { manifest = "(unparsed)"; problems = problems }
    | Ok written ->
      match! Install.resolve pm written with
      | Error rejection -> return Error rejection
      | Ok manifest ->
        match Artifacts.materialize hash bytes with
        | Error e ->
          return
            Error
              { manifest = Platform.External.Manifest.coordinate manifest
                problems = [ $"could not cache the manifest: {e}" ] }
        | Ok _ ->
          add manifest.name hash
          return Ok(hash, manifest)
  }

/// Rebuild the installed platforms, for composing into a set.
///
/// Anything that cannot be rebuilt is SKIPPED with its reason rather than raising: an instance with
/// one broken install should still start, and the broken one should be visible rather than fatal.
/// The reasons come back so a caller can print them.
let platforms
  (pm : PT.PackageManager)
  (rid : string)
  : Ply.Ply<List<Platform.Platform> * List<string * string>> =
  uply {
    let mutable built = []
    let mutable skipped = []

    for (name, manifestHash) in Map.toList (get ()) do
      match Artifacts.path manifestHash with
      | Error e -> skipped <- skipped @ [ (name, e) ]
      | Ok manifestFile ->
        if not (IO.File.Exists manifestFile) then
          skipped <- skipped @ [ (name, "its manifest is not in the cache") ]
        else
          let text = IO.File.ReadAllText manifestFile
          match Platform.Written.parse text with
          | Error problems ->
            skipped <- skipped @ [ (name, String.concat "; " problems) ]
          | Ok written ->
            match! Install.resolve pm written with
            | Error rejection ->
              skipped <- skipped @ [ (name, String.concat "; " rejection.problems) ]
            | Ok manifest ->
              match Platform.External.Manifest.artifactFor rid manifest with
              | None ->
                skipped <- skipped @ [ (name, $"it does not build for {rid}") ]
              | Some artifactHash ->
                match Artifacts.path artifactHash with
                | Error e -> skipped <- skipped @ [ (name, e) ]
                | Ok executable ->
                  let handle = Spawn.handleFor manifest.name executable
                  match
                    Platform.External.Manifest.toPlatform (Spawn.invoke handle) manifest
                  with
                  | Error rejection ->
                    skipped <- skipped @ [ (name, String.concat "; " rejection.problems) ]
                  | Ok platform -> built <- built @ [ platform ]

    return (built, skipped)
  }
