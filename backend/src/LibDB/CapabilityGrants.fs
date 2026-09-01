/// CapabilityGrants — the per-instance capability GRANT, stored as a local file.
///
/// "This instance may do GET to any host, and use random." A LOCAL setting, never synced and not
/// package data — so it lives in `capabilities.bin` beside the store, NOT a SQL table. It's a binary blob of
/// the `Capabilities` record (via the reflection-free `LibSerialization` format — the release CLI is
/// AOT-trimmed). Default is NONE: a missing file ⇒ `Capabilities.noCaps`. View/edit it through
/// `dark caps`; the CLI grant grammar talks spec strings, which `pmCapsGet`/`pmCapsSet` map to/from the
/// record.
module LibDB.CapabilityGrants

open Prelude

module Cap = LibExecution.Capabilities
module CapBin = LibSerialization.Binary.Serializers.Capabilities

/// Beside the store, because the grant is per-INSTANCE. Keyed off `$HOME` it was
/// per-MACHINE: two instances shared one grant, and neither could be given less than the
/// other.
let private filePath () : string =
  System.IO.Path.Combine(LibConfig.Config.runDir, "capabilities.bin")

/// Where the grant used to live. Still READ, never written: moving the file must not
/// silently widen a restrictive grant into the no-file default, which is `allCaps`.
let private legacyFilePath () : string =
  let home =
    match System.Environment.GetEnvironmentVariable "HOME" with
    | null
    | "" -> "/tmp"
    | h -> h
  System.IO.Path.Combine(home, ".darklang", "capabilities.bin")

/// The grant file to read: this instance's, or the machine-wide one it may not have moved
/// off yet.
let private readablePath () : Option<string> =
  if System.IO.File.Exists(filePath ()) then Some(filePath ())
  elif System.IO.File.Exists(legacyFilePath ()) then Some(legacyFilePath ())
  else None

let toBytes (caps : Cap.Capabilities) : byte[] =
  use ms = new System.IO.MemoryStream()
  use w = new System.IO.BinaryWriter(ms)
  CapBin.write w caps
  w.Flush()
  ms.ToArray()

let fromBytes (bytes : byte[]) : Cap.Capabilities =
  use ms = new System.IO.MemoryStream(bytes)
  use r = new System.IO.BinaryReader(ms)
  CapBin.read r

/// The current grant, read from the binary file. Missing/unreadable ⇒ NONE.
let getGrant () : Cap.Capabilities =
  try
    match readablePath () with
    | Some path -> fromBytes (System.IO.File.ReadAllBytes path)
    | None -> Cap.noCaps
  with _ ->
    Cap.noCaps

/// The HOST's effective capabilities — what `eval` (and the interactive instance) runs under. Permissive
/// by DEFAULT (no config file ⇒ `allCaps`, so a fresh install is unrestricted), but once you configure a
/// grant the host RESPECTS it. Distinct from `getGrant` (NONE-default), the secure-by-default sandbox
/// grant `dark run` uses.
let hostCaps () : Cap.Capabilities =
  match readablePath () with
  | Some _ -> getGrant ()
  | None -> Cap.allCaps

/// Overwrite the grant (written as the binary blob). The grant-spec language is parsed/validated in
/// `.dark` (`LanguageTools.Capabilities.parse`); F# only stores the already-structured record.
let setGrant (caps : Cap.Capabilities) : unit =
  let path = filePath ()
  System.IO.Directory.CreateDirectory(System.IO.Path.GetDirectoryName path)
  |> ignore<System.IO.DirectoryInfo>
  System.IO.File.WriteAllBytes(path, toBytes caps)
