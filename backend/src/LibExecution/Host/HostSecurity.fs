/// Host-owned paths that guest filesystem operations must never access.
///
/// The in-process interpreter still needs an OS sandbox for Process and
/// Native effects. This boundary prevents checked Dark file operations from
/// reading or replacing the policy that authorizes them.
module LibExecution.HostSecurity

open System
open System.IO

/// Filesystem paths as the policy sees them. A lexical path is not a
/// filesystem identity: on macOS `/tmp` is a link to `/private/tmp`, and a
/// link placed under an allowed root can point anywhere. Both the request and
/// a rule root are therefore resolved through their symlinked ancestors before
/// they are compared, so a rule names the directory it really is, and a
/// request names the file it really opens. A link that cannot be resolved
/// (dangling, inaccessible, or cyclic) is left in place for the host boundary
/// to reject.
module FilePath =

  let private maxLinkDepth = 40

  /// The final target of `candidate` when it is a link, `None` when it is a
  /// plain entry or does not exist. Raises for an inaccessible entry.
  let private linkTarget (candidate : string) : Option<string> =
    let info : FileSystemInfo =
      if Directory.Exists candidate then
        DirectoryInfo candidate
      else
        FileInfo candidate
    if not info.Exists || isNull info.LinkTarget then
      None
    else
      match info.ResolveLinkTarget true with
      | null -> None
      | target -> Some target.FullName

  let rec private resolve
    (includeFinal : bool)
    (depth : int)
    (path : string)
    : string =
    let full = Path.GetFullPath path
    let root = Path.GetPathRoot full
    if System.String.IsNullOrWhiteSpace root || depth > maxLinkDepth then
      full
    else
      let parts : string[] =
        full
          .Substring(root.Length)
          .Split(
            [| Path.DirectorySeparatorChar; Path.AltDirectorySeparatorChar |],
            System.StringSplitOptions.RemoveEmptyEntries
          )
      let last = parts.Length - 1
      let mutable current = root
      let mutable i = 0
      let mutable resolved = None
      while i <= last && resolved.IsNone do
        let candidate = Path.Combine(current, parts[i])
        let target =
          if i = last && not includeFinal then
            None
          else
            try
              linkTarget candidate
            with _ ->
              None
        match target with
        | None ->
          current <- candidate
          i <- i + 1
        | Some target ->
          // The target may itself sit under links; resolve it as a whole,
          // then continue with what follows the link.
          let rest = Array.sub parts (i + 1) (last - i)
          let resumed : string = Path.Combine(Array.append [| target |] rest)
          resolved <- Some(resolve includeFinal (depth + 1) resumed)
      match resolved with
      | Some path -> path
      | None -> current

  /// `path` with every symlinked ancestor replaced by its target; the final
  /// component is kept as written, so an operation on a link entry itself
  /// (readlink, unlink) still names the link.
  let canonicalAncestors (path : string) : string = resolve false 0 path

  /// `path` fully resolved, the final component included: what a rule root
  /// means.
  let canonical (path : string) : string = resolve true 0 path

let policyDirectory () : Result<string, string> =
  let home = Environment.GetFolderPath(Environment.SpecialFolder.UserProfile)
  if String.IsNullOrWhiteSpace home then
    Error "The current user's profile directory could not be resolved"
  else
    Ok(Path.GetFullPath(Path.Combine(home, ".darklang", "policy")))

/// The policy directory as written and as it resolves through links (a
/// symlinked home, for instance). A guest path is compared against both, so
/// neither spelling of the same directory slips past the guard.
let private policyDirectories () : Result<List<string>, string> =
  policyDirectory ()
  |> Result.map (fun directory ->
    let resolved =
      try
        FilePath.canonical directory
      with _ ->
        directory
    List.distinct [ directory; resolved ])

let private comparison () : StringComparison =
  // Windows and macOS default to case-insensitive filesystems, so a
  // different-case spelling of the policy path (`~/.Darklang/Policy/…`) is the
  // same file and must still be caught. Comparing case-insensitively there is
  // also the fail-safe direction: on a case-sensitive volume it over-matches
  // (denies a distinct same-spelled-differently path), which only ever denies
  // more, never less.
  if OperatingSystem.IsWindows() || OperatingSystem.IsMacOS() then
    StringComparison.OrdinalIgnoreCase
  else
    StringComparison.Ordinal

let private sameOrChild (parent : string) (path : string) : bool =
  let comparison = comparison ()
  let parentWithSeparator =
    if
      parent.EndsWith(string Path.DirectorySeparatorChar, comparison)
      || parent.EndsWith(string Path.AltDirectorySeparatorChar, comparison)
    then
      parent
    else
      parent + string Path.DirectorySeparatorChar
  path.Equals(parent, comparison)
  || path.StartsWith(parentWithSeparator, comparison)

/// True for the policy directory and anything stored below it.
let isPolicyPath (path : string) : bool =
  try
    match policyDirectories () with
    | Error _ -> true
    | Ok directories ->
      let path = FilePath.canonicalAncestors path
      directories |> List.exists (fun directory -> sameOrChild directory path)
  with _ ->
    true

/// True when mutating `path` could replace, move, or delete policy state.
/// Ancestors are included: permitting a rename of `~/.darklang` would let a
/// guest move the policy elsewhere, edit it under an unprotected spelling,
/// and move it back.
let canAffectPolicyPath (path : string) : bool =
  try
    match policyDirectories () with
    | Error _ -> true
    | Ok directories ->
      let path = FilePath.canonicalAncestors path
      directories
      |> List.exists (fun directory ->
        sameOrChild directory path || sameOrChild path directory)
  with _ ->
    true

/// The instance's package store (`data.db`) path, set by the CLI at startup (it
/// owns the config). Guest file and SQLite operations must not touch it:
/// bundled trust is derived from its `locations` rows, so a guest write could
/// forge the exemption, and the reserved-owner op guards do not cover a direct
/// SQLite write. `None` until configured.
let mutable private packageDbPath : string option = None

let setPackageDbPath (path : string) : unit =
  let full = Path.GetFullPath path
  let canonical =
    try
      FilePath.canonical full
    with _ ->
      full
  packageDbPath <- Some canonical

/// True for the package store or one of its SQLite side-files
/// (`-wal`/`-shm`/`-journal`). Fails safe: an unresolvable path is treated as
/// protected.
let isPackageDbPath (path : string) : bool =
  match packageDbPath with
  | None -> false
  | Some dbPath ->
    try
      let full = Path.GetFullPath path
      let canonical = FilePath.canonical full
      let comparison = comparison ()
      [ ""; "-wal"; "-shm"; "-journal" ]
      |> List.exists (fun suffix ->
        full.Equals(dbPath + suffix, comparison)
        || canonical.Equals(dbPath + suffix, comparison))
    with _ ->
      true
