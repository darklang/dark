/// Host-owned file operations for local policy state. Paths stay inside the
/// policy directory; missing and unreadable files remain distinct.
module LibDB.LocalFile

open System
open System.IO
open System.Threading

type ReadResult =
  | Missing
  | Read of byte[]
  | Unreadable of exn

let private maxBytes = 16L * 1024L * 1024L

/// Resolve a policy filename inside the fixed policy directory.
let path (fileName : string) : Result<string, string> =
  if String.IsNullOrWhiteSpace fileName || fileName <> Path.GetFileName fileName then
    Error "Invalid local policy filename"
  else
    LibExecution.HostSecurity.policyDirectory ()
    |> Result.map (fun directory -> Path.Combine(directory, fileName))

/// Resolve a known-good policy filename, raising if the filename is invalid.
let private resolve (fileName : string) : string =
  match path fileName with
  | Ok filePath -> filePath
  | Error message -> invalidArg (nameof fileName) message

/// A link only root could have placed: the link and the directory holding it
/// are both owned by root. macOS ships `/var`, `/tmp` and `/etc` as links like
/// this into `/private`, and every temporary directory there sits under one.
let private placedByRoot (link : string) (parent : string) : bool =
  (OperatingSystem.IsLinux() || OperatingSystem.IsMacOS())
  && LibExecution.HostPosix.fileOwner link = Ok "0"
  && LibExecution.HostPosix.fileOwner parent = Ok "0"

/// Reject the policy directory if it, or any ancestor, is a link someone other
/// than root could have made. The directory itself must never be a link, which
/// is also what `read` checks. Links are followed one hop at a time, so a
/// root-owned link cannot lead on through a user-owned one.
let checkDirectoryChain (directory : string) : Result<unit, string> =
  let full = Path.GetFullPath directory
  let root = Path.GetPathRoot full
  if String.IsNullOrWhiteSpace root then
    Error $"Policy directory has no filesystem root: {directory}"
  else
    let split (path : string) =
      path.Split(
        [| Path.DirectorySeparatorChar; Path.AltDirectorySeparatorChar |],
        StringSplitOptions.RemoveEmptyEntries
      )
      |> List.ofArray
    let rec walk (resolved : string) (parts : List<string>) (hops : int) =
      match parts with
      | [] -> Ok()
      | "." :: rest -> walk resolved rest hops
      | ".." :: rest ->
        let parent = Path.GetDirectoryName resolved
        walk (if isNull parent then resolved else parent) rest hops
      | part :: rest ->
        let current = Path.Combine(resolved, part)
        let info = DirectoryInfo current
        let isLink =
          not (isNull info.LinkTarget)
          || (info.Exists && info.Attributes.HasFlag FileAttributes.ReparsePoint)
        if not isLink then
          if info.Exists then walk current rest hops else Ok()
        elif
          List.isEmpty rest || hops >= 40 || not (placedByRoot current resolved)
        then
          Error $"Refusing to use linked policy directory: {current}"
        else
          let target = info.LinkTarget
          let start = if Path.IsPathRooted target then root else resolved
          walk start (split target @ rest) (hops + 1)
    walk root (split (full.Substring root.Length)) 0

let private rejectLinkedDirectoryChain (directory : string) : unit =
  match checkDirectoryChain directory with
  | Ok() -> ()
  | Error message -> invalidOp message

let private secureDirectory (directory : string) : unit =
  // Check before and after creation so linked parent directories cannot redirect
  // policy state to another location.
  rejectLinkedDirectoryChain directory
  Directory.CreateDirectory directory |> ignore<DirectoryInfo>
  rejectLinkedDirectoryChain directory

  // Restrict Unix policy state to the current user. Windows uses profile ACLs.
  if OperatingSystem.IsLinux() || OperatingSystem.IsMacOS() then
    File.SetUnixFileMode(
      directory,
      UnixFileMode.UserRead ||| UnixFileMode.UserWrite ||| UnixFileMode.UserExecute
    )

/// Run `action` while holding the policy directory's cross-process lock.
let withExclusiveLock (action : unit -> 'a) : 'a =
  match LibExecution.HostSecurity.policyDirectory () with
  | Error message -> invalidOp message
  | Ok directory ->
    secureDirectory directory
    // Include the directory in the name so separate users/configurations do
    // not share a lock.
    let lockName =
      let digest =
        System.Security.Cryptography.SHA256.HashData(
          System.Text.Encoding.UTF8.GetBytes directory
        )
      "Darklang.policies." + System.Convert.ToHexString(digest, 0, 8)
    use mutex = new Mutex(false, lockName)
    try
      mutex.WaitOne() |> ignore<bool>
    with :? AbandonedMutexException ->
      // An abandoned mutex is acquired by this call; atomic writes make it safe
      // to continue.
      ()
    try
      action ()
    finally
      mutex.ReleaseMutex()

/// Return a cheap change stamp for cache invalidation, or `None` when the file
/// is missing or cannot be inspected.
let stamp (fileName : string) : Option<struct (int64 * int64 * int64)> =
  try
    let info = FileInfo(resolve fileName)
    if info.Exists then
      Some(
        struct (info.LastWriteTimeUtc.Ticks, info.CreationTimeUtc.Ticks, info.Length)
      )
    else
      None
  with _ ->
    None

let read (fileName : string) : ReadResult =
  let filePath = resolve fileName
  try
    let directory = Path.GetDirectoryName filePath
    let directoryResult =
      if String.IsNullOrWhiteSpace directory then
        Error(InvalidDataException "Local policy directory is empty")
      else
        let directoryInfo = DirectoryInfo directory
        if directoryInfo.LinkTarget <> null then
          Error(
            InvalidDataException
              "Refusing to read through a symlinked policy directory"
          )
        else
          Ok()
    match directoryResult with
    | Error error -> Unreadable error
    | Ok() ->
      let fileInfo = FileInfo filePath
      if fileInfo.LinkTarget <> null then
        Unreadable(
          InvalidDataException "Refusing to read through a symlinked policy file"
        )
      else
        use stream =
          new FileStream(
            filePath,
            FileMode.Open,
            FileAccess.Read,
            FileShare.ReadWrite ||| FileShare.Delete
          )
        if stream.Length > maxBytes then
          Unreadable(InvalidDataException "Local policy file is too large")
        else
          use buffer = new MemoryStream()
          stream.CopyTo buffer
          Read(buffer.ToArray())
  with
  | :? FileNotFoundException
  | :? DirectoryNotFoundException -> Missing
  | error -> Unreadable error

/// Write a policy file atomically and with owner-only Unix permissions.
let writeAtomic (fileName : string) (bytes : byte[]) : unit =
  let filePath = resolve fileName
  let directory = Path.GetDirectoryName filePath
  if String.IsNullOrWhiteSpace directory then
    invalidArg (nameof fileName) "Local policy directory is empty"
  secureDirectory directory

  let temporary =
    Path.Combine(directory, $".{Path.GetFileName filePath}.tmp-{Guid.NewGuid():N}")

  try
    // Close the temporary file before renaming; this is required on Windows.
    let writeTemporary () =
      use stream =
        new FileStream(
          temporary,
          FileMode.CreateNew,
          FileAccess.Write,
          FileShare.None
        )
      if OperatingSystem.IsLinux() || OperatingSystem.IsMacOS() then
        File.SetUnixFileMode(
          temporary,
          UnixFileMode.UserRead ||| UnixFileMode.UserWrite
        )
      stream.Write(bytes, 0, bytes.Length)
      stream.Flush(true)
    writeTemporary ()
    File.Move(temporary, filePath, true)
    // Persist the directory entry after the rename where supported.
    LibExecution.Host.syncPolicyDirectory directory
    if OperatingSystem.IsLinux() || OperatingSystem.IsMacOS() then
      File.SetUnixFileMode(
        filePath,
        UnixFileMode.UserRead ||| UnixFileMode.UserWrite
      )
  finally
    if File.Exists temporary then File.Delete temporary
