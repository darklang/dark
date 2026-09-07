/// Host-owned file operations for local policy state. Paths stay inside the
/// policy directory; missing and unreadable files remain distinct.
module LibDB.LocalFile

open System
open System.IO
open System.Threading
open System.Runtime.InteropServices

[<DllImport("libc", SetLastError = true, EntryPoint = "open")>]
extern int private cOpen(string path, int flags)

[<DllImport("libc", SetLastError = true, EntryPoint = "fsync")>]
extern int private cFsync(int fd)

[<DllImport("libc", SetLastError = true, EntryPoint = "close")>]
extern int private cClose(int fd)

/// Make an atomic rename durable on Unix. Best-effort; Windows uses the flushed
/// file and rename.
let private syncDirectory (directory : string) : unit =
  if OperatingSystem.IsLinux() || OperatingSystem.IsMacOS() then
    try
      let fd = cOpen (directory, 0) // O_RDONLY; a directory opens read-only
      if fd >= 0 then
        try
          cFsync fd |> ignore<int>
        finally
          cClose fd |> ignore<int>
    with _ ->
      ()

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

let private rejectLinkedDirectoryChain (directory : string) : unit =
  let full = Path.GetFullPath directory
  let root = Path.GetPathRoot full
  if String.IsNullOrWhiteSpace root then
    invalidOp $"Policy directory has no filesystem root: {directory}"
  let parts =
    full
      .Substring(root.Length)
      .Split(
        [| Path.DirectorySeparatorChar; Path.AltDirectorySeparatorChar |],
        StringSplitOptions.RemoveEmptyEntries
      )
  let mutable current = root
  for part in parts do
    current <- Path.Combine(current, part)
    if Directory.Exists current then
      let info = DirectoryInfo current
      if
        info.LinkTarget <> null
        || info.Attributes.HasFlag(FileAttributes.ReparsePoint)
      then
        invalidOp $"Refusing to use linked policy directory: {current}"

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
    syncDirectory directory
    if OperatingSystem.IsLinux() || OperatingSystem.IsMacOS() then
      File.SetUnixFileMode(
        filePath,
        UnixFileMode.UserRead ||| UnixFileMode.UserWrite
      )
  finally
    if File.Exists temporary then File.Delete temporary
