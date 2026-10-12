/// Descriptor-safe POSIX operations owned by the checked host boundary.
/// Kernel calls use HostKernel; process-managed state uses .NET. No libc ABI.
module LibExecution.HostPosix

open System
open System.Runtime.InteropServices
open Microsoft.Win32.SafeHandles
open Prelude
open LibExecution.HostKernel

// -- Platform detection ---------------------------------------------
let private isMac = RuntimeInformation.IsOSPlatform OSPlatform.OSX
let private isArm64 = RuntimeInformation.ProcessArchitecture = Architecture.Arm64
let private isX64 = RuntimeInformation.ProcessArchitecture = Architecture.X64
/// 32-bit ARM (armv7). Distinct from Arm64 in more than pointer width: off_t
/// uses the kernel stat64 layout: 64-bit size and 32-bit timestamps.
let private isArm32 = RuntimeInformation.ProcessArchitecture = Architecture.Arm
/// The browser (wasm32 under emscripten). native syscall instructions are unavailable there, so
/// every wrapper below answers first: those with a .NET equivalent through `Managed`
/// (including open/read/write/seek/close, over `FileStream`), the rest with ENOSYS.
/// `Host.fs` already takes its own .NET path for file reads, writes, stats and listings
/// when `isPosix` is false.
let private isWasm = System.OperatingSystem.IsBrowser()

do
  if not isMac && not isArm64 && not isX64 && not isArm32 && not isWasm then
    raise (
      System.PlatformNotSupportedException(
        $"Posix builtins: unsupported architecture {RuntimeInformation.ProcessArchitecture} on Linux. "
        + "Struct offsets are only known for x86_64, aarch64 and armv7."
      )
    )

// -- Open flags (platform-specific) --------------------------------
let O_RDONLY = 0
let O_WRONLY = 1
let O_RDWR = 2

let O_CREAT = if isMac then 0x200 else 0x40 // Linux

let O_TRUNC = if isMac then 0x400 else 0x200 // Linux

let O_APPEND = if isMac then 0x8 else 0x400 // Linux
let private O_EXCL = if isMac then 0x800 else 0x80
// Linux O_PATH obtains a reference to an inode without requiring read or
// write permission. macOS takes its metadata path below through *at calls.
let private O_PATH = 0x200000
// O_NOFOLLOW and O_DIRECTORY are the two open flags whose values differ
// between Linux architectures: x86_64 keeps its historical numbers, while
// aarch64 and armv7 use the asm-generic ones. Getting this wrong is silent —
// 0x20000 on aarch64 is O_LARGEFILE, a no-op, so a "no-follow" open followed
// the link — so they are per-architecture, not per-OS.
let O_NOFOLLOW =
  if isMac then 0x100
  elif isX64 then 0x20000
  else 0x8000 // aarch64, armv7 (asm-generic)
let private O_DIRECTORY =
  if isMac then 0x100000
  elif isX64 then 0x10000
  else 0x4000 // aarch64, armv7 (asm-generic)
let private AT_REMOVEDIR = if isMac then 0x80 else 0x200
let private AT_SYMLINK_NOFOLLOW = if isMac then 0x20 else 0x100
let private AT_EMPTY_PATH = 0x1000 // Linux
let private open_raw (path, flags, mode) =
  openat_raw ((if isMac then -2 else -100), path, flags, mode)
/// True where descriptor-safe kernel operations implement the filesystem; Windows
/// keeps the .NET calls behind the lexical pre-check.
let isPosix : bool =
  not (RuntimeInformation.IsOSPlatform OSPlatform.Windows) && not isWasm
let SEEK_SET = 0
let SEEK_CUR = 1
let SEEK_END = 2



module private Managed =
  let private attempt (f : unit -> 'a) : Result<'a, int * string> =
    try
      Ok(f ())
    with e ->
      Error(5, e.Message)
  let getcwd () = attempt (fun () -> System.IO.Directory.GetCurrentDirectory())
  let chdir (p : string) =
    attempt (fun () -> System.IO.Directory.SetCurrentDirectory p)
  let mkdir (p : string) =
    attempt (fun () ->
      System.IO.Directory.CreateDirectory p |> ignore<System.IO.DirectoryInfo>)
  let rmdir (p : string) = attempt (fun () -> System.IO.Directory.Delete p)
  let unlink (p : string) = attempt (fun () -> System.IO.File.Delete p)
  let rename (a : string) (b : string) =
    attempt (fun () -> System.IO.File.Move(a, b, true))
  let listDir (p : string) =
    attempt (fun () ->
      System.IO.Directory.EnumerateFileSystemEntries p
      |> Seq.map System.IO.Path.GetFileName
      |> Seq.toList)
  /// (mode, size, mtime seconds)
  let stat (p : string) : Result<int * int64 * int64, int * string> =
    let secs (t : DateTime) = DateTimeOffset(t).ToUnixTimeSeconds()
    if System.IO.Directory.Exists p then
      Ok(0x4000 ||| 0o755, 0L, secs (System.IO.DirectoryInfo p).LastWriteTimeUtc)
    elif System.IO.File.Exists p then
      let f = System.IO.FileInfo p
      Ok(0x8000 ||| 0o644, f.Length, secs f.LastWriteTimeUtc)
    else
      Error(2, "No such file or directory")
  let enosys () = Error(38, "not available in the browser")

  /// Descriptors over `FileStream`, so the descriptor half of the Posix builtins
  /// works against the in-memory filesystem. `Stdlib.Cli.File.readText` reads through
  /// open/read/close; with these refusing, `dark run <file>` and `dark module <file>`
  /// could not see a file the same tab had just written through
  /// `Stdlib.Cli.FileSystem`. 0 to 2 are the terminal's and stay refused; the browser
  /// builtins own stdin and output.
  let private files = Collections.Generic.Dictionary<int, IO.FileStream>()
  let mutable private nextFd = 3

  let openFile (path : string) (flags : int) : Result<int, int * string> =
    let access =
      match flags &&& 3 with
      | 1 -> IO.FileAccess.Write
      | 2 -> IO.FileAccess.ReadWrite
      | _ -> IO.FileAccess.Read
    let creates = flags &&& O_CREAT <> 0
    let mode =
      if flags &&& O_APPEND <> 0 then
        IO.FileMode.Append
      elif flags &&& O_TRUNC <> 0 then
        (if creates then IO.FileMode.Create else IO.FileMode.Truncate)
      elif creates then
        IO.FileMode.OpenOrCreate
      else
        IO.FileMode.Open
    try
      let stream = new IO.FileStream(path, mode, access)
      let fd = nextFd
      nextFd <- nextFd + 1
      files[fd] <- stream
      Ok fd
    with
    | :? IO.FileNotFoundException
    | :? IO.DirectoryNotFoundException -> Error(2, "No such file or directory")
    | :? UnauthorizedAccessException -> Error(13, "Permission denied")
    | e -> Error(5, e.Message)

  let private withFile
    (fd : int)
    (f : IO.FileStream -> 'a)
    : Result<'a, int * string> =
    match files.TryGetValue fd with
    | true, stream -> attempt (fun () -> f stream)
    | _ -> Error(9, "Bad file descriptor")

  let fdRead (fd : int) (count : int) =
    withFile fd (fun s ->
      let buf = Array.zeroCreate<byte> count
      let n = s.Read(buf, 0, count)
      buf[0 .. n - 1])

  let fdWrite (fd : int) (data : byte[]) =
    withFile fd (fun s ->
      s.Write(data, 0, data.Length)
      data.Length)

  let fdSeek (fd : int) (offset : int64) (whence : int) =
    withFile fd (fun s ->
      let origin =
        match whence with
        | 1 -> IO.SeekOrigin.Current
        | 2 -> IO.SeekOrigin.End
        | _ -> IO.SeekOrigin.Begin
      s.Seek(offset, origin))

  let fdClose (fd : int) =
    match files.TryGetValue fd with
    | true, stream ->
      files.Remove fd |> ignore<bool>
      attempt (fun () -> stream.Dispose())
    | _ -> Error(9, "Bad file descriptor")

// -- Wrappers -----------------------------------------------------

let lastError () : int * string =
  let errno = Marshal.GetLastPInvokeError()
  // Public Dark wrappers render errno. Host failures still carry diagnostic text.
  (errno, $"OS error {errno}")

let getcwdBytes (count : int) : Result<byte[], int * string> =
  if isWasm then
    Managed.enosys ()
  elif count <= 0 then
    Error(22, "invalid getcwd buffer")
  else
    let bytes = Array.zeroCreate<byte> count
    let rc = getcwdBytes_raw bytes
    if rc < 0 then Error(lastError ()) else Ok(Array.take rc bytes)

let fcntlGetPath (fd : int) : Result<byte[], int * string> =
  if isWasm then
    Managed.enosys ()
  else
    // XNU F_GETPATH has no length argument: its ABI requires MAXPATHLEN.
    let bytes = Array.zeroCreate<byte> 1024
    if fcntlGetPath_raw (fd, bytes) < 0 then Error(lastError ()) else Ok bytes

let private components (path : string) : string[] =
  path.Split([| '/' |], StringSplitOptions.RemoveEmptyEntries)

let private failed<'a> () : Result<'a, int * string> = Error(lastError ())

let private withParent
  (path : string)
  (action : int -> string -> Result<'a, int * string>)
  : Result<'a, int * string> =
  let parts = components path
  if parts.Length = 0 then
    Error(22, "path has no final component")
  else
    let mutable dirFd = open_raw ("/", O_RDONLY ||| O_DIRECTORY, 0)
    if dirFd < 0 then
      failed ()
    else
      let mutable failure = None
      let mutable i = 0
      while failure.IsNone && i < parts.Length - 1 do
        let next =
          openat_raw (dirFd, parts[i], O_RDONLY ||| O_DIRECTORY ||| O_NOFOLLOW, 0)
        if next < 0 then
          failure <- Some(lastError ())
        else
          close_raw dirFd |> ignore<int>
          dirFd <- next
        i <- i + 1
      let result =
        match failure with
        | Some e -> Error e
        | None -> action dirFd parts[parts.Length - 1]
      close_raw dirFd |> ignore<int>
      result

/// The root directory itself, for the operations that read it. `withParent`
/// is a walk to a parent plus a final name, which the root does not have;
/// listing, stat and chdir on `/` all failed with "no final component" and
/// `pathExists "/"` answered false. Entry-creating operations keep the
/// requirement: there is nothing to create AT the root.
let private withRoot
  (action : int -> Result<'a, int * string>)
  : Result<'a, int * string> =
  let fd = open_raw ("/", O_RDONLY ||| O_DIRECTORY, 0)
  if fd < 0 then
    failed ()
  else
    let result = action fd
    close_raw fd |> ignore<int>
    result

let private isRoot (path : string) : bool = (components path).Length = 0

/// Open the final component itself as a directory (listing, chdir).
let private withDirectory
  (path : string)
  (action : int -> Result<'a, int * string>)
  : Result<'a, int * string> =
  if isRoot path then
    withRoot action
  else
    withParent path (fun dirFd name ->
      let fd = openat_raw (dirFd, name, O_RDONLY ||| O_DIRECTORY ||| O_NOFOLLOW, 0)
      if fd < 0 then
        failed ()
      else
        let result = action fd
        close_raw fd |> ignore<int>
        result)

let private unitResult (rc : int) : Result<unit, int * string> =
  if rc < 0 then failed () else Ok()

let chdir (path : string) : Result<unit, int * string> =
  if isWasm then
    Managed.chdir path
  else
    withDirectory path (fun fd -> unitResult (fchdir_raw fd))

let mkdir (path : string) (mode : int) : Result<unit, int * string> =
  if isWasm then
    Managed.mkdir path
  else
    withParent path (fun d n -> unitResult (mkdirat_raw (d, n, mode)))

let unlinkAt (path : string) (flags : int) : Result<unit, int * string> =
  if isWasm then
    if flags = 0 then Managed.unlink path
    elif flags = AT_REMOVEDIR then Managed.rmdir path
    else Error(22, "invalid unlinkat flags")
  else
    withParent path (fun d n -> unitResult (unlinkat_raw (d, n, flags)))

let rmdir (path : string) : Result<unit, int * string> = unlinkAt path AT_REMOVEDIR

let unlink (path : string) : Result<unit, int * string> = unlinkAt path 0

let rename (oldpath : string) (newpath : string) : Result<unit, int * string> =
  if isWasm then
    Managed.rename oldpath newpath
  else
    withParent oldpath (fun d1 n1 ->
      withParent newpath (fun d2 n2 -> unitResult (renameat_raw (d1, n1, d2, n2))))

let symlink (target : string) (linkpath : string) : Result<unit, int * string> =
  if isWasm then
    Managed.enosys ()
  else
    withParent linkpath (fun d n -> unitResult (symlinkat_raw (target, d, n)))

let readlinkBytes (path : string) (count : int) : Result<byte[], int * string> =
  if isWasm then
    Managed.enosys ()
  elif count <= 0 then
    Error(22, "readlink count must be positive")
  else
    withParent path (fun d n ->
      let buf = Array.zeroCreate<byte> count
      let len = readlinkat_raw (d, n, buf, count)
      if len < 0 then failed () else Ok(Array.take len buf))

/// Six random name characters, as mkstemp uses.
let private randomSuffix () : string =
  let alphabet = "abcdefghijklmnopqrstuvwxyzABCDEFGHIJKLMNOPQRSTUVWXYZ0123456789"
  let bytes = Array.zeroCreate<byte> 6
  System.Security.Cryptography.RandomNumberGenerator.Fill bytes
  bytes |> Array.map (fun b -> alphabet[int b % alphabet.Length]) |> String

/// One exclusive creation attempt; retry behavior belongs to Darklang.
let private createUnique
  (prefix : string)
  (create : int -> string -> int)
  : Result<string, int * string> =
  let directory = System.IO.Path.GetDirectoryName prefix
  withParent prefix (fun d basePrefix ->
    let name = basePrefix + randomSuffix ()
    if create d name >= 0 then
      Ok(System.IO.Path.Combine(directory, name))
    else
      failed ())

let mkstemp (prefix : string) : Result<int * string, int * string> =
  if isWasm then
    Managed.enosys ()
  else
    let mutable opened = -1
    createUnique prefix (fun d name ->
      let fd =
        openat_raw (d, name, O_RDWR ||| O_CREAT ||| O_EXCL ||| O_NOFOLLOW, 0o600)
      opened <- fd
      fd)
    |> Result.map (fun path -> opened, path)

let mkdtemp (prefix : string) : Result<string, int * string> =
  if isWasm then
    Managed.enosys ()
  else
    createUnique prefix (fun d name -> mkdirat_raw (d, name, 0o700))

let openFile (path : string) (flags : int) (mode : int) : Result<int, int * string> =
  if isWasm then
    Managed.openFile path flags
  elif path <> "" && path.Trim('/') = "" then
    let fd = open_raw ("/", flags ||| O_NOFOLLOW, mode)
    if fd < 0 then failed () else Ok fd
  else
    withParent path (fun d n ->
      let fd = openat_raw (d, n, flags ||| O_NOFOLLOW, mode)
      if fd < 0 then failed () else Ok fd)

let private statAt fd name buf flags = fstatat_raw (fd, name, buf, flags)

/// Walk to the parent and stat the entry into `buf`. The root is stat'd as
/// "." relative to its own descriptor, which is portable (`AT_EMPTY_PATH` is
/// Linux-only) and names no parent.
let private statInto (path : string) (buf : IntPtr) : Result<unit, int * string> =
  if isRoot path then
    withRoot (fun fd -> unitResult (statAt fd "." buf 0))
  else
    withParent path (fun d n -> unitResult (statAt d n buf AT_SYMLINK_NOFOLLOW))

let private modeFromStatBuffer (buf : IntPtr) : int =
  if isMac then
    int (Marshal.ReadInt16(buf, 4)) &&& 0xFFFF
  elif isArm64 || isArm32 then
    Marshal.ReadInt32(buf, 16)
  else // x86_64 Linux (guarded by startup check)
    Marshal.ReadInt32(buf, 24)

/// The policy boundary owns the no-follow walk; Dark owns the record layout.
let statBuffer (path : string) : Result<byte[], int * string> =
  if isWasm || OperatingSystem.IsWindows() then
    Error(38, "kernel stat unavailable on this target")
  else
    let bytes = Array.zeroCreate<byte> 256
    let pinned = GCHandle.Alloc(bytes, GCHandleType.Pinned)
    try
      statInto path (pinned.AddrOfPinnedObject()) |> Result.map (fun () -> bytes)
    finally
      pinned.Free()

/// One kernel directory read. No iteration, name filtering, or record decoding.
let directoryBytes (fd : int) (count : int) : Result<byte[], int * string> =
  if count < 0 then
    Error(22, "Invalid argument")
  elif isWasm || OperatingSystem.IsWindows() then
    Error(38, "kernel directory reads unavailable on this target")
  else
    let bytes = Array.zeroCreate<byte> count
    let position = Marshal.AllocHGlobal 8
    try
      Marshal.WriteInt64(position, 0L)
      let rc = getdents_raw (fd, bytes, position)
      if rc < 0 then Error(lastError ()) else Ok(bytes |> Array.take rc)
    finally
      Marshal.FreeHGlobal position

/// Linux metadata operations need to work on mode-000 files while binding the
/// operation to the entry checked at execution time. O_PATH holds the exact
/// inode without data-access permission; fstatat(AT_EMPTY_PATH) rejects a
/// final symlink. Metadata operations then use that held inode; older kernels
/// need a procfd fallback, which fails closed when procfs is unavailable.
let private withLinuxMetadataDescriptor
  (path : string)
  (action : int -> Result<'a, int * string>)
  : Result<'a, int * string> =
  withParent path (fun dirFd name ->
    let fd = openat_raw (dirFd, name, O_PATH ||| O_NOFOLLOW, 0)
    if fd < 0 then
      failed ()
    else
      let buf = Marshal.AllocHGlobal(256)
      try
        if statAt fd "" buf AT_EMPTY_PATH < 0 then
          failed ()
        elif modeFromStatBuffer buf &&& 0o170000 = 0o120000 then
          Error(40, "Too many levels of symbolic links") // Linux ELOOP
        else
          action fd
      finally
        Marshal.FreeHGlobal buf
        close_raw fd |> ignore<int>)

let private withLinuxMetadataPath path action =
  withLinuxMetadataDescriptor path (fun fd -> action $"/proc/self/fd/{fd}")

let fclonefileAt source path flags : Result<unit, int * string> =
  if isWasm then
    Managed.enosys ()
  elif not isMac then
    Error(38, "fclonefileat is unavailable")
  else
    let metadata = Array.zeroCreate<byte> 256
    if fstat_raw (source, metadata) < 0 then
      failed ()
    // A single-file capability must never authorize recursive directory clone.
    elif int (BitConverter.ToUInt16(metadata, 4)) &&& 0o170000 <> 0o100000 then
      Error(21, "file clone requires a regular source file")
    else
      withParent path (fun parent name ->
        if fclonefileat_raw (source, parent, name, flags ||| 1) < 0 then
          failed ()
        else
          Ok())

let inotifyInit1 flags : Result<int, int * string> =
  if isWasm then
    Managed.enosys ()
  else
    let fd = inotifyInit1_raw flags
    if fd < 0 then failed () else Ok fd

let inotifyAddWatch fd path mask : Result<int, int * string> =
  if isWasm || isMac then
    Error((if isMac then 78 else 38), "inotify is unavailable")
  else
    withLinuxMetadataPath path (fun heldPath ->
      // The final user entry was checked without following links. The internal
      // procfd link names that held inode, so IN_DONT_FOLLOW must not inspect it.
      let wd = inotifyAddWatch_raw (fd, heldPath, mask &&& ~~~ 0x02000000)
      if wd < 0 then failed () else Ok wd)

/// Marshal pollfd/timespec ABI only. Return every raw pollfd, including revents;
/// interpreting readiness, retrying EINTR, and draining queues belong in Dark.
let pollDescriptors
  darwin
  (fds : List<int * int>)
  timeoutMs
  : Result<byte[], int * string> =
  if isWasm then
    Managed.enosys ()
  elif
    timeoutMs < 0
    || List.length fds > 1024
    || List.exists (fun (_, events) -> events < 0 || events > 32767) fds
  then
    Error(22, "invalid poll arguments")
  else
    let bytes = Array.zeroCreate<byte> (max 1 (List.length fds * 8))
    fds
    |> List.iteri (fun index (fd, events) ->
      BitConverter.GetBytes(fd).CopyTo(bytes, index * 8)
      BitConverter.GetBytes(int16 events).CopyTo(bytes, index * 8 + 4))
    let rc =
      if darwin then
        pollDarwin_raw (bytes, List.length fds, timeoutMs)
      else
        let width = if isArm32 then 4 else 8
        let timeout = Array.zeroCreate<byte> (width * 2)
        if width = 4 then
          BitConverter.GetBytes(timeoutMs / 1000).CopyTo(timeout, 0)
          BitConverter.GetBytes((timeoutMs % 1000) * 1000000).CopyTo(timeout, width)
        else
          BitConverter.GetBytes(int64 (timeoutMs / 1000)).CopyTo(timeout, 0)
          BitConverter
            .GetBytes(int64 ((timeoutMs % 1000) * 1000000))
            .CopyTo(timeout, width)
        ppoll_raw (bytes, List.length fds, timeout)
    if rc < 0 then failed () else Ok(bytes |> Array.take (List.length fds * 8))

let kqueue () : Result<int, int * string> =
  if isWasm then
    Managed.enosys ()
  else
    let fd = kqueue_raw ()
    if fd < 0 then failed () else Ok fd

let keventVnode
  fd
  (changes : List<int * int * int>)
  capacity
  timeoutMs
  : Result<byte[], int * string> =
  if isWasm then
    Managed.enosys ()
  elif
    capacity < 0
    || capacity > 1024
    || timeoutMs < 0
    || List.length changes > 1024
    || List.exists
      (fun (_, flags, mask) -> flags < 0 || flags > 65535 || mask < 0)
      changes
  then
    Error(22, "invalid kevent arguments")
  else
    let input = Array.zeroCreate<byte> (max 1 (List.length changes * 32))
    changes
    |> List.iteri (fun index (source, flags, mask) ->
      BitConverter.GetBytes(int64 source).CopyTo(input, index * 32)
      BitConverter.GetBytes(-4s).CopyTo(input, index * 32 + 8)
      BitConverter.GetBytes(uint16 flags).CopyTo(input, index * 32 + 10)
      BitConverter.GetBytes(mask).CopyTo(input, index * 32 + 12))
    let output = Array.zeroCreate<byte> (max 1 (capacity * 32))
    let timeout = Array.zeroCreate<byte> 16
    BitConverter.GetBytes(int64 (timeoutMs / 1000)).CopyTo(timeout, 0)
    BitConverter.GetBytes(int64 ((timeoutMs % 1000) * 1000000)).CopyTo(timeout, 8)
    let count =
      kevent_raw (fd, input, List.length changes, output, capacity, timeout)
    if count < 0 then failed ()
    elif count > capacity then Error(5, "invalid kevent count")
    else Ok(Array.take (count * 32) output)

let chmod (path : string) (mode : int) : Result<unit, int * string> =
  if isWasm then
    Managed.enosys ()
  else if isMac then
    withParent path (fun d n ->
      unitResult (fchmodat_raw (d, n, mode, AT_SYMLINK_NOFOLLOW)))
  else
    withParent path (fun d n ->
      // fchmodat2 binds no-follow semantics to the actual operation. Older
      // kernels retain the O_PATH/procfd fallback, which fails closed without procfs.
      let rc = fchmodat2_raw (d, n, mode, AT_SYMLINK_NOFOLLOW)
      if rc >= 0 then
        Ok()
      elif Marshal.GetLastPInvokeError() = 38 then
        withLinuxMetadataPath path (fun held ->
          unitResult (fchmodat_raw (-100, held, mode, 0)))
      else
        failed ())

/// ABI marshalling only: Dark selects timestamps and the NOW sentinel.
let utimensAt
  (path : string)
  (accessSec : int64)
  (accessNanos : int64)
  (modifySec : int64)
  (modifyNanos : int64)
  : Result<unit, int * string> =
  if isMac then
    Error(78, "Function not implemented")
  elif isWasm then
    Managed.enosys ()
  elif
    isArm32
    && ([ accessSec; accessNanos; modifySec; modifyNanos ]
        |> List.exists (fun value ->
          value < int64 System.Int32.MinValue || value > int64 System.Int32.MaxValue))
  then
    Error(75, "timestamp does not fit this kernel ABI")
  else
    let width = if isArm32 then 4 else 8
    let times = Marshal.AllocHGlobal(width * 4)
    try
      [ accessSec; accessNanos; modifySec; modifyNanos ]
      |> List.iteri (fun index value ->
        if width = 4 then
          Marshal.WriteInt32(times, index * width, int value)
        else
          Marshal.WriteInt64(times, index * width, value))
      // Bind updates to a held inode and reject final symlinks.
      withLinuxMetadataDescriptor path (fun fd ->
        let rc = utimensat_raw (fd, "", times, AT_EMPTY_PATH)
        if rc >= 0 then
          Ok()
        elif Marshal.GetLastPInvokeError() = 22 then
          unitResult (utimensat_raw (-100, $"/proc/self/fd/{fd}", times, 0))
        else
          failed ())
    finally
      Marshal.FreeHGlobal times

let setattrlistTimes
  (path : string)
  (accessSec : int64)
  (accessNanos : int64)
  (modifySec : int64)
  (modifyNanos : int64)
  : Result<unit, int * string> =
  if isWasm || not isMac then
    Managed.enosys ()
  else
    withParent path (fun d n ->
      // XNU attribute order is MODTIME followed by ACCTIME.
      let times = Array.zeroCreate<byte> 32
      [ modifySec; modifyNanos; accessSec; accessNanos ]
      |> List.iteri (fun index value ->
        BitConverter.GetBytes(value).CopyTo(times, index * 8))
      unitResult (setattrlistat_raw (d, n, times)))

/// Extracts (mode, size, mtimeSec) from a struct stat buffer. Offsets are
/// platform-specific (Linux vs macOS struct layouts differ).
let stat (path : string) : Result<int * int64 * int64, int * string> =
  if isWasm then
    Managed.stat path
  else
    let buf = Marshal.AllocHGlobal(256)
    try
      match statInto path buf with
      | Error e -> Error e
      | Ok() ->
        // struct stat field offsets differ across OS and architecture:
        //   macOS (all):   st_mode at 4 (int16), st_size at 96, st_mtime at 48
        //   Linux x86_64:  st_mode at 24, st_size at 48, st_mtime at 88
        //   Linux aarch64: st_mode at 16, st_size at 48, st_mtime at 88
        //   Linux armv7 kernel stat64: st_mode at 16, size at 48, mtime at 80.
        // This is the syscall ABI, not the libc struct stat layout.
        let mode = modeFromStatBuffer buf
        let size =
          if isMac then Marshal.ReadInt64(buf, 96)
          elif isArm32 then Marshal.ReadInt64(buf, 48)
          else Marshal.ReadInt64(buf, 48)
        let mtimeSec =
          if isMac then Marshal.ReadInt64(buf, 48)
          elif isArm32 then int64 (Marshal.ReadInt32(buf, 80))
          else Marshal.ReadInt64(buf, 88)
        Ok(mode, size, mtimeSec)
    finally
      Marshal.FreeHGlobal buf

/// Single-call raw records; shared Dark code selects and decodes fields.
let private clockRecord size call : Result<byte[], int * string> =
  if isWasm then
    Managed.enosys ()
  else
    let bytes = Array.zeroCreate<byte> size
    if call bytes < 0 then failed () else Ok bytes

let clockGettime clock =
  clockRecord (if isArm32 then 8 else 16) (fun bytes ->
    clockGettime_raw (clock, bytes))
let gettimeofday () = clockRecord (if isArm32 then 8 else 16) gettimeofday_raw
let machAbsoluteTime () = clockRecord 8 machAbsoluteTime_raw
let machTimebaseInfo () = clockRecord 8 machTimebaseInfo_raw

/// Return the one-shot status AND remainder, including EINTR; Dark retries.
let nanosleep (seconds : int64) (nanos : int64) : int * byte[] =
  let width = if isArm32 then 4 else 8
  let request = Array.zeroCreate<byte> (width * 2)
  let remaining = Array.zeroCreate<byte> (width * 2)
  if isWasm then
    (-38, remaining)
  elif seconds < 0L || nanos < 0L || nanos >= 1000000000L then
    (-22, remaining)
  elif
    width = 4
    && (seconds < int64 System.Int32.MinValue
        || seconds > int64 System.Int32.MaxValue)
  then
    (-75, remaining)
  else
    if width = 4 then
      BitConverter.GetBytes(int seconds).CopyTo(request, 0)
      BitConverter.GetBytes(int nanos).CopyTo(request, width)
    else
      BitConverter.GetBytes(seconds).CopyTo(request, 0)
      BitConverter.GetBytes(nanos).CopyTo(request, width)
    let rc = nanosleep_raw (request, remaining)
    (if rc < 0 then - Marshal.GetLastPInvokeError() else rc), remaining

let unameBytes () : Result<byte[], int * string> =
  if isWasm then
    Managed.enosys ()
  else
    let bytes = Array.zeroCreate<byte> 390
    if unameBytes_raw bytes < 0 then failed () else Ok bytes

let sysctlBytes (mib : List<int>) (count : int) : Result<byte[], int * string> =
  if isWasm then
    Managed.enosys ()
  elif count <= 0 || List.isEmpty mib then
    Error(22, "invalid sysctl buffer")
  else
    let mibBytes = mib |> List.map BitConverter.GetBytes |> Array.concat
    let buffer = Marshal.AllocHGlobal count
    let size = Marshal.AllocHGlobal IntPtr.Size
    try
      Marshal.WriteIntPtr(size, IntPtr count)
      if sysctl_raw (mibBytes, buffer, size) < 0 then
        failed ()
      else
        let length = Marshal.ReadIntPtr(size).ToInt64()
        if length < 0L || length > int64 count then
          Error(5, "invalid kernel record length")
        else
          let bytes = Array.zeroCreate<byte> (int length)
          Marshal.Copy(buffer, bytes, 0, bytes.Length)
          Ok bytes
    finally
      Marshal.FreeHGlobal size
      Marshal.FreeHGlobal buffer

let schedGetaffinity (pid : int) (count : int) : Result<byte[], int * string> =
  if isWasm then
    Managed.enosys ()
  elif count <= 0 then
    Error(22, "affinity count must be positive")
  else
    let bytes = Array.zeroCreate<byte> count
    let length = schedGetaffinity_raw (pid, bytes)
    if length < 0 then failed () else Ok(Array.take length bytes)

let getpid () : int = if isWasm then Environment.ProcessId else getpid_raw ()

let getuid () : uint32 = if isWasm then 1000u else getuid_raw ()

/// flock operations
let LOCK_EX = 2
let LOCK_UN = 8

let flock (fd : int) (operation : int) : Result<unit, int * string> =
  if isWasm then Ok()
  else if flock_raw (fd, operation) < 0 then Error(lastError ())
  else Ok()

/// Return the numeric owner UID; Darklang resolves account names.
let fileOwner (path : string) : Result<string, int * string> =
  if isWasm then
    Managed.enosys ()
  else
    let buf = Marshal.AllocHGlobal(256)
    try
      match statInto path buf with
      | Error e -> Error e
      | Ok() ->
        // struct stat st_uid offset:
        //   macOS: 16, Linux x86_64: 28, Linux aarch64: 24, Linux armv7: 24
        let uid =
          if isMac then
            uint32 (Marshal.ReadInt32(buf, 16))
          elif isArm64 || isArm32 then
            uint32 (Marshal.ReadInt32(buf, 24))
          else // x86_64 Linux (guarded by startup check)
            uint32 (Marshal.ReadInt32(buf, 28))
        Ok(string uid)
    finally
      Marshal.FreeHGlobal buf

let kill (pid : int) (signal : int) : Result<unit, int * string> =
  if isWasm then Managed.enosys ()
  else if kill_raw (pid, signal) < 0 then Error(lastError ())
  else Ok()

let fdRead (fd : int) (count : int) : Result<byte[], int * string> =
  if count < 0 then
    Error(22, "Invalid argument") // EINVAL
  else if isWasm then
    Managed.fdRead fd count
  else
    let buf = Array.zeroCreate<byte> count
    let n = read_raw (fd, buf, count)
    if n < 0 then Error(lastError ()) else Ok(buf[0 .. n - 1])

let fdSeek (fd : int) (offset : int64) (whence : int) : Result<int64, int * string> =
  if isWasm then
    Managed.fdSeek fd offset whence
  else
    let position = lseek_raw (fd, offset, whence)
    if position < 0L then Error(lastError ()) else Ok position

/// Read one terminal file descriptor's window size as (columns, rows).
/// Direct kernel calls avoid Darwin libc's variadic ioctl calling convention.
let tryTerminalWindowSize (fd : int) : Option<int64 * int64> =
  if isWasm then
    None
  else if OperatingSystem.IsWindows() then
    None
  else
    let request = if isMac then 0x40087468UL else 0x5413UL // TIOCGWINSZ

    let buffer = Marshal.AllocHGlobal 8
    try
      if ioctl_raw (fd, request, buffer) = 0 then
        let rows = uint16 (Marshal.ReadInt16(buffer, 0))
        let columns = uint16 (Marshal.ReadInt16(buffer, 2))
        if rows > 0us && columns > 0us then Some(int64 columns, int64 rows) else None
      else
        None
    finally
      Marshal.FreeHGlobal buffer

let fdWrite (fd : int) (data : byte[]) : Result<int, int * string> =
  if isWasm then
    Managed.fdWrite fd data
  else if data.Length = 0 then
    Ok 0
  else
    let n = write_raw (fd, data, data.Length)
    if n < 0 then Error(lastError ()) else Ok n

let fdClose (fd : int) : Result<unit, int * string> =
  if isWasm then Managed.fdClose fd
  else if close_raw (fd) < 0 then Error(lastError ())
  else Ok()

let copyFileRange source destination count : Result<int64, int * string> =
  if count < 0 then
    Error(22, "Invalid argument")
  elif isWasm || OperatingSystem.IsWindows() then
    Error(38, "unavailable")
  else
    let rc = copyFileRange_raw (source, destination, count)
    if rc < 0L then failed () else Ok rc

let cloneFile (source : int) (destination : int) : Result<unit, int * string> =
  if isMac || isWasm || OperatingSystem.IsWindows() then
    Error(38, "unavailable")
  else
    unitResult (ioctl_raw (destination, 0x40049409UL, IntPtr source))

let ftruncate fd length =
  if isWasm || OperatingSystem.IsWindows() then
    Error(38, "unavailable")
  else
    unitResult (ftruncate_raw (fd, length))
let fchmod fd mode =
  if isWasm || OperatingSystem.IsWindows() then
    Error(38, "unavailable")
  else
    unitResult (fchmod_raw (fd, mode))

let fstat fd : Result<byte[], int * string> =
  if isWasm || OperatingSystem.IsWindows() then
    Error(38, "unavailable")
  else
    let bytes = Array.zeroCreate<byte> 256
    let rc = fstat_raw (fd, bytes)
    if rc < 0 then failed () else Ok bytes

/// One entropy call. Dark owns filling/retry loops. Darwin caps requests at 256.
let getentropy count : Result<byte[], int * string> =
  if count < 0 || count > 256 then
    Error(22, "Invalid argument")
  elif isWasm || OperatingSystem.IsWindows() then
    Error(38, "unavailable")
  else
    let bytes = Array.zeroCreate<byte> count
    let rc = getentropy_raw bytes
    if rc < 0 then failed ()
    elif isMac then Ok bytes
    else Ok(Array.take rc bytes)

let pread fd count offset : Result<byte[], int * string> =
  if count < 0 then
    Error(22, "Invalid argument")
  elif isWasm || OperatingSystem.IsWindows() then
    Error(38, "unavailable")
  else
    let bytes = Array.zeroCreate<byte> count
    let rc = pread_raw (fd, bytes, offset)
    if rc < 0 then failed () else Ok(Array.take rc bytes)

let pwrite fd bytes offset : Result<int, int * string> =
  if isWasm || OperatingSystem.IsWindows() then
    Error(38, "unavailable")
  else
    let rc = pwrite_raw (fd, bytes, offset)
    if rc < 0 then failed () else Ok rc

/// Read kernel directory records from a held, no-follow directory descriptor.
let listDir (path : string) : Result<List<string>, int * string> =
  if isWasm then
    Managed.listDir path
  else
    withDirectory path (fun fd ->
      let bytes = Array.zeroCreate<byte> 32768
      let position = Marshal.AllocHGlobal(8)
      try
        Marshal.WriteInt64(position, 0L)
        let entries = Collections.Generic.List<string>()
        let mutable error = None
        let mutable count = getdents_raw (fd, bytes, position)
        while count > 0 && error.IsNone do
          let mutable offset = 0
          while offset < count && error.IsNone do
            let nameOffset = if isMac then 21 else 19
            let recordOffset = 16
            if offset + nameOffset >= count then
              error <- Some(5, "Invalid directory record")
            else
              let length = int (BitConverter.ToUInt16(bytes, offset + recordOffset))
              if length <= nameOffset || offset + length > count then
                error <- Some(5, "Invalid directory record length")
              else
                let start = offset + nameOffset
                let mutable finish = start
                while finish < offset + length && bytes[finish] <> 0uy do
                  finish <- finish + 1
                if finish = offset + length then
                  error <- Some(5, "Unterminated directory entry")
                else
                  let name =
                    Text.Encoding.UTF8.GetString(bytes, start, finish - start)
                  if name <> "." && name <> ".." then entries.Add name
                offset <- offset + length
          if error.IsNone then count <- getdents_raw (fd, bytes, position)
        if count < 0 then
          Error(lastError ())
        else
          match error with
          | Some e -> Error e
          | None -> Ok(List.ofSeq entries)
      finally
        Marshal.FreeHGlobal position)

// -- Atomic, symlink-safe opens (Linux) ------------------------------
// The lexical `pathContainsLink` pre-walk in `Host` is advisory: an attacker
// can swap a path component for a symlink between the check and the open.
// `openat2(RESOLVE_NO_SYMLINKS)` makes symlink rejection part of the open
// itself — the same rejection semantics as the pre-walk (any symlink in the
// path fails), but with no window. On older Linux kernels and other POSIX
// systems, callers fall back to the descriptor-based O_NOFOLLOW walk.

[<Literal>]
let private AT_FDCWD = -100

// Resolve every component without following any symlink.
[<Literal>]
let private RESOLVE_NO_SYMLINKS = 0x04UL

[<Literal>]
let private ENOSYS = 38

let mutable private openat2Available = true

/// Open an absolute path rejecting any symlink component, atomically. None
/// signals "openat2 unavailable on this kernel" (caller falls back); an OS
/// error (including a symlink hit, ELOOP) raises like the ordinary API would.
let private openNoSymlinks
  (path : string)
  (flags : int)
  (mode : uint64)
  : Option<SafeFileHandle> =
  if not (openat2Available && RuntimeInformation.IsOSPlatform OSPlatform.Linux) then
    None
  else
    let how = Array.zeroCreate 24
    System.BitConverter.GetBytes(uint64 flags).CopyTo(how, 0)
    System.BitConverter.GetBytes(mode).CopyTo(how, 8)
    System.BitConverter.GetBytes(RESOLVE_NO_SYMLINKS).CopyTo(how, 16)
    let fd = openat2_raw (AT_FDCWD, path, how, unativeint 24)
    if fd >= 0L then
      Some(new SafeFileHandle(nativeint fd, ownsHandle = true))
    else
      let errno = Marshal.GetLastPInvokeError()
      if errno = ENOSYS then
        // No openat2 on this kernel; remember and fall back everywhere.
        openat2Available <- false
        None
      else
        // A real error — a symlink component (ELOOP), missing file, denied,
        // etc. Surface it as the matching .NET exception so callers classify
        // it exactly as they would an ordinary open failure.
        raise (System.ComponentModel.Win32Exception errno)

/// Open without following any link: openat2 where the kernel has it, the
/// descriptor walk elsewhere on POSIX, and `None` only on Windows (which
/// keeps the .NET call behind the lexical pre-check).
let private openSafely
  (path : string)
  (flags : int)
  (mode : int)
  : Option<SafeFileHandle> =
  match openNoSymlinks path flags (uint64 mode) with
  | Some handle -> Some handle
  | None ->
    if not isPosix then
      None
    else
      match openFile path flags mode with
      | Ok fd -> Some(new SafeFileHandle(nativeint fd, ownsHandle = true))
      | Error(errno, _) -> raise (System.ComponentModel.Win32Exception errno)

/// Read the whole file, rejecting symlinked paths atomically.
let readAllBytes (path : string) : byte[] =
  match openSafely path O_RDONLY 0 with
  | Some handle ->
    use handle = handle
    use stream = new System.IO.FileStream(handle, System.IO.FileAccess.Read)
    use memory = new System.IO.MemoryStream()
    stream.CopyTo memory
    memory.ToArray()
  | None -> System.IO.File.ReadAllBytes path

/// Create or truncate the file and write it, rejecting symlinked paths.
let writeAllBytes (path : string) (bytes : byte[]) : unit =
  match openSafely path (O_WRONLY ||| O_CREAT ||| O_TRUNC) 0o644 with
  | Some handle ->
    use handle = handle
    use stream = new System.IO.FileStream(handle, System.IO.FileAccess.Write)
    stream.Write(bytes, 0, bytes.Length)
  | None -> System.IO.File.WriteAllBytes(path, bytes)

/// Append text (creating the file if needed), rejecting symlinked paths.
let appendAllText (path : string) (content : string) : unit =
  match openSafely path (O_WRONLY ||| O_CREAT ||| O_APPEND) 0o644 with
  | Some handle ->
    use handle = handle
    use stream = new System.IO.FileStream(handle, System.IO.FileAccess.Write)
    let bytes = System.Text.Encoding.UTF8.GetBytes content
    stream.Write(bytes, 0, bytes.Length)
  | None -> System.IO.File.AppendAllText(path, content)

/// Entry metadata for the boundary's `FileStat`: (exists, isDirectory).
/// A missing entry is not an error; anything else is.
let statEntry (path : string) : Result<bool * bool, int * string> =
  match stat path with
  | Ok(mode, _, _) -> Ok(true, (mode &&& 0xF000) = 0x4000)
  | Error(2, _)
  | Error(20, _) -> Ok(false, false) // ENOENT, ENOTDIR
  | Error e -> Error e

/// Trusted policy persistence, using the same no-follow descriptor walk.
let syncDirectory (directory : string) : unit =
  if isPosix then
    try
      withDirectory directory (fun fd -> unitResult (fsync_raw fd))
      |> ignore<Result<unit, int * string>>
    with _ ->
      ()
