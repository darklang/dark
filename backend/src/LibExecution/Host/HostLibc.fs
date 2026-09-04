/// Posix libc bridge via P/Invoke — thin wrappers around OS functions,
/// owned by the checked host boundary. Every wrapper returns the libc
/// `(errno, message)` pair on failure. Scoped operations (paths, env names,
/// raw fds) reach these through `Host.perform`; the few ambient Native-gated
/// builtins that carry no state (uname, getpid, ...) call them directly, with
/// the interpreter's ambient Native check as their gate. Process spawning is
/// .NET-backed and lives in `HostProcess`. Linux (x86_64, aarch64, armv7) and
/// macOS only. Will not work on Windows.
module LibExecution.HostLibc

open System
open System.Runtime.InteropServices
open Microsoft.Win32.SafeHandles

open Prelude

// F# generates implicit failwith calls inside extern/P/Invoke stubs.
// The project bans the built-in failwith, so we shadow it here.
let private failwith (s : string) : 'a = raise (System.Exception(s))

// -- Filesystem ---------------------------------------------------
[<DllImport("libc", EntryPoint = "mkdir", SetLastError = true)>]
extern int private mkdir_raw(string path, int mode)

[<DllImport("libc", EntryPoint = "rmdir", SetLastError = true)>]
extern int private rmdir_raw(string path)

[<DllImport("libc", EntryPoint = "unlink", SetLastError = true)>]
extern int private unlink_raw(string path)

[<DllImport("libc", EntryPoint = "stat", SetLastError = true)>]
extern int private stat_raw(string path, IntPtr buf)

// glibc only began exporting `stat` and `lstat` as ordinary symbols in
// 2.33. Before that they were header inlines over `__xstat`/`__lxstat`,
// which take a leading struct-version argument. A P/Invoke to "stat" throws
// EntryPointNotFoundException on any older system, so we need both and pick
// at runtime.
//
// This matters more than it looks: it's not an exotic-platform problem. The
// release build targets glibc 2.29 precisely so it runs on older distros,
// and those are exactly the ones without `stat`. Verified failing on Ubuntu
// 20.04 (glibc 2.31) with the AOT binary; the R2R build has the same bug.
// It goes unnoticed because dev machines and CI runners are all 2.33+.
[<DllImport("libc", EntryPoint = "__xstat", SetLastError = true)>]
extern int private xstat_raw(int version, string path, IntPtr buf)

[<DllImport("libc", EntryPoint = "rename", SetLastError = true)>]
extern int private rename_raw(string oldpath, string newpath)

[<DllImport("libc", EntryPoint = "chmod", SetLastError = true)>]
extern int private chmod_raw(string path, int mode)

[<DllImport("libc", EntryPoint = "symlink", SetLastError = true)>]
extern int private symlink_raw(string target, string linkpath)

[<DllImport("libc", EntryPoint = "readlink", SetLastError = true)>]
extern int private readlink_raw(string path, byte[] buf, int bufsiz)

[<DllImport("libc", EntryPoint = "mkstemp", SetLastError = true)>]
extern int private mkstemp_raw(byte[] template)

[<DllImport("libc", EntryPoint = "mkdtemp", SetLastError = true)>]
extern IntPtr private mkdtemp_raw(byte[] template)

[<DllImport("libc", EntryPoint = "fnmatch")>]
extern int private fnmatch_raw(string pattern, string str, int flags)

[<DllImport("libc", EntryPoint = "flock", SetLastError = true)>]
extern int private flock_raw(int fd, int operation)

[<DllImport("libc", EntryPoint = "getpwuid")>]
extern IntPtr private getpwuid_raw(uint32 uid)

[<DllImport("libc", EntryPoint = "chdir", SetLastError = true)>]
extern int private chdir_raw(string path)

[<DllImport("libc", EntryPoint = "setenv", SetLastError = true)>]
extern int private setenv_raw(string name, string value, int overwrite)

[<DllImport("libc", EntryPoint = "unsetenv", SetLastError = true)>]
extern int private unsetenv_raw(string name)

[<DllImport("libc", EntryPoint = "getcwd", SetLastError = true)>]
extern IntPtr private getcwd_raw(IntPtr buf, int size)

[<DllImport("libc", EntryPoint = "uname", SetLastError = true)>]
extern int private uname_raw(IntPtr buf)

[<DllImport("libc", EntryPoint = "getpid")>]
extern int private getpid_raw()

[<DllImport("libc", EntryPoint = "getuid")>]
extern uint32 private getuid_raw()

[<DllImport("libc", EntryPoint = "sysconf")>]
extern int64 private sysconf_raw(int name)

[<DllImport("libc", EntryPoint = "strerror")>]
extern IntPtr private strerror_raw(int errnum)

[<DllImport("libc", EntryPoint = "opendir", SetLastError = true)>]
extern IntPtr private opendir_raw(string path)

[<DllImport("libc", EntryPoint = "readdir", SetLastError = true)>]
extern IntPtr private readdir_raw(IntPtr dirp)

[<DllImport("libc", EntryPoint = "closedir")>]
extern int private closedir_raw(IntPtr dirp)

[<DllImport("libc", EntryPoint = "getenv")>]
extern IntPtr private getenv_raw(string name)

[<DllImport("libc", EntryPoint = "utimes", SetLastError = true)>]
extern int private utimes_raw(string path, IntPtr times)

// -- Directory-relative (*at) calls -----------------------------------
// Every path operation below walks to the parent directory one component at
// a time with O_NOFOLLOW and finishes with one of these on the held
// descriptor, so a component cannot be swapped for a link between the
// permission check and the operation.
[<DllImport("libc", EntryPoint = "openat", SetLastError = true)>]
extern int private openat_raw(int dirfd, string path, int flags, int mode)

[<DllImport("libc", EntryPoint = "unlinkat", SetLastError = true)>]
extern int private unlinkat_raw(int dirfd, string path, int flags)

[<DllImport("libc", EntryPoint = "mkdirat", SetLastError = true)>]
extern int private mkdirat_raw(int dirfd, string path, int mode)

[<DllImport("libc", EntryPoint = "fstatat", SetLastError = true)>]
extern int private fstatat_raw(int dirfd, string path, IntPtr buf, int flags)

// The pre-2.33 glibc spelling, like `__xstat` for `stat` below.
[<DllImport("libc", EntryPoint = "__fxstatat", SetLastError = true)>]
extern int private fxstatat_raw(
  int version,
  int dirfd,
  string path,
  IntPtr buf,
  int flags
)

[<DllImport("libc", EntryPoint = "renameat", SetLastError = true)>]
extern int private renameat_raw(
  int olddirfd,
  string oldpath,
  int newdirfd,
  string newpath
)

[<DllImport("libc", EntryPoint = "fchmodat", SetLastError = true)>]
extern int private fchmodat_raw(int dirfd, string path, int mode, int flags)

[<DllImport("libc", EntryPoint = "utimensat", SetLastError = true)>]
extern int private utimensat_raw(int dirfd, string path, IntPtr times, int flags)

[<DllImport("libc", EntryPoint = "symlinkat", SetLastError = true)>]
extern int private symlinkat_raw(string target, int newdirfd, string linkpath)

[<DllImport("libc", EntryPoint = "readlinkat", SetLastError = true)>]
extern int private readlinkat_raw(int dirfd, string path, byte[] buf, int bufsiz)

[<DllImport("libc", EntryPoint = "fdopendir", SetLastError = true)>]
extern IntPtr private fdopendir_raw(int fd)

[<DllImport("libc", EntryPoint = "fchdir", SetLastError = true)>]
extern int private fchdir_raw(int fd)

[<DllImport("libc", EntryPoint = "dup", SetLastError = true)>]
extern int private dup_raw(int fd)

// -- Process ------------------------------------------------------
[<DllImport("libc", EntryPoint = "kill", SetLastError = true)>]
extern int private kill_raw(int pid, int signal)

// -- File I/O -----------------------------------------------------
[<DllImport("libc", EntryPoint = "open", SetLastError = true)>]
extern int private open_raw(string path, int flags, int mode)

[<DllImport("libc", EntryPoint = "read", SetLastError = true)>]
extern int private read_raw(int fd, byte[] buf, int count)

[<DllImport("libc", EntryPoint = "lseek", SetLastError = true)>]
extern int64 private lseek_raw(int fd, int64 offset, int whence)

[<DllImport("libc", EntryPoint = "ioctl", SetLastError = true)>]
extern int private ioctl_raw(int fd, uint64 request, IntPtr argument)

[<DllImport("libc", EntryPoint = "write", SetLastError = true)>]
extern int private write_raw(int fd, byte[] buf, int count)

[<DllImport("libc", EntryPoint = "close", SetLastError = true)>]
extern int private close_raw(int fd)


// -- Platform detection ---------------------------------------------
let private isMac = RuntimeInformation.IsOSPlatform OSPlatform.OSX
let private isArm64 = RuntimeInformation.ProcessArchitecture = Architecture.Arm64
let private isX64 = RuntimeInformation.ProcessArchitecture = Architecture.X64
/// 32-bit ARM (armv7). Distinct from Arm64 in more than pointer width: off_t
/// and time_t are 32 bits here, so struct stat's size and mtime fields are
/// half the width they are everywhere else.
let private isArm32 = RuntimeInformation.ProcessArchitecture = Architecture.Arm

do
  if not isMac && not isArm64 && not isX64 && not isArm32 then
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
let private EEXIST = 17
/// True where the libc bridge is the filesystem implementation; Windows
/// keeps the .NET calls behind the lexical pre-check.
let isPosix : bool = not (RuntimeInformation.IsOSPlatform OSPlatform.Windows)
let SEEK_SET = 0
let SEEK_CUR = 1
let SEEK_END = 2


// -- Wrappers -----------------------------------------------------

let lastError () : int * string =
  let errno = Marshal.GetLastPInvokeError()
  let ptr = strerror_raw (errno)
  let msg =
    if ptr = IntPtr.Zero then $"errno {errno}" else Marshal.PtrToStringAnsi ptr
  (errno, msg)

let getcwd () : Result<string, int * string> =
  let buf = Marshal.AllocHGlobal(4096)
  try
    let ptr = getcwd_raw (buf, 4096)
    if ptr = IntPtr.Zero then
      Error(lastError ())
    else
      Ok(Marshal.PtrToStringAnsi ptr)
  finally
    Marshal.FreeHGlobal buf

let setenv (name : string) (value : string) : Result<unit, int * string> =
  if setenv_raw (name, value, 1) < 0 then Error(lastError ()) else Ok()

let unsetenv (name : string) : Result<unit, int * string> =
  if unsetenv_raw (name) < 0 then Error(lastError ()) else Ok()


// -- The directory walk ----------------------------------------------------
// `withParent` opens "/" and then each ancestor with O_NOFOLLOW|O_DIRECTORY,
// so a link anywhere on the way fails with ELOOP/ENOTDIR at the moment it
// is met, and hands the caller the parent's descriptor plus the final name.
// The boundary already resolved the ancestors it allowed, so on the happy
// path no link is met; the walk is what makes that resolution binding.

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

/// Open the final component itself as a directory (listing, chdir).
let private withDirectory
  (path : string)
  (action : int -> Result<'a, int * string>)
  : Result<'a, int * string> =
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
  withDirectory path (fun fd -> unitResult (fchdir_raw fd))

let mkdir (path : string) (mode : int) : Result<unit, int * string> =
  withParent path (fun d n -> unitResult (mkdirat_raw (d, n, mode)))

let rmdir (path : string) : Result<unit, int * string> =
  withParent path (fun d n -> unitResult (unlinkat_raw (d, n, AT_REMOVEDIR)))

let unlink (path : string) : Result<unit, int * string> =
  withParent path (fun d n -> unitResult (unlinkat_raw (d, n, 0)))

let rename (oldpath : string) (newpath : string) : Result<unit, int * string> =
  withParent oldpath (fun d1 n1 ->
    withParent newpath (fun d2 n2 -> unitResult (renameat_raw (d1, n1, d2, n2))))

let symlink (target : string) (linkpath : string) : Result<unit, int * string> =
  withParent linkpath (fun d n -> unitResult (symlinkat_raw (target, d, n)))

let readlink (path : string) : Result<string, int * string> =
  withParent path (fun d n ->
    let buf = Array.zeroCreate<byte> 4096
    let len = readlinkat_raw (d, n, buf, 4096)
    if len < 0 then
      failed ()
    else
      Ok(System.Text.Encoding.UTF8.GetString(buf, 0, len)))

/// Six random name characters, as mkstemp uses.
let private randomSuffix () : string =
  let alphabet = "abcdefghijklmnopqrstuvwxyzABCDEFGHIJKLMNOPQRSTUVWXYZ0123456789"
  let bytes = Array.zeroCreate<byte> 6
  System.Security.Cryptography.RandomNumberGenerator.Fill bytes
  bytes |> Array.map (fun b -> alphabet[int b % alphabet.Length]) |> String

/// Create a unique entry next to `prefix`, retrying on EEXIST the way
/// mkstemp/mkdtemp do, but relative to the walked parent directory.
let private createUnique
  (prefix : string)
  (create : int -> string -> int)
  : Result<string, int * string> =
  let directory = System.IO.Path.GetDirectoryName prefix
  withParent prefix (fun d basePrefix ->
    let rec attempt (remaining : int) : Result<string, int * string> =
      let name = basePrefix + randomSuffix ()
      let rc = create d name
      if rc >= 0 then
        Ok(System.IO.Path.Combine(directory, name))
      else
        let (errno, message) = lastError ()
        if errno = EEXIST && remaining > 0 then
          attempt (remaining - 1)
        else
          Error(errno, message)
    attempt 100)

let mkstemp (prefix : string) : Result<int * string, int * string> =
  let mutable opened = -1
  createUnique prefix (fun d name ->
    let fd =
      openat_raw (d, name, O_RDWR ||| O_CREAT ||| O_EXCL ||| O_NOFOLLOW, 0o600)
    opened <- fd
    fd)
  |> Result.map (fun path -> opened, path)

let mkdtemp (prefix : string) : Result<string, int * string> =
  createUnique prefix (fun d name -> mkdirat_raw (d, name, 0o700))

let openFile (path : string) (flags : int) (mode : int) : Result<int, int * string> =
  withParent path (fun d n ->
    let fd = openat_raw (d, n, flags ||| O_NOFOLLOW, mode)
    if fd < 0 then failed () else Ok fd)

/// The struct-version argument __xstat expects, per architecture. Measured
/// from _STAT_VER in each target's glibc headers rather than guessed:
/// x86_64 is 1, aarch64 is 0, armv7 is 3. macOS never uses this path.
let private statVer =
  if isX64 then 1
  elif isArm64 then 0
  else 3 // armv7

/// Whether libc exports `stat` directly (glibc 2.33+), or we have to go
/// through `__xstat`. Resolved once, on first use: the check itself is a
/// P/Invoke that throws when the symbol is absent.
let private useXstat =
  lazy
    (if isMac then
       false
     else
       // Probe with a real buffer and a real path. Passing NULL would work
       // only for as long as the path stays nonexistent, and libc writing
       // to NULL is a segfault rather than an error return. We only care
       // whether the *symbol* resolves, so the result is discarded.
       let buf = Marshal.AllocHGlobal(256)

       try
         try
           stat_raw ("/", buf) |> ignore<int>
           false
         with :? EntryPointNotFoundException ->
           true
       finally
         Marshal.FreeHGlobal buf)

/// stat(2), routed through whichever entry point this libc actually has.
let private stat_compat (path : string) (buf : IntPtr) : int =
  if useXstat.Force() then
    xstat_raw (statVer, path, buf)
  else
    stat_raw (path, buf)

/// fstatat(2) through whichever entry point this libc has; the final
/// component is not followed, so a link that appeared since the check is
/// reported as itself rather than resolved.
let private fstatat_compat
  (dirfd : int)
  (name : string)
  (buf : IntPtr)
  (flags : int)
  : int =
  if useXstat.Force() then
    fxstatat_raw (statVer, dirfd, name, buf, flags)
  else
    fstatat_raw (dirfd, name, buf, flags)

/// Walk to the parent and stat the entry into `buf`.
let private statInto (path : string) (buf : IntPtr) : Result<unit, int * string> =
  withParent path (fun d n ->
    unitResult (fstatat_compat d n buf AT_SYMLINK_NOFOLLOW))

let private modeFromStatBuffer (buf : IntPtr) : int =
  if isMac then
    int (Marshal.ReadInt16(buf, 4)) &&& 0xFFFF
  elif isArm64 || isArm32 then
    Marshal.ReadInt32(buf, 16)
  else // x86_64 Linux (guarded by startup check)
    Marshal.ReadInt32(buf, 24)

/// Linux metadata operations need to work on mode-000 files while binding the
/// operation to the entry checked at execution time. O_PATH holds the exact
/// inode without data-access permission; fstatat(AT_EMPTY_PATH) rejects a
/// final symlink, then /proc/self/fd addresses that held inode rather than a
/// racy pathname. `/proc` being unavailable fails closed.
let private withLinuxMetadataPath
  (path : string)
  (action : string -> Result<'a, int * string>)
  : Result<'a, int * string> =
  withParent path (fun dirFd name ->
    let fd = openat_raw (dirFd, name, O_PATH ||| O_NOFOLLOW, 0)
    if fd < 0 then
      failed ()
    else
      let buf = Marshal.AllocHGlobal(256)
      try
        if fstatat_compat fd "" buf AT_EMPTY_PATH < 0 then
          failed ()
        elif modeFromStatBuffer buf &&& 0o170000 = 0o120000 then
          Error(40, "Too many levels of symbolic links") // Linux ELOOP
        else
          action $"/proc/self/fd/{fd}"
      finally
        Marshal.FreeHGlobal buf
        close_raw fd |> ignore<int>)

let chmod (path : string) (mode : int) : Result<unit, int * string> =
  if isMac then
    withParent path (fun d n ->
      unitResult (fchmodat_raw (d, n, mode, AT_SYMLINK_NOFOLLOW)))
  else
    withLinuxMetadataPath path (fun held -> unitResult (chmod_raw (held, mode)))

/// Update atime and mtime to now without following the final component.
let utimesNow (path : string) : Result<unit, int * string> =
  if isMac then
    withParent path (fun d n ->
      unitResult (utimensat_raw (d, n, IntPtr.Zero, AT_SYMLINK_NOFOLLOW)))
  else
    withLinuxMetadataPath path (fun held ->
      unitResult (utimes_raw (held, IntPtr.Zero)))

/// Extracts (mode, size, mtimeSec) from a struct stat buffer. Offsets are
/// platform-specific (Linux vs macOS struct layouts differ).
let stat (path : string) : Result<int * int64 * int64, int * string> =
  let buf = Marshal.AllocHGlobal(256)
  try
    match statInto path buf with
    | Error e -> Error e
    | Ok() ->
      // struct stat field offsets differ across OS and architecture:
      //   macOS (all):   st_mode at 4 (int16), st_size at 96, st_mtime at 48
      //   Linux x86_64:  st_mode at 24, st_size at 48, st_mtime at 88
      //   Linux aarch64: st_mode at 16, st_size at 48, st_mtime at 88
      //   Linux armv7:   st_mode at 16, st_size at 44, st_mtime at 64
      // armv7 is the odd one: off_t and time_t are 32 bits, so size and
      // mtime are Int32 reads. Reading them as Int64 there gets garbage from
      // the adjacent field. Offsets measured against glibc 2.31 armhf.
      let mode = modeFromStatBuffer buf
      let size =
        if isMac then Marshal.ReadInt64(buf, 96)
        elif isArm32 then int64 (Marshal.ReadInt32(buf, 44))
        else Marshal.ReadInt64(buf, 48)
      let mtimeSec =
        if isMac then Marshal.ReadInt64(buf, 48)
        elif isArm32 then int64 (Marshal.ReadInt32(buf, 64))
        else Marshal.ReadInt64(buf, 88)
      Ok(mode, size, mtimeSec)
  finally
    Marshal.FreeHGlobal buf

/// Calls uname() and returns (sysname, nodename, machine).
let uname () : Result<string * string * string, int * string> =
  let fieldSize = if isMac then 256 else 65
  let bufSize = fieldSize * 6 // 5 fields + extra
  let buf = Marshal.AllocHGlobal(bufSize)
  try
    if uname_raw (buf) < 0 then
      Error(lastError ())
    else
      let sysname = Marshal.PtrToStringAnsi(IntPtr.Add(buf, 0))
      let nodename = Marshal.PtrToStringAnsi(IntPtr.Add(buf, fieldSize))
      let machine = Marshal.PtrToStringAnsi(IntPtr.Add(buf, fieldSize * 4))
      Ok(sysname, nodename, machine)
  finally
    Marshal.FreeHGlobal buf

let getpid () : int = getpid_raw ()

let getuid () : uint32 = getuid_raw ()

let cpuCount () : int64 =
  let scNprocessorsOnl = if isMac then 58 else 84 // Linux _SC_NPROCESSORS_ONLN
  sysconf_raw (scNprocessorsOnl)

/// fnmatch returns true if the string matches the pattern.
let fnmatch (pattern : string) (str : string) (flags : int) : bool =
  fnmatch_raw (pattern, str, flags) = 0

let FNM_PATHNAME = if isMac then 2 else 1 // Linux

/// flock operations
let LOCK_EX = 2
let LOCK_UN = 8

let flock (fd : int) (operation : int) : Result<unit, int * string> =
  if flock_raw (fd, operation) < 0 then Error(lastError ()) else Ok()

/// Get username from uid via getpwuid
let getUserName (uid : uint32) : Option<string> =
  let ptr = getpwuid_raw (uid)
  if ptr = IntPtr.Zero then
    None
  else
    // First field of struct passwd is char *pw_name
    let namePtr = Marshal.ReadIntPtr(ptr, 0)
    if namePtr = IntPtr.Zero then None else Some(Marshal.PtrToStringAnsi namePtr)

/// Get home directory for the current user via getpwuid(getuid()).
/// Returns pw_dir from the passwd db. The $HOME fallback is in Cli.Env.home().
let getHomeDir () : Option<string> =
  let uid = getuid_raw ()
  let ptr = getpwuid_raw (uid)
  if ptr = IntPtr.Zero then
    None
  else
    let dirOffset = if isMac then 48 else 32
    let dirPtr = Marshal.ReadIntPtr(ptr, dirOffset)
    if dirPtr = IntPtr.Zero then None else Some(Marshal.PtrToStringAnsi dirPtr)

/// Get the owner username of a file (stat + getpwuid).
let fileOwner (path : string) : Result<string, int * string> =
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
      match getUserName uid with
      | Some name -> Ok name
      | None -> Ok(string uid)
  finally
    Marshal.FreeHGlobal buf

let getenv (name : string) : Option<string> =
  let ptr = getenv_raw (name)
  if ptr = IntPtr.Zero then None else Some(Marshal.PtrToStringAnsi ptr)

let kill (pid : int) (signal : int) : Result<unit, int * string> =
  if kill_raw (pid, signal) < 0 then Error(lastError ()) else Ok()

let fdRead (fd : int) (count : int) : Result<byte[], int * string> =
  if count < 0 then
    Error(22, "Invalid argument") // EINVAL
  else
    let buf = Array.zeroCreate<byte> count
    let n = read_raw (fd, buf, count)
    if n < 0 then Error(lastError ()) else Ok(buf[0 .. n - 1])

let fdSeek (fd : int) (offset : int64) (whence : int) : Result<int64, int * string> =
  let position = lseek_raw (fd, offset, whence)
  if position < 0L then Error(lastError ()) else Ok position

/// Read one terminal file descriptor's window size as (columns, rows).
///
/// Disabled on macOS. Darwin's ioctl is variadic, but this P/Invoke declares
/// a fixed third argument. On Apple arm64, those signatures use different
/// calling conventions, so ioctl may receive an invalid output pointer and
/// corrupt memory. The caller uses its terminal-size fallback instead.
let tryTerminalWindowSize (fd : int) : Option<int64 * int64> =
  if OperatingSystem.IsWindows() || isMac then
    None
  else
    let request = 0x5413UL // TIOCGWINSZ on Linux

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
  let mutable offset = 0
  let mutable error = None
  while offset < data.Length && error.IsNone do
    let slice = if offset = 0 then data else data[offset..]
    let n = write_raw (fd, slice, data.Length - offset)
    if n < 0 then error <- Some(lastError ())
    elif n = 0 then error <- Some(0, "write returned 0")
    else offset <- offset + n
  match error with
  | Some e -> Error e
  | None -> Ok offset

let fdClose (fd : int) : Result<unit, int * string> =
  if close_raw (fd) < 0 then Error(lastError ()) else Ok()

/// List directory entries (wraps opendir/readdir/closedir loop).
/// Returns filenames only, not "." or "..".
let listDir (path : string) : Result<List<string>, int * string> =
  withDirectory path (fun fd ->
    // fdopendir takes ownership of a duplicate; closedir releases it, and
    // withDirectory closes the original.
    let dirp = fdopendir_raw (dup_raw fd)
    if dirp = IntPtr.Zero then
      Error(lastError ())
    else
      let entries = System.Collections.Generic.List<string>()
      let mutable keepGoing = true
      let mutable error = None
      while keepGoing do
        Marshal.SetLastPInvokeError(0)
        let entryPtr = readdir_raw (dirp)
        if entryPtr = IntPtr.Zero then
          let errno = Marshal.GetLastPInvokeError()
          if errno <> 0 then error <- Some(lastError ())
          keepGoing <- false
        else
          // struct dirent: d_name offset varies by platform
          let nameOffset = if isMac then 21 else 19 // Linux
          let namePtr = IntPtr.Add(entryPtr, nameOffset)
          let name = Marshal.PtrToStringAnsi namePtr
          if name <> "." && name <> ".." then entries.Add(name)
      closedir_raw (dirp) |> ignore<int>
      match error with
      | Some e -> Error e
      | None -> Ok(Seq.toList entries))


// -- Atomic, symlink-safe opens (Linux) ------------------------------
// The lexical `pathContainsLink` pre-walk in `Host` is advisory: an attacker
// can swap a path component for a symlink between the check and the open.
// `openat2(RESOLVE_NO_SYMLINKS)` makes symlink rejection part of the open
// itself — the same rejection semantics as the pre-walk (any symlink in the
// path fails), but with no window. On older Linux kernels and other POSIX
// systems, callers fall back to the descriptor-based O_NOFOLLOW walk.

[<Literal>]
let private SYS_openat2 = 437L

[<Literal>]
let private AT_FDCWD = -100

// Resolve every component without following any symlink.
[<Literal>]
let private RESOLVE_NO_SYMLINKS = 0x04UL

[<Literal>]
let private ENOSYS = 38

/// struct open_how { __u64 flags; __u64 mode; __u64 resolve; } — 24 bytes.
[<DllImport("libc", SetLastError = true, EntryPoint = "syscall")>]
extern int64 private openat2_raw(
  int64 number,
  int dirfd,
  string pathname,
  byte[] how,
  unativeint size
)

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
    let fd = openat2_raw (SYS_openat2, AT_FDCWD, path, how, unativeint 24)
    if fd >= 0L then
      Some(new SafeFileHandle(nativeint fd, ownsHandle = true))
    else
      let errno = Marshal.GetLastWin32Error()
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
