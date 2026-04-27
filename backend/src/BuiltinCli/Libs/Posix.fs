/// Posix libc bridge via P/Invoke — thin wrappers around OS functions.
/// Dark code builds on top of these instead of shelling out to bash.
/// Linux (x86_64, aarch64) and macOS only. Will not work on Windows.
module BuiltinCli.Libs.Posix

open System
open System.Runtime.InteropServices

open Prelude
open LibExecution.RuntimeTypes

module VT = LibExecution.ValueType
module Dval = LibExecution.Dval
module Builtin = LibExecution.Builtin
module PackageRefs = LibExecution.PackageRefs
module NR = LibExecution.RuntimeTypes.NameResolution
open Builtin.Shortcuts


// =====================================================================
// libc via P/Invoke
// =====================================================================
module Libc =
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

  // -- Process ------------------------------------------------------
  [<DllImport("libc", EntryPoint = "kill", SetLastError = true)>]
  extern int private kill_raw(int pid, int signal)

  // -- File I/O -----------------------------------------------------
  [<DllImport("libc", EntryPoint = "open", SetLastError = true)>]
  extern int private open_raw(string path, int flags, int mode)

  [<DllImport("libc", EntryPoint = "read", SetLastError = true)>]
  extern int private read_raw(int fd, byte[] buf, int count)

  [<DllImport("libc", EntryPoint = "write", SetLastError = true)>]
  extern int private write_raw(int fd, byte[] buf, int count)

  [<DllImport("libc", EntryPoint = "close", SetLastError = true)>]
  extern int private close_raw(int fd)


  // -- Platform detection ---------------------------------------------
  let private isMac = RuntimeInformation.IsOSPlatform OSPlatform.OSX
  let private isArm64 = RuntimeInformation.ProcessArchitecture = Architecture.Arm64
  let private isX64 = RuntimeInformation.ProcessArchitecture = Architecture.X64

  do
    if not isMac && not isArm64 && not isX64 then
      raise (
        System.PlatformNotSupportedException(
          $"Posix builtins: unsupported architecture {RuntimeInformation.ProcessArchitecture} on Linux. "
          + "Struct offsets are only known for x86_64 and aarch64."
        )
      )

  // -- Open flags (platform-specific) --------------------------------
  let O_RDONLY = 0
  let O_WRONLY = 1
  let O_RDWR = 2

  let O_CREAT = if isMac then 0x200 else 0x40 // Linux

  let O_TRUNC = if isMac then 0x400 else 0x200 // Linux

  let O_APPEND = if isMac then 0x8 else 0x400 // Linux


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

  let chdir (path : string) : Result<unit, int * string> =
    if chdir_raw (path) < 0 then Error(lastError ()) else Ok()

  let setenv (name : string) (value : string) : Result<unit, int * string> =
    if setenv_raw (name, value, 1) < 0 then Error(lastError ()) else Ok()

  let unsetenv (name : string) : Result<unit, int * string> =
    if unsetenv_raw (name) < 0 then Error(lastError ()) else Ok()

  let mkdir (path : string) (mode : int) : Result<unit, int * string> =
    if mkdir_raw (path, mode) < 0 then Error(lastError ()) else Ok()

  let rmdir (path : string) : Result<unit, int * string> =
    if rmdir_raw (path) < 0 then Error(lastError ()) else Ok()

  let unlink (path : string) : Result<unit, int * string> =
    if unlink_raw (path) < 0 then Error(lastError ()) else Ok()

  let rename (oldpath : string) (newpath : string) : Result<unit, int * string> =
    if rename_raw (oldpath, newpath) < 0 then Error(lastError ()) else Ok()

  let chmod (path : string) (mode : int) : Result<unit, int * string> =
    if chmod_raw (path, mode) < 0 then Error(lastError ()) else Ok()

  /// Update atime and mtime to now via utimes(path, NULL).
  let utimesNow (path : string) : Result<unit, int * string> =
    if utimes_raw (path, IntPtr.Zero) < 0 then Error(lastError ()) else Ok()

  let symlink (target : string) (linkpath : string) : Result<unit, int * string> =
    if symlink_raw (target, linkpath) < 0 then Error(lastError ()) else Ok()

  let readlink (path : string) : Result<string, int * string> =
    let buf = Array.zeroCreate<byte> 4096
    let n = readlink_raw (path, buf, 4096)
    if n < 0 then
      Error(lastError ())
    else
      Ok(System.Text.Encoding.UTF8.GetString(buf, 0, n))

  let mkstemp (prefix : string) : Result<int * string, int * string> =
    let template = prefix + "XXXXXX"
    let buf = System.Text.Encoding.UTF8.GetBytes(template + "\000")
    let fd = mkstemp_raw (buf)
    if fd < 0 then
      Error(lastError ())
    else
      let path = System.Text.Encoding.UTF8.GetString(buf, 0, buf.Length - 1)
      Ok(fd, path)

  let mkdtemp (prefix : string) : Result<string, int * string> =
    let template = prefix + "XXXXXX"
    let buf = System.Text.Encoding.UTF8.GetBytes(template + "\000")
    let result = mkdtemp_raw (buf)
    if result = IntPtr.Zero then
      Error(lastError ())
    else
      let path = System.Text.Encoding.UTF8.GetString(buf, 0, buf.Length - 1)
      Ok path

  let openFile
    (path : string)
    (flags : int)
    (mode : int)
    : Result<int, int * string> =
    let fd = open_raw (path, flags, mode)
    if fd < 0 then Error(lastError ()) else Ok fd

  /// Calls stat() and extracts (mode, size, mtimeSec) from the struct.
  /// Offsets are platform-specific (Linux vs macOS struct layouts differ).
  let stat (path : string) : Result<int * int64 * int64, int * string> =
    let buf = Marshal.AllocHGlobal(256)
    try
      if stat_raw (path, buf) < 0 then
        Error(lastError ())
      else
        // struct stat field offsets differ across OS and architecture:
        //   macOS (all):   st_mode at 4 (int16), st_size at 96, st_mtime at 48
        //   Linux x86_64:  st_mode at 24, st_size at 48, st_mtime at 88
        //   Linux aarch64: st_mode at 16, st_size at 48, st_mtime at 88
        let mode =
          if isMac then
            int (Marshal.ReadInt16(buf, 4)) &&& 0xFFFF
          elif isArm64 then
            Marshal.ReadInt32(buf, 16)
          else // x86_64 Linux (guarded by startup check)
            Marshal.ReadInt32(buf, 24)
        let size =
          if isMac then Marshal.ReadInt64(buf, 96) else Marshal.ReadInt64(buf, 48)
        let mtimeSec =
          if isMac then Marshal.ReadInt64(buf, 48) else Marshal.ReadInt64(buf, 88)
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
      if stat_raw (path, buf) < 0 then
        Error(lastError ())
      else
        // struct stat st_uid offset:
        //   macOS: 16, Linux x86_64: 28, Linux aarch64: 24
        let uid =
          if isMac then
            uint32 (Marshal.ReadInt32(buf, 16))
          elif isArm64 then
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

  /// Spawn a child process, capture stdout+stderr, wait for exit.
  /// Uses .NET Process.Start (which uses posix_spawn internally)
  /// because raw fork() is unsafe in managed runtimes.
  let spawnAndWait
    (program : string)
    (args : List<string>)
    : Result<int * string * string, int * string> =
    try
      let psi = System.Diagnostics.ProcessStartInfo()
      psi.FileName <- program
      for arg in args do
        psi.ArgumentList.Add(arg)
      psi.UseShellExecute <- false
      psi.RedirectStandardOutput <- true
      psi.RedirectStandardError <- true
      psi.CreateNoWindow <- true

      use p = System.Diagnostics.Process.Start(psi)
      // Read both streams concurrently to avoid deadlock when pipe buffers fill
      let stdoutTask = p.StandardOutput.ReadToEndAsync()
      let stderrTask = p.StandardError.ReadToEndAsync()
      let stdout = stdoutTask.Result
      let stderr = stderrTask.Result
      p.WaitForExit()
      Ok(p.ExitCode, stdout, stderr)
    with e ->
      Error(-1, e.Message)

  /// Like spawnAndWait but kills the process if it exceeds timeoutMs.
  let spawnAndWaitWithTimeout
    (program : string)
    (args : List<string>)
    (timeoutMs : int)
    : Result<int * string * string, int * string> =
    try
      let psi = System.Diagnostics.ProcessStartInfo()
      psi.FileName <- program
      for arg in args do
        psi.ArgumentList.Add(arg)
      psi.UseShellExecute <- false
      psi.RedirectStandardOutput <- true
      psi.RedirectStandardError <- true
      psi.CreateNoWindow <- true

      use p = System.Diagnostics.Process.Start(psi)
      let stdoutTask = p.StandardOutput.ReadToEndAsync()
      let stderrTask = p.StandardError.ReadToEndAsync()
      if p.WaitForExit(timeoutMs) then
        let stdout = stdoutTask.Result
        let stderr = stderrTask.Result
        Ok(p.ExitCode, stdout, stderr)
      else
        p.Kill()
        p.WaitForExit()
        Error(110, "Process timed out") // ETIMEDOUT
    with e ->
      Error(-1, e.Message)

  /// List directory entries (wraps opendir/readdir/closedir loop).
  /// Returns filenames only, not "." or "..".
  let listDir (path : string) : Result<List<string>, int * string> =
    let dirp = opendir_raw (path)
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
      | None -> Ok(Seq.toList entries)


// =====================================================================
// Builtin functions exposed to Dark
// =====================================================================
let private posixErrorTypeName () =
  FQTypeName.fqPackage (PackageRefs.Type.Stdlib.Cli.Posix.error ())

let private posixErrorTypeRef () = TCustomType(NR.ok (posixErrorTypeName ()), [])

let private posixErrorKT () = KTCustomType(posixErrorTypeName (), [])

let private dPosixError (errno : int, msg : string) : Dval =
  let tn = posixErrorTypeName ()
  DRecord(tn, tn, [], Map [ "errno", DInt64(int64 errno); "message", DString msg ])

let fns () : List<BuiltInFn> =
  [ { name = fn "posixGetcwd" 0
      typeParams = []
      parameters = [ Param.make "unit" TUnit "" ]
      returnType = TypeReference.result TString (posixErrorTypeRef ())
      description = "Returns the current working directory via libc getcwd()"
      fn =
        (function
        | _, _, _, [ DUnit ] ->
          match Libc.getcwd () with
          | Ok cwd -> Dval.resultOk KTString (posixErrorKT ()) (DString cwd) |> Ply
          | Error e ->
            Dval.resultError KTString (posixErrorKT ()) (dPosixError e) |> Ply
        | _ -> incorrectArgs ())
      sqlSpec = NotQueryable
      previewable = Impure
      deprecated = NotDeprecated }


    { name = fn "posixChdir" 0
      typeParams = []
      parameters = [ Param.make "path" TString "Directory to change to" ]
      returnType = TypeReference.result TUnit (posixErrorTypeRef ())
      description = "Changes the current working directory via libc chdir()"
      fn =
        (function
        | _, _, _, [ DString path ] ->
          match Libc.chdir path with
          | Ok() -> Dval.resultOk KTUnit (posixErrorKT ()) DUnit |> Ply
          | Error e ->
            Dval.resultError KTUnit (posixErrorKT ()) (dPosixError e) |> Ply
        | _ -> incorrectArgs ())
      sqlSpec = NotQueryable
      previewable = Impure
      deprecated = NotDeprecated }


    { name = fn "posixSetenv" 0
      typeParams = []
      parameters =
        [ Param.make "name" TString "Environment variable name"
          Param.make "value" TString "Value to set" ]
      returnType = TypeReference.result TUnit (posixErrorTypeRef ())
      description = "Sets an environment variable via libc setenv()"
      fn =
        (function
        | _, _, _, [ DString name; DString value ] ->
          match Libc.setenv name value with
          | Ok() -> Dval.resultOk KTUnit (posixErrorKT ()) DUnit |> Ply
          | Error e ->
            Dval.resultError KTUnit (posixErrorKT ()) (dPosixError e) |> Ply
        | _ -> incorrectArgs ())
      sqlSpec = NotQueryable
      previewable = Impure
      deprecated = NotDeprecated }


    { name = fn "posixUnsetenv" 0
      typeParams = []
      parameters = [ Param.make "name" TString "Environment variable to remove" ]
      returnType = TypeReference.result TUnit (posixErrorTypeRef ())
      description = "Removes an environment variable via libc unsetenv()"
      fn =
        (function
        | _, _, _, [ DString name ] ->
          match Libc.unsetenv name with
          | Ok() -> Dval.resultOk KTUnit (posixErrorKT ()) DUnit |> Ply
          | Error e ->
            Dval.resultError KTUnit (posixErrorKT ()) (dPosixError e) |> Ply
        | _ -> incorrectArgs ())
      sqlSpec = NotQueryable
      previewable = Impure
      deprecated = NotDeprecated }


    { name = fn "posixMkdir" 0
      typeParams = []
      parameters =
        [ Param.make "path" TString "The directory path to create"
          Param.make "mode" TInt64 "Permission bits (e.g. 493 for 0755)" ]
      returnType = TypeReference.result TUnit (posixErrorTypeRef ())
      description = "Creates a directory via libc mkdir()"
      fn =
        (function
        | _, _, _, [ DString path; DInt64 mode ] ->
          match Libc.mkdir path (int mode) with
          | Ok() -> Dval.resultOk KTUnit (posixErrorKT ()) DUnit |> Ply
          | Error e ->
            Dval.resultError KTUnit (posixErrorKT ()) (dPosixError e) |> Ply
        | _ -> incorrectArgs ())
      sqlSpec = NotQueryable
      previewable = Impure
      deprecated = NotDeprecated }


    { name = fn "posixRmdir" 0
      typeParams = []
      parameters = [ Param.make "path" TString "The directory to remove" ]
      returnType = TypeReference.result TUnit (posixErrorTypeRef ())
      description = "Removes an empty directory via libc rmdir()"
      fn =
        (function
        | _, _, _, [ DString path ] ->
          match Libc.rmdir path with
          | Ok() -> Dval.resultOk KTUnit (posixErrorKT ()) DUnit |> Ply
          | Error e ->
            Dval.resultError KTUnit (posixErrorKT ()) (dPosixError e) |> Ply
        | _ -> incorrectArgs ())
      sqlSpec = NotQueryable
      previewable = Impure
      deprecated = NotDeprecated }


    { name = fn "posixUnlink" 0
      typeParams = []
      parameters = [ Param.make "path" TString "The file to remove" ]
      returnType = TypeReference.result TUnit (posixErrorTypeRef ())
      description = "Removes a file via libc unlink()"
      fn =
        (function
        | _, _, _, [ DString path ] ->
          match Libc.unlink path with
          | Ok() -> Dval.resultOk KTUnit (posixErrorKT ()) DUnit |> Ply
          | Error e ->
            Dval.resultError KTUnit (posixErrorKT ()) (dPosixError e) |> Ply
        | _ -> incorrectArgs ())
      sqlSpec = NotQueryable
      previewable = Impure
      deprecated = NotDeprecated }


    { name = fn "posixRename" 0
      typeParams = []
      parameters =
        [ Param.make "oldpath" TString "Current path"
          Param.make "newpath" TString "New path" ]
      returnType = TypeReference.result TUnit (posixErrorTypeRef ())
      description = "Renames/moves a file or directory via libc rename()"
      fn =
        (function
        | _, _, _, [ DString oldpath; DString newpath ] ->
          match Libc.rename oldpath newpath with
          | Ok() -> Dval.resultOk KTUnit (posixErrorKT ()) DUnit |> Ply
          | Error e ->
            Dval.resultError KTUnit (posixErrorKT ()) (dPosixError e) |> Ply
        | _ -> incorrectArgs ())
      sqlSpec = NotQueryable
      previewable = Impure
      deprecated = NotDeprecated }


    { name = fn "posixChmod" 0
      typeParams = []
      parameters =
        [ Param.make "path" TString "File path"
          Param.make "mode" TInt64 "Permission bits (e.g. 493 for 0755)" ]
      returnType = TypeReference.result TUnit (posixErrorTypeRef ())
      description = "Changes file permissions via libc chmod()"
      fn =
        (function
        | _, _, _, [ DString path; DInt64 mode ] ->
          match Libc.chmod path (int mode) with
          | Ok() -> Dval.resultOk KTUnit (posixErrorKT ()) DUnit |> Ply
          | Error e ->
            Dval.resultError KTUnit (posixErrorKT ()) (dPosixError e) |> Ply
        | _ -> incorrectArgs ())
      sqlSpec = NotQueryable
      previewable = Impure
      deprecated = NotDeprecated }


    { name = fn "posixUtimesNow" 0
      typeParams = []
      parameters = [ Param.make "path" TString "File path" ]
      returnType = TypeReference.result TUnit (posixErrorTypeRef ())
      description = "Updates atime and mtime to now via libc utimes(path, NULL)"
      fn =
        (function
        | _, _, _, [ DString path ] ->
          match Libc.utimesNow path with
          | Ok() -> Dval.resultOk KTUnit (posixErrorKT ()) DUnit |> Ply
          | Error e ->
            Dval.resultError KTUnit (posixErrorKT ()) (dPosixError e) |> Ply
        | _ -> incorrectArgs ())
      sqlSpec = NotQueryable
      previewable = Impure
      deprecated = NotDeprecated }


    { name = fn "posixSymlink" 0
      typeParams = []
      parameters =
        [ Param.make "target" TString "Path the symlink points to"
          Param.make "linkpath" TString "Path of the symlink to create" ]
      returnType = TypeReference.result TUnit (posixErrorTypeRef ())
      description = "Creates a symbolic link via libc symlink()"
      fn =
        (function
        | _, _, _, [ DString target; DString linkpath ] ->
          match Libc.symlink target linkpath with
          | Ok() -> Dval.resultOk KTUnit (posixErrorKT ()) DUnit |> Ply
          | Error e ->
            Dval.resultError KTUnit (posixErrorKT ()) (dPosixError e) |> Ply
        | _ -> incorrectArgs ())
      sqlSpec = NotQueryable
      previewable = Impure
      deprecated = NotDeprecated }


    { name = fn "posixReadlink" 0
      typeParams = []
      parameters = [ Param.make "path" TString "Symlink path to read" ]
      returnType = TypeReference.result TString (posixErrorTypeRef ())
      description = "Reads the target of a symbolic link via libc readlink()"
      fn =
        (function
        | _, _, _, [ DString path ] ->
          match Libc.readlink path with
          | Ok target ->
            Dval.resultOk KTString (posixErrorKT ()) (DString target) |> Ply
          | Error e ->
            Dval.resultError KTString (posixErrorKT ()) (dPosixError e) |> Ply
        | _ -> incorrectArgs ())
      sqlSpec = NotQueryable
      previewable = Impure
      deprecated = NotDeprecated }


    { name = fn "posixMkstemp" 0
      typeParams = []
      parameters =
        [ Param.make
            "prefix"
            TString
            "Prefix for the temp file path (e.g. \"/tmp/dark-\")" ]
      returnType =
        TypeReference.result (TTuple(TInt64, TString, [])) (posixErrorTypeRef ())
      description =
        "Creates a unique temp file via libc mkstemp(). Returns (fd, path)."
      fn =
        (function
        | _, _, _, [ DString prefix ] ->
          let resultOk =
            Dval.resultOk (KTTuple(VT.int64, VT.string, [])) (posixErrorKT ())
          let resultError =
            Dval.resultError (KTTuple(VT.int64, VT.string, [])) (posixErrorKT ())
          match Libc.mkstemp prefix with
          | Ok(fd, path) ->
            resultOk (DTuple(DInt64(int64 fd), DString path, [])) |> Ply
          | Error e -> resultError (dPosixError e) |> Ply
        | _ -> incorrectArgs ())
      sqlSpec = NotQueryable
      previewable = Impure
      deprecated = NotDeprecated }


    { name = fn "posixMkdtemp" 0
      typeParams = []
      parameters =
        [ Param.make
            "prefix"
            TString
            "Prefix for the temp directory path (e.g. \"/tmp/dark-\")" ]
      returnType = TypeReference.result TString (posixErrorTypeRef ())
      description =
        "Creates a unique temp directory via libc mkdtemp(). Returns the path."
      fn =
        (function
        | _, _, _, [ DString prefix ] ->
          match Libc.mkdtemp prefix with
          | Ok path -> Dval.resultOk KTString (posixErrorKT ()) (DString path) |> Ply
          | Error e ->
            Dval.resultError KTString (posixErrorKT ()) (dPosixError e) |> Ply
        | _ -> incorrectArgs ())
      sqlSpec = NotQueryable
      previewable = Impure
      deprecated = NotDeprecated }


    { name = fn "posixListDir" 0
      typeParams = []
      parameters = [ Param.make "path" TString "The directory to list" ]
      returnType = TypeReference.result (TList TString) (posixErrorTypeRef ())
      description =
        "Lists entries in a directory via libc opendir/readdir/closedir. Excludes '.' and '..'."
      fn =
        (function
        | _, _, _, [ DString path ] ->
          let resultOk =
            Dval.resultOk (KTList(ValueType.Known KTString)) (posixErrorKT ())
          let resultError =
            Dval.resultError (KTList(ValueType.Known KTString)) (posixErrorKT ())
          match Libc.listDir path with
          | Ok entries ->
            let dvals = entries |> List.map DString
            resultOk (DList(ValueType.Known KTString, dvals)) |> Ply
          | Error e -> resultError (dPosixError e) |> Ply
        | _ -> incorrectArgs ())
      sqlSpec = NotQueryable
      previewable = Impure
      deprecated = NotDeprecated }


    { name = fn "posixGetenv" 0
      typeParams = []
      parameters = [ Param.make "name" TString "" ]
      returnType = TypeReference.option TString
      description = "Gets an environment variable via libc getenv()"
      fn =
        (function
        | _, _, _, [ DString name ] ->
          match Libc.getenv name with
          | Some v -> Dval.optionSome KTString (DString v) |> Ply
          | None -> Dval.optionNone KTString |> Ply
        | _ -> incorrectArgs ())
      sqlSpec = NotQueryable
      previewable = Impure
      deprecated = NotDeprecated }


    { name = fn "posixSpawnAndWait" 0
      typeParams = []
      parameters =
        [ Param.make "program" TString "Path to the executable"
          Param.make "args" (TList TString) "Arguments to pass" ]
      returnType =
        TypeReference.result
          (TTuple(TInt64, TString, [ TString ]))
          (posixErrorTypeRef ())
      description =
        "Spawns a child process, waits for it to finish, returns (exitCode, stdout, stderr)."
      fn =
        (function
        | _, _, _, [ DString program; DList(_, args) ] ->
          let resultOk =
            Dval.resultOk
              (KTTuple(VT.int64, VT.string, [ VT.string ]))
              (posixErrorKT ())
          let resultError =
            Dval.resultError
              (KTTuple(VT.int64, VT.string, [ VT.string ]))
              (posixErrorKT ())
          let argStrs =
            args
            |> List.map (fun d ->
              match d with
              | DString s -> s
              | _ -> incorrectArgs ())
          match Libc.spawnAndWait program argStrs with
          | Ok(exitCode, stdout, stderr) ->
            resultOk (
              DTuple(DInt64(int64 exitCode), DString stdout, [ DString stderr ])
            )
            |> Ply
          | Error e -> resultError (dPosixError e) |> Ply
        | _ -> incorrectArgs ())
      sqlSpec = NotQueryable
      previewable = Impure
      deprecated = NotDeprecated }


    { name = fn "posixSpawnAndWaitWithTimeout" 0
      typeParams = []
      parameters =
        [ Param.make "program" TString "Path to the executable"
          Param.make "args" (TList TString) "Arguments to pass"
          Param.make "timeoutMs" TInt64 "Timeout in milliseconds" ]
      returnType =
        TypeReference.result
          (TTuple(TInt64, TString, [ TString ]))
          (posixErrorTypeRef ())
      description =
        "Spawns a child process with a timeout. Returns (exitCode, stdout, stderr) or Error on timeout."
      fn =
        (function
        | _, _, _, [ DString program; DList(_, args); DInt64 timeoutMs ] ->
          let resultOk =
            Dval.resultOk
              (KTTuple(VT.int64, VT.string, [ VT.string ]))
              (posixErrorKT ())
          let resultError =
            Dval.resultError
              (KTTuple(VT.int64, VT.string, [ VT.string ]))
              (posixErrorKT ())
          let argStrs =
            args
            |> List.map (fun d ->
              match d with
              | DString s -> s
              | _ -> incorrectArgs ())
          match Libc.spawnAndWaitWithTimeout program argStrs (int timeoutMs) with
          | Ok(exitCode, stdout, stderr) ->
            resultOk (
              DTuple(DInt64(int64 exitCode), DString stdout, [ DString stderr ])
            )
            |> Ply
          | Error e -> resultError (dPosixError e) |> Ply
        | _ -> incorrectArgs ())
      sqlSpec = NotQueryable
      previewable = Impure
      deprecated = NotDeprecated }


    { name = fn "posixKill" 0
      typeParams = []
      parameters =
        [ Param.make "pid" TInt64 "Process ID"
          Param.make
            "signal"
            TInt64
            "Signal number (e.g. 9 for SIGKILL, 15 for SIGTERM)" ]
      returnType = TypeReference.result TUnit (posixErrorTypeRef ())
      description = "Sends a signal to a process."
      fn =
        (function
        | _, _, _, [ DInt64 pid; DInt64 signal ] ->
          match Libc.kill (int pid) (int signal) with
          | Ok() -> Dval.resultOk KTUnit (posixErrorKT ()) DUnit |> Ply
          | Error e ->
            Dval.resultError KTUnit (posixErrorKT ()) (dPosixError e) |> Ply
        | _ -> incorrectArgs ())
      sqlSpec = NotQueryable
      previewable = Impure
      deprecated = NotDeprecated }


    { name = fn "posixFdRead" 0
      typeParams = []
      parameters =
        [ Param.make "fd" TInt64 "File descriptor to read from"
          Param.make "count" TInt64 "Max bytes to read" ]
      returnType = TypeReference.result TBlob (posixErrorTypeRef ())
      description =
        "Reads up to count bytes from a file descriptor into an ephemeral Blob."
      fn =
        (function
        | state, _, _, [ DInt64 fd; DInt64 count ] ->
          let resultOk = Dval.resultOk KTBlob (posixErrorKT ())
          let resultError = Dval.resultError KTBlob (posixErrorKT ())
          match Libc.fdRead (int fd) (int count) with
          | Ok bytes -> resultOk (Dval.newEphemeralBlob state bytes) |> Ply
          | Error e -> resultError (dPosixError e) |> Ply
        | _ -> incorrectArgs ())
      sqlSpec = NotQueryable
      previewable = Impure
      deprecated = NotDeprecated }


    { name = fn "posixFdWrite" 0
      typeParams = []
      parameters =
        [ Param.make "fd" TInt64 "File descriptor to write to"
          Param.make "blob" TBlob "Bytes to write" ]
      returnType = TypeReference.result TInt64 (posixErrorTypeRef ())
      description = "Writes bytes to a file descriptor. Returns bytes written."
      fn =
        (function
        | state, _, _, [ DInt64 fd; DBlob ref ] ->
          uply {
            let! bytes = Dval.readBlobBytes state ref
            match Libc.fdWrite (int fd) bytes with
            | Ok n ->
              return Dval.resultOk KTInt64 (posixErrorKT ()) (DInt64(int64 n))
            | Error e ->
              return Dval.resultError KTInt64 (posixErrorKT ()) (dPosixError e)
          }
        | _ -> incorrectArgs ())
      sqlSpec = NotQueryable
      previewable = Impure
      deprecated = NotDeprecated }


    { name = fn "posixFdClose" 0
      typeParams = []
      parameters = [ Param.make "fd" TInt64 "File descriptor to close" ]
      returnType = TypeReference.result TUnit (posixErrorTypeRef ())
      description = "Closes a file descriptor."
      fn =
        (function
        | _, _, _, [ DInt64 fd ] ->
          match Libc.fdClose (int fd) with
          | Ok() -> Dval.resultOk KTUnit (posixErrorKT ()) DUnit |> Ply
          | Error e ->
            Dval.resultError KTUnit (posixErrorKT ()) (dPosixError e) |> Ply
        | _ -> incorrectArgs ())
      sqlSpec = NotQueryable
      previewable = Impure
      deprecated = NotDeprecated }


    { name = fn "posixOpen" 0
      typeParams = []
      parameters =
        [ Param.make "path" TString "File path to open"
          Param.make "flags" TInt64 "Open flags (e.g. O_RDONLY, O_WRONLY | O_CREAT)"
          Param.make
            "mode"
            TInt64
            "Permission bits for new files (e.g. 420 for 0644)" ]
      returnType = TypeReference.result TInt64 (posixErrorTypeRef ())
      description = "Opens a file via libc open(). Returns a file descriptor."
      fn =
        (function
        | _, _, _, [ DString path; DInt64 flags; DInt64 mode ] ->
          match Libc.openFile path (int flags) (int mode) with
          | Ok fd ->
            Dval.resultOk KTInt64 (posixErrorKT ()) (DInt64(int64 fd)) |> Ply
          | Error e ->
            Dval.resultError KTInt64 (posixErrorKT ()) (dPosixError e) |> Ply
        | _ -> incorrectArgs ())
      sqlSpec = NotQueryable
      previewable = Impure
      deprecated = NotDeprecated }


    { name = fn "posixFlagRdonly" 0
      typeParams = []
      parameters = [ Param.make "unit" TUnit "" ]
      returnType = TInt64
      description = "Returns the O_RDONLY flag for open()"
      fn =
        (function
        | _, _, _, [ DUnit ] -> DInt64(int64 Libc.O_RDONLY) |> Ply
        | _ -> incorrectArgs ())
      sqlSpec = NotQueryable
      previewable = Pure
      deprecated = NotDeprecated }


    { name = fn "posixFlagWronly" 0
      typeParams = []
      parameters = [ Param.make "unit" TUnit "" ]
      returnType = TInt64
      description = "Returns the O_WRONLY flag for open()"
      fn =
        (function
        | _, _, _, [ DUnit ] -> DInt64(int64 Libc.O_WRONLY) |> Ply
        | _ -> incorrectArgs ())
      sqlSpec = NotQueryable
      previewable = Pure
      deprecated = NotDeprecated }


    { name = fn "posixFlagRdwr" 0
      typeParams = []
      parameters = [ Param.make "unit" TUnit "" ]
      returnType = TInt64
      description = "Returns the O_RDWR flag for open()"
      fn =
        (function
        | _, _, _, [ DUnit ] -> DInt64(int64 Libc.O_RDWR) |> Ply
        | _ -> incorrectArgs ())
      sqlSpec = NotQueryable
      previewable = Pure
      deprecated = NotDeprecated }


    { name = fn "posixFlagCreat" 0
      typeParams = []
      parameters = [ Param.make "unit" TUnit "" ]
      returnType = TInt64
      description = "Returns the O_CREAT flag for open() (platform-aware)"
      fn =
        (function
        | _, _, _, [ DUnit ] -> DInt64(int64 Libc.O_CREAT) |> Ply
        | _ -> incorrectArgs ())
      sqlSpec = NotQueryable
      previewable = Pure
      deprecated = NotDeprecated }


    { name = fn "posixFlagTrunc" 0
      typeParams = []
      parameters = [ Param.make "unit" TUnit "" ]
      returnType = TInt64
      description = "Returns the O_TRUNC flag for open() (platform-aware)"
      fn =
        (function
        | _, _, _, [ DUnit ] -> DInt64(int64 Libc.O_TRUNC) |> Ply
        | _ -> incorrectArgs ())
      sqlSpec = NotQueryable
      previewable = Pure
      deprecated = NotDeprecated }


    { name = fn "posixFlagAppend" 0
      typeParams = []
      parameters = [ Param.make "unit" TUnit "" ]
      returnType = TInt64
      description = "Returns the O_APPEND flag for open() (platform-aware)"
      fn =
        (function
        | _, _, _, [ DUnit ] -> DInt64(int64 Libc.O_APPEND) |> Ply
        | _ -> incorrectArgs ())
      sqlSpec = NotQueryable
      previewable = Pure
      deprecated = NotDeprecated }


    { name = fn "posixStat" 0
      typeParams = []
      parameters = [ Param.make "path" TString "File path to stat" ]
      returnType =
        TypeReference.result
          (TTuple(TInt64, TInt64, [ TInt64 ]))
          (posixErrorTypeRef ())
      description = "Stats a file via libc stat(). Returns (mode, size, mtimeSec)."
      fn =
        (function
        | _, _, _, [ DString path ] ->
          let resultOk =
            Dval.resultOk
              (KTTuple(VT.int64, VT.int64, [ VT.int64 ]))
              (posixErrorKT ())
          let resultError =
            Dval.resultError
              (KTTuple(VT.int64, VT.int64, [ VT.int64 ]))
              (posixErrorKT ())
          match Libc.stat path with
          | Ok(mode, size, mtimeSec) ->
            resultOk (DTuple(DInt64(int64 mode), DInt64 size, [ DInt64 mtimeSec ]))
            |> Ply
          | Error e -> resultError (dPosixError e) |> Ply
        | _ -> incorrectArgs ())
      sqlSpec = NotQueryable
      previewable = Impure
      deprecated = NotDeprecated }


    { name = fn "posixUname" 0
      typeParams = []
      parameters = [ Param.make "unit" TUnit "" ]
      returnType =
        TypeReference.result
          (TTuple(TString, TString, [ TString ]))
          (posixErrorTypeRef ())
      description = "Calls uname(). Returns (sysname, nodename, machine)."
      fn =
        (function
        | _, _, _, [ DUnit ] ->
          let resultOk =
            Dval.resultOk
              (KTTuple(VT.string, VT.string, [ VT.string ]))
              (posixErrorKT ())
          let resultError =
            Dval.resultError
              (KTTuple(VT.string, VT.string, [ VT.string ]))
              (posixErrorKT ())
          match Libc.uname () with
          | Ok(sysname, nodename, machine) ->
            resultOk (DTuple(DString sysname, DString nodename, [ DString machine ]))
            |> Ply
          | Error e -> resultError (dPosixError e) |> Ply
        | _ -> incorrectArgs ())
      sqlSpec = NotQueryable
      previewable = Impure
      deprecated = NotDeprecated }


    { name = fn "posixGetpid" 0
      typeParams = []
      parameters = [ Param.make "unit" TUnit "" ]
      returnType = TInt64
      description = "Returns the current process ID via libc getpid()"
      fn =
        (function
        | _, _, _, [ DUnit ] -> DInt64(int64 (Libc.getpid ())) |> Ply
        | _ -> incorrectArgs ())
      sqlSpec = NotQueryable
      previewable = Impure
      deprecated = NotDeprecated }


    { name = fn "posixGetuid" 0
      typeParams = []
      parameters = [ Param.make "unit" TUnit "" ]
      returnType = TInt64
      description = "Returns the current user ID via libc getuid()"
      fn =
        (function
        | _, _, _, [ DUnit ] -> DInt64(int64 (Libc.getuid ())) |> Ply
        | _ -> incorrectArgs ())
      sqlSpec = NotQueryable
      previewable = Impure
      deprecated = NotDeprecated }


    { name = fn "posixGetCurrentUserName" 0
      typeParams = []
      parameters = [ Param.make "unit" TUnit "" ]
      returnType = TypeReference.option TString
      description =
        "Returns the login name of the current user via getuid() + getpwuid()"
      fn =
        (function
        | _, _, _, [ DUnit ] ->
          let uid = Libc.getuid ()
          match Libc.getUserName (uint32 uid) with
          | Some name -> Dval.optionSome KTString (DString name) |> Ply
          | None -> Dval.optionNone KTString |> Ply
        | _ -> incorrectArgs ())
      sqlSpec = NotQueryable
      previewable = Impure
      deprecated = NotDeprecated }


    { name = fn "posixCpuCount" 0
      typeParams = []
      parameters = [ Param.make "unit" TUnit "" ]
      returnType = TInt64
      description = "Returns the number of online CPUs via sysconf()"
      fn =
        (function
        | _, _, _, [ DUnit ] -> DInt64(Libc.cpuCount ()) |> Ply
        | _ -> incorrectArgs ())
      sqlSpec = NotQueryable
      previewable = Impure
      deprecated = NotDeprecated }


    { name = fn "posixGetHomeDir" 0
      typeParams = []
      parameters = [ Param.make "unit" TUnit "" ]
      returnType = TypeReference.option TString
      description =
        "Returns the home directory of the current user via getpwuid(getuid())"
      fn =
        (function
        | _, _, _, [ DUnit ] ->
          match Libc.getHomeDir () with
          | Some dir -> Dval.optionSome KTString (DString dir) |> Ply
          | None -> Dval.optionNone KTString |> Ply
        | _ -> incorrectArgs ())
      sqlSpec = NotQueryable
      previewable = Impure
      deprecated = NotDeprecated }


    { name = fn "posixFnmatch" 0
      typeParams = []
      parameters =
        [ Param.make "pattern" TString "Glob pattern"
          Param.make "str" TString "String to match against"
          Param.make "pathMode" TBool "If true, * does not match /" ]
      returnType = TBool
      description = "Matches a string against a glob pattern via libc fnmatch()"
      fn =
        (function
        | _, _, _, [ DString pattern; DString str; DBool pathMode ] ->
          let flags = if pathMode then Libc.FNM_PATHNAME else 0
          DBool(Libc.fnmatch pattern str flags) |> Ply
        | _ -> incorrectArgs ())
      sqlSpec = NotQueryable
      previewable = Pure
      deprecated = NotDeprecated }


    { name = fn "posixFlock" 0
      typeParams = []
      parameters =
        [ Param.make "fd" TInt64 "File descriptor"
          Param.make "exclusive" TBool "True for exclusive lock, false to unlock" ]
      returnType = TypeReference.result TUnit (posixErrorTypeRef ())
      description = "Locks or unlocks a file via libc flock()"
      fn =
        (function
        | _, _, _, [ DInt64 fd; DBool exclusive ] ->
          let op = if exclusive then Libc.LOCK_EX else Libc.LOCK_UN
          match Libc.flock (int fd) op with
          | Ok() -> Dval.resultOk KTUnit (posixErrorKT ()) DUnit |> Ply
          | Error e ->
            Dval.resultError KTUnit (posixErrorKT ()) (dPosixError e) |> Ply
        | _ -> incorrectArgs ())
      sqlSpec = NotQueryable
      previewable = Impure
      deprecated = NotDeprecated }


    { name = fn "posixFileOwner" 0
      typeParams = []
      parameters = [ Param.make "path" TString "File path" ]
      returnType = TypeReference.result TString (posixErrorTypeRef ())
      description = "Returns the owner username of a file via stat() + getpwuid()"
      fn =
        (function
        | _, _, _, [ DString path ] ->
          match Libc.fileOwner path with
          | Ok name -> Dval.resultOk KTString (posixErrorKT ()) (DString name) |> Ply
          | Error e ->
            Dval.resultError KTString (posixErrorKT ()) (dPosixError e) |> Ply
        | _ -> incorrectArgs ())
      sqlSpec = NotQueryable
      previewable = Impure
      deprecated = NotDeprecated } ]


let builtins () : Builtins = Builtin.make [] (fns ())
