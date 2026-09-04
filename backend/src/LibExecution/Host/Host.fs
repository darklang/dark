/// Boundary for guest code that accesses the operating system.
///
/// A scoped builtin builds a normalized `Operation` and passes it to
/// `perform`. `perform` derives the matching permission `Request`, checks it
/// against `Access`, and executes that same operation only when authorized.
/// This keeps authorization and execution in sync. Ambient effects and
/// deliberately unscoped `Native` builtins are checked by the interpreter
/// before their bodies run.
///
/// Host types are in `HostTypes`; host-owned subsystems live in the other
/// `Host*` modules. Keeping this module below `RuntimeTypes` avoids depending
/// on VM state.
module LibExecution.Host

open System.Runtime.InteropServices
open System.Threading.Tasks
open FSharp.Control.Tasks

open Prelude

module Permission = LibExecution.Permissions

// The boundary's vocabulary, under the name callers use for the door.
type Operation = HostTypes.Operation
type Response = HostTypes.Response
type FailureKind = HostTypes.FailureKind
type Failure = HostTypes.Failure
type Outcome = HostTypes.Outcome

// ── response extractors ───────────────────────────────────────────────────────
// A builtin knows which shape its operation produces; anything else is an
// interpreter bug, not a guest-visible error.

let private unexpected (response : Response) : 'a =
  Exception.raiseInternal "unexpected host response" [ "got", response ]

let expectBytes (response : Response) : byte[] =
  match response with
  | Response.Bytes bytes -> bytes
  | other -> unexpected other

let expectUnit (response : Response) : unit =
  match response with
  | Response.Unit -> ()
  | other -> unexpected other

let expectStat (response : Response) : bool * bool =
  match response with
  | Response.Stat(exists, isDirectory) -> (exists, isDirectory)
  | other -> unexpected other

let expectPath (response : Response) : string =
  match response with
  | Response.Path path -> path
  | other -> unexpected other

let expectEntries (response : Response) : List<string> =
  match response with
  | Response.Entries entries -> entries
  | other -> unexpected other

let expectEnvValue (response : Response) : Option<string> =
  match response with
  | Response.EnvValue value -> value
  | other -> unexpected other

let expectEnvEntries (response : Response) : List<string * string> =
  match response with
  | Response.EnvEntries entries -> entries
  | other -> unexpected other

let expectProcessOutcome (response : Response) : int * string * string =
  match response with
  | Response.ProcessOutcome(exitCode, stdout, stderr) -> (exitCode, stdout, stderr)
  | other -> unexpected other

let expectProcessHandle (response : Response) : int64 =
  match response with
  | Response.ProcessHandle handle -> handle
  | other -> unexpected other

let expectHttpServerHandle (response : Response) : int64 =
  match response with
  | Response.HttpServerHandle handle -> handle
  | other -> unexpected other

let expectHttp
  (response : Response)
  : Result<HostTypes.HttpResponse, HostTypes.HttpRequestError> =
  match response with
  | Response.Http result -> result
  | other -> unexpected other

let expectHttpStream
  (response : Response)
  : Result<HostTypes.HttpStreamHead, HostTypes.HttpRequestError> =
  match response with
  | Response.HttpStream result -> result
  | other -> unexpected other

let expectFd (response : Response) : int =
  match response with
  | Response.Fd fd -> fd
  | other -> unexpected other

let expectFdPath (response : Response) : int * string =
  match response with
  | Response.FdPath(fd, path) -> (fd, path)
  | other -> unexpected other

let expectStatInfo (response : Response) : int * int64 * int64 =
  match response with
  | Response.StatInfo(mode, size, mtimeSec) -> (mode, size, mtimeSec)
  | other -> unexpected other

let expectOffset (response : Response) : int64 =
  match response with
  | Response.Offset offset -> offset
  | other -> unexpected other

let expectWritten (response : Response) : int =
  match response with
  | Response.Written count -> count
  | other -> unexpected other

let expectText (response : Response) : string =
  match response with
  | Response.Text text -> text
  | other -> unexpected other

let expectOptionalText (response : Response) : Option<string> =
  match response with
  | Response.OptionalText text -> text
  | other -> unexpected other

// ── failures ──────────────────────────────────────────────────────────────────

/// A libc failure. ENOENT, EPERM and EACCES carry the same numbers on Linux
/// and macOS, so the kind can be read off the errno.
let failureOfErrno (errno : int, message : string) : Failure =
  let kind =
    match errno with
    | 2 -> FailureKind.NotFound
    | 1
    | 13 -> FailureKind.PermissionDenied
    | _ -> FailureKind.Other
  { kind = kind; errno = errno; message = message }

// HTTP listeners remain host-owned until a checked bind returns. An opaque
// handle keeps HostTypes transport-shaped for a future process boundary.
let private httpServerListeners =
  System.Collections.Concurrent.ConcurrentDictionary<int64, System.Net.HttpListener>()

let mutable private nextHttpServerHandle = 0L

let takeHttpServerListener (handle : int64) : System.Net.HttpListener =
  match httpServerListeners.TryRemove handle with
  | true, listener -> listener
  | false, _ ->
    Exception.raiseInternal
      "unknown or already-claimed HTTP server handle"
      [ "handle", string handle ]

let private bindHttpServer (port : int) : Result<Response, int * string> =
  let listener = new System.Net.HttpListener()
  listener.Prefixes.Add($"http://*:{port}/")
  let closeListener () = listener.Close()
  try
    listener.Start()
    let handle = System.Threading.Interlocked.Increment(&nextHttpServerHandle)
    if httpServerListeners.TryAdd(handle, listener) then
      Ok(Response.HttpServerHandle handle)
    else
      closeListener ()
      Error(-1, "could not allocate an HTTP server handle")
  with :? System.Net.HttpListenerException as e ->
    closeListener ()
    let inUse =
      List.contains e.ErrorCode [ 98; 48; 183; 32 ]
      || e.Message.Contains("Address already in use")
      || e.Message.Contains("address already in use")
    if inUse then
      Error(
        e.ErrorCode,
        $"port {port} is already in use — something else is listening there. "
        + "Stop it, or serve on another port."
      )
    else
      Error(e.ErrorCode, $"couldn't listen on port {port}: {e.Message}")

/// A .NET failure; there is no errno to report.
let private classify (e : exn) : Failure =
  let kind =
    match e with
    | :? System.IO.FileNotFoundException
    | :? System.IO.DirectoryNotFoundException -> FailureKind.NotFound
    | :? System.UnauthorizedAccessException -> FailureKind.PermissionDenied
    | _ -> FailureKind.Other
  { kind = kind; errno = -1; message = e.Message }

// ── input normalization ───────────────────────────────────────────────────────

/// The path used by both the permission check and the filesystem call:
/// lexically resolved, then with every symlinked ancestor replaced by its
/// target (`Permissions.FilePath`), so `/tmp/x` on macOS is checked and
/// opened as `/private/tmp/x`. Resolving once here prevents relative-path or
/// link tricks from splitting the checked target from the used target; an
/// ancestor that could not be resolved stays a link and is rejected below.
let normalizeFilePath (path : string) : string =
  try
    HostSecurity.FilePath.canonicalAncestors path
  with _ ->
    path

/// After normalization no resolvable ancestor is a link; one that still is
/// (dangling, cyclic, inaccessible) must not reach an OS API. Fail-closed for
/// inaccessible path components as well. On POSIX this pre-check is advisory:
/// the libc bridge walks the directories again with O_NOFOLLOW at operation
/// time, so a link swapped in between is met and refused there. On Windows
/// the .NET calls run behind this check alone.
let private pathContainsLink (includeFinal : bool) (path : string) : bool =
  try
    let full = System.IO.Path.GetFullPath path
    let root = System.IO.Path.GetPathRoot full
    if System.String.IsNullOrWhiteSpace root then
      true
    else
      let parts =
        full
          .Substring(root.Length)
          .Split(
            [| System.IO.Path.DirectorySeparatorChar
               System.IO.Path.AltDirectorySeparatorChar |],
            System.StringSplitOptions.RemoveEmptyEntries
          )
      let parts =
        if includeFinal || parts.Length = 0 then
          parts
        else
          Array.take (parts.Length - 1) parts
      let mutable current = root
      let mutable linked = false
      for part in parts do
        if not linked then
          current <- System.IO.Path.Combine(current, part)
          try
            let attributes = System.IO.File.GetAttributes current
            linked <- attributes.HasFlag(System.IO.FileAttributes.ReparsePoint)
          with
          | :? System.IO.FileNotFoundException
          | :? System.IO.DirectoryNotFoundException -> ()
          | _ -> linked <- true
      linked
  with _ ->
    true

/// Expand `$HOME`/`${HOME}` in user-supplied input — a path, or a shell
/// command line. Host-side sugar applied by the .NET file and process
/// builtins (the posix twins are raw syscalls and take paths verbatim); the
/// result is still checked by whichever operation uses it.
let expandHome (input : string) : string =
  let home =
    (if System.OperatingSystem.IsWindows() then "USERPROFILE" else "HOME")
    |> System.Environment.GetEnvironmentVariable
    |> Option.ofObj
    |> Option.defaultValue ""
  input.Replace("${HOME}", home).Replace("$HOME", home)

/// Prepare a user-supplied command line for the platform shell. Host-side
/// input prep; the spawn it feeds is still checked at the boundary.
let prepareShellCommand (command : string) : string * List<string> =
  let command = expandHome command
  if RuntimeInformation.IsOSPlatform OSPlatform.Windows then
    "cmd.exe", [ "/c"; command ]
  else if
    RuntimeInformation.IsOSPlatform OSPlatform.Linux
    || RuntimeInformation.IsOSPlatform OSPlatform.OSX
  then
    let shell =
      System.Environment.GetEnvironmentVariable "SHELL"
      |> Option.ofObj
      |> Option.defaultValue "/bin/bash"
    shell, [ "-c"; command ]
  else
    Exception.raiseInternal
      "Executing CLI commands is not supported for your operating system"
      []

/// Resolve a program name the way the OS would: absolute and
/// separator-containing paths lexically, bare names against PATH. Both the
/// permission check and the spawn receive this result, so lookup cannot
/// diverge from execution.
let private resolveExecutable (program : string) : string =
  let containsSeparator =
    program.Contains System.IO.Path.DirectorySeparatorChar
    || program.Contains System.IO.Path.AltDirectorySeparatorChar
  if System.IO.Path.IsPathFullyQualified program || containsSeparator then
    System.IO.Path.GetFullPath program
  else
    let path =
      System.Environment.GetEnvironmentVariable "PATH"
      |> Option.ofObj
      |> Option.defaultValue ""
    let candidates =
      path.Split(System.IO.Path.PathSeparator)
      |> Array.toList
      |> List.map (fun dir -> System.IO.Path.Combine(dir, program))
    match List.tryFind System.IO.File.Exists candidates with
    | Some candidate -> System.IO.Path.GetFullPath candidate
    | None -> program

/// Checks paths that are always protected, regardless of the final-component
/// symlink rule.
let private protectedPathGuard (write : bool) (path : string) : Option<string> =
  if write && HostSecurity.canAffectPolicyPath path then
    Some
      $"permission denied: mutating `{path}` could replace protected host policy state"
  elif not write && HostSecurity.isPolicyPath path then
    Some $"permission denied: `{path}` is protected host policy state"
  elif HostSecurity.isPackageDbPath path then
    Some $"permission denied: `{path}` is the host-owned package store"
  else
    None

/// Guards shared by every filesystem operation. Produce the complete error
/// message, or None to proceed.
let fileGuard (write : bool) (path : string) : Option<string> =
  match protectedPathGuard write path with
  | Some message -> Some message
  | None when pathContainsLink true path ->
    Some
      $"permission request rejected: filesystem path contains a symlink or reparse point: {path}"
  | None -> None

/// Operations on a directory entry itself (readlink/unlink/rename) may name a
/// symlink as their final component without following it. Ancestor links are
/// still rejected, as are protected policy and package-store paths.
let private fileEntryGuard (write : bool) (path : string) : Option<string> =
  match protectedPathGuard write path with
  | Some message -> Some message
  | None when pathContainsLink false path ->
    Some
      $"permission request rejected: filesystem path contains a symlink or reparse point: {path}"
  | None -> None

// ── resolution ────────────────────────────────────────────────────────────────

/// The execution half of an operation: runs only after its checks pass. An
/// `Error` is an OS-level failure the guest sees as a Result; an exception
/// is classified into the same shape by `perform`.
type private Execute = unit -> Task<Result<Response, Failure>>

/// What `perform` needs to know about one operation: the exact requests to
/// check (each with the description used in denial messages), and how to
/// execute it. Most operations check one request; a read-write libc
/// operation checks two; one authorized elsewhere checks none.
type private Resolved =
  { checks : List<Permission.Request * string>; execute : Execute }

let private rejected (message : string) : string =
  $"permission request rejected: {message}"

/// Collect an operation's requests in order, rejecting on the first
/// malformed one, and pair them with its execution.
let private withChecks
  (checks : List<Result<Permission.Request, string> * string>)
  (execute : Execute)
  : Result<Resolved, string> =
  let rec collect acc remaining =
    match remaining with
    | [] -> Ok { checks = List.rev acc; execute = execute }
    | (Error message, _) :: _ -> Error(rejected message)
    | (Ok request, resource) :: rest -> collect ((request, resource) :: acc) rest
  collect [] checks

/// An operation authorized elsewhere, which only needs to run where the host
/// state lives: a continuation on a host-issued handle (decided when the
/// handle was created), a Native-gated builtin (decided by the interpreter's
/// ambient gate before the body ran), or input that already failed
/// validation with a guest-visible typed error. Audited like the rest.
let private unchecked (execute : Execute) : Result<Resolved, string> =
  Ok { checks = []; execute = execute }

/// A synchronous execution that cannot fail short of an exception.
let private produce (make : unit -> Response) : Execute =
  fun () -> Task.FromResult(Ok(make ()))

/// A synchronous execution whose failure is an (errno, message) pair — a
/// libc call, or a spawn that can time out.
let private attempt (call : unit -> Result<Response, int * string>) : Execute =
  fun () -> Task.FromResult(call () |> Result.mapError failureOfErrno)

let private unitOk
  (result : Result<unit, int * string>)
  : Result<Response, int * string> =
  result |> Result.map (fun () -> Response.Unit)

let private processOutcome
  (result : Result<int * string * string, int * string>)
  : Result<Response, int * string> =
  result |> Result.map Response.ProcessOutcome

let private fileCheck
  (access : Permission.AccessKind)
  (path : string)
  : Result<Permission.Request, string> * string =
  let verb = if access = Permission.AccessKind.Write then "writing" else "reading"
  (Permission.Request.file access path, $"{verb} `{path}`")

let private envCheck
  (access : Permission.AccessKind)
  (name : string)
  : Result<Permission.Request, string> * string =
  let verb = if access = Permission.AccessKind.Write then "setting" else "reading"
  (Permission.Request.env access name, $"{verb} env `{name}`")

/// Normalize and guard a path before its requests are built; the
/// continuation receives the normalized path both the check and the OS call
/// use.
let private guarded
  (write : bool)
  (rawPath : string)
  (resolve : string -> Result<Resolved, string>)
  : Result<Resolved, string> =
  let path = normalizeFilePath rawPath
  match fileGuard write path with
  | Some message -> Error message
  | None -> resolve path

let private guardedEntry
  (write : bool)
  (rawPath : string)
  (resolve : string -> Result<Resolved, string>)
  : Result<Resolved, string> =
  let path = normalizeFilePath rawPath
  match fileEntryGuard write path with
  | Some message -> Error message
  | None -> resolve path

let private resolveFile
  (access : Permission.AccessKind)
  (rawPath : string)
  (execute : string -> Execute)
  : Result<Resolved, string> =
  guarded (access = Permission.AccessKind.Write) rawPath (fun path ->
    withChecks [ fileCheck access path ] (execute path))

/// A .NET file mutation: the write request, then a unit result.
let private unitFile
  (rawPath : string)
  (action : string -> unit)
  : Result<Resolved, string> =
  resolveFile Permission.AccessKind.Write rawPath (fun path ->
    produce (fun () ->
      action path
      Response.Unit))

let private libcPath
  (access : Permission.AccessKind)
  (rawPath : string)
  (call : string -> Result<Response, int * string>)
  : Result<Resolved, string> =
  resolveFile access rawPath (fun path -> attempt (fun () -> call path))

/// Like `libcPath`, but the syscall operates on the final directory entry
/// rather than following it. This is what makes readlink and unlink usable
/// without allowing symlinked ancestors.
let private libcEntry
  (access : Permission.AccessKind)
  (rawPath : string)
  (call : string -> Result<Response, int * string>)
  : Result<Resolved, string> =
  guardedEntry (access = Permission.AccessKind.Write) rawPath (fun path ->
    withChecks [ fileCheck access path ] (attempt (fun () -> call path)))

let private libcReadWrite
  (rawPath : string)
  (call : string -> Result<Response, int * string>)
  : Result<Resolved, string> =
  guarded true rawPath (fun path ->
    withChecks
      [ fileCheck Permission.AccessKind.Read path
        fileCheck Permission.AccessKind.Write path ]
      (attempt (fun () -> call path)))

/// mkstemp/mkdtemp create a random sibling of the prefix, so authorizing the
/// prefix would check a different path from the one used. Authorize and guard
/// the containing directory, which necessarily contains the generated entry.
let private libcTemp
  (rawPrefix : string)
  (call : string -> Result<Response, int * string>)
  : Result<Resolved, string> =
  let prefix = normalizeFilePath rawPrefix
  match System.IO.Path.GetDirectoryName prefix |> Option.ofObj with
  | None -> Error(rejected "temporary path has no containing directory")
  | Some directory ->
    guarded true directory (fun directory ->
      withChecks
        [ fileCheck Permission.AccessKind.Write directory ]
        (attempt (fun () -> call prefix)))

/// A Native-gated libc call on a handle or process state: nothing to check
/// here, an errno on failure.
let private native
  (call : unit -> Result<Response, int * string>)
  : Result<Resolved, string> =
  unchecked (attempt call)

/// The spawn is authorized against the real program. Confining the child at
/// the OS level (a launcher wrapper) is the deferred OS-sandbox layer's job,
/// not this runtime-authorization boundary's.
let private resolveProcess
  (program : string)
  (args : List<string>)
  (execute : string -> List<string> -> Execute)
  : Result<Resolved, string> =
  let resolved = resolveExecutable program
  withChecks
    [ (Permission.Request.processSpawn resolved args, $"running `{program}`") ]
    (execute resolved args)

/// Validate an HTTP request against its profile; a validation failure is a
/// guest-visible typed error that completes immediately, a valid one checks
/// the exact method + canonical URI before any network IO.
let private resolveHttp
  (profile : HostTypes.HttpProfile)
  (method : string)
  (url : string)
  (headers : List<string * string>)
  (error : HostTypes.HttpRequestError -> Response)
  (execute : HostHttp.Profile -> string -> Execute)
  : Result<Resolved, string> =
  match HostHttp.prepare profile method url headers with
  | Error typedError -> unchecked (produce (fun () -> error typedError))
  | Ok(canonicalUri, profile) ->
    withChecks
      [ (Permission.Request.http method canonicalUri,
         $"{method.ToUpperInvariant()} {canonicalUri}") ]
      (execute profile canonicalUri)

// The libc twins mirror the checks their builtins performed inline: the
// read-grade guard and a read request for chdir, a write check on the link
// path for symlink creation, and read-write for every mutating path op. The
// handle and process-state ops are Native-gated: the interpreter's ambient
// gate authorized the builtin, so they run unchecked here (and audited).
let private resolvePosix (op : HostTypes.PosixOp) : Result<Resolved, string> =
  let read = Permission.AccessKind.Read
  let write = Permission.AccessKind.Write
  match op with
  | HostTypes.PosixOp.Chdir path ->
    resolveFile read path (fun path ->
      attempt (fun () -> HostLibc.chdir path |> unitOk))
  | HostTypes.PosixOp.Setenv(name, value) ->
    withChecks
      [ envCheck write name ]
      (attempt (fun () -> HostLibc.setenv name value |> unitOk))
  | HostTypes.PosixOp.Unsetenv name ->
    withChecks
      [ envCheck write name ]
      (attempt (fun () -> HostLibc.unsetenv name |> unitOk))
  | HostTypes.PosixOp.Getenv name ->
    withChecks
      [ envCheck read name ]
      (produce (fun () -> Response.EnvValue(HostLibc.getenv name)))
  | HostTypes.PosixOp.Mkdir(path, mode) ->
    libcPath write path (fun path -> HostLibc.mkdir path mode |> unitOk)
  | HostTypes.PosixOp.Rmdir path -> libcPath write path (HostLibc.rmdir >> unitOk)
  | HostTypes.PosixOp.Unlink path -> libcEntry write path (HostLibc.unlink >> unitOk)
  | HostTypes.PosixOp.Rename(oldPath, newPath) ->
    // Rename mutates both directory entries without following either final
    // symlink. Each endpoint therefore needs a write rule.
    guardedEntry true oldPath (fun oldPath ->
      guardedEntry true newPath (fun newPath ->
        withChecks
          [ fileCheck write oldPath; fileCheck write newPath ]
          (attempt (fun () -> HostLibc.rename oldPath newPath |> unitOk))))
  | HostTypes.PosixOp.Chmod(path, mode) ->
    libcPath write path (fun path -> HostLibc.chmod path mode |> unitOk)
  | HostTypes.PosixOp.UtimesNow path ->
    libcPath write path (HostLibc.utimesNow >> unitOk)
  | HostTypes.PosixOp.Symlink(target, linkPath) ->
    // The target is data stored in the link, not a resource being accessed;
    // creating the link is a write at linkPath.
    resolveFile write linkPath (fun linkPath ->
      attempt (fun () -> HostLibc.symlink target linkPath |> unitOk))
  | HostTypes.PosixOp.Readlink path ->
    libcEntry read path (HostLibc.readlink >> Result.map Response.Path)
  | HostTypes.PosixOp.Mkstemp prefix ->
    libcTemp prefix (HostLibc.mkstemp >> Result.map Response.FdPath)
  | HostTypes.PosixOp.Mkdtemp prefix ->
    libcTemp prefix (HostLibc.mkdtemp >> Result.map Response.Path)
  | HostTypes.PosixOp.ListDir path ->
    libcPath read path (HostLibc.listDir >> Result.map Response.Entries)
  | HostTypes.PosixOp.Stat path ->
    libcPath read path (HostLibc.stat >> Result.map Response.StatInfo)
  | HostTypes.PosixOp.Open(path, flags, mode) ->
    let accessMode = flags &&& 3
    let reads = accessMode = HostLibc.O_RDONLY || accessMode = HostLibc.O_RDWR
    let writes =
      accessMode = HostLibc.O_WRONLY
      || accessMode = HostLibc.O_RDWR
      || (flags &&& (HostLibc.O_CREAT ||| HostLibc.O_TRUNC ||| HostLibc.O_APPEND))
         <> 0
    if not reads && not writes then
      Error(rejected $"unsupported open access mode: {accessMode}")
    elif reads && writes then
      libcReadWrite path (fun path ->
        HostLibc.openFile path flags mode |> Result.map Response.Fd)
    else
      let access = if writes then write else read
      libcPath access path (fun path ->
        HostLibc.openFile path flags mode |> Result.map Response.Fd)
  | HostTypes.PosixOp.Getcwd ->
    // Like `DirectoryCurrent`: the request depends on host state, and reading
    // the working directory is a read of that path.
    match HostLibc.getcwd () with
    | Error e -> Ok { checks = []; execute = attempt (fun () -> Error e) }
    | Ok cwd ->
      resolveFile read cwd (fun cwd -> produce (fun () -> Response.Path cwd))
  | HostTypes.PosixOp.Kill(pid, signal) ->
    native (fun () -> HostLibc.kill pid signal |> unitOk)
  | HostTypes.PosixOp.FdRead(fd, count) ->
    native (fun () -> HostLibc.fdRead fd count |> Result.map Response.Bytes)
  | HostTypes.PosixOp.FdSeek(fd, offset, whence) ->
    native (fun () -> HostLibc.fdSeek fd offset whence |> Result.map Response.Offset)
  | HostTypes.PosixOp.FdWrite(fd, data) ->
    native (fun () -> HostLibc.fdWrite fd data |> Result.map Response.Written)
  | HostTypes.PosixOp.FdClose fd -> native (fun () -> HostLibc.fdClose fd |> unitOk)
  | HostTypes.PosixOp.Flock(fd, operation) ->
    native (fun () -> HostLibc.flock fd operation |> unitOk)
  | HostTypes.PosixOp.FileOwner path ->
    libcPath read path (HostLibc.fileOwner >> Result.map Response.Text)
  | HostTypes.PosixOp.UserName uid ->
    // A host fact with no resource; the builtin is effect-free.
    unchecked (produce (fun () -> Response.OptionalText(HostLibc.getUserName uid)))
  | HostTypes.PosixOp.HomeDir ->
    // The answer is a path, so it is a read of that path.
    match HostLibc.getHomeDir () with
    | None -> unchecked (produce (fun () -> Response.OptionalText None))
    | Some home ->
      resolveFile read home (fun home ->
        produce (fun () -> Response.OptionalText(Some home)))

let private resolve (op : Operation) : Result<Resolved, string> =
  let read = Permission.AccessKind.Read
  match op with
  | Operation.FileRead path ->
    // Symlink rejection is atomic with the open: openat2(RESOLVE_NO_SYMLINKS)
    // on Linux, the O_NOFOLLOW directory walk elsewhere on POSIX.
    resolveFile read path (fun path ->
      produce (fun () -> Response.Bytes(HostLibc.readAllBytes path)))
  | Operation.FileWrite(path, contents) ->
    unitFile path (fun path -> HostLibc.writeAllBytes path contents)
  | Operation.FileAppendText(path, content) ->
    unitFile path (fun path -> HostLibc.appendAllText path content)
  | Operation.FileDelete path ->
    if HostLibc.isPosix then
      resolveFile Permission.AccessKind.Write path (fun path ->
        attempt (fun () -> HostLibc.unlink path |> unitOk))
    else
      unitFile path System.IO.File.Delete
  | Operation.FileStat path ->
    resolveFile read path (fun path ->
      if HostLibc.isPosix then
        attempt (fun () ->
          HostLibc.statEntry path
          |> Result.map (fun (exists, isDirectory) ->
            Response.Stat(exists, isDirectory)))
      else
        produce (fun () ->
          let isDirectory =
            try
              (System.IO.File.GetAttributes path)
                .HasFlag(System.IO.FileAttributes.Directory)
            with _ ->
              false
          let exists =
            System.IO.File.Exists path || System.IO.Directory.Exists path
          Response.Stat(exists, isDirectory)))
  | Operation.DirectoryCurrent ->
    // The request depends on host state: reading the working directory is a
    // read of that path.
    let cwd = System.IO.Directory.GetCurrentDirectory()
    resolveFile read cwd (fun cwd -> produce (fun () -> Response.Path cwd))
  | Operation.DirectoryList path ->
    resolveFile read path (fun path ->
      if HostLibc.isPosix then
        attempt (fun () ->
          HostLibc.listDir path
          |> Result.map (fun names ->
            names
            |> List.map (fun name -> System.IO.Path.Combine(path, name))
            |> Response.Entries))
      else
        produce (fun () ->
          System.IO.Directory.EnumerateFileSystemEntries path
          |> Seq.toList
          |> Response.Entries))
  | Operation.CurrentExecutablePath ->
    // The answer is a path, so it is a read of that path.
    let path = HostProcess.currentExecutablePath ()
    resolveFile read path (fun path -> produce (fun () -> Response.Path path))
  | Operation.EnvGet name ->
    withChecks
      [ envCheck read name ]
      (produce (fun () ->
        System.Environment.GetEnvironmentVariable name
        |> Option.ofObj
        |> Response.EnvValue))
  | Operation.EnvList ->
    withChecks
      [ (Ok Permission.Request.envList, "reading the environment") ]
      (produce (fun () ->
        System.Environment.GetEnvironmentVariables()
        |> Seq.cast<System.Collections.DictionaryEntry>
        |> Seq.map (fun kv -> (string kv.Key, string kv.Value))
        |> Seq.toList
        |> Response.EnvEntries))
  | Operation.HttpRequest(profile, method, url, headers, body) ->
    resolveHttp
      profile
      method
      url
      headers
      (Error >> Response.Http)
      (fun profile uri () ->
        task {
          let! result = HostHttp.send profile method uri headers body
          return Ok(Response.Http result)
        })
  | Operation.HttpStreamOpen(profile, method, url, headers) ->
    resolveHttp
      profile
      method
      url
      headers
      (Error >> Response.HttpStream)
      (fun profile uri () ->
        task {
          let! result = HostHttp.openStream profile method uri headers
          return Ok(Response.HttpStream result)
        })
  | Operation.HttpServerBind port ->
    withChecks
      [ (Permission.Request.httpServer port, $"binding HTTP server port {port}") ]
      (attempt (fun () -> bindHttpServer port))
  | Operation.ProcessRun(program, args, timeoutMs) ->
    resolveProcess program args (fun program args ->
      attempt (fun () -> HostProcess.run program args timeoutMs |> processOutcome))
  | Operation.ProcessRunInteractive(program, args) ->
    resolveProcess program args (fun program args ->
      attempt (fun () ->
        Ok(
          Response.ProcessOutcome(HostProcess.runInteractive program args, "", "")
        )))
  | Operation.ProcessSpawn(program, args) ->
    resolveProcess program args (fun program args ->
      produce (fun () -> Response.ProcessHandle(HostProcess.spawn program args)))
  | Operation.ProcessIO(handle, input) ->
    unchecked (
      produce (fun () -> Response.ProcessOutcome(HostProcess.io handle input))
    )
  | Operation.ProcessTerminate handle ->
    unchecked (
      produce (fun () -> Response.ProcessOutcome(HostProcess.terminate handle))
    )
  | Operation.Posix op -> resolvePosix op

// ── perform ───────────────────────────────────────────────────────────────────

/// Audit sink: every host operation and its outcome, recorded in-process at
/// the single choke point (`perform`), each operation exactly once. Set by
/// the host (CLI); a no-op until then.
let mutable private auditSink : Operation -> Outcome -> unit = fun _ _ -> ()

let setAuditSink (sink : Operation -> Outcome -> unit) : unit = auditSink <- sink

/// Every request in order; the first denial that stands decides. The
/// suggested fix comes structurally from the denied request, which already
/// encodes the exact access (read vs write).
let private firstDenial
  (relax : Permission.Relax)
  (access : Permission.Access)
  (checks : List<Permission.Request * string>)
  : Option<Outcome> =
  checks
  |> List.tryPick (fun (request, resource) ->
    Permission.Access.decide relax (fun () -> resource) request access
    |> Option.map (fun denial ->
      Outcome.Denied(
        denial.layer,
        denial.reason,
        resource,
        Permission.Request.suggestRule request
      )))

let private execute (run : Execute) : Task<Outcome> =
  task {
    try
      let! result = run ()
      match result with
      | Ok response -> return Outcome.Success response
      | Error failure -> return Outcome.Failed failure
    with e ->
      return Outcome.Failed(classify e)
  }

/// Check and perform one host operation, then audit it exactly once. Derives
/// the requests from the operation itself, checks them against the access,
/// and executes.
let perform
  (relax : Permission.Relax)
  (access : Permission.Access)
  (op : Operation)
  : Task<Outcome> =
  task {
    let! outcome =
      match resolve op with
      | Error message -> Task.FromResult(Outcome.Rejected message)
      | Ok resolved ->
        match firstDenial relax access resolved.checks with
        | Some denied -> Task.FromResult denied
        | None -> execute resolved.execute
    try
      auditSink op outcome
    with _ ->
      () // auditing must never change the result of a host operation
    return outcome
  }

// ── stream-handle continuations ───────────────────────────────────────────────
// Used by the HTTP client's DStream closures. The open operation already made
// the policy decision; chunk reads and disposal use the resulting host handle.

let httpStreamRead (handle : int64) (maxBytes : int) : Task<Option<byte[]>> =
  task {
    try
      return! HostHttp.readChunk handle maxBytes
    with e ->
      return
        Exception.raiseInternal "http stream read failed" [ "message", e.Message ]
  }

let httpStreamClose (handle : int64) : unit = HostHttp.closeStream handle
