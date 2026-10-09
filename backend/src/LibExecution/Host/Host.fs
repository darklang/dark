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

// Keep construction inside the checked host even in optimized builds. Pure
// executions need no descriptor dictionaries or child-resource bookkeeping.
[<System.Runtime.CompilerServices.MethodImpl(System.Runtime.CompilerServices.MethodImplOptions.NoInlining)>]
let createDescriptorSession () : HostDescriptors.Session =
  new HostDescriptors.Session(HostPosix.fdClose)

let createLazyDescriptorSession () : Lazy<HostDescriptors.Session> =
  lazy (createDescriptorSession ())

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

let expectDescriptor (response : Response) : bigint =
  match response with
  | Response.Fd fd -> bigint fd
  | Response.Descriptor token -> token
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

/// A kernel failure. ENOENT, EPERM and EACCES carry the same numbers on Linux
/// and macOS, so the kind can be read off the errno.
let failureOfErrno (errno : int, message : string) : Failure =
  let kind =
    match errno with
    | 2 -> FailureKind.NotFound
    | 1
    | 13 -> FailureKind.OsAccessDenied
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

/// An exception out of an execution. `HostPosix.openSafely` reports an open
/// failure as a `Win32Exception` carrying the errno, so on Linux every file
/// read that fails comes through here that way; read the kind off the errno
/// as `failureOfErrno` does, or ENOENT and EACCES both collapse into `Other`.
/// A .NET exception has no errno to report.
let private classify (e : exn) : Failure =
  match e with
  | :? System.ComponentModel.Win32Exception as e ->
    failureOfErrno (e.NativeErrorCode, e.Message)
  | _ ->
    let kind =
      match e with
      | :? System.IO.FileNotFoundException
      | :? System.IO.DirectoryNotFoundException -> FailureKind.NotFound
      | :? System.UnauthorizedAccessException -> FailureKind.OsAccessDenied
      | _ -> FailureKind.Other
    { kind = kind; errno = -1; message = e.Message }

// ── input normalization ───────────────────────────────────────────────────────

/// Whether the file at `path` has been written since `since`. For a resume's stale-read warning
/// (`Interpreter.ReplayPolicy`); a path that is not a file answers false.
let fileChangedSince (path : string) (since : System.DateTime) : bool =
  try
    System.IO.File.Exists path && System.IO.File.GetLastWriteTimeUtc path > since
  with _ ->
    false


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
/// the POSIX host adapter walks the directories again with O_NOFOLLOW at operation
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
/// execute it. Most operations check one request; a read-write POSIX
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
/// POSIX call, or a spawn that can time out.
let private attempt (call : unit -> Result<Response, int * string>) : Execute =
  fun () -> Task.FromResult(call () |> Result.mapError failureOfErrno)

/// An execution that waits on another OS process (a run to completion, a
/// round of input and output): on the pool, so the thread that asked, which
/// is a scheduler's when a Dark process asked, is free to run its other
/// processes meanwhile. A file or POSIX call is not moved: those finish in
/// microseconds and the hop would cost more than the wait.
let private blocking (call : unit -> Result<Response, int * string>) : Execute =
  fun () -> Task.Run(fun () -> call () |> Result.mapError failureOfErrno)

let private unitOk
  (result : Result<unit, int * string>)
  : Result<Response, int * string> =
  result |> Result.map (fun () -> Response.Unit)

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

/// For an operation that moves or removes a directory ENTRY -- rename, rmdir,
/// unlink, delete -- refuse an ancestor of the package store: moving the
/// directory that contains the store carries the store with it, to a path the
/// exact-match guard does not know. Only these operations get the ancestor
/// rule. A `mkdir` or `chmod` naming an existing ancestor cannot displace the
/// store, and refusing them would break `mkdir -p` under every ancestor, as
/// the policy directory's broader rule already does under `$HOME`.
let private mutatingEntry
  (rawPath : string)
  (proceed : unit -> Result<Resolved, string>)
  : Result<Resolved, string> =
  let path = normalizeFilePath rawPath
  if HostSecurity.canAffectPackageDbPath path then
    Error
      $"permission denied: mutating `{path}` could move or replace the host-owned package store"
  else
    proceed ()

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

let private posixPath
  (access : Permission.AccessKind)
  (rawPath : string)
  (call : string -> Result<Response, int * string>)
  : Result<Resolved, string> =
  resolveFile access rawPath (fun path -> attempt (fun () -> call path))

/// Like `posixPath`, but the syscall operates on the final directory entry
/// rather than following it. This is what makes readlink and unlink usable
/// without allowing symlinked ancestors.
let private posixEntry
  (access : Permission.AccessKind)
  (rawPath : string)
  (call : string -> Result<Response, int * string>)
  : Result<Resolved, string> =
  guardedEntry (access = Permission.AccessKind.Write) rawPath (fun path ->
    withChecks [ fileCheck access path ] (attempt (fun () -> call path)))

let private posixReadWrite
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
let private posixTemp
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

/// Raw descriptors and process state require Native at this boundary too.
/// Do not rely solely on a builtin's coarse effect metadata: scoped handle
/// continuations will eventually enter this boundary without that gate.
let private native
  (call : unit -> Result<Response, int * string>)
  : Result<Resolved, string> =
  withChecks
    [ (Permission.Request.native "raw POSIX operation", "raw POSIX operation") ]
    (attempt call)

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

// The POSIX twins mirror the checks their builtins performed inline: the
// read-grade guard and a read request for chdir, a write check on the link
// path for symlink creation, and read-write for every mutating path op. The
// raw handle and process-state ops also check Native here, independently of
// the interpreter's ambient builtin gate.
let private rawDescriptor =
  function
  | HostTypes.FileDescriptor.Raw fd -> fd
  | HostTypes.FileDescriptor.Scoped _ ->
    Exception.raiseInternal "scoped descriptor escaped normalization" []

let private resolvePosix
  (session : Option<HostDescriptors.Session>)
  (op : HostTypes.PosixOp)
  : Result<Resolved, string> =
  let read = Permission.AccessKind.Read
  let write = Permission.AccessKind.Write
  let queue fd =
    match session with
    | None -> Response.Fd fd
    | Some owner ->
      owner.Register
        { fd = fd; paths = []; processes = []; reads = true; writes = false }
      |> Response.Descriptor
  match op with
  | HostTypes.PosixOp.InotifyInit1 flags ->
    unchecked (attempt (fun () -> HostPosix.inotifyInit1 flags |> Result.map queue))
  | HostTypes.PosixOp.Kqueue ->
    unchecked (attempt (fun () -> HostPosix.kqueue () |> Result.map queue))
  | HostTypes.PosixOp.KeventVnode(fd, changes, capacity, timeout) ->
    let changes =
      changes
      |> List.map (fun (source, flags, mask) -> rawDescriptor source, flags, mask)
    withChecks
      [ (Permission.Request.native "raw POSIX operation", "raw POSIX operation")
        (Ok Permission.Request.clock, "waiting for vnode events") ]
      (blocking (fun () ->
        HostPosix.keventVnode (rawDescriptor fd) changes capacity timeout
        |> Result.map (fun bytes ->
          session
          |> Option.iter (fun owner ->
            owner.WatchDescriptors(
              rawDescriptor fd,
              changes |> List.map (fun (source, _, _) -> source)
            ))
          Response.Bytes bytes)))
  | HostTypes.PosixOp.InotifyAddWatch(fd, path, mask) ->
    guarded false path (fun path ->
      withChecks
        [ fileCheck read path
          (Permission.Request.native "raw POSIX operation", "raw POSIX operation") ]
        (attempt (fun () ->
          HostPosix.inotifyAddWatch (rawDescriptor fd) path mask
          |> Result.map (fun wd ->
            session
            |> Option.iter (fun owner -> owner.WatchPath(rawDescriptor fd, path))
            Response.Written wd))))
  | HostTypes.PosixOp.Ppoll(fds, timeout)
  | HostTypes.PosixOp.PollDarwin(fds, timeout) ->
    let darwin =
      match op with
      | HostTypes.PosixOp.PollDarwin _ -> true
      | _ -> false
    withChecks
      [ (Permission.Request.native "raw POSIX operation", "raw POSIX operation")
        (Ok Permission.Request.clock, "waiting for descriptor events") ]
      (blocking (fun () ->
        HostPosix.pollDescriptors
          darwin
          (fds |> List.map (fun (fd, events) -> rawDescriptor fd, events))
          timeout
        |> Result.map Response.Bytes))
  | HostTypes.PosixOp.Chdir path ->
    resolveFile read path (fun path ->
      attempt (fun () -> HostPosix.chdir path |> unitOk))
  | HostTypes.PosixOp.Mkdir(path, mode) ->
    posixPath write path (fun path -> HostPosix.mkdir path mode |> unitOk)
  | HostTypes.PosixOp.Rmdir path ->
    mutatingEntry path (fun () -> posixPath write path (HostPosix.rmdir >> unitOk))
  | HostTypes.PosixOp.Unlink path ->
    mutatingEntry path (fun () -> posixEntry write path (HostPosix.unlink >> unitOk))
  | HostTypes.PosixOp.UnlinkAt(path, flags) ->
    mutatingEntry path (fun () ->
      posixEntry write path (fun path -> HostPosix.unlinkAt path flags |> unitOk))
  | HostTypes.PosixOp.Rename(oldPath, newPath) ->
    // Rename mutates both directory entries without following either final
    // symlink. Each endpoint therefore needs a write rule, and neither may be
    // an ancestor of the package store.
    mutatingEntry oldPath (fun () ->
      mutatingEntry newPath (fun () ->
        guardedEntry true oldPath (fun oldPath ->
          guardedEntry true newPath (fun newPath ->
            withChecks
              [ fileCheck write oldPath; fileCheck write newPath ]
              (attempt (fun () -> HostPosix.rename oldPath newPath |> unitOk))))))
  | HostTypes.PosixOp.Chmod(path, mode) ->
    posixPath write path (fun path -> HostPosix.chmod path mode |> unitOk)
  | HostTypes.PosixOp.UtimensAt(path, a, an, m, mn) ->
    posixPath write path (fun path -> HostPosix.utimensAt path a an m mn |> unitOk)
  | HostTypes.PosixOp.SetattrlistTimes(path, a, an, m, mn) ->
    posixPath write path (fun path ->
      HostPosix.setattrlistTimes path a an m mn |> unitOk)
  | HostTypes.PosixOp.Symlink(target, linkPath) ->
    // The target is data stored in the link, not a resource being accessed;
    // creating the link is a write at linkPath.
    resolveFile write linkPath (fun linkPath ->
      attempt (fun () -> HostPosix.symlink target linkPath |> unitOk))
  | HostTypes.PosixOp.ReadlinkBytes(path, count) ->
    posixEntry read path (fun path ->
      HostPosix.readlinkBytes path count |> Result.map Response.Bytes)
  | HostTypes.PosixOp.Mkstemp prefix ->
    posixTemp prefix (HostPosix.mkstemp >> Result.map Response.FdPath)
  | HostTypes.PosixOp.Mkdtemp prefix ->
    posixTemp prefix (HostPosix.mkdtemp >> Result.map Response.Path)
  | HostTypes.PosixOp.ListDir path ->
    posixPath read path (HostPosix.listDir >> Result.map Response.Entries)
  | HostTypes.PosixOp.Stat path ->
    posixPath read path (HostPosix.stat >> Result.map Response.StatInfo)
  | HostTypes.PosixOp.StatBuffer path ->
    posixPath read path (HostPosix.statBuffer >> Result.map Response.Bytes)
  | HostTypes.PosixOp.DirectoryBytes(fd, count) ->
    native (fun () ->
      HostPosix.directoryBytes (rawDescriptor fd) count |> Result.map Response.Bytes)
  | HostTypes.PosixOp.CopyFileRange(source, destination, count) ->
    native (fun () ->
      HostPosix.copyFileRange
        (rawDescriptor source)
        (rawDescriptor destination)
        count
      |> Result.map Response.Offset)
  | HostTypes.PosixOp.CloneFile(source, destination) ->
    native (fun () ->
      HostPosix.cloneFile (rawDescriptor source) (rawDescriptor destination)
      |> unitOk)
  | HostTypes.PosixOp.FclonefileAt(source, destination, flags) ->
    guarded true destination (fun path ->
      withChecks
        [ fileCheck write path
          (Permission.Request.native "raw POSIX operation", "raw POSIX operation") ]
        (attempt (fun () ->
          HostPosix.fclonefileAt (rawDescriptor source) path flags |> unitOk)))
  | HostTypes.PosixOp.Ftruncate(fd, length) ->
    native (fun () -> HostPosix.ftruncate (rawDescriptor fd) length |> unitOk)
  | HostTypes.PosixOp.Fchmod(fd, mode) ->
    native (fun () -> HostPosix.fchmod (rawDescriptor fd) mode |> unitOk)
  | HostTypes.PosixOp.Fstat fd ->
    native (fun () ->
      HostPosix.fstat (rawDescriptor fd) |> Result.map Response.Bytes)
  | HostTypes.PosixOp.Getentropy count ->
    unchecked (
      attempt (fun () -> HostPosix.getentropy count |> Result.map Response.Bytes)
    )
  | HostTypes.PosixOp.ClockGettime clock ->
    withChecks
      [ (Ok Permission.Request.clock, "reading clock") ]
      (attempt (fun () -> HostPosix.clockGettime clock |> Result.map Response.Bytes))
  | HostTypes.PosixOp.Gettimeofday ->
    withChecks
      [ (Ok Permission.Request.clock, "reading clock") ]
      (attempt (fun () -> HostPosix.gettimeofday () |> Result.map Response.Bytes))
  | HostTypes.PosixOp.MachAbsoluteTime ->
    withChecks
      [ (Ok Permission.Request.clock, "reading clock") ]
      (attempt (fun () -> HostPosix.machAbsoluteTime () |> Result.map Response.Bytes))
  | HostTypes.PosixOp.MachTimebaseInfo ->
    withChecks
      [ (Ok Permission.Request.clock, "reading clock") ]
      (attempt (fun () -> HostPosix.machTimebaseInfo () |> Result.map Response.Bytes))
  | HostTypes.PosixOp.Nanosleep(seconds, nanos) ->
    withChecks
      [ (Ok Permission.Request.clock, "sleeping") ]
      (blocking (fun () ->
        let rc, remaining = HostPosix.nanosleep seconds nanos
        Ok(Response.KernelSleep(rc, remaining))))
  | HostTypes.PosixOp.UnameBytes ->
    unchecked (
      attempt (fun () -> HostPosix.unameBytes () |> Result.map Response.Bytes)
    )
  | HostTypes.PosixOp.SysctlBytes(mib, count) ->
    let run () = HostPosix.sysctlBytes mib count |> Result.map Response.Bytes
    // These are the host facts exposed by Sys. Other sysctl keys can expose
    // process/environment data and require unscoped Native authority.
    if List.contains mib [ [ 1; 1 ]; [ 1; 10 ]; [ 6; 1 ]; [ 6; 3 ] ] then
      unchecked (attempt run)
    else
      native run
  | HostTypes.PosixOp.SchedGetaffinity(pid, count) ->
    let run () = HostPosix.schedGetaffinity pid count |> Result.map Response.Bytes
    if pid = 0 then unchecked (attempt run) else native run
  | HostTypes.PosixOp.Pread(fd, count, offset) ->
    native (fun () ->
      HostPosix.pread (rawDescriptor fd) count offset |> Result.map Response.Bytes)
  | HostTypes.PosixOp.Pwrite(fd, bytes, offset) ->
    native (fun () ->
      HostPosix.pwrite (rawDescriptor fd) bytes offset
      |> Result.map Response.Written)
  | HostTypes.PosixOp.Open(path, flags, mode) ->
    let opened (path : string) (fd : int) : Response =
      match session with
      | None -> Response.Fd fd
      | Some owner ->
        let accessMode = flags &&& 3
        let token =
          owner.Register
            { fd = fd
              paths = [ path ]
              processes = []
              reads =
                accessMode = HostPosix.O_RDONLY || accessMode = HostPosix.O_RDWR
              writes =
                accessMode = HostPosix.O_WRONLY || accessMode = HostPosix.O_RDWR }
        Response.Descriptor token
    let accessMode = flags &&& 3
    let reads = accessMode = HostPosix.O_RDONLY || accessMode = HostPosix.O_RDWR
    let writes =
      accessMode = HostPosix.O_WRONLY
      || accessMode = HostPosix.O_RDWR
      || (flags &&& (HostPosix.O_CREAT ||| HostPosix.O_TRUNC ||| HostPosix.O_APPEND))
         <> 0
    if not reads && not writes then
      Error(rejected $"unsupported open access mode: {accessMode}")
    elif reads && writes then
      posixReadWrite path (fun path ->
        HostPosix.openFile path flags mode |> Result.map (opened path))
    else
      let access = if writes then write else read
      posixPath access path (fun path ->
        HostPosix.openFile path flags mode |> Result.map (opened path))
  | HostTypes.PosixOp.GetcwdBytes count ->
    // Authorize the exact kernel answer, not an earlier managed cwd snapshot.
    // Decoding here is only for policy; public decoding remains in Dark.
    match HostPosix.getcwdBytes count with
    | Error e -> Ok { checks = []; execute = attempt (fun () -> Error e) }
    | Ok bytes ->
      let length =
        bytes |> Array.tryFindIndex ((=) 0uy) |> Option.defaultValue bytes.Length
      let cwd = System.Text.Encoding.UTF8.GetString(bytes, 0, length)
      resolveFile read cwd (fun _ -> produce (fun () -> Response.Bytes bytes))
  | HostTypes.PosixOp.FcntlGetPath fd ->
    // A directory may have been renamed since open. Check the returned path
    // as well as the descriptor's original grant before releasing any bytes.
    match HostPosix.fcntlGetPath (rawDescriptor fd) with
    | Error e -> native (fun () -> Error e)
    | Ok bytes ->
      let length =
        bytes |> Array.tryFindIndex ((=) 0uy) |> Option.defaultValue bytes.Length
      let path = System.Text.Encoding.UTF8.GetString(bytes, 0, length)
      guarded false path (fun path ->
        withChecks
          [ (Permission.Request.native "raw POSIX operation", "raw POSIX operation")
            fileCheck read path ]
          (produce (fun () -> Response.Bytes bytes)))
  | HostTypes.PosixOp.Kill(pid, signal) ->
    native (fun () -> HostPosix.kill pid signal |> unitOk)
  | HostTypes.PosixOp.FdRead(fd, count) ->
    native (fun () ->
      HostPosix.fdRead (rawDescriptor fd) count |> Result.map Response.Bytes)
  | HostTypes.PosixOp.FdSeek(fd, offset, whence) ->
    native (fun () ->
      HostPosix.fdSeek (rawDescriptor fd) offset whence
      |> Result.map Response.Offset)
  | HostTypes.PosixOp.FdWrite(fd, data) ->
    native (fun () ->
      HostPosix.fdWrite (rawDescriptor fd) data |> Result.map Response.Written)
  | HostTypes.PosixOp.FdClose fd ->
    native (fun () -> HostPosix.fdClose (rawDescriptor fd) |> unitOk)
  | HostTypes.PosixOp.Flock(fd, operation) ->
    native (fun () -> HostPosix.flock (rawDescriptor fd) operation |> unitOk)
  | HostTypes.PosixOp.FileOwner path ->
    posixPath read path (HostPosix.fileOwner >> Result.map Response.Text)
  | HostTypes.PosixOp.HomeDir home ->
    // Darklang selected a candidate; exposing it still requires path authorization.
    resolveFile read home (fun home ->
      produce (fun () -> Response.OptionalText(Some home)))

let private resolve (op : Operation) : Result<Resolved, string> =
  let read = Permission.AccessKind.Read
  match op with
  | Operation.FileRead path ->
    // Symlink rejection is atomic with the open: openat2(RESOLVE_NO_SYMLINKS)
    // on Linux, the O_NOFOLLOW directory walk elsewhere on POSIX.
    resolveFile read path (fun path ->
      produce (fun () -> Response.Bytes(HostPosix.readAllBytes path)))
  | Operation.FileWrite(path, contents) ->
    unitFile path (fun path -> HostPosix.writeAllBytes path contents)
  | Operation.FileAppendText(path, content) ->
    unitFile path (fun path -> HostPosix.appendAllText path content)
  | Operation.FileDelete path ->
    mutatingEntry path (fun () ->
      if HostPosix.isPosix then
        resolveFile Permission.AccessKind.Write path (fun path ->
          attempt (fun () -> HostPosix.unlink path |> unitOk))
      else
        unitFile path System.IO.File.Delete)
  | Operation.FileStat path ->
    resolveFile read path (fun path ->
      if HostPosix.isPosix then
        attempt (fun () ->
          HostPosix.statEntry path
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
      if HostPosix.isPosix then
        attempt (fun () ->
          HostPosix.listDir path
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
  | Operation.Posix op -> resolvePosix None op
  | Operation.ChildStart _
  | Operation.ChildDescriptors _
  | Operation.ChildPoll _
  | Operation.ChildWait _
  | Operation.ChildKill _
  | Operation.ChildClose _
  | Operation.ChildClosePort _
  | Operation.ChildBufferGet _
  | Operation.ChildBufferSet _ ->
    Error "child operation requires execution ownership"
  | Operation.TestStoreSnapshot snapshot ->
    withChecks
      [ (Permission.Request.native "test baseline", "preparing a test baseline") ]
      (attempt (fun () ->
        Ok(Response.TestStoreSnapshot(HostProcess.testStoreSnapshot snapshot))))
  | Operation.IsolatedTest(snapshot,
                           branch,
                           request,
                           policyStore,
                           access,
                           timeoutMs,
                           columns,
                           rows) ->
    withChecks
      [ (Permission.Request.native "isolated test", "running an isolated test") ]
      (blocking (fun () ->
        HostProcess.runIsolatedTest
          (HostProcess.currentExecutablePath ())
          snapshot
          branch
          request
          policyStore
          access
          timeoutMs
          columns
          rows
        |> Result.map Response.IsolatedTestOutcome))

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

/// A tab has no processes and no listening sockets, and the .NET exceptions for both are
/// unreadable: `dark serve` reported `SystemNetHttpListener_PlatformNotSupported`, a resource
/// KEY rather than a sentence because `UseSystemResourceKeys` is on, and running a program
/// surfaced an internal error with a .NET stack. Refuse here instead, with the text a person
/// should read. `HostPosix` already does this for POSIX calls it cannot make (errno 38,
/// "not available in the browser").
///
/// Only what genuinely cannot work. Files, directories and environment variables work in the
/// browser against emscripten's in-memory filesystem (descriptor calls through `HostPosix`'s
/// managed table, the rest through `System.IO`), nothing of it survives a reload, and HTTP
/// requests work over `fetch` for hosts that allow it.
let private browserRefusal (op : Operation) : Option<Outcome> =
  if not (System.OperatingSystem.IsBrowser()) then
    None
  else
    match op with
    | Operation.ChildStart _
    | Operation.ChildDescriptors _
    | Operation.ChildPoll _
    | Operation.ChildWait _
    | Operation.ChildKill _
    | Operation.ChildClose _
    | Operation.ChildClosePort _
    | Operation.ChildBufferGet _
    | Operation.ChildBufferSet _ ->
      Some(
        Outcome.Rejected
          "running another program is not available in the browser: a tab has no processes"
      )
    // A failure rather than a rejection: `serve` already answers a failed bind with a
    // Result error, which the CLI prints. A rejection raises, and at the interactive
    // prompt a raise from a command ends the session, so the refusal took the tab with it.
    | Operation.HttpServerBind _ ->
      Some(
        Outcome.Failed
          { kind = FailureKind.Other
            errno = 38
            message =
              "serving HTTP is not available in the browser: a tab cannot listen on a "
              + "port. A page can still make requests, so Http client calls do work here." }
      )
    | _ -> None


type private DescriptorRight =
  | Read
  | Write
  | Metadata
  | Cleanup

let private descriptorUses (op : HostTypes.PosixOp) =
  let read fd = [ fd, Read ]
  let write fd = [ fd, Write ]
  match op with
  | HostTypes.PosixOp.FclonefileAt(source, _, _) -> read source
  | HostTypes.PosixOp.KeventVnode(queue, changes, _, _) ->
    (queue, Metadata) :: (changes |> List.map (fun (fd, _, _) -> fd, Read))
  | HostTypes.PosixOp.InotifyAddWatch(fd, _, _) -> [ fd, Metadata ]
  | HostTypes.PosixOp.Ppoll(fds, _)
  | HostTypes.PosixOp.PollDarwin(fds, _) ->
    fds |> List.map (fun (fd, _) -> fd, Metadata)
  | HostTypes.PosixOp.FdRead(fd, _)
  | HostTypes.PosixOp.DirectoryBytes(fd, _)
  | HostTypes.PosixOp.Pread(fd, _, _) -> read fd
  | HostTypes.PosixOp.FdWrite(fd, _)
  | HostTypes.PosixOp.Pwrite(fd, _, _)
  | HostTypes.PosixOp.Ftruncate(fd, _)
  | HostTypes.PosixOp.Fchmod(fd, _) -> write fd
  | HostTypes.PosixOp.Fstat fd
  | HostTypes.PosixOp.FcntlGetPath fd
  | HostTypes.PosixOp.FdSeek(fd, _, _)
  | HostTypes.PosixOp.Flock(fd, _) -> [ fd, Metadata ]
  | HostTypes.PosixOp.FdClose fd -> [ fd, Cleanup ]
  | HostTypes.PosixOp.CopyFileRange(source, destination, _)
  | HostTypes.PosixOp.CloneFile(source, destination) ->
    [ source, Read; destination, Write ]
  | _ -> []

let private mapDescriptors map (op : HostTypes.PosixOp) =
  match op with
  | HostTypes.PosixOp.FclonefileAt(source, destination, flags) ->
    HostTypes.PosixOp.FclonefileAt(map source, destination, flags)
  | HostTypes.PosixOp.KeventVnode(queue, changes, capacity, timeout) ->
    HostTypes.PosixOp.KeventVnode(
      map queue,
      changes |> List.map (fun (fd, flags, mask) -> map fd, flags, mask),
      capacity,
      timeout
    )
  | HostTypes.PosixOp.InotifyAddWatch(fd, path, mask) ->
    HostTypes.PosixOp.InotifyAddWatch(map fd, path, mask)
  | HostTypes.PosixOp.Ppoll(fds, timeout) ->
    HostTypes.PosixOp.Ppoll(
      fds |> List.map (fun (fd, events) -> map fd, events),
      timeout
    )
  | HostTypes.PosixOp.PollDarwin(fds, timeout) ->
    HostTypes.PosixOp.PollDarwin(
      fds |> List.map (fun (fd, events) -> map fd, events),
      timeout
    )
  | HostTypes.PosixOp.FdRead(fd, n) -> HostTypes.PosixOp.FdRead(map fd, n)
  | HostTypes.PosixOp.DirectoryBytes(fd, n) ->
    HostTypes.PosixOp.DirectoryBytes(map fd, n)
  | HostTypes.PosixOp.Pread(fd, n, offset) ->
    HostTypes.PosixOp.Pread(map fd, n, offset)
  | HostTypes.PosixOp.FdWrite(fd, data) -> HostTypes.PosixOp.FdWrite(map fd, data)
  | HostTypes.PosixOp.Pwrite(fd, data, offset) ->
    HostTypes.PosixOp.Pwrite(map fd, data, offset)
  | HostTypes.PosixOp.Ftruncate(fd, n) -> HostTypes.PosixOp.Ftruncate(map fd, n)
  | HostTypes.PosixOp.Fchmod(fd, n) -> HostTypes.PosixOp.Fchmod(map fd, n)
  | HostTypes.PosixOp.Fstat fd -> HostTypes.PosixOp.Fstat(map fd)
  | HostTypes.PosixOp.FcntlGetPath fd -> HostTypes.PosixOp.FcntlGetPath(map fd)
  | HostTypes.PosixOp.FdSeek(fd, offset, whence) ->
    HostTypes.PosixOp.FdSeek(map fd, offset, whence)
  | HostTypes.PosixOp.Flock(fd, operation) ->
    HostTypes.PosixOp.Flock(map fd, operation)
  | HostTypes.PosixOp.FdClose fd -> HostTypes.PosixOp.FdClose(map fd)
  | HostTypes.PosixOp.CopyFileRange(source, destination, n) ->
    HostTypes.PosixOp.CopyFileRange(map source, map destination, n)
  | HostTypes.PosixOp.CloneFile(source, destination) ->
    HostTypes.PosixOp.CloneFile(map source, map destination)
  | _ -> op

let private invalidDescriptor =
  Outcome.Failed
    { kind = FailureKind.Other
      errno = 9
      message = "descriptor is closed, foreign, or incompatible with its open mode" }

/// Called under the descriptor leases; they cannot be closed or reused until the
/// syscall completes. Current access is checked on every use, not just open.
let private resolveSessionPosix
  (session : Option<HostDescriptors.Session>)
  (op : HostTypes.PosixOp)
  : Result<Resolved, Outcome> =
  let uses = descriptorUses op
  let rec collect checks bindings raw remaining =
    match remaining with
    | [] -> Ok(checks, bindings, raw)
    | (HostTypes.FileDescriptor.Raw _, _) :: rest ->
      collect checks bindings true rest
    | (HostTypes.FileDescriptor.Scoped token, right) :: rest ->
      match session |> Option.bind (fun owner -> owner.Find token) with
      | None -> Error invalidDescriptor
      | Some entry ->
        let compatible =
          match right with
          | Read -> entry.reads
          | Write -> entry.writes
          | Metadata
          | Cleanup -> true
        if not compatible then
          Error invalidDescriptor
        else
          let checks =
            match right with
            | Cleanup -> checks
            | _ ->
              let access =
                match right with
                | Read -> Permission.AccessKind.Read
                | Write -> Permission.AccessKind.Write
                | Metadata ->
                  if entry.writes then
                    Permission.AccessKind.Write
                  else
                    Permission.AccessKind.Read
                | Cleanup ->
                  Exception.raiseInternal "cleanup has no resource check" []
              (entry.paths |> List.map (fileCheck access))
              @ (entry.processes
                 |> List.map (fun (program, args) ->
                   Permission.Request.processSpawn program args,
                   $"using child `{program}`"))
              @ checks
          collect checks (Map.add token entry.fd bindings) raw rest
  // A scoped queue cannot retain authority for an unowned raw vnode: its
  // checked path is unknown. Fully raw calls remain available behind Native.
  let unownedVnode =
    match op with
    | HostTypes.PosixOp.KeventVnode(HostTypes.FileDescriptor.Scoped _, changes, _, _) ->
      changes
      |> List.exists (function
        | HostTypes.FileDescriptor.Raw _, _, _ -> true
        | _ -> false)
    | _ -> false
  match
    (if unownedVnode then
       Error invalidDescriptor
     else
       collect [] Map.empty false uses)
  with
  | Error outcome -> Error outcome
  | Ok(checks, bindings, raw) ->
    let normalized =
      mapDescriptors
        (function
        | HostTypes.FileDescriptor.Raw fd -> HostTypes.FileDescriptor.Raw fd
        | HostTypes.FileDescriptor.Scoped token ->
          match Map.tryFind token bindings with
          | Some fd -> HostTypes.FileDescriptor.Raw fd
          | None -> Exception.raiseInternal "validated descriptor is missing" [])
        op
    match resolvePosix session normalized with
    | Error message -> Error(Outcome.Rejected message)
    | Ok resolved ->
      match withChecks (List.rev checks) resolved.execute with
      | Error message -> Error(Outcome.Rejected message)
      | Ok scoped ->
        let baseChecks =
          if uses <> [] && not raw then
            resolved.checks
            |> List.filter (fun (request, _) ->
              Permission.Request.effect request
              <> LibExecution.Effects.Effect.Native)
          else
            resolved.checks
        // Waiting for owned child IO is already authorized by that child's
        // retained executable/argv grant, just like the managed launcher was.
        // Raw descriptors and file/watch queues still require Clock. This keeps
        // Process.run usable under a narrow process-only policy.
        let ownedChildPoll =
          match op with
          | HostTypes.PosixOp.Ppoll(descriptors, _)
          | HostTypes.PosixOp.PollDarwin(descriptors, _) ->
            descriptors <> []
            && (descriptors
                |> List.forall (function
                  | HostTypes.FileDescriptor.Scoped token, _ ->
                    session
                    |> Option.bind (fun owner -> owner.Find token)
                    |> Option.exists (fun entry ->
                      entry.paths = [] && entry.processes <> [])
                  | _ -> false))
          | _ -> false
        let baseChecks =
          if ownedChildPoll then
            baseChecks
            |> List.filter (fun (request, _) ->
              Permission.Request.effect request <> LibExecution.Effects.Effect.Clock)
          else
            baseChecks
        let execute () =
          match op, session with
          | HostTypes.PosixOp.FdClose(HostTypes.FileDescriptor.Scoped token),
            Some owner -> owner.Consume token
          | _ -> ()
          resolved.execute ()
        Ok { checks = scoped.checks @ baseChecks; execute = execute }

/// Check and perform one host operation, then audit it exactly once. Derives
/// the requests from the operation itself, checks them against the access,
/// and executes.
let private performWithSession
  (session : Option<HostDescriptors.Session>)
  (relax : Permission.Relax)
  (access : Permission.Access)
  (op : Operation)
  : Task<Outcome> =
  task {
    let! outcome =
      match browserRefusal op with
      | Some refused -> Task.FromResult refused
      | None ->
        match op with
        | Operation.ChildStart(program, args, inheritTerminal) ->
          match session with
          | None -> Task.FromResult invalidDescriptor
          | Some owner ->
            match
              resolveProcess program args (fun program args ->
                blocking (fun () ->
                  HostProcess.startChild program args inheritTerminal
                  |> owner.RegisterChild
                  |> Response.ChildHandle
                  |> Ok))
            with
            | Error message -> Task.FromResult(Outcome.Rejected message)
            | Ok resolved ->
              match firstDenial relax access resolved.checks with
              | Some denied -> Task.FromResult denied
              | None -> execute resolved.execute
        | Operation.ChildWait handle ->
          let prepare () =
            match session |> Option.bind (fun owner -> owner.FindChild handle) with
            | None -> Error invalidDescriptor
            | Some entry ->
              match
                withChecks
                  [ Permission.Request.processSpawn
                      entry.child.program
                      entry.child.args,
                    $"waiting for child `{entry.child.program}`" ]
                  (produce (fun () -> Response.Unit))
              with
              | Error message -> Error(Outcome.Rejected message)
              | Ok resolved ->
                match firstDenial relax access resolved.checks with
                | Some denied -> Error denied
                | None -> Ok(HostProcess.prepareChildWait entry.child)
          Task.Run(fun () ->
            try
              let prepared =
                match session with
                | Some owner -> owner.WithChildLease(handle, prepare)
                | None -> prepare ()
              match prepared with
              | Error outcome -> outcome
              | Ok wait -> Outcome.Success(Response.ChildStatus(Some(wait ())))
            with e ->
              Outcome.Failed(classify e))
        | Operation.ChildDescriptors handle
        | Operation.ChildPoll handle
        | Operation.ChildKill handle
        | Operation.ChildClose handle
        | Operation.ChildClosePort(handle, _)
        | Operation.ChildBufferGet(handle, _)
        | Operation.ChildBufferSet(handle, _, _) ->
          let run () =
            match session |> Option.bind (fun owner -> owner.FindChild handle) with
            | None -> invalidDescriptor
            | Some entry ->
              let checks =
                match op with
                | Operation.ChildClose _
                | Operation.ChildClosePort _ -> []
                | _ ->
                  let request =
                    match
                      Permission.Request.processSpawn
                        entry.child.program
                        entry.child.args
                    with
                    | Ok request -> request
                    | Error message ->
                      Exception.raiseInternal
                        "invalid retained process grant"
                        [ "message", message ]
                  [ request, $"using child `{entry.child.program}`" ]
              match firstDenial relax access checks with
              | Some denied -> denied
              | None ->
                try
                  match op with
                  | Operation.ChildDescriptors _ ->
                    Outcome.Success(Response.ChildDescriptors entry.ports)
                  | Operation.ChildPoll _ ->
                    Outcome.Success(
                      Response.ChildStatus(HostProcess.pollChild entry.child)
                    )
                  | Operation.ChildKill _ ->
                    HostProcess.killChild entry.child
                    Outcome.Success Response.Unit
                  | Operation.ChildClose _ ->
                    session |> Option.iter (fun owner -> owner.CloseChild handle)
                    Outcome.Success Response.Unit
                  | Operation.ChildClosePort(_, index) ->
                    match session with
                    | Some owner ->
                      match owner.CloseChildPort(handle, index) with
                      | Ok() -> Outcome.Success Response.Unit
                      | Error(errno, message) ->
                        Outcome.Failed
                          { kind = FailureKind.Other
                            errno = errno
                            message = message }
                    | None -> invalidDescriptor
                  | Operation.ChildBufferGet(_, index) ->
                    if index < 0 || index > 1 then
                      invalidDescriptor
                    else
                      Outcome.Success(Response.Bytes entry.buffers[index])
                  | Operation.ChildBufferSet(_, index, bytes) ->
                    if index < 0 || index > 1 then
                      invalidDescriptor
                    else
                      entry.buffers[index] <- bytes
                      Outcome.Success Response.Unit
                  | _ -> Exception.raiseInternal "unexpected child operation" []
                with e ->
                  Outcome.Failed(classify e)
          Task.Run(fun () ->
            match session with
            | Some owner -> owner.WithChildLease(handle, run)
            | None -> run ())
        | Operation.Posix posix ->
          let resolve () = resolveSessionPosix session posix
          let run () =
            match resolve () with
            | Error outcome -> outcome
            | Ok resolved ->
              match firstDenial relax access resolved.checks with
              | Some denied -> denied
              | None ->
                // POSIX adapters are synchronous. Keep the descriptor lease
                // through the kernel call, including close and multi-fd copy.
                (execute resolved.execute).GetAwaiter().GetResult()
          let tokens =
            descriptorUses posix
            |> List.choose (function
              | HostTypes.FileDescriptor.Scoped token, _ -> Some token
              | _ -> None)
          if tokens <> [] then
            // Waiting reads/polls must not occupy the interpreter's execution
            // thread or serialize operations on unrelated owned descriptors.
            Task.Run(fun () ->
              match session with
              | Some owner -> owner.WithLeases(tokens, run)
              | None -> run ())
          else
            match resolve () with
            | Error outcome -> Task.FromResult outcome
            | Ok resolved ->
              match firstDenial relax access resolved.checks with
              | Some denied -> Task.FromResult denied
              | None -> execute resolved.execute
        | _ ->
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

let perform relax access op = performWithSession None relax access op

let performInSession session relax access op =
  performWithSession (Some session) relax access op

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

// ── host facts ────────────────────────────────────────────────────────────────

/// The platform value of one open() flag.
let openFlag (flag : HostTypes.OpenFlag) : int =
  match flag with
  | HostTypes.OpenFlag.ReadOnly -> HostPosix.O_RDONLY
  | HostTypes.OpenFlag.WriteOnly -> HostPosix.O_WRONLY
  | HostTypes.OpenFlag.ReadWrite -> HostPosix.O_RDWR
  | HostTypes.OpenFlag.Create -> HostPosix.O_CREAT
  | HostTypes.OpenFlag.Truncate -> HostPosix.O_TRUNC
  | HostTypes.OpenFlag.Append -> HostPosix.O_APPEND

/// The kernel's current window size for a terminal descriptor, as
/// (columns, rows). None when the descriptor is not a terminal, on Windows,
/// and on macOS, where the ioctl bridge is unsafe (see `HostPosix`).
let terminalWindowSize (fd : int) : Option<int64 * int64> =
  HostPosix.tryTerminalWindowSize fd

/// Trusted host-owned policy storage; never exposed as a Darklang builtin.
let syncPolicyDirectory (directory : string) : unit =
  HostPosix.syncDirectory directory
