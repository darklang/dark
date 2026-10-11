/// Value types used by the checked host boundary: what a builtin asks for and
/// what comes back. See `LibExecution.Host` for the door itself.
module LibExecution.HostTypes

open Prelude

module Permission = LibExecution.Permissions

/// OS numbers require Native; scoped tokens are owned by one execution.
[<RequireQualifiedAccess>]
type FileDescriptor =
  | Raw of int
  | Scoped of bigint

/// A flag for the kernel's openat() call, named rather than numbered.
/// `Host.openFlag` turns it into the platform's number.
[<RequireQualifiedAccess>]
type OpenFlag =
  | ReadOnly
  | WriteOnly
  | ReadWrite
  | Create
  | Truncate
  | Append

/// POSIX operations retain kernel semantics and descriptor handles while using
/// the same scoped authorization as the interpreter's managed file operations.
/// Scoped descriptor tokens carry checked file authority; raw OS numbers and
/// unscoped process operations still require Native at the host boundary.
[<RequireQualifiedAccess>]
type PosixOp =
  | Chdir of path : string
  | Mkdir of path : string * mode : int
  | Rmdir of path : string
  | Unlink of path : string
  | UnlinkAt of path : string * flags : int
  | Rename of oldPath : string * newPath : string
  | Chmod of path : string * mode : int
  | UtimensAt of path : string * accessSec : int64 * accessNanos : int64 * modifySec : int64 * modifyNanos : int64
  | SetattrlistTimes of path : string * accessSec : int64 * accessNanos : int64 * modifySec : int64 * modifyNanos : int64
  | Symlink of target : string * linkPath : string
  | ReadlinkBytes of path : string * count : int
  | Mkstemp of prefix : string
  | Mkdtemp of prefix : string
  | ListDir of path : string
  | Stat of path : string
  | StatBuffer of path : string
  | DirectoryBytes of fd : FileDescriptor * count : int
  | CopyFileRange of source : FileDescriptor * destination : FileDescriptor * count : int
  | CloneFile of source : FileDescriptor * destination : FileDescriptor
  | Ftruncate of fd : FileDescriptor * length : int64
  | Fchmod of fd : FileDescriptor * mode : int
  | Fsync of fd : FileDescriptor
  | Fstat of fd : FileDescriptor
  | Getentropy of count : int
  | UnameBytes
  | SysctlBytes of mib : List<int> * count : int
  | SchedGetaffinity of pid : int * count : int
  | ClockGettime of clock : int
  | Gettimeofday
  | MachAbsoluteTime
  | MachTimebaseInfo
  | Nanosleep of seconds : int64 * nanos : int64
  | Pread of fd : FileDescriptor * count : int * offset : int64
  | Pwrite of fd : FileDescriptor * data : byte[] * offset : int64
  | Open of path : string * flags : int * mode : int
  // Descriptor operations derive file requests from execution-owned tokens,
  // or Native for raw OS numbers. Path answers are checked as path reads.
  | GetcwdBytes of count : int
  | FcntlGetPath of fd : FileDescriptor
  | Kill of pid : int * signal : int
  | FdRead of fd : FileDescriptor * count : int
  | FdSeek of fd : FileDescriptor * offset : int64 * whence : int
  | FdWrite of fd : FileDescriptor * data : byte[]
  | FdClose of fd : FileDescriptor
  | Flock of fd : FileDescriptor * operation : int
  | FileOwner of path : string
  | HomeDir of path : string

/// Which host HTTP configuration an operation runs under. `Guest` is the
/// SSRF-guarded configuration every guest request uses (the host may replace
/// it with `HostHttp.setGuestConfig`); `Sync` is the trusted tailnet pull the
/// bundled sync code uses, which reaches loopback and private ranges.
[<RequireQualifiedAccess>]
type HttpProfile =
  | Guest
  | Sync

/// One host action, with everything needed to both check and perform it.
[<RequireQualifiedAccess>]
type Operation =
  | FileRead of path : string
  | FileWrite of path : string * contents : byte[]
  | FileAppendText of path : string * content : string
  | FileDelete of path : string
  /// Metadata read: existence and directory-ness in one query.
  | FileStat of path : string
  | DirectoryCurrent
  | DirectoryList of path : string
  /// The running binary's path: a read of that path, routed through the door
  /// so builtin assemblies never call host subsystems directly.
  | CurrentExecutablePath
  | EnvGet of name : string
  | EnvList
  | HttpRequest of
    profile : HttpProfile *
    method : string *
    url : string *
    headers : List<string * string> *
    body : byte[]
  /// Open a streaming response; the handle in the reply reads from the
  /// host-owned stream table.
  | HttpStreamOpen of
    profile : HttpProfile *
    method : string *
    url : string *
    headers : List<string * string>
  /// Bind an HTTP listener and return an opaque host-owned handle.
  | HttpServerBind of port : int
  /// Run to completion; the program is resolved against PATH at the boundary.
  /// With a timeout, a child still running when it elapses is killed and the
  /// operation fails with ETIMEDOUT.
  | ProcessRun of program : string * args : List<string> * timeoutMs : Option<int>
  /// Run to completion on THIS terminal: stdin, stdout and stderr are inherited
  /// rather than captured, and only the exit code comes back. For programs that
  /// draw, like an editor; a redirected one paints into a pipe.
  | ProcessRunInteractive of program : string * args : List<string>
  /// Start an interactive process owned by the host; the response is an
  /// opaque handle id into the host's process table.
  | ProcessSpawn of program : string * args : List<string>
  | ProcessIO of handle : int64 * input : string
  | ProcessTerminate of handle : int64
  | Posix of PosixOp

// HTTP wire data. The error shape is inspired by Elm's HttpError
// (https://package.elm-lang.org/packages/elm/http/latest/Http#Error); the
// builtin maps it to the Stdlib.HttpClient types.

[<RequireQualifiedAccess>]
type HttpBadHeader =
  | EmptyKey
  | InvalidContentType

[<RequireQualifiedAccess>]
type HttpBadUrl =
  | UnsupportedProtocol
  | InvalidHost
  | InvalidUri
  | InvalidRequest

[<RequireQualifiedAccess>]
type HttpRequestError =
  | BadUrl of HttpBadUrl
  | Timeout
  | BadHeader of HttpBadHeader
  | NetworkError
  | BadMethod

type HttpResponse =
  { statusCode : int; headers : List<string * string>; body : byte[] }

/// An opened streaming response: status and headers are here; the body is
/// pulled chunk-by-chunk from the host's stream table via the handle.
type HttpStreamHead =
  { handle : int64; statusCode : int; headers : List<string * string> }

/// What a successful operation hands back. One vocabulary for the .NET and
/// POSIX families: a posix `stat` and a .NET file read both answer here.
[<RequireQualifiedAccess>]
type Response =
  | Bytes of byte[]
  | KernelSleep of rc : int * remaining : byte[]
  | Unit
  | Stat of exists : bool * isDirectory : bool
  | Path of string
  | Entries of List<string>
  | EnvValue of Option<string>
  | EnvEntries of List<string * string>
  | ProcessOutcome of exitCode : int * stdout : string * stderr : string
  | ProcessHandle of int64
  | HttpServerHandle of int64
  /// Send-time failures (timeout, network, bad header) are guest-visible
  /// typed errors, not runtime errors, so they ride inside the response.
  | Http of Result<HttpResponse, HttpRequestError>
  | HttpStream of Result<HttpStreamHead, HttpRequestError>
  /// A raw file descriptor (posix open).
  | Fd of int
  | Descriptor of bigint
  /// A descriptor and the path it was created at (posix mkstemp).
  | FdPath of fd : int * path : string
  | StatInfo of mode : int * size : int64 * mtimeSec : int64
  | Offset of int64
  | Written of int
  /// A name (a file's owner).
  | Text of string
  /// A name that may be absent (a user name, a home directory).
  | OptionalText of Option<string>

[<RequireQualifiedAccess>]
type FailureKind =
  | NotFound
  /// The OS refused (EACCES, UnauthorizedAccessException) — not a policy
  /// denial, which raises before the operation runs and never appears here.
  | OsAccessDenied
  | Other

/// An OS-level failure after the policy check passed. Guest-visible as a
/// Result error, unlike policy denials which are runtime errors. `errno` is
/// the kernel errno when the failure came from a syscall, and -1 otherwise (a .NET
/// exception) — it mirrors the Stdlib PosixError type.
type Failure = { kind : FailureKind; errno : int; message : string }

[<RequireQualifiedAccess>]
type Outcome =
  | Success of Response
  | Failed of Failure
  /// The operation was malformed or touched host-owned state; the message is
  /// the complete, user-facing error text.
  | Rejected of message : string
  /// A policy layer denied the derived request. Carries exactly what the
  /// denial message needs — the layer, the reason, the human-readable
  /// description of what was attempted, and the narrow rule that would fix it
  /// (derived structurally from the denied request, or why there is none), so
  /// diagnostics do not need the original request.
  | Denied of
    layer : Permission.Layer *
    reason : Permission.PolicyDenial *
    resource : string *
    suggestion : Permission.Suggestion

/// Drop the query string from a URL for the audit summary — query parameters
/// routinely carry secrets (tokens, signatures) and the log is default-on.
let private redactUrl (url : string) : string =
  match url.IndexOf('?') with
  | -1 -> url
  | i -> url.Substring(0, i) + "?<redacted>"

/// A short, human-readable summary of an operation, for the audit log. Deep
/// content (file bytes, request bodies) is deliberately omitted — the audit
/// records what was attempted, not the payload.
let describeOperation (op : Operation) : string =
  let posix (p : PosixOp) : string =
    match p with
    | PosixOp.Chdir path -> $"chdir {path}"
    | PosixOp.Mkdir(path, _) -> $"mkdir {path}"
    | PosixOp.Rmdir path -> $"rmdir {path}"
    | PosixOp.Unlink path -> $"unlink {path}"
    | PosixOp.UnlinkAt(path, flags) -> $"unlinkat {path} {flags}"
    | PosixOp.Rename(o, n) -> $"rename {o} -> {n}"
    | PosixOp.Chmod(path, _) -> $"chmod {path}"
    | PosixOp.UtimensAt(path, _, _, _, _) -> $"utimensAt {path}"
    | PosixOp.SetattrlistTimes(path, _, _, _, _) -> $"setattrlistTimes {path}"
    | PosixOp.Symlink(t, l) -> $"symlink {l} -> {t}"
    | PosixOp.ReadlinkBytes(path, count) -> $"readlinkat {path} {count}"
    | PosixOp.UnameBytes -> "uname"
    | PosixOp.SysctlBytes(mib, count) -> $"sysctl {mib} {count}"
    | PosixOp.SchedGetaffinity(pid, count) -> $"sched_getaffinity {pid} {count}"
    | PosixOp.ClockGettime clock -> $"clock_gettime {clock}"
    | PosixOp.Gettimeofday -> "gettimeofday"
    | PosixOp.MachAbsoluteTime -> "mach_absolute_time"
    | PosixOp.MachTimebaseInfo -> "mach_timebase_info"
    | PosixOp.Nanosleep(seconds, nanos) -> $"nanosleep {seconds} {nanos}"
    | PosixOp.Mkstemp prefix -> $"mkstemp {prefix}"
    | PosixOp.Mkdtemp prefix -> $"mkdtemp {prefix}"
    | PosixOp.ListDir path -> $"listdir {path}"
    | PosixOp.Stat path -> $"stat {path}"
    | PosixOp.StatBuffer path -> $"stat-bytes {path}"
    | PosixOp.DirectoryBytes(fd, _) -> $"directory-bytes {fd}"
    | PosixOp.CopyFileRange(source, destination, _) ->
      $"copy-file-range {source} -> {destination}"
    | PosixOp.CloneFile(source, destination) -> $"clone {source} -> {destination}"
    | PosixOp.Ftruncate(fd, _) -> $"ftruncate {fd}"
    | PosixOp.Fchmod(fd, _) -> $"fchmod {fd}"
    | PosixOp.Fsync fd -> $"fsync {fd}"
    | PosixOp.Fstat fd -> $"fstat {fd}"
    | PosixOp.Getentropy count -> $"getentropy {count}"
    | PosixOp.Pread(fd, _, _) -> $"pread {fd}"
    | PosixOp.Pwrite(fd, _, _) -> $"pwrite {fd}"
    | PosixOp.Open(path, _, _) -> $"open {path}"
    | PosixOp.GetcwdBytes _ -> "getcwd"
    | PosixOp.FcntlGetPath _ -> "fcntlGetPath"
    | PosixOp.Kill(pid, signal) -> $"kill {pid} {signal}"
    | PosixOp.FdRead(fd, _) -> $"fd-read {fd}"
    | PosixOp.FdSeek(fd, _, _) -> $"fd-seek {fd}"
    | PosixOp.FdWrite(fd, _) -> $"fd-write {fd}"
    | PosixOp.FdClose fd -> $"fd-close {fd}"
    | PosixOp.Flock(fd, _) -> $"flock {fd}"
    | PosixOp.FileOwner path -> $"file-owner {path}"
    | PosixOp.HomeDir path -> $"home-dir {path}"
  match op with
  | Operation.FileRead path -> $"file-read {path}"
  | Operation.FileWrite(path, _) -> $"file-write {path}"
  | Operation.FileAppendText(path, _) -> $"file-append {path}"
  | Operation.FileDelete path -> $"file-delete {path}"
  | Operation.FileStat path -> $"file-stat {path}"
  | Operation.DirectoryCurrent -> "cwd"
  | Operation.DirectoryList path -> $"dir-list {path}"
  | Operation.CurrentExecutablePath -> "current-executable-path"
  | Operation.EnvGet name -> $"env-get {name}"
  | Operation.EnvList -> "env-list"
  | Operation.HttpRequest(_, method, url, _, _) -> $"http {method} {redactUrl url}"
  | Operation.HttpStreamOpen(_, method, url, _) ->
    $"http-stream {method} {redactUrl url}"
  | Operation.HttpServerBind port -> $"http-server-bind {port}"
  | Operation.ProcessRun(program, _, _) -> $"process-run {program}"
  | Operation.ProcessRunInteractive(program, _) ->
    $"process-run-interactive {program}"
  | Operation.ProcessSpawn(program, _) -> $"process-spawn {program}"
  | Operation.ProcessIO(handle, _) -> $"process-io {handle}"
  | Operation.ProcessTerminate handle -> $"process-terminate {handle}"
  | Operation.Posix p -> $"posix {posix p}"

/// Remove secrets from outcome details before they reach the default audit
/// log. A denial or host failure can repeat a URL that the summary redacted.
let redactAuditDetail (op : Operation) (detail : string) : string =
  match op with
  | Operation.HttpRequest _
  | Operation.HttpStreamOpen _ -> redactUrl detail
  | _ -> detail
