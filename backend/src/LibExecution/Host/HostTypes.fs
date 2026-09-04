/// Value types used by the checked host boundary: what a builtin asks for and
/// what comes back. See `LibExecution.Host` for the door itself.
module LibExecution.HostTypes

open Prelude

module Permission = LibExecution.Permissions

/// Posix operations with a scoped resource (a path, env name, or spawn). The
/// libc twins of the .NET operations exist because libc semantics differ (the
/// libc environment vs the CLR cache, fork/exec details), so they keep their
/// own operations rather than collapsing into the .NET-backed ones. Ambient
/// Native-gated posix builtins (uname, getpid, ...) that carry no state do not
/// appear here; they call `HostLibc` directly under the interpreter's Native
/// gate.
[<RequireQualifiedAccess>]
type PosixOp =
  | Chdir of path : string
  | Setenv of name : string * value : string
  | Unsetenv of name : string
  | Getenv of name : string
  | Mkdir of path : string * mode : int
  | Rmdir of path : string
  | Unlink of path : string
  | Rename of oldPath : string * newPath : string
  | Chmod of path : string * mode : int
  | UtimesNow of path : string
  | Symlink of target : string * linkPath : string
  | Readlink of path : string
  | Mkstemp of prefix : string
  | Mkdtemp of prefix : string
  | ListDir of path : string
  | Stat of path : string
  | Open of path : string * flags : int * mode : int
  // Native-gated handle and process operations. They carry no scoped
  // resource — the interpreter's ambient native gate authorized the builtin —
  // but must execute where the handles and processes live: fds come from
  // Open/Mkstemp, pids from `Operation.ProcessRun`. Getcwd, HomeDir and
  // FileOwner answer with (or take) a path and are checked as reads of it;
  // UserName is a plain host fact.
  | Getcwd
  | Kill of pid : int * signal : int
  | FdRead of fd : int * count : int
  | FdSeek of fd : int * offset : int64 * whence : int
  | FdWrite of fd : int * data : byte[]
  | FdClose of fd : int
  | Flock of fd : int * operation : int
  | FileOwner of path : string
  | UserName of uid : uint32
  | HomeDir

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
/// libc families: a posix `stat` and a .NET file read both answer here.
[<RequireQualifiedAccess>]
type Response =
  | Bytes of byte[]
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
  | PermissionDenied
  | Other

/// An OS-level failure after the policy check passed. Guest-visible as a
/// Result error, unlike policy denials which are runtime errors. `errno` is
/// the libc code when the failure came from libc, and -1 otherwise (a .NET
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
  /// (derived structurally from the denied request, `None` where no scoped
  /// rule fits) — so diagnostics do not need the original request.
  | Denied of
    layer : Permission.Layer *
    reason : Permission.PolicyDenial *
    resource : string *
    suggestion : Option<string>

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
    | PosixOp.Setenv(name, _) -> $"setenv {name}"
    | PosixOp.Unsetenv name -> $"unsetenv {name}"
    | PosixOp.Getenv name -> $"getenv {name}"
    | PosixOp.Mkdir(path, _) -> $"mkdir {path}"
    | PosixOp.Rmdir path -> $"rmdir {path}"
    | PosixOp.Unlink path -> $"unlink {path}"
    | PosixOp.Rename(o, n) -> $"rename {o} -> {n}"
    | PosixOp.Chmod(path, _) -> $"chmod {path}"
    | PosixOp.UtimesNow path -> $"utimes {path}"
    | PosixOp.Symlink(t, l) -> $"symlink {l} -> {t}"
    | PosixOp.Readlink path -> $"readlink {path}"
    | PosixOp.Mkstemp prefix -> $"mkstemp {prefix}"
    | PosixOp.Mkdtemp prefix -> $"mkdtemp {prefix}"
    | PosixOp.ListDir path -> $"listdir {path}"
    | PosixOp.Stat path -> $"stat {path}"
    | PosixOp.Open(path, _, _) -> $"open {path}"
    | PosixOp.Getcwd -> "getcwd"
    | PosixOp.Kill(pid, signal) -> $"kill {pid} {signal}"
    | PosixOp.FdRead(fd, _) -> $"fd-read {fd}"
    | PosixOp.FdSeek(fd, _, _) -> $"fd-seek {fd}"
    | PosixOp.FdWrite(fd, _) -> $"fd-write {fd}"
    | PosixOp.FdClose fd -> $"fd-close {fd}"
    | PosixOp.Flock(fd, _) -> $"flock {fd}"
    | PosixOp.FileOwner path -> $"file-owner {path}"
    | PosixOp.UserName uid -> $"user-name {uid}"
    | PosixOp.HomeDir -> "home-dir"
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
