/// Static effects attached to functions.
///
/// Effects describe behavior; they do not grant runtime permission. Keep this
/// module independent of RuntimeTypes so it can be used by both ProgramTypes
/// and RuntimeTypes without introducing a compile-order cycle.
module LibExecution.Effects

open Prelude

/// A deliberately small initial vocabulary. Add a case only when callers need
/// to distinguish it for typechecking, preview, replay, or scheduling.
[<RequireQualifiedAccess>]
type Effect =
  | Http
  | HttpServer
  | FileRead
  | FileWrite
  | EnvRead
  | EnvWrite
  | DbRead
  | DbWrite
  | Stdin
  | Stdout
  | Clock
  | Random
  | Process
  | PackageRead
  | PackageWrite
  | TraceRead
  | TraceWrite
  /// Starting a process that runs beside this one (`Exec.spawn`). Ambient: the child runs under
  /// the spawner's access, so what it may do is already decided; this names only that it runs at
  /// all, on another core if one is free.
  | Concurrency
  /// The effect for a builtin nobody can scope: it can reach anything on the
  /// host, and no rule could honestly say otherwise. `Sqlite.query` is the
  /// canonical case: it is given one database path, but the SQL it runs can
  /// say `ATTACH '/home/you/.ssh/id_rsa' AS x` and open any file on the
  /// machine. Checking the database path alone would pretend to confine
  /// something the runtime cannot see, so the builtin declares `Native`
  /// instead, which means "granting this hands over the keys". The same
  /// applies to raw descriptors and process handles (the operation names a
  /// number, not a resource) and to plain host facts such as `uname`, which
  /// have nothing to scope. There is deliberately no scoped form: a policy
  /// grants it whole, with `allow native`, or not at all.
  | Native

let name (effect : Effect) : string =
  match effect with
  | Effect.Http -> "http"
  | Effect.HttpServer -> "http-server"
  | Effect.FileRead -> "file-read"
  | Effect.FileWrite -> "file-write"
  | Effect.EnvRead -> "env-read"
  | Effect.EnvWrite -> "env-write"
  | Effect.DbRead -> "db-read"
  | Effect.DbWrite -> "db-write"
  | Effect.Stdin -> "stdin"
  | Effect.Stdout -> "stdout"
  | Effect.Clock -> "clock"
  | Effect.Random -> "random"
  | Effect.Process -> "process"
  | Effect.PackageRead -> "package-read"
  | Effect.PackageWrite -> "package-write"
  | Effect.TraceRead -> "trace-read"
  | Effect.TraceWrite -> "trace-write"
  | Effect.Concurrency -> "concurrency"
  | Effect.Native -> "native"

/// Every effect, in declaration order.
let all : List<Effect> =
  [ Effect.Http
    Effect.HttpServer
    Effect.FileRead
    Effect.FileWrite
    Effect.EnvRead
    Effect.EnvWrite
    Effect.DbRead
    Effect.DbWrite
    Effect.Stdin
    Effect.Stdout
    Effect.Clock
    Effect.Random
    Effect.Process
    Effect.PackageRead
    Effect.PackageWrite
    Effect.TraceRead
    Effect.TraceWrite
    Effect.Concurrency
    Effect.Native ]

let fromName (wanted : string) : Option<Effect> =
  all |> List.tryFind (fun effect -> name effect = wanted)

/// A read effect observes the world without changing it, so calls whose effects are all reads
/// may run concurrently and complete in any order; the interpreter hands such a call back as a
/// promise when it has to wait (`docs/processes.md`, "Reads are concurrent"). `Http` is not one
/// here, since the one word covers every method and a policy grants it whole; the GET and HEAD
/// builtin is named a read below (`readsOnly`). `Stdin` consumes input, so it is not one either.
/// `Clock` and `Random` are left out on purpose: reading the clock or a random number never
/// waits, so nothing is gained, and the one `Clock` call that does wait, `sleep`, is a wait the
/// program means to take, not a read to overlap.
let isRead (effect : Effect) : bool =
  match effect with
  | Effect.FileRead
  | Effect.EnvRead
  | Effect.DbRead
  | Effect.PackageRead
  | Effect.TraceRead -> true
  | Effect.Http
  | Effect.HttpServer
  | Effect.FileWrite
  | Effect.EnvWrite
  | Effect.DbWrite
  | Effect.Stdin
  | Effect.Stdout
  | Effect.Clock
  | Effect.Random
  | Effect.Process
  | Effect.PackageWrite
  | Effect.TraceWrite
  | Effect.Concurrency
  | Effect.Native -> false

/// Non-empty, and nothing but reads.
let private allReads (effects : Set<Effect>) : bool =
  not (Set.isEmpty effects) && Set.forall isRead effects

/// Builtins whose declared effect is not a read but whose every call is one: the HTTP client's
/// GET and HEAD builtin. `Http` stays one word in the permission language (a policy allows a
/// URL, not a method), so the read-ness of a GET is stated here, per builtin, rather than by a
/// second effect name every policy and older binary would have to know.
let private readsOnlyByName : Set<string> = set [ "httpClientRead" ]

/// Whether every call of this builtin is a read: its effects are all reads, or it is one of the
/// builtins named above. What decides if a call whose result is not ready is handed back as a
/// promise rather than parking the process.
let readsOnly (builtinName : string) (effects : Set<Effect>) : bool =
  allReads effects || Set.contains builtinName readsOnlyByName

/// A scoped effect names a resource (a path, a URL, a table, an executable),
/// so its exact request can only be built by the builtin body — or, for the
/// OS-facing ones, by the checked host boundary from the `Operation`. An
/// ambient effect has no resource and is checked once, from the builtin's
/// declared effects, before the body runs.
let isScoped (effect : Effect) : bool =
  match effect with
  | Effect.Http
  | Effect.HttpServer
  | Effect.FileRead
  | Effect.FileWrite
  | Effect.EnvRead
  | Effect.EnvWrite
  | Effect.DbRead
  | Effect.DbWrite
  | Effect.Process -> true
  | Effect.Stdin
  | Effect.Stdout
  | Effect.Clock
  | Effect.Random
  | Effect.PackageRead
  | Effect.PackageWrite
  | Effect.TraceRead
  | Effect.TraceWrite
  | Effect.Concurrency
  | Effect.Native -> false
