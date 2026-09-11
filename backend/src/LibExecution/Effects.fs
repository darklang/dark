/// Static effects attached to functions.
///
/// Effects describe behavior; they do not grant runtime permission. Keep this
/// module independent of RuntimeTypes so it can be used by both ProgramTypes
/// and RuntimeTypes without introducing a compile-order cycle.
module LibExecution.Effects

open Prelude

/// The well-known vocabulary, plus `Custom` for anything a platform outside this repo needs to
/// name.
///
/// The well-known cases are the ones the runtime itself understands: the host boundary builds a
/// scoped `Request` for them, the policy grammar has resource-shaped rules for several, and the
/// permission check knows what each one means. Add one only when callers need to distinguish it for
/// typechecking, preview, replay, or scheduling.
///
/// `Custom` exists because the goal is that a LIBRARY can bring capabilities, and a capability
/// vocabulary only its authors may extend is not one. Before this the choices open to a platform
/// with a genuinely new capability -- a serial port, a vendor SDK -- were to mislabel itself as an
/// existing effect or to declare `Native`, which announces "granting this hands over the machine"
/// and is the opposite of advertising something narrow.
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

  /// Reading and writing the HOST's own permission state: the instance policy, package approvals,
  /// function pins, and which platforms this instance has switched on.
  ///
  /// Ambient and unscoped, exactly like `PackageRead` and `TraceRead`: the policy store is a
  /// host-owned whole with no per-resource handle to name, so a rule grants it or does not.
  ///
  /// These exist so that reading your own policy is not the same grant as handing over the
  /// machine. Every builtin here used to declare `Native`, which meant `dark permissions` and
  /// anything touching an approval looked, to the effect system, exactly like `Sqlite.query`.
  | PolicyRead
  | PolicyWrite

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

  /// An effect named by a platform this runtime did not ship, as `owner/name`.
  ///
  /// Namespaced, and enforced by `custom`: two vendors must not be able to collide on `serial`,
  /// and a custom effect must never be mistakable for a well-known one. Whole-or-nothing at the
  /// policy layer for the same reason `Native` is -- the runtime cannot build a scoped request for
  /// a resource it knows nothing about, so it will not pretend to confine one.
  | Custom of string

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
  | Effect.PolicyRead -> "policy-read"
  | Effect.PolicyWrite -> "policy-write"
  | Effect.Native -> "native"
  | Effect.Custom name -> name

/// Every WELL-KNOWN effect, in declaration order. Custom effects are not enumerable: they exist
/// because a platform declared one, so the platform set is what knows them.
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
    Effect.PolicyRead
    Effect.PolicyWrite
    Effect.Native ]

/// The shape a custom effect name must have: `owner/name`, both segments lowercase alphanumeric
/// with dashes. The slash is what makes a collision with a well-known name impossible, since none
/// of those contain one.
let private customNamePattern =
  System.Text.RegularExpressions.Regex(
    @"^[a-z0-9]([a-z0-9-]*[a-z0-9])?/[a-z0-9]([a-z0-9-]*[a-z0-9])?$",
    System.Text.RegularExpressions.RegexOptions.Compiled
  )

/// Build a custom effect, or `None` if the name is not `owner/name`.
///
/// The only way to make one, deliberately: an unvalidated `Custom "http"` would shadow a
/// well-known effect in every comparison and every policy rule, and nothing downstream would
/// notice.
let custom (name : string) : Option<Effect> =
  if customNamePattern.IsMatch name then Some(Effect.Custom name) else None

/// Resolve a name to an effect: a well-known one, or a validated custom one.
let fromName (wanted : string) : Option<Effect> =
  match all |> List.tryFind (fun effect -> name effect = wanted) with
  | Some wellKnown -> Some wellKnown
  | None -> custom wanted

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
  | Effect.PolicyRead
  | Effect.PolicyWrite
  | Effect.Native
  // A runtime that has never heard of this effect cannot build a request naming the resource it
  // is about, so it grants the whole thing or nothing. Same honesty as `Native`.
  | Effect.Custom _ -> false
