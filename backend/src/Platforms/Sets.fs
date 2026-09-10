/// The catalog of platforms this build ships, and the named sets an executable runs.
///
/// This is the ONLY place that knows the full list, and it needs to stay that way. A list written
/// out per executable (`Cli.fs`, `LocalExec`, `Builtins.CliHost`, `TestUtils`) drifts, and
/// `Builtin.combine` is last-write-wins, so a duplicate is invisible rather than harmless.
///
/// Adding a platform is one line here. Shipping a smaller executable is a shorter list here (or a
/// set composed at the call site, as `Wasm/Repl.fs` does without linking this assembly at all).
module Platforms.Sets

open Prelude
open LibExecution.Platform

module RT = LibExecution.RuntimeTypes

// NOT `open System.Collections.Generic`: it shadows F#'s `List<'a>` with `ResizeArray`, and every
// `List<Platform>` in this file then means something else.
type private Dictionary<'k, 'v> = System.Collections.Generic.Dictionary<'k, 'v>


/// Every platform this build can offer, over a given package manager.
///
/// **The `pm` is not decoration.** `Store` reads through whatever package manager it is handed, and
/// the tests hand it an ephemeral one holding the declarations of the file under test. Wiring the
/// catalog to `LibDB.PackageManager.pt` unconditionally makes every test resolve against the real
/// store instead, and the symptom is not an error: names resolve to something plausible and the
/// round-trip tests print `<hash:Tests..M>` where a type name should be.
///
/// This is the shape of a platform that depends on a host service, and it will not be the last one:
/// `Data` will want the store path, `HttpClient` the guest network config. When there are three,
/// they become one `HostServices` record rather than three parameters.
///
/// A function rather than a value, also deliberately: `LibDB.PackageManager.pt` touches the store,
/// and a module-level value would do that during static initialization, before
/// `Seed.growIfNeeded` has run. Same invariant as `Cli.fs`'s `lazy`; the comment there says what it
/// cost to find.
let catalogFor (pm : LibExecution.ProgramTypes.PackageManager) : List<Platform> =
  [ Builtins.Pure.Builtin.platform
    Builtins.Time.Builtin.platform
    Builtins.Random.Builtin.platform
    Builtins.Cli.Builtin.terminalPlatform
    Builtins.Cli.Builtin.filesPlatform
    Builtins.Cli.Builtin.processPlatform
    Builtins.Cli.Builtin.posixPlatform
    Builtins.Http.Client.Builtin.platform
    Builtins.Http.Server.Builtin.platform
    Builtins.Store.Builtin.platform pm
    Builtins.Store.Builtin.authoringPlatform pm
    Builtins.Admin.Builtin.instancePlatform
    Builtins.Admin.Builtin.seedPlatform
    Builtins.Admin.Builtin.policyPlatform
    Builtins.Language.Builtin.platform
    Builtins.Data.Builtin.dbPlatform
    Builtins.Data.Builtin.tracesPlatform
    Builtins.Data.Builtin.accountsPlatform
    Builtins.Data.Builtin.sqlitePlatform
    Builtins.CliHost.Builtin.platform ]


/// The catalog over the real on-disk package store: what a running `darklang` has.
let catalog () : List<Platform> = catalogFor LibDB.PackageManager.pt


/// The names in the catalog, in catalog order.
let catalogNames () : List<string> = catalog () |> List.map _.name


/// Renames that apply to the composed set. Empty, and a rename that crosses a platform boundary
/// would belong here rather than inside either platform, since neither owns both names.
let fnRenames : LibExecution.Builtin.FnRenames = []


/// The platforms a session gets whether it asked for them or not.
///
/// Module-level rather than local to `activating`, because it is a fact other things need: the CLI
/// refuses to switch one of these off, and an F# test pins the Dark copy of this list against it
/// (`Platform.Tests`), the same way the effect table is pinned. A list stated twice with nothing
/// checking it is a list that drifts.
let alwaysOn : List<string> = [ "Core"; "Store" ]


/// Build a set from platform names. Raises (via `PlatformSet.make`) on an unknown name, a duplicate
/// builtin, or a missing requirement.
let byNames (names : List<string>) : PlatformSet =
  let available = catalog ()
  let chosen =
    names
    |> List.map (fun n ->
      match available |> List.tryFind (fun p -> p.name = n) with
      | Some p -> p
      | None ->
        let known = String.concat ", " (available |> List.map _.name)
        Exception.raiseInternal
          "no such platform"
          [ "requested", n; "available", known ])
  PlatformSet.make chosen fnRenames


/// Everything this build ships.
///
/// What `LocalExec` parses against: a package may reference any builtin, so the parser's name
/// resolver needs the whole floor regardless of what the running program will use.
let everything () : PlatformSet = PlatformSet.make (catalog ()) fnRenames


/// Everything, over a caller-supplied package manager. What the tests run against: they build an
/// ephemeral package manager holding the declarations of the file under test, and `Store` has to
/// read through THAT one for a name in the file to resolve.
let everythingFor (pm : LibExecution.ProgramTypes.PackageManager) : PlatformSet =
  PlatformSet.make (catalogFor pm) fnRenames


/// What the shipped CLI runs.
///
/// Currently the whole catalog. Kept as its own name anyway, because "what the CLI links" and "what
/// this build knows how to link" are different questions that happen to have the same answer today,
/// and the first one is the one that shrinks.
let cli () : PlatformSet = everything ()


/// Computation with no way to reach the world: `Core` alone, no store, no clock, no entropy.
///
/// Nothing ships this yet. It is here because it is the assertion that the split is real — if
/// `Core` cannot be composed on its own, the platform boundary is decoration.
let sealedCompute () : PlatformSet =
  PlatformSet.make [ Builtins.Pure.Builtin.platform ] []


/// One line per platform: coordinate, counts, and what it may do. The body of `dark platforms`.
let describe (set : PlatformSet) : List<string> =
  set.platforms
  |> List.sortBy _.name
  |> List.map (fun p ->
    let effects =
      match Platform.effectSurface p |> Set.toList with
      | [] -> "pure"
      | es ->
        es |> List.map LibExecution.Effects.name |> List.sort |> String.concat " "
    let coord = Platform.coordinate p
    let fns = Platform.fnCount p
    $"{coord}  {fns} fns  [{effects}]  {p.description}")


/// The tightening report: for every platform, every effect it reaches and which of its builtins are
/// responsible.
///
/// Sorted so the actionable lines come first. An effect with one contributor is a platform that
/// could stop reaching that effect by moving one function; an effect with thirty is the platform
/// doing its job. Reading it top to bottom is the fastest way to see where the surface is wider
/// than it needs to be.
let tighteningReport (set : PlatformSet) : List<string> =
  set.platforms
  |> List.sortBy _.name
  |> List.collect (fun p ->
    let contributors = Platform.effectContributors p
    let lines =
      contributors
      |> Map.toList
      |> List.sortBy (fun (e, owners) ->
        (List.length owners, LibExecution.Effects.name e))
      |> List.map (fun (e, owners) ->
        let n = List.length owners
        let shown =
          if n <= 4 then
            String.concat ", " owners
          else
            String.concat ", " (List.truncate 4 owners) + $", +{n - 4} more"
        let flag = if n = 1 then "  <- one builtin" else ""
        $"    {LibExecution.Effects.name e, -14} {n, 3}  {shown}{flag}")
    let header =
      if Map.isEmpty contributors then
        $"{Platform.coordinate p}  ({Platform.fnCount p} fns)  reaches nothing"
      else
        $"{Platform.coordinate p}  ({Platform.fnCount p} fns)"
    header :: lines)


/// Builtins whose declared effects are empty but which are marked `Impure`.
///
/// Not necessarily wrong: the sqlite builtins are deliberately in this state and decide in the body.
/// But it is the shape an UNDER-declaration takes, and the list should be short enough to read.
let undeclaredImpure (set : PlatformSet) : List<string> =
  set.platforms
  |> List.sortBy _.name
  |> List.collect (fun p ->
    p.builtins.fns.Values
    |> Seq.filter (fun fn ->
      Set.isEmpty fn.callEffects
      && fn.previewable = LibExecution.RuntimeTypes.Impure)
    |> Seq.map (fun fn -> $"{p.name}#{fn.name.name}")
    |> List.ofSeq)
  |> List.sort


/// Split the catalog into the platforms a session ACTIVATES and the rest, for lazy activation.
///
/// The binary links everything; this decides what a run can reach. Returns the active set plus a
/// map from every inactive builtin to the platform that provides it, which is what lets the
/// interpreter say "Sqlite is not active" instead of "no such function".
///
/// `Core` is always active and cannot be deactivated. Every other platform `requires` it, so a set
/// without it does not compose, and a session that cannot add two numbers is not a useful default.
/// Making that a rule here rather than a caller's responsibility means the near-pure default is
/// expressible as `activate []`.
let activating
  (wanted : List<string>)
  : PlatformSet * Dictionary<RT.FQFnName.Builtin, Platform> =
  let available = catalog ()

  // The floor is `Core` and `Store`, not `Core` alone, and finding that out is what building this
  // was for. Every platform `requires` Core, so a set without it does not compose. `Store` is less
  // obvious: it is how a NAME resolves to a hash, so a session without it cannot call a package
  // function at all, and Dark's own error printer -- which is Dark, and turns hashes back into
  // names -- cannot render the very error that says a platform is missing. The first run of this
  // printed `<pretty-print failed>` inside the message explaining the failure.
  //
  // Affordable only because `Store` reaches exactly `package-read`. A floor that had to include
  // `file-write` or `native` to resolve a name would not be a floor worth having, so `Store`
  // growing a second effect is a reason to revisit this list.
  let activeNames = Set.ofList (alwaysOn @ wanted)

  let isActive (p : Platform) = Set.contains p.name activeNames

  let unknown = wanted |> List.filter (fun n -> available |> List.forall (fun p -> p.name <> n))
  if not (List.isEmpty unknown) then
    let known = String.concat ", " (available |> List.map _.name)
    Exception.raiseInternal
      "no such platform"
      [ "requested", String.concat ", " unknown; "available", known ]

  let active = available |> List.filter isActive
  let inactive = available |> List.filter (isActive >> not)

  let inactiveBuiltins = Dictionary<RT.FQFnName.Builtin, Platform>()
  for p in inactive do
    for name in p.builtins.fns.Keys do
      // A name can only belong to one platform: `PlatformSet.make` refuses collisions, and the
      // whole catalog composes, so there is nothing to resolve here.
      inactiveBuiltins[name] <- p

  PlatformSet.make active fnRenames, inactiveBuiltins
