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
/// The whole catalog. Kept as its own name rather than as a use of `everything`, because "what the
/// CLI links" and "what this build knows how to link" are different questions that happen to share
/// an answer, and only the first one is expected to shrink.
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


/// Every effect, with how many builtins reach it and which platforms they come from.
///
/// The tightening report asks "what does this platform reach"; this asks the question the other way
/// round, which is the one that matters for shrinking the surface: how many DOORS are there to each
/// effect, and where are they. An effect reached by two builtins is nearly grantable per door; one
/// reached by forty is a category, and no policy rule over it will ever mean much.
///
/// Pure builtins are counted too, because "how much of this is not a door at all" is half the
/// answer and the half that is easy to forget.
let effectDoors (set : PlatformSet) : List<string> =
  let doors =
    set.platforms
    |> List.collect (fun p ->
      p.builtins.fns.Values
      |> Seq.collect (fun fn ->
        fn.callEffects |> Set.toList |> List.map (fun e -> (e, p.name, fn.name.name)))
      |> List.ofSeq)

  let byEffect =
    doors
    |> List.groupBy (fun (e, _, _) -> e)
    |> Map.map (fun rows ->
      let platforms = rows |> List.map (fun (_, p, _) -> p) |> List.distinct |> List.sort
      let fns = rows |> List.map (fun (_, _, f) -> f) |> List.distinct |> List.sort
      (platforms, fns))

  let effectful =
    set.platforms
    |> List.collect (fun p -> p.builtins.fns.Values |> Seq.toList)
    |> List.filter (fun fn -> not (Set.isEmpty fn.callEffects))
    |> List.length

  let total =
    set.platforms |> List.sumBy (fun p -> p.builtins.fns.Count)

  let lines =
    byEffect
    |> Map.toList
    // Most doors first: that is the effect whose rules mean the least and the one worth splitting.
    |> List.sortBy (fun (e, (_, fns)) -> (-(List.length fns), LibExecution.Effects.name e))
    |> List.map (fun (e, (platforms, fns)) ->
      let where = String.concat ", " platforms
      let shown =
        if List.length fns <= 6 then
          String.concat ", " fns
        else
          String.concat ", " (List.truncate 6 fns) + $", +{List.length fns - 6} more"
      let door = if List.length fns = 1 then "door " else "doors"
      $"{LibExecution.Effects.name e, -14} {List.length fns, 4} {door}  ({where})
                        {shown}")

  let gatedTwice =
    doors
    |> List.filter (fun (_, _, f) -> Set.contains f LibExecution.PermissionCheck.firstPartyOnly)
    |> List.map (fun (_, _, f) -> f)
    |> List.distinct
    |> List.sort

  lines
  @ [ ""
      $"{effectful} of {total} builtins declare an effect; the rest are doors to nothing."
      ""
      "Gated a second time, by caller trust rather than by policy:"
      "    " + String.concat ", " gatedTwice ]


/// Every builtin that reaches one named effect, with its platform and its signature.
///
/// The summary truncates, and the effect worth splitting is always the one whose list was too long
/// to print. Signatures are here because the question "could this be narrower" is usually answered
/// by what the function takes: one that names a resource can be scoped, one that takes nothing
/// cannot.
let doorsTo (set : PlatformSet) (effectName : string) : List<string> =
  set.platforms
  |> List.sortBy _.name
  |> List.collect (fun p ->
    p.builtins.fns.Values
    |> Seq.filter (fun fn ->
      fn.callEffects
      |> Set.exists (fun e -> LibExecution.Effects.name e = effectName))
    |> Seq.map (fun fn ->
      // A short rendering, not `string t`: a `TCustomType` prints its whole resolved hash record,
      // which is several lines and drowns the thing being read.
      let rec typ (t : RT.TypeReference) : string =
        match t with
        | RT.TCustomType _ -> "<type>"
        | RT.TList inner -> $"List<{typ inner}>"
        | RT.TDict(_, v) -> $"Dict<{typ v}>"
        | RT.TTuple(a, b, rest) ->
          let parts = (a :: b :: rest) |> List.map typ |> String.concat ", "
          $"({parts})"
        | other -> string other
      let ps = fn.parameters |> List.map (fun p -> typ p.typ) |> String.concat ", "
      let trust =
        if Set.contains fn.name.name LibExecution.PermissionCheck.firstPartyOnly then "  first-party only" else ""
      $"{p.name, -12} {fn.name.name, -32} ({ps}){trust}")
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
///
/// Takes the catalog rather than reading it, because what is available is not always what this
/// build links: an instance with external platforms installed composes them in, and an activation
/// naming one has to resolve against the composed list or the platform a person just installed is
/// "no such platform" the moment they switch it on.
///
/// There is deliberately no version that reads the catalog itself. Every caller wants the composed
/// set, and one that silently got the linked set instead would be wrong only on the machines that
/// had installed something, which is the worst place for it to be wrong.
let activatingFrom
  (available : List<Platform>)
  (wanted : List<string>)
  : PlatformSet * Dictionary<RT.FQFnName.Builtin, Platform> =
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

  // A name nobody ships is SKIPPED here rather than refused, and the split is deliberate: a choice
  // is validated when it is WRITTEN, so a typo cannot be stored, and by the time it is read the
  // thing it named is allowed to have gone away. Uninstall a platform, or run a build that no
  // longer ships one, and raising here would leave an instance that cannot run any command at all,
  // including the one that would fix it. `dark platforms` shows what is actually on.

  let active = available |> List.filter isActive
  let inactive = available |> List.filter (isActive >> not)

  let inactiveBuiltins = Dictionary<RT.FQFnName.Builtin, Platform>()
  for p in inactive do
    for name in p.builtins.fns.Keys do
      // A name can only belong to one platform: `PlatformSet.make` refuses collisions, and the
      // whole catalog composes, so there is nothing to resolve here.
      inactiveBuiltins[name] <- p

  PlatformSet.make active fnRenames, inactiveBuiltins

