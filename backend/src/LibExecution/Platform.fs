/// Platforms: the named, versioned unit a runtime is assembled from.
///
/// A **platform** is a bundle of builtins plus the effects they may perform, and a statement of
/// whether it needs a store on disk. An executable is a choice of platforms; that choice is a
/// `PlatformSet`, and its `fingerprint` is what everything downstream compares against.
///
/// This module owns the vocabulary only. WHICH platforms exist is a `Platforms` concern (that
/// assembly references the `Builtins.*` ones; this one cannot, since they reference us).
///
/// Why this exists rather than a hardcoded `Builtin.combine [ ... ]` list per executable:
///
///  1. **A collision becomes an error.** `Builtin.combine` is last-write-wins over a `Dictionary`,
///     so two libraries claiming one name silently pick one. Inside a single curated set that is
///     survivable. Across independently shipped platforms it is the bug that eats an afternoon, so
///     `PlatformSet.make` refuses instead, and names both platforms.
///  2. **The set gets an identity.** `fingerprint` is a content hash of every builtin's owner,
///     identity, signature and effects. That one string answers "is the primitive floor the same as
///     when this was compiled / approved / cached?", which is the question behind the package
///     reload, the approval-staleness check, and any future instruction cache.
///  3. **Composition is data.** `run-local-exec platforms` lists what a build ships, and a smaller
///     executable is a shorter list rather than an edit to four files. `Wasm/Repl.fs` is the worked
///     example: it composes `Core` + `HttpClient` + one of its own without linking the catalog.
///
/// Effects vs permissions, restated because the distinction is the whole safety story: a platform
/// DECLARES what its builtins may do (`Effects.Effect`, static, part of the fingerprint). It never
/// GRANTS anything. Grants live in `LibDB.PolicyStore`, outside the package database, and a
/// platform cannot write them.
///
/// **A platform has to earn its assembly.** Build cost here is linear in the number of projects in
/// the closure you ask for, measured at about 1.4 seconds each and paid whether or not anything
/// changed, so every split adds that to every build that reaches it. Splitting `Matter` into
/// `Store`, `Data` and `Admin` earned it: they have different effect surfaces, different audiences,
/// and `Store` stopped dragging `LibCloud`. Splitting four instrumentation functions out of `Lang`
/// would not. The test is whether the pieces are wanted apart, not whether they are different.
///
/// A platform's builtins do NOT have to be one assembly forever, either. `HttpClient` and
/// `HttpServer` are separate platforms over separate assemblies because they are separate risks;
/// nothing stops a future platform spanning two assemblies or two platforms sharing one, and the
/// record is what says which.
module LibExecution.Platform

open Prelude
open LibExecution.RuntimeTypes

module Builtin = LibExecution.Builtin


/// A platform's linear version. Forward-only, one integer, same shape as the store's `Release`: a
/// binary refuses a platform version it does not know rather than guessing at the difference.
type Version = int


/// `Platform` itself is `RuntimeTypes.Platform`, so that `ExecutionState` can carry one. Aliased
/// here so `LibExecution.Platform.Platform` keeps working: that is the name every platform record
/// annotates itself with, and it reads better at those sites than the runtime-types path does.
type Platform = RuntimeTypes.Platform


module Platform =
  /// Everything this platform's builtins may do. The install-time review surface: what a person is
  /// agreeing to when they add it, before any policy narrows it.
  ///
  /// A union over the whole platform, so it is deliberately coarser than any single call. `Native`
  /// appearing here means the platform contains something nobody can scope (see `Effects.Native`),
  /// and the honest summary is that granting it hands over the machine.
  let effectSurface (p : Platform) : Set<Effects.Effect> =
    p.builtins.fns.Values
    |> Seq.fold (fun acc fn -> Set.union acc fn.callEffects) p.dynamicEffects

  /// For each effect this platform reaches, which of its builtins declare it.
  ///
  /// The tightening report, and the reason it is worth having: an effect declared by exactly ONE
  /// builtin is an effect the whole platform could lose by moving or narrowing that one function.
  /// `Store` reached `file-write` because of `pmSeedExport` and nothing else, and nobody was going
  /// to notice that by reading ten files.
  ///
  /// `dynamicEffects` are attributed to the platform itself under the name `(dynamic)`, since no
  /// single builtin declares them.
  let effectContributors (p : Platform) : Map<Effects.Effect, List<string>> =
    let fromFns =
      p.builtins.fns.Values
      |> Seq.collect (fun fn ->
        fn.callEffects |> Set.toList |> List.map (fun e -> (e, fn.name.name)))
      |> List.ofSeq
    let fromDynamic =
      p.dynamicEffects |> Set.toList |> List.map (fun e -> (e, "(dynamic)"))
    (fromFns @ fromDynamic)
    // Prelude's `List.groupBy` answers a `Map`, not F#'s list of pairs.
    |> List.groupBy fst
    |> Map.map (fun pairs -> pairs |> List.map snd |> List.distinct |> List.sort)

  /// Does this platform reach the operating system at all?
  let isPure (p : Platform) : bool = Set.isEmpty (effectSurface p)

  let fnCount (p : Platform) : int = p.builtins.fns.Count
  let valueCount (p : Platform) : int = p.builtins.values.Count

  /// `Name@version`.
  let coordinate (p : Platform) : string = $"{p.name}@{p.version}"

  /// One line per builtin this platform contributes: name, version, type parameters, parameter
  /// types, return type and effects. The same fields `PlatformSet`'s manifest uses, and
  /// deliberately the same omissions (descriptions, parameter names, `sqlSpec`), so a doc fix does
  /// not move it.
  ///
  /// `dynamicEffects` is in it too. A platform whose builtins decide their effects by argument is
  /// a different platform from one that does not, even with identical signatures, and that is
  /// exactly the difference a consumer deciding whether to trust it would want to see move.
  let private manifestText (p : Platform) : string =
    let typ (t : TypeReference) : string = string t

    let fnLine (name : FQFnName.Builtin, fn : BuiltInFn) : string =
      let ps = fn.parameters |> List.map (fun p -> typ p.typ) |> String.concat ","
      let tps = fn.typeParams |> String.concat ","
      let effects =
        fn.callEffects
        |> Set.toList
        |> List.map Effects.name
        |> List.sort
        |> String.concat ","
      $"fn {name.name}@{name.version}<{tps}>({ps}):{typ fn.returnType} [{effects}]"

    let valueLine (name : FQValueName.Builtin, v : BuiltInValue) : string =
      $"val {name.name}@{name.version}:{typ v.typ}"

    let dynamic =
      p.dynamicEffects
      |> Set.toList
      |> List.map Effects.name
      |> List.sort
      |> String.concat ","

    let requires = p.requires |> List.sort |> String.concat ","

    ([ $"platform {coordinate p} requires [{requires}] dynamic [{dynamic}] store {p.requiresStore}" ]
     @ (p.builtins.fns |> Dictionary.toSortedList |> List.map fnLine)
     @ (p.builtins.values |> Dictionary.toSortedList |> List.map valueLine))
    |> String.concat "\n"

  /// 16 hex characters of SHA-256 over this ONE platform's manifest.
  ///
  /// The set's fingerprint answers "is the whole floor the same". This answers "is THIS piece the
  /// same", which is the question a consumer of somebody else's platform has: they did not choose
  /// the rest of your binary and should not be told their dependency moved when it did not.
  ///
  /// Computed rather than stored: nothing is gained by a number a platform asserts about itself,
  /// and a stored one can lie.
  let fingerprint (p : Platform) : string =
    use sha = System.Security.Cryptography.SHA256.Create()
    sha.ComputeHash(System.Text.Encoding.UTF8.GetBytes(manifestText p))
    |> Array.take 8
    |> Array.map (fun b -> b.ToString "x2")
    |> String.concat ""


/// A chosen set of platforms, and the single `Builtins` an `ExecutionState` runs against.
///
/// Build one with `PlatformSet.make`. The combined `builtins` is computed once: it is read on every
/// builtin call, so it is a `Dictionary`, and rebuilding it per call site is what the old
/// three-hardcoded-lists arrangement did (the CLI combined the same libraries three times over).
type PlatformSet =
  {
    platforms : List<Platform>
    builtins : Builtins
    /// Deferred: hashing every signature in the set costs more than most CLI commands do, and only the reload
    /// gate, the approval check and `run-local-exec platforms` ever ask. Read it via `.fingerprint`.
    fingerprintLazy : Lazy<string>
  }

  member this.fingerprint : string = this.fingerprintLazy.Force()


module PlatformSet =

  /// The canonical text a fingerprint is taken over. One line per builtin, sorted, plus one line
  /// per platform coordinate.
  ///
  /// **What is in it:** the platform coordinates, and for every builtin its name, version, type
  /// parameters, parameter types, return type and effects. Those are the things a package compiled
  /// against this floor can observe.
  ///
  /// **What is deliberately not:** descriptions, parameter *names*, and `sqlSpec`. A doc fix must
  /// not invalidate every cached package and every capability approval on the machine; that is a
  /// re-review nobody reads, which is worse than no re-review at all. If a parameter rename ever
  /// becomes observable (named arguments), it moves into the fingerprint that day.
  ///
  /// **Every line names its owning platform**, and that is load-bearing rather than decorative. It
  /// is what lets the qualified form `Files#fileRead` stay a DERIVED name instead of a stored one:
  /// the concern with deriving it is that `fileRead` could silently come to mean a different
  /// platform's function, and with the owner in the fingerprint that move changes the fingerprint
  /// even when the signature is identical. Take the owner out and stored qualifiers become the only
  /// safe option, at the price of rehashing every package that calls a builtin.
  /// The manifest covers the COMBINED set, not the union of the members, so a builtin that only
  /// exists after `renames` synthesized it is still in there. Those have no declaring platform and
  /// are attributed to `renames`, which is the truth: the set as a whole made them, not any member.
  let private manifestText
    (platforms : List<Platform>)
    (combined : Builtins)
    : string =
    let typ (t : TypeReference) : string = string t

    let fnOwner = System.Collections.Generic.Dictionary<FQFnName.Builtin, string>()
    let valueOwner =
      System.Collections.Generic.Dictionary<FQValueName.Builtin, string>()
    platforms
    |> List.iter (fun p ->
      let coord = Platform.coordinate p
      p.builtins.fns.Keys |> Seq.iter (fun k -> fnOwner[k] <- coord)
      p.builtins.values.Keys |> Seq.iter (fun k -> valueOwner[k] <- coord))

    let ownerOfFn (k : FQFnName.Builtin) : string =
      match fnOwner.TryGetValue k with
      | true, owner -> owner
      | false, _ -> "renames"

    let ownerOfValue (k : FQValueName.Builtin) : string =
      match valueOwner.TryGetValue k with
      | true, owner -> owner
      | false, _ -> "renames"

    let fnLine (name : FQFnName.Builtin, fn : BuiltInFn) : string =
      let ps = fn.parameters |> List.map (fun p -> typ p.typ) |> String.concat ","
      let tps = fn.typeParams |> String.concat ","
      let effects =
        fn.callEffects
        |> Set.toList
        |> List.map Effects.name
        |> List.sort
        |> String.concat ","
      let owner = ownerOfFn name
      $"fn {owner}#{name.name}@{name.version}<{tps}>({ps}):{typ fn.returnType} [{effects}]"

    let valueLine (name : FQValueName.Builtin, v : BuiltInValue) : string =
      $"val {ownerOfValue name}#{name.name}@{name.version}:{typ v.typ}"

    ((combined.fns |> Dictionary.toSortedList |> List.map fnLine)
     @ (combined.values |> Dictionary.toSortedList |> List.map valueLine))
    |> String.concat "\n"


  /// 16 hex characters of SHA-256 over `manifestText`. Short enough to print in a status line,
  /// long enough that a collision is not a thing that happens.
  let private hashOf (text : string) : string =
    use sha = System.Security.Cryptography.SHA256.Create()
    sha.ComputeHash(System.Text.Encoding.UTF8.GetBytes text)
    |> Array.take 8
    |> Array.map (fun b -> b.ToString "x2")
    |> String.concat ""


  /// Is any builtin name claimed by more than one platform?
  ///
  /// Runs on every process start, so the answer path allocates two `HashSet`s of names and nothing
  /// else: no per-name string, no per-name list. Keying a `Dictionary` on an interpolated name
  /// costs a measurable fraction of a whole CLI command to answer "no" once per builtin.
  ///
  /// Naming WHICH names collided is the caller's problem, and `describeCollisions` does it at the
  /// cost of the allocation this avoids. That only runs when the process is about to raise anyway.
  let private hasCollision (platforms : List<Platform>) : bool =
    let fnSeen = System.Collections.Generic.HashSet<FQFnName.Builtin>()
    let valueSeen = System.Collections.Generic.HashSet<FQValueName.Builtin>()
    let mutable found = false
    for p in platforms do
      for k in p.builtins.fns.Keys do
        if not (fnSeen.Add k) then found <- true
      for k in p.builtins.values.Keys do
        if not (valueSeen.Add k) then found <- true
    found


  /// Every builtin name claimed by more than one platform, with the platforms that claim it.
  /// Only reached when `hasCollision` already said yes.
  let private describeCollisions
    (platforms : List<Platform>)
    : List<string * List<string>> =
    let claims = System.Collections.Generic.Dictionary<string, ResizeArray<string>>()
    let claim (key : string) (owner : string) =
      match claims.TryGetValue key with
      | true, owners -> owners.Add owner
      | false, _ ->
        let owners = ResizeArray<string>()
        owners.Add owner
        claims[key] <- owners
    platforms
    |> List.iter (fun p ->
      p.builtins.fns.Keys
      |> Seq.iter (fun k -> claim $"fn {k.name}@{k.version}" p.name)
      p.builtins.values.Keys
      |> Seq.iter (fun k -> claim $"val {k.name}@{k.version}" p.name))
    claims
    |> Dictionary.toSortedList
    |> List.choose (fun (key, owners) ->
      if owners.Count > 1 then Some(key, List.ofSeq owners) else None)


  /// Platform names a member requires that the set does not contain.
  let private missingRequirements
    (platforms : List<Platform>)
    : List<string * string> =
    let present = platforms |> List.map _.name |> Set.ofList
    platforms
    |> List.collect (fun p ->
      p.requires
      |> List.filter (fun r -> not (Set.contains r present))
      |> List.map (fun r -> (p.name, r)))


  /// Assemble a set. Raises on a name claimed by two platforms, or on a missing requirement.
  ///
  /// Both are internal errors rather than runtime errors on purpose: they are decided by which
  /// platforms this executable was built or configured with, not by anything a guest can reach, so
  /// the only useful moment to find out is startup.
  let make (platforms : List<Platform>) (renames : Builtin.FnRenames) : PlatformSet =
    if hasCollision platforms then
      let rendered =
        describeCollisions platforms
        |> List.map (fun (key, owners) ->
          let names = String.concat ", " owners
          $"{key} claimed by {names}")
        |> String.concat "; "
      Exception.raiseInternal
        "two platforms claim the same builtin name"
        [ "collisions", rendered ]

    match missingRequirements platforms with
    | [] -> ()
    | missing ->
      let rendered =
        missing |> List.map (fun (p, r) -> $"{p} requires {r}") |> String.concat "; "
      Exception.raiseInternal
        "a platform in this set is missing something it requires"
        [ "missing", rendered ]

    let combined = Builtin.combine (platforms |> List.map _.builtins) renames
    { platforms = platforms
      builtins = combined
      fingerprintLazy = lazy (hashOf (manifestText platforms combined)) }


  /// Does any platform in this set need a store on disk?
  let needsStore (set : PlatformSet) : bool =
    set.platforms |> List.exists _.requiresStore

  /// Is this platform in the set?
  let contains (name : string) (set : PlatformSet) : bool =
    set.platforms |> List.exists (fun p -> p.name = name)

  let tryFind (name : string) (set : PlatformSet) : Option<Platform> =
    set.platforms |> List.tryFind (fun p -> p.name = name)

  /// Everything the set's builtins may do: the union of the members' surfaces.
  let effectSurface (set : PlatformSet) : Set<Effects.Effect> =
    set.platforms
    |> List.fold (fun acc p -> Set.union acc (Platform.effectSurface p)) Set.empty

  /// `Core@0, Host@0, Net@0` — for a status line.
  let coordinates (set : PlatformSet) : string =
    set.platforms |> List.map Platform.coordinate |> List.sort |> String.concat ", "


  /// Which platform declares this builtin function, by bare name.
  ///
  /// `PlatformSet.make` has already refused a set where two platforms claim one name, so at most one
  /// answer exists. This is what makes `Files#fileRead` a name you can WRITE without storing the
  /// qualifier anywhere: the manifest resolves it, and the fingerprint notices if the answer moves.
  let ownerOf (fnName : string) (set : PlatformSet) : Option<Platform> =
    set.platforms
    |> List.tryFind (fun p ->
      p.builtins.fns.Keys |> Seq.exists (fun k -> k.name = fnName))

  /// `Files#fileRead`, or the bare name when nothing in the set declares it.
  let qualify (fnName : string) (set : PlatformSet) : string =
    match ownerOf fnName set with
    | Some p -> $"{p.name}#{fnName}"
    | None -> fnName

  /// Resolve a qualified name. `None` when the platform is absent from the set, or present and does
  /// not declare that function — the two cases a person writing `Files#fileRead` can get wrong, and
  /// the reason to resolve rather than to split the string and hope.
  let resolveQualified
    (qualified : string)
    (set : PlatformSet)
    : Option<FQFnName.Builtin> =
    match qualified.Split('#') with
    | [| platformName; fnName |] ->
      set.platforms
      |> List.tryFind (fun p -> p.name = platformName)
      |> Option.bind (fun p ->
        p.builtins.fns.Keys |> Seq.tryFind (fun k -> k.name = fnName))
    | _ -> None

  /// Which platforms introduce each effect, sorted.
  ///
  /// The install-time review ("adding Host adds file-read, file-write, process, native") read one
  /// way, and the dead-rule check read the other: a policy naming an effect no installed platform
  /// can produce is a rule that will never fire, which is usually a typo rather than caution.
  let effectOrigins (set : PlatformSet) : Map<Effects.Effect, List<string>> =
    set.platforms
    |> List.collect (fun p ->
      Platform.effectSurface p |> Set.toList |> List.map (fun e -> (e, p.name)))
    // Prelude's `List.groupBy` answers a `Map`, not F#'s list of pairs.
    |> List.groupBy fst
    |> Map.map (fun pairs -> pairs |> List.map snd |> List.sort)

  /// Effects named in a policy that nothing in this set can produce.
  let unreachableEffects
    (named : Set<Effects.Effect>)
    (set : PlatformSet)
    : List<Effects.Effect> =
    let reachable = effectSurface set
    named |> Set.toList |> List.filter (fun e -> not (Set.contains e reachable))


/// A platform whose builtins are DESCRIBED rather than written.
///
/// Every platform in this repo is a list of `BuiltInFn` records written by hand in F#, which is
/// fine while we are the only people shipping them. A platform that arrives as an artifact cannot
/// be that: nothing in this binary knows its function names or its signatures until it says so.
///
/// So it says so as data, and this turns that data into the same `Builtins` a hand-written platform
/// produces. Everything above stays identical, which is the point: `PlatformSet.make` composes it,
/// the fingerprint covers it, the interpreter dispatches to it, and the permission gate checks its
/// declared effects, all without knowing where it came from.
module External =

  /// One builtin, described the way a manifest would describe it.
  ///
  /// Parameter NAMES are for error messages; the TYPES are what a call site compiles against, and
  /// are why the description has to reach the runtime before the platform is ever run.
  type Fn =
    {
      name : string
      version : int
      parameters : List<string * TypeReference>
      returnType : TypeReference
      /// Everything a call may do, including `Effects.Effect.Custom` ones this binary has never
      /// heard of. The ambient gate checks these before the body runs, so an undeclared effect is
      /// not a loophole, it is a lie the platform told at install time.
      effects : Set<Effects.Effect>
      description : string
    }

  /// What actually performs the call: the builtin's INDEX in the platform's own list, and its
  /// already-evaluated arguments.
  ///
  /// An index rather than a name because that is what a wire protocol wants, and because it keeps
  /// this module free of any opinion about transport. A test passes a function; a shipped platform
  /// passes something that writes to a pipe.
  type Invoke = int -> List<Dval> -> Ply<Dval>

  /// Describe-to-`Builtins`, pairing each description with its index.
  ///
  /// `previewable` is `Impure` for all of them, unconditionally. A described builtin cannot be
  /// shown to be pure: purity is a claim about a body we cannot see, and guessing generously here
  /// would let an analysis preview something with side effects.
  ///
  /// `sqlSpec` is `NotQueryable` for the same reason.
  let builtins (invoke : Invoke) (fns : List<Fn>) : Builtins =
    fns
    |> List.mapi (fun index (fn : Fn) ->
      { name = FQFnName.builtin fn.name fn.version
        typeParams = []
        parameters =
          fn.parameters
          |> List.map (fun (name, typ) -> BuiltInParam.make name typ "")
        returnType = fn.returnType
        description = fn.description
        previewable = Impure
        deprecated = NotDeprecated
        sqlSpec = NotQueryable
        callEffects = fn.effects
        fn =
          (function
          | _, _, _, args -> invoke index (List.ofArray args)) })
    |> Builtin.make []


  /// A whole platform, described. What arrives beside an artifact.
  ///
  /// Deliberately close to `Platform` itself, minus the one thing that cannot travel: the builtin
  /// implementations. Everything else a `PlatformSet` needs to compose, fingerprint and review is
  /// here, which is what lets the runtime decide whether to accept a platform before it has run a
  /// line of its code.
  type Manifest =
    {
      owner : string
      name : string
      version : int
      description : string
      requires : List<string>
      requiresStore : bool
      fns : List<Fn>
    }

  /// Why a manifest was refused. Plural, because a person fixing one wants every problem at once
  /// rather than one per attempt.
  type Rejection = { manifest : string; problems : List<string> }

  module Manifest =
    /// `owner/name`, the coordinate a consumer pins.
    let coordinate (m : Manifest) : string = $"{m.owner}/{m.name}@{m.version}"

    /// Can a value of this type cross a pipe?
    ///
    /// Recursive on purpose. A bare `TFn` parameter is the obvious case, but `List<Int -> Int>`
    /// and a dict of them cannot travel either, and finding that out when somebody finally passes
    /// a lambda is much worse than finding it out at install.
    ///
    /// `TDB` and `TStream` are refused for the same reason in different words: both are handles
    /// into state this runtime owns, and their meaning does not survive leaving the process.
    let rec private travels (typ : TypeReference) : bool =
      match typ with
      | TFn _ -> false
      | TDB _ -> false
      | TStream _ -> false
      | TList inner -> travels inner
      | TDict(key, value) -> travels key && travels value
      | TTuple(a, b, rest) -> travels a && travels b && List.forall travels rest
      | TCustomType(_, typeArgs) -> List.forall travels typeArgs
      | _ -> true

    let private nameShape =
      System.Text.RegularExpressions.Regex(
        @"^[a-zA-Z][a-zA-Z0-9_]*$",
        System.Text.RegularExpressions.RegexOptions.Compiled
      )

    /// Every problem with a manifest, or an empty list.
    ///
    /// Checks the SHAPE, not the truth. That the platform can do what it claims is not knowable
    /// from here and never will be; what is knowable is that the claim is well formed, that the
    /// signatures can cross, and that the effects it names are effects.
    let problems (m : Manifest) : List<string> =
      let platformNames =
        [ if not (nameShape.IsMatch m.name) then
            $"platform name '{m.name}' is not a plain identifier"
          if not (nameShape.IsMatch m.owner) then
            $"owner '{m.owner}' is not a plain identifier"
          if m.version < 0 then $"version {m.version} is negative" ]

      let duplicates =
        m.fns
        |> List.countBy (fun fn -> (fn.name, fn.version))
        |> List.filter (fun (_, count) -> count > 1)
        |> List.map (fun ((name, version), _) ->
          $"builtin '{name}@{version}' is declared more than once")

      let perFn =
        m.fns
        |> List.collect (fun fn ->
          [ if not (nameShape.IsMatch fn.name) then
              $"builtin name '{fn.name}' is not a plain identifier"
            if fn.version < 0 then $"builtin '{fn.name}' has a negative version"
            for (paramName, typ) in fn.parameters do
              if not (travels typ) then
                $"builtin '{fn.name}' takes '{paramName}' of a type that cannot cross a process boundary"
            if not (travels fn.returnType) then
              $"builtin '{fn.name}' returns a type that cannot cross a process boundary" ])

      platformNames @ duplicates @ perFn

    /// Turn a manifest into a platform, or say why not.
    ///
    /// This is the whole reason a manifest is data. A call site compiles against a signature, so
    /// the runtime has to know the signature; it cannot ask a process it has not started, and it
    /// should not start one it has not checked.
    ///
    /// `dynamicEffects` is empty and there is no field for it. Those are effects a builtin requests
    /// from inside its own body, which means from inside code this runtime is running. A platform
    /// on the other side of a pipe has no such path: everything it can do is in `fns`.
    let toPlatform (invoke : Invoke) (m : Manifest) : Result<Platform, Rejection> =
      match problems m with
      | [] ->
        Ok
          { name = m.name
            version = m.version
            description = m.description
            builtins = builtins invoke m.fns
            requires = m.requires
            dynamicEffects = Set.empty
            requiresStore = m.requiresStore }
      | problems -> Error { manifest = coordinate m; problems = problems }


  /// A type as a manifest writes it: by NAME, never by hash.
  ///
  /// `TypeReference` cannot be what a manifest carries. Its `TCustomType` holds an `FQTypeName`,
  /// and an `FQTypeName` is a content hash, so `Result<String, String>` would travel as the hash of
  /// `Stdlib.Result` in the store that wrote it. A platform author cannot know that hash, it moves
  /// whenever the type does, and a manifest carrying one is pinned to one corpus.
  ///
  /// So a manifest says `Stdlib.Result<String, String>` and the CONSUMER resolves it. A name that
  /// does not resolve is a manifest problem, and a useful one: this platform wants a type you do
  /// not have.
  type NamedType =
    | NBuiltin of string
    | NList of NamedType
    | NDict of NamedType * NamedType
    | NTuple of List<NamedType>
    | NCustom of name : string * args : List<NamedType>

  module NamedType =
    /// The types a manifest may name without qualification. Deliberately not every
    /// `TypeReference`: what is missing is what cannot cross a pipe, which is the same rule 73
    /// applies to a whole signature, stated here as a grammar rather than as a check.
    let private scalars : Map<string, TypeReference> =
      Map
        [ "Unit", TUnit
          "Bool", TBool
          "Int8", TInt8
          "UInt8", TUInt8
          "Int16", TInt16
          "UInt16", TUInt16
          "Int32", TInt32
          "UInt32", TUInt32
          "Int64", TInt64
          "UInt64", TUInt64
          "Int128", TInt128
          "UInt128", TUInt128
          "Int", TInt
          "Float", TFloat
          "Char", TChar
          "String", TString
          "Uuid", TUuid
          "DateTime", TDateTime
          "Blob", TBlob ]

    /// Parse `Stdlib.Result<String, List<Int64>>` and friends.
    ///
    /// Hand-written rather than reusing `LibParser`, because that one is Dark and needs a store to
    /// run, and this has to work while deciding whether to accept a manifest at all. The grammar is
    /// small enough that the cost is a few dozen lines and the benefit is no dependency.
    let parse (input : string) : Result<NamedType, string> =
      let mutable pos = 0
      let text = input.Trim()

      let peek () = if pos < text.Length then Some text[pos] else None
      let skipSpace () =
        while pos < text.Length && text[pos] = ' ' do
          pos <- pos + 1

      let ident () =
        skipSpace ()
        let start = pos
        while pos < text.Length
              && (System.Char.IsLetterOrDigit text[pos] || text[pos] = '.' || text[pos] = '_') do
          pos <- pos + 1
        text.Substring(start, pos - start)

      let rec typ () : Result<NamedType, string> =
        let name = ident ()
        if name = "" then
          Error $"expected a type name at offset {pos} of '{text}'"
        else
          skipSpace ()
          match peek () with
          | Some '<' ->
            pos <- pos + 1
            match args [] with
            | Error e -> Error e
            | Ok args ->
              match name, args with
              | "List", [ inner ] -> Ok(NList inner)
              | "List", _ -> Error "List takes exactly one type argument"
              | "Dict", [ k; v ] -> Ok(NDict(k, v))
              | "Dict", _ -> Error "Dict takes exactly two type arguments"
              | "Tuple", (_ :: _ :: _) -> Ok(NTuple args)
              | "Tuple", _ -> Error "Tuple takes at least two type arguments"
              | _, _ -> Ok(NCustom(name, args))
          | _ ->
            if Map.containsKey name scalars then Ok(NBuiltin name)
            else Ok(NCustom(name, []))

      and args (acc : List<NamedType>) : Result<List<NamedType>, string> =
        match typ () with
        | Error e -> Error e
        | Ok one ->
          skipSpace ()
          match peek () with
          | Some ',' ->
            pos <- pos + 1
            args (acc @ [ one ])
          | Some '>' ->
            pos <- pos + 1
            Ok(acc @ [ one ])
          | _ -> Error $"expected ',' or '>' at offset {pos} of '{text}'"

      match typ () with
      | Error e -> Error e
      | Ok parsed ->
        skipSpace ()
        if pos < text.Length then
          Error $"unexpected '{text.Substring pos}' after a complete type in '{text}'"
        else
          Ok parsed

    /// Render back, so a round trip is checkable and `dark platforms` can show what was declared.
    let rec render (t : NamedType) : string =
      match t with
      | NBuiltin name -> name
      | NList inner -> $"List<{render inner}>"
      | NDict(k, v) -> $"Dict<{render k}, {render v}>"
      | NTuple items ->
        let rendered = items |> List.map render |> String.concat ", "
        $"Tuple<{rendered}>"
      | NCustom(name, []) -> name
      | NCustom(name, args) ->
        let rendered = args |> List.map render |> String.concat ", "
        $"{name}<{rendered}>"

    /// Resolve names to a `TypeReference` against the consumer's store.
    ///
    /// `lookup` answers what a package type name resolves to here, and `None` is a real answer: the
    /// platform wants a type this instance does not have, which is worth saying plainly rather than
    /// failing later at a call site.
    let rec resolve
      (lookup : string -> Option<FQTypeName.FQTypeName>)
      (t : NamedType)
      : Result<TypeReference, string> =
      let resolveAll ts =
        ts
        |> List.fold
          (fun acc item ->
            match acc, resolve lookup item with
            | Error e, _ -> Error e
            | _, Error e -> Error e
            | Ok sofar, Ok r -> Ok(sofar @ [ r ]))
          (Ok [])

      match t with
      | NBuiltin name ->
        match Map.tryFind name scalars with
        | Some typ -> Ok typ
        | None -> Error $"'{name}' is not a builtin type"
      | NList inner -> resolve lookup inner |> Result.map TList
      | NDict(k, v) ->
        match resolve lookup k, resolve lookup v with
        | Ok k, Ok v -> Ok(TDict(k, v))
        | Error e, _ -> Error e
        | _, Error e -> Error e
      | NTuple items ->
        match resolveAll items with
        | Error e -> Error e
        | Ok(a :: b :: rest) -> Ok(TTuple(a, b, rest))
        | Ok _ -> Error "a tuple needs at least two elements"
      | NCustom(name, args) ->
        match lookup name with
        | None -> Error $"no type named '{name}' in this instance"
        | Some fq ->
          resolveAll args
          |> Result.map (fun args -> TCustomType(NameResolution.ok fq, args))



/// A manifest as WRITTEN, before anything is resolved.
///
/// Same split the parser already makes between `WrittenTypes` and `ProgramTypes`, and for the
/// same reason: what a person typed and what it means here are two things, and resolution can
/// fail. A `Written.Manifest` names types and effects; a `Manifest` holds the resolved ones.
module Written =
  type Fn =
    {
      name : string
      version : int
      parameters : List<string * External.NamedType>
      returnType : External.NamedType
      /// Effect NAMES, well-known or `owner/name`. Resolved through `Effects.fromName`, so a
      /// platform declaring a capability this runtime never shipped is ordinary rather than
      /// special.
      effects : List<string>
      description : string
    }

  type Manifest =
    {
      owner : string
      name : string
      version : int
      description : string
      requires : List<string>
      requiresStore : bool
      fns : List<Fn>
    }

  let private header = "DARK-PLATFORM-MANIFEST 1"

  /// Line-oriented, like the activation file, and for the same reason: a plugin author writes
  /// this by hand or emits it from C with `fprintf`. A format needing a library to produce is a
  /// format that makes the first platform in a new language a project rather than an afternoon.
  ///
  /// `key rest-of-line`, blank lines and `#` comments ignored. A `fn` line opens a function and
  /// the `param`, `returns`, `effect` and `doc` lines after it belong to that one.
  let render (m : Manifest) : string =
    let lines =
      [ yield header
        yield $"owner {m.owner}"
        yield $"name {m.name}"
        yield $"version {m.version}"
        if m.description <> "" then
          yield $"description {m.description}"
        for r in m.requires do
          yield $"requires {r}"
        yield "store " + (if m.requiresStore then "yes" else "no")
        for fn in m.fns do
          yield ""
          yield $"fn {fn.name} {fn.version}"
          for (paramName, typ) in fn.parameters do
            yield $"param {paramName} {External.NamedType.render typ}"
          yield $"returns {External.NamedType.render fn.returnType}"
          for e in fn.effects do
            yield $"effect {e}"
          if fn.description <> "" then
            yield $"doc {fn.description}" ]
    String.concat "\n" lines + "\n"

  /// Parse, collecting every problem rather than stopping at the first.
  ///
  /// Unknown keys are an error rather than ignored. A manifest is a contract, and silently
  /// dropping a line somebody wrote is how a platform ends up doing less than it says.
  let parse (text : string) : Result<Manifest, List<string>> =
    let problems = ResizeArray<string>()

    let lines =
      text.Split('\n')
      |> Array.toList
      |> List.map (fun line -> line.Trim())
      |> List.indexed
      |> List.filter (fun (_, line) -> line <> "" && not (line.StartsWith "#"))

    let mutable owner = ""
    let mutable name = ""
    let mutable version = 0
    let mutable description = ""
    let requires = ResizeArray<string>()
    let mutable requiresStore = false
    let fns = ResizeArray<Fn>()

    let split (line : string) =
      match line.IndexOf ' ' with
      | -1 -> line, ""
      | i -> line.Substring(0, i), line.Substring(i + 1).Trim()

    let namedType (lineNo : int) (raw : string) : Option<External.NamedType> =
      match External.NamedType.parse raw with
      | Ok t -> Some t
      | Error e ->
        problems.Add $"line {lineNo + 1}: {e}"
        None

    match lines with
    | [] -> Error [ "the manifest is empty" ]
    | (headerLine, first) :: rest ->
      if first <> header then
        Error
          [ $"line {headerLine + 1}: expected '{header}', found '{first}'" ]
      else
        for (lineNo, line) in rest do
          let key, rest = split line
          // A `fn` line opens a function; everything below attaches to the last one opened, so
          // a `param` before any `fn` is a manifest that got its order wrong.
          let onCurrentFn (f : Fn -> Fn) =
            if fns.Count = 0 then
              problems.Add $"line {lineNo + 1}: '{key}' before any 'fn' line"
            else
              fns[fns.Count - 1] <- f fns[fns.Count - 1]

          match key with
          | "owner" -> owner <- rest
          | "name" -> name <- rest
          | "description" -> description <- rest
          | "requires" -> requires.Add rest
          | "version" ->
            match System.Int32.TryParse rest with
            | true, v -> version <- v
            | false, _ -> problems.Add $"line {lineNo + 1}: version '{rest}' is not a number"
          | "store" ->
            match rest with
            | "yes" -> requiresStore <- true
            | "no" -> requiresStore <- false
            | other -> problems.Add $"line {lineNo + 1}: store must be yes or no, not '{other}'"
          | "fn" ->
            let fnName, fnVersion = split rest
            match System.Int32.TryParse fnVersion with
            | true, v ->
              fns.Add
                { name = fnName
                  version = v
                  parameters = []
                  returnType = External.NBuiltin "Unit"
                  effects = []
                  description = "" }
            | false, _ ->
              problems.Add
                $"line {lineNo + 1}: 'fn' wants a name and a version, found '{rest}'"
          | "param" ->
            let paramName, typeText = split rest
            match namedType lineNo typeText with
            | Some t ->
              onCurrentFn (fun fn ->
                { fn with parameters = fn.parameters @ [ (paramName, t) ] })
            | None -> ()
          | "returns" ->
            match namedType lineNo rest with
            | Some t -> onCurrentFn (fun fn -> { fn with returnType = t })
            | None -> ()
          | "effect" -> onCurrentFn (fun fn -> { fn with effects = fn.effects @ [ rest ] })
          | "doc" -> onCurrentFn (fun fn -> { fn with description = rest })
          | other -> problems.Add $"line {lineNo + 1}: unknown key '{other}'"

        if problems.Count > 0 then
          Error(List.ofSeq problems)
        else
          Ok
            { owner = owner
              name = name
              version = version
              description = description
              requires = List.ofSeq requires
              requiresStore = requiresStore
              fns = List.ofSeq fns }

  /// Resolve a written manifest against this instance: type names to hashes, effect names to
  /// effects. Then `Manifest.problems` has the last word on whether it is acceptable.
  let resolve
    (lookup : string -> Option<FQTypeName.FQTypeName>)
    (written : Manifest)
    : Result<External.Manifest, External.Rejection> =
    let problems = ResizeArray<string>()

    let resolveType (context : string) (t : External.NamedType) : TypeReference =
      match External.NamedType.resolve lookup t with
      | Ok typ -> typ
      | Error e ->
        problems.Add $"{context}: {e}"
        TUnit

    let fns =
      written.fns
      |> List.map (fun fn ->
        let effects =
          fn.effects
          |> List.choose (fun name ->
            match Effects.fromName name with
            | Some effect -> Some effect
            | None ->
              problems.Add $"builtin '{fn.name}': '{name}' is not an effect"
              None)
          |> Set.ofList

        let resolved : External.Fn =
          { name = fn.name
            version = fn.version
            parameters =
              fn.parameters
              |> List.map (fun (paramName, t) ->
                (paramName, resolveType $"builtin '{fn.name}' parameter '{paramName}'" t))
            returnType = resolveType $"builtin '{fn.name}' return type" fn.returnType
            effects = effects
            description = fn.description }
        resolved)

    let manifest : External.Manifest =
      { owner = written.owner
        name = written.name
        version = written.version
        description = written.description
        requires = written.requires
        requiresStore = written.requiresStore
        fns = fns }

    let all = List.ofSeq problems @ External.Manifest.problems manifest
    if List.isEmpty all then
      Ok manifest
    else
      Error { manifest = External.Manifest.coordinate manifest; problems = all }
