/// Where the CLI can stop with a runtime error: every call from the CLI's own code to a builtin
/// that declares `Raise`, or to a stdlib function that reaches one.
///
/// The stdlib keeps its raising functions for users, so a call into it is where the CLI chooses
/// to raise; a call between two CLI functions is not a site, the raise is inside the callee.
module LibDB.RaiseSites

open Prelude

module PT = LibExecution.ProgramTypes
module Effects = LibExecution.Effects
module Requirements = LibExecution.CallGraph.Requirements

/// One call, by name: `caller` is the CLI's, `callee` a raising builtin or stdlib function.
type Site = { caller : string; callee : string }

/// Where the walk starts: what a person running `dark` can reach.
let entryPoints : List<string> =
  [ "Darklang.Cli.executeCliCommand"
    "Darklang.Cli.Registry.allCommands"
    "Darklang.Cli.initState"
    "Darklang.Cli.Workbench.execute" ]

/// The library users call too. Everything else the CLI reaches ships as part of the CLI.
let isLibrary (name : string) : bool = name.StartsWith "Darklang.Stdlib."

/// The CLI's fns named in a string in its source and resolved at run time, such as a live view's
/// `init = "Darklang.Cli.TraceWatch.init"`. No body references them, so they are roots too.
let private namedInStrings (sourceRoot : string) : List<string> =
  let re = System.Text.RegularExpressions.Regex "\"(Darklang(?:\\.[A-Za-z0-9_]+)+)\""
  [ "packages/darklang/cli"; "packages/darklang/languageTools" ]
  |> List.map (fun dir -> System.IO.Path.Combine(sourceRoot, dir))
  |> List.filter System.IO.Directory.Exists
  |> List.collect (fun dir ->
    System.IO.Directory.EnumerateFiles(
      dir,
      "*.dark",
      System.IO.SearchOption.AllDirectories
    )
    |> List.ofSeq)
  |> List.collect (fun f ->
    re.Matches(System.IO.File.ReadAllText f)
    |> Seq.map (fun m -> m.Groups[1].Value)
    |> List.ofSeq)
  |> List.distinct

/// The package fns reachable from the entry points, each body analyzed once.
let closure
  (sourceRoot : string)
  (names : Map<PT.Hash, string>)
  (load : PackagePermissions.Load)
  : Ply<Requirements.Closure> =
  let byName = names |> Map.toList |> List.map (fun (h, n) -> (n, h)) |> Map.ofList
  entryPoints @ namedInStrings sourceRoot
  |> List.choose (fun n -> Map.tryFind n byName)
  |> List.distinct
  |> Ply.List.foldSequentially
    (fun loaded root ->
      if Map.containsKey root loaded then
        Ply loaded
      else
        PackagePermissions.loadClosure load root
        |> Ply.map (fun more ->
          Map.fold (fun acc k v -> Map.add k v acc) loaded more))
    Map.empty

/// What the analysis found: each raising call, and for each callee, the raising builtins it
/// reaches, which say why it is on the list.
type Found = { sites : List<Site>; reasons : Map<string, Set<string>> }

let find
  (callEffectsFor : PackagePermissions.CallEffectsFor)
  (names : Map<PT.Hash, string>)
  (closure : Requirements.Closure)
  : Found =
  let nameOf (h : PT.Hash) =
    match Map.tryFind h names with
    | Some n -> n
    | None ->
      let (PT.Hash raw) = h
      raw

  let builtinRaises (b : PT.FQFnName.Builtin) =
    callEffectsFor (b.name, b.version)
    |> Option.exists (Set.contains Effects.Effect.Raise)

  // each fn's calls that can raise, less the ones a literal divisor rules out
  let calls =
    closure |> Map.map (fun (fn, analysis) -> Raises.calls nameOf fn analysis)

  let direct (names : List<PT.FQFnName.FQFnName>) : Set<string> =
    names
    |> List.choose (function
      | PT.FQFnName.Builtin b when builtinRaises b -> Some $"Builtin.{b.name}"
      | _ -> None)
    |> Set.ofList

  let callees (names : List<PT.FQFnName.FQFnName>) : List<PT.Hash> =
    names
    |> List.choose (function
      | PT.FQFnName.Package p -> Some p
      | _ -> None)

  // Each fn's raising builtins, own and reached, settled by iterating until nothing grows.
  let rec settle (reached : Map<PT.Hash, Set<string>>) : Map<PT.Hash, Set<string>> =
    let next =
      reached
      |> Map.mapWithIndex (fun h own ->
        if Set.contains (nameOf h) Raises.cannotRaise then
          Set.empty
        else
          match Map.tryFind h calls with
          | None -> own
          | Some names ->
            callees names
            |> List.choose (fun c -> Map.tryFind c reached)
            |> List.fold Set.union own)
    let size (m : Map<PT.Hash, Set<string>>) =
      m |> Map.values |> List.sumBy Set.count
    if size next = size reached then reached else settle next

  let reached =
    calls
    |> Map.mapWithIndex (fun h names ->
      if Set.contains (nameOf h) Raises.cannotRaise then Set.empty else direct names)
    |> settle

  let raises (h : PT.Hash) =
    Map.tryFind h reached |> Option.exists (fun s -> not (Set.isEmpty s))

  let sites =
    calls
    |> Map.toList
    |> List.collect (fun (h, names) ->
      let caller = nameOf h
      if isLibrary caller then
        []
      else
        names
        |> List.choose (function
          | PT.FQFnName.Builtin b when builtinRaises b ->
            Some { caller = caller; callee = $"Builtin.{b.name}" }
          | PT.FQFnName.Package p when raises p && isLibrary (nameOf p) ->
            Some { caller = caller; callee = nameOf p }
          | _ -> None))

  let reasons =
    closure
    |> Map.toList
    |> List.choose (fun (h, _) ->
      let name = nameOf h
      if isLibrary name && raises h then
        Some(name, Map.tryFind h reached |> Option.defaultValue Set.empty)
      else
        None)
    |> Map.ofList

  { sites = sites; reasons = reasons }

/// The baseline's lines: `<count> <caller> <callee>`, sorted, one per caller and callee.
let baselineLines (sites : List<Site>) : List<string> =
  sites
  |> List.countBy identity
  |> List.map (fun (s, count) -> $"{count} {s.caller} {s.callee}")
  |> List.sort
