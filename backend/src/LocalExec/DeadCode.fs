/// Dead-code finder for `.dark` package items.
///
/// Walks the package_dependencies table to find items that nothing
/// else depends on, then filters out a hardcoded roots set (entry
/// points the F# CLI / tests / serve loop reach into directly).
///
/// Output is grouped by item_type with FQ names. Nothing is deleted;
/// the script just reports.
///
/// Run with: ./scripts/run-local-exec find-dead [--main-branch-only]
module LocalExec.DeadCode

open System.Threading.Tasks
open FSharp.Control.Tasks

open Fumble
open LibDB.Sqlite

open Prelude

module PT = LibExecution.ProgramTypes


/// Roots: FQ names ("Owner.Module1.Module2.name") that count as
/// "reachable from outside the package graph" — entry points the F#
/// runtime calls into directly. Anything reachable from these is
/// alive transitively; anything NOT reachable is a candidate.
///
/// Two sources:
///   1. `LibExecution.PackageRefs.{Type,Fn,Value}._lookup` — the
///      registry of F# direct references. Auto-populated when
///      PackageRefs's `p` helper is called at module init. We
///      enumerate it here.
///   2. A hand-curated list for things the F# code reaches by
///      string-name rather than via PackageRefs (rare, mostly for
///      `darklang serve <fn-path>` arguments — but those are
///      caller-supplied, not part of the dead-code analysis here).
let private collectRootsFromPackageRefs () : Set<string> =
  let collect (kind : string) (lookup : Map<string list * string, string>) =
    lookup
    |> Map.toSeq
    |> Seq.map (fun ((modules, name), _hash) ->
      // PackageRefs registers names as (modules, name) where modules
      // are the dotted segments AFTER the implicit "Stdlib" or
      // "Internal" or "Cli" or "LanguageTools" root. We don't know
      // the owner here — the locations table records `Darklang` for
      // most things — so we match on (modules, name) only.
      ignore<string> kind
      let modulesStr = String.concat "." modules
      $"{modulesStr}.{name}")
    |> Set.ofSeq

  Set.unionMany
    [ collect "type" LibExecution.PackageRefs.Type._lookup
      collect "fn" LibExecution.PackageRefs.Fn._lookup ]


/// Exact-match additions. Most "real" entry points should already
/// be in PackageRefs, but if you find something the analysis flags
/// as dead that isn't, add it here.
let private extraRoots : Set<string> =
  Set.ofList
    [ // CLI top-level dispatch (called by name from Cli.fs)
      "Cli.executeCliCommand"
      // Parser entry called from CliHost
      "LanguageTools.Parser.CliScript.parseForCli" ]


/// Prefix whitelist — modules whose contents are entry points, public
/// APIs, or intentional scaffolding. An item is whitelisted if its
/// matchKey (modules + name) starts with any of these prefixes.
///
/// The general rule: things end up here because something OUTSIDE the
/// `.dark` package graph reaches them — a user typing a CLI command,
/// a JSON-RPC peer sending a message, a future user calling a public
/// API, or a script being run manually. The dependency table only
/// captures `.dark` → `.dark` calls, so anything reached from outside
/// looks "dead" from the table alone.
let private prefixWhitelist : List<string> =
  [ // Stdlib is the public API for user code. Even fns with no
    // current internal callers are intentionally part of the surface.
    "Stdlib."

    // CLI command modules — invoked by name from user input via the
    // Cli dispatch. Subcommand fns aren't called by other .dark code,
    // they're called by the runtime command router.
    "Cli."

    // CLI UI library — components meant to be used by future CLI
    // views, currently scaffolded but not all wired up.
    "CLI.UI."

    // Work-in-progress scratch namespace, intentionally unreached.
    "WIP."

    // Demo data and scripts — entry points run manually.
    "DemoData."
    "Test.Values."
    "LLM.Examples."
    "LLM.Demos."

    // Pretty-printer module — F# code reaches in by name from
    // LibExecution but only some entries are in PackageRefs.
    "PrettyPrinter."

    // Language tools — LSP server, parser, semantic tokens, etc.
    // Reached via wire protocol, not by other .dark code.
    "LanguageTools.LspServer."
    "LanguageTools.LanguageServerProtocol."
    "LanguageTools.SemanticTokens."
    "LanguageTools.Parser."
    "LanguageTools.PackageManager."
    "LanguageTools.WrittenTypesToProgramTypes."

    // Standalone protocol/integration namespaces — top-level entries
    // reached by external clients, not internal callers.
    "LanguageServerProtocol."
    "LspExtension."
    "ModelContextProtocol."
    "JsonRPC."
    "Discord."

    // SCM (source control) public API — exposed to user scripts;
    // see for-ai.dark docs that reference Darklang.SCM.Branch.*.
    "SCM."

    // VSCode integration — exposed via the LSP extension.
    "VSCode."

    // Internal MCP server — entry points called by MCP clients.
    "Internal.DarklangInternalMcpServer."

    // LLM library top-level — agent/model/provider modules form the
    // public API; users instantiate these from scripts.
    "LLM." ]


let private isWhitelisted (matchKey : string) : bool =
  prefixWhitelist |> List.exists (fun p -> matchKey.StartsWith p)


type DeadItem =
  { hash : string
    itemType : string // "fn" / "type" / "value"
    owner : string
    modules : string
    name : string }

  member this.fqName : string =
    let mods = if this.modules = "" then "" else this.modules + "."
    $"{this.owner}.{mods}{this.name}"

  /// The form roots are matched against (no owner prefix).
  member this.matchKey : string =
    let mods = if this.modules = "" then "" else this.modules + "."
    $"{mods}{this.name}"


/// Find items with zero incoming `depends_on_hash` references.
/// Excludes items whose name matches a known root.
let findDead (branchId : PT.BranchId) : Task<List<DeadItem>> =
  task {
    // SELECT all items on this branch that have NO row in
    // package_dependencies pointing to their hash. Branch-scope via
    // the locations table; package_dependencies is global by hash.
    let! candidates =
      Sql.query
        """
        SELECT l.item_hash, l.item_type, l.owner, l.modules, l.name
        FROM locations l
        WHERE l.branch_id = @branchId
          AND l.unlisted_at IS NULL
          AND NOT EXISTS (
            SELECT 1 FROM package_dependencies pd
            WHERE pd.depends_on_hash = l.item_hash
          )
        ORDER BY l.item_type, l.owner, l.modules, l.name
        """
      |> Sql.parameters [ "branchId", Sql.string (string branchId) ]
      |> Sql.executeAsync (fun read ->
        { hash = read.string "item_hash"
          itemType = read.string "item_type"
          owner = read.string "owner"
          modules = read.string "modules"
          name = read.string "name" })

    let roots = Set.union (collectRootsFromPackageRefs ()) extraRoots

    return
      candidates
      |> List.filter (fun c ->
        not (Set.contains c.matchKey roots) && not (isWhitelisted c.matchKey))
  }


let report () : Ply<Result<unit, string>> =
  uply {
    let! dead = findDead PT.mainBranchId

    let byKind : List<string * List<DeadItem>> =
      dead
      |> Microsoft.FSharp.Collections.List.groupBy (fun (d : DeadItem) -> d.itemType)
      |> Microsoft.FSharp.Collections.List.sortBy fst

    print ""
    print $"Dead-code candidates ({List.length dead}):"
    print ""

    for (kind, items) in byKind do
      print $"## {kind} ({List.length items})"
      print ""
      for (item : DeadItem) in items do
        print $"  {item.fqName}"
      print ""

    print "Roots considered:"
    let rootCount = Set.count (collectRootsFromPackageRefs ()) + Set.count extraRoots
    print
      $"  {rootCount} exact ({Set.count extraRoots} hand-curated, rest from PackageRefs)"
    print $"  {List.length prefixWhitelist} prefix whitelist entries"
    print ""

    return Ok()
  }
