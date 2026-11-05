/// Native dependency-reader regressions. Each case gets a private copy of the store.
module Tests.ReferenceNames

open System.Threading.Tasks
open FSharp.Control.Tasks
open Expecto
open Fumble
open LibDB.Sqlite
open TestUtils.TestUtils

module PT = LibExecution.ProgramTypes
module ReferenceDependencies = LibDB.ReferenceDependencies

let private location moduleName name : PT.PackageLocation =
  { owner = "Tests"; modules = [ moduleName ]; name = name }

let private source moduleName name =
  task {
    let location = location moduleName name
    let! hash = pmPT.findFn location |> Ply.toTask
    let hash =
      hash
      |> Option.defaultWith (fun () -> failtest $"missing fixture function {name}")
    return location, PT.PackageFn hash
  }

// Switching LibDB's store is process-wide, so these tests run sequentially.
let private withStore (run : unit -> Task<unit>) =
  task {
    let directory = System.IO.Directory.CreateTempSubdirectory("reference-names-")
    let original = LibDB.Sqlite.currentDbPath
    let path = System.IO.Path.Combine(directory.FullName, "data.db")
    match LibDB.Sqlite.Backup.toTestBaseline path with
    | Error message -> failtest message
    | Ok() -> ()
    try
      Sql.useStoreForTesting path
      LibDB.Caching.invalidateAll ()
      do! run ()
    finally
      Sql.useStoreForTesting original
      LibDB.Caching.invalidateAll ()
      try
        directory.Delete true
      with _ ->
        ()
  }

let wideDependencyReadsRespectSelection =
  testTask "wide dependency reads respect selected callers" {
    do!
      withStore (fun () ->
        task {
          let moduleName = "ReferenceBatches"
          let names = [ 1..105 ] |> List.map (fun i -> $"caller{i}")
          let declarations =
            [ "let leaf(): Int64 = 932102L"
              for i in 1..105 do
                $"let caller{i}(): Int64 = Tests.{moduleName}.leaf() + {i}L" ]
            |> String.concat "\n"
          let! _ = authorIntoMain $"module Tests.{moduleName}\n{declarations}"
          let! sources = Task.WhenAll(names |> List.map (source moduleName))
          let sources = Array.toList sources
          let leaf = location moduleName "leaf"
          let! result =
            ReferenceDependencies.namedReferences PT.BranchId.Main sources
          let edges =
            match result with
            | Ok edges -> edges
            | Error message -> failtest message
          let callers =
            edges
            |> List.choose (fun (caller, _, _, dependency) ->
              if dependency = leaf then Some caller.name else None)
          Expect.equal
            (List.sort callers)
            (List.sort names)
            "no caller is missing or duplicated"
          let! selected =
            ReferenceDependencies.namedReferences PT.BranchId.Main [ sources[0] ]
          let selected =
            match selected with
            | Ok edges -> edges
            | Error message -> failtest message
          let callers =
            selected
            |> List.choose (fun (caller, _, _, dependency) ->
              if dependency = leaf then Some caller else None)
          Expect.equal
            callers
            [ fst sources[0] ]
            "a selected read includes only the requested caller"
        })
  }

let savedNamesNeedTheStoredDeclaration =
  testTask "saved reference names require the stored declaration body" {
    do!
      withStore (fun () ->
        task {
          let moduleName = "MissingReferenceBody"
          let! ops =
            authorIntoMain
              $"module Tests.{moduleName}\nlet leaf(): Int64 = 932103L\nlet caller(): Int64 = leaf()"
          let hash = hashBoundTo ops "caller"
          let caller = location moduleName "caller", PT.PackageFn hash
          let! stored = LibDB.ReferenceNamesContent.storedNames (snd caller)
          let names =
            match stored with
            | Some names -> names
            | None -> failtest "the fixture has a stored body"
          let! _ =
            LibDB.Inserts.insertAndApplyOpsAsWip
              [ PT.PackageOp.SetReferenceNames(fst caller, snd caller, names, None) ]
          // Keep the saved names and dependency index, but remove the body they describe.
          let (PT.Hash hash) = hash
          do!
            Sql.query "DELETE FROM package_functions WHERE hash = @hash"
            |> Sql.parameters [ "hash", Sql.string hash ]
            |> Sql.executeStatementAsync
          let! result =
            ReferenceDependencies.namedReferences PT.BranchId.Main [ caller ]
          Expect.equal
            result
            (Error "Cannot read reference names: stored declaration is missing")
            "saved names cannot be checked without the declaration body"
          let! review = ReferenceDependencies.dependencyReview PT.BranchId.Main []
          Expect.equal
            review
            (Error "Cannot read reference names: stored declaration is missing")
            "review must not hide an unreadable graph behind an empty findings list"
        })
  }

let ambiguousAliasesNeedTheStoredDeclaration =
  testTask "ambiguous reference aliases require the stored declaration body" {
    do!
      withStore (fun () ->
        task {
          let hash =
            "0123456789abcdef0123456789abcdef0123456789abcdef0123456789abcdef"
          let target =
            "abcdef0123456789abcdef0123456789abcdef0123456789abcdef0123456789"
          // No saved names or body exist for this caller, and the index records two aliases.
          do!
            Sql.query
              "INSERT INTO package_dependencies
            (item_hash, depends_on_hash, depends_on_item_type, depends_on_owner, depends_on_modules, depends_on_name)
            VALUES (@hash, @target, 'fn', 'Tests', 'MissingAliasBody', 'leaf'),
                   (@hash, @target, 'fn', 'Tests', 'MissingAliasBody', 'alias')"
            |> Sql.parameters
              [ "hash", Sql.string hash; "target", Sql.string target ]
            |> Sql.executeStatementAsync
          let caller =
            location "MissingAliasBody" "caller", PT.PackageFn(PT.Hash hash)
          let! result =
            ReferenceDependencies.namedReferences PT.BranchId.Main [ caller ]
          Expect.equal
            result
            (Error "Cannot read reference names: stored declaration is missing")
            "an ambiguous index cannot supply the caller's reference names"
        })
  }

let reviewUsesSavedAliasNames =
  testTask
    "native review respects saved alias names when shared content becomes stale" {
    do!
      withStore (fun () ->
        task {
          let moduleName = "ReviewAliases"
          let! _ =
            authorIntoMain
              $"""module Tests.{moduleName}
let leaf(): Int64 = 932110L
let alias(): Int64 = 932110L
let replacement(): Int64 = 932111L
"""
          // Bare calls to functions not yet stored lower to local variables.
          // Store the helpers first so both callers have resolved dependency edges.
          let! _ =
            authorIntoMain
              $"""module Tests.{moduleName}
let caller(): Int64 = leaf()
let aliasCaller(): Int64 = alias()
"""
          let! _ =
            authorIntoMain
              $"""module Tests.{moduleName}
let downstream(): Int64 = caller()
"""
          let! (leaf, original) = source moduleName "leaf"
          let! (alias, aliasTarget) = source moduleName "alias"
          let! (_, replacement) = source moduleName "replacement"
          let! caller = source moduleName "caller"
          let! aliasCaller = source moduleName "aliasCaller"
          Expect.equal aliasTarget original "the helpers share content"
          Expect.notEqual replacement original "the replacement changes content"
          Expect.equal (snd caller) (snd aliasCaller) "the callers share content"
          let! references =
            ReferenceDependencies.namedReferences
              PT.BranchId.Main
              [ caller; aliasCaller ]
          match references with
          | Error message -> failtest message
          | Ok edges ->
            Expect.equal
              (edges
               |> List.map (fun (usage, _, _, dependency) -> usage, dependency)
               |> List.sort)
              (List.sort [ fst caller, leaf; fst aliasCaller, alias ])
              "shared caller content retains each authored dependency name"
          let! clean = ReferenceDependencies.dependencyReview PT.BranchId.Main []
          match clean with
          | Error message -> failtest message
          | Ok(stale, _) ->
            Expect.isFalse
              (stale
               |> List.exists (fun ((usage, _, _, _), _) ->
                 usage.modules = [ moduleName ]))
              "the initial aliases are current"
          // Change the binding without propagation so review has a stale edge to find.
          let! _ =
            LibDB.Inserts.insertAndApplyOpsAsWip
              [ PT.PackageOp.SetName(leaf, replacement, None) ]
          let! (_, current) = source moduleName "leaf"
          Expect.equal
            current
            replacement
            "the changed binding is visible to review"
          let! review =
            ReferenceDependencies.dependencyReview
              PT.BranchId.Main
              [ leaf, replacement ]
          match review with
          | Error message -> failtest message
          | Ok(stale, repoints) ->
            let usages =
              stale
              |> List.choose (fun ((usage, _, _, _), _) ->
                if usage.modules = [ moduleName ] then Some usage.name else None)
            Expect.equal
              usages
              [ "caller" ]
              "the shared-body alias caller stays current"
            Expect.equal
              (Set.ofList repoints)
              (Set.ofList
                [ location moduleName "caller", leaf
                  location moduleName "downstream", leaf ])
              "only the changed name's callers and their downstream callers repoint"
          // Compare selective reads with the full graph, including a seed reached
          // through another seed while its own direct edges are still current.
          let checkFullGraph changed =
            task {
              let! all = ReferenceDependencies.namedReferences PT.BranchId.Main []
              let edges =
                match all with
                | Ok edges -> edges
                | Error message -> failtest message
              let! rows =
                Sql.query
                  "SELECT owner, modules, name, item_type, item_hash FROM locations WHERE unlisted_at IS NULL"
                |> Sql.executeAsync (fun row ->
                  let modules = row.string "modules"
                  let location : PT.PackageLocation =
                    { owner = row.string "owner"
                      modules =
                        if modules = "" then
                          []
                        else
                          modules.Split('.') |> Array.toList
                      name = row.string "name" }
                  location,
                  PT.Reference.fromHashAndKind (
                    PT.Hash(row.string "item_hash"),
                    PT.ItemKind.fromString (row.string "item_type")
                  ))
              let expected =
                ReferenceDependencies.reviewGraph (Map.ofList rows) edges changed
              let! actual =
                ReferenceDependencies.dependencyReview PT.BranchId.Main changed
              let normalize (stale, repoints) =
                Set.ofList stale, Set.ofList repoints
              Expect.equal
                (actual |> Result.map normalize)
                (Ok(normalize expected))
                "selective review agrees with the full graph"
            }
          do! checkFullGraph []
          do! checkFullGraph [ leaf, replacement; caller ]
          // Now stale through the saved alias rather than the canonical body name.
          let! _ =
            LibDB.Inserts.insertAndApplyOpsAsWip
              [ PT.PackageOp.SetName(alias, replacement, None) ]
          do! checkFullGraph [ alias, replacement ]
        })
  }

/// Graph review must keep names distinct even when content is shared, traverse
/// current downstream edges, and terminate cycles without reporting draft roots.
let reviewKeepsAliasesAndTraversesCycles =
  test "dependency review filters stale edges and traverses callers by name" {
    let loc name = location "ReviewGraph" name
    let version digit = PT.PackageFn(PT.Hash(String.replicate 64 digit))
    let old, current, body = version "a", version "b", version "c"
    let root, alias, caller, downstream, missing =
      loc "root", loc "alias", loc "caller", loc "downstream", loc "missing"
    let live =
      Map.ofList [ root, current; alias, old; caller, body; downstream, body ]
    let edges : ReferenceDependencies.NamedReference list =
      [ caller, body, old, root
        alias, old, old, alias
        downstream, body, body, caller
        caller, body, body, downstream
        downstream, body, old, missing ]
    let stale, repoints =
      ReferenceDependencies.reviewGraph live edges [ root, current ]
    Expect.equal
      stale
      [ (edges[0], current) ]
      "only the outdated named reference is stale; missing bindings are ignored"
    Expect.equal
      (Set.ofList repoints)
      (Set.ofList [ caller, root; downstream, root ])
      "current downstream edges propagate, cycles terminate, and aliases stay separate"
    let _, overlapping =
      ReferenceDependencies.reviewGraph live edges [ root, current; caller, current ]
    Expect.equal
      overlapping
      [ downstream, root ]
      "changed callers are hidden and the first changed seed owns overlapping causes"
    let changedKind = PT.PackageValue(PT.Hash(String.replicate 64 "d"))
    let kindStale, _ =
      ReferenceDependencies.reviewGraph (Map.add root changedKind live) edges []
    Expect.equal
      kindStale
      [ (edges[0], changedKind) ]
      "review retains the current kind so Dark can report a kind change"
    let cleanStale, cleanRepoints =
      ReferenceDependencies.reviewGraph (Map.add root old live) edges []
    Expect.isEmpty cleanStale "a current graph returns no stale references"
    Expect.isEmpty cleanRepoints "a constraints-only review produces no repoints"
    let _, unchanged = ReferenceDependencies.reviewGraph live edges [ root, old ]
    Expect.isEmpty unchanged "a seed with no stale direct edge causes no repoints"
  }

let tests =
  testSequenced
  <| testList
    "ReferenceNames"
    [ reviewUsesSavedAliasNames
      reviewKeepsAliasesAndTraversesCycles
      wideDependencyReadsRespectSelection
      savedNamesNeedTheStoredDeclaration
      ambiguousAliasesNeedTheStoredDeclaration ]
