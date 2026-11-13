/// Package fns don't take new opaque Bool flags.
module Tests.BoolFlagSites

open Expecto
open Prelude

/// The calls passing a bare `true`/`false` to a `Bool` parameter (`LibDB.BoolFlagSites`) are
/// the ones `backend/tests/bool-flag-sites.txt` lists. A new one fails here: take a two-case
/// enum named for what the flag means instead (#6150). One taken away fails too, until the file
/// is rewritten, so the list only shrinks.
let bareBoolCallsMatchTheBaseline =
  testTask
    "bare true/false calls are the ones backend/tests/bool-flag-sites.txt lists" {
    let path =
      System.IO.Path.Combine(
        LibConfig.Dirs.sourceRootDir,
        "backend/tests/bool-flag-sites.txt"
      )
    let! names = LibDB.Raises.allFnNames () |> Ply.toTask
    let! sites = LibDB.BoolFlagSites.find names |> Ply.toTask
    let found = LibDB.BoolFlagSites.baselineLines sites
    let listed =
      System.IO.File.ReadAllLines path
      |> List.ofArray
      |> List.filter (fun line -> line <> "")
    let rewrite =
      "scripts/run-local-exec bool-flag-sites --write backend/tests/bool-flag-sites.txt"
    let fail (what : string) (fix : string) (lines : List<string>) =
      if not (List.isEmpty lines) then
        let shown = lines |> List.map (fun line -> $"  {line}") |> String.concat "\n"
        failtest $"{what} bare true/false calls; {fix}:\n{shown}"
    fail
      "new or more"
      "take a two-case enum named for what the flag means (#6150)"
      (found |> List.filter (fun line -> not (List.contains line listed)))
    fail
      "fewer"
      $"rewrite with `{rewrite}`"
      (listed |> List.filter (fun line -> not (List.contains line found)))
  }

let tests = testList "boolFlagSites" [ bareBoolCallsMatchTheBaseline ]
