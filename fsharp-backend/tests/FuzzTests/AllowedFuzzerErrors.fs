/// Manages differences between OCaml and F# backends currently allowed in
/// fuzztesting, in a `.jsonc` file so re-compilations are not required upon
/// every adjustment of these rules.
module FuzzTests.AllowedFuzzerErrors

open System.Threading.Tasks
open FSharp.Control.Tasks
open System.Text.RegularExpressions

open Prelude
open Tablecloth

module PT = LibExecution.ProgramTypes
module RT = LibExecution.RuntimeTypes
module PT2RT = LibExecution.ProgramTypesToRuntimeTypes

type AllowedFuzzerErrorFileStructure =
  { functionToTest : Option<string>
    knownDifferingFunctions : Set<string>
    knownErrors : List<List<string>> }

// Keep a list of allowed errors where we can edit it without recompiling
let readAllowedErrors () =
  use r = new System.IO.StreamReader "tests/allowedFuzzerErrors.jsonc"
  let json = r.ReadToEnd()

  Json.Vanilla.deserializeWithComments<AllowedFuzzerErrorFileStructure> json

let allowedErrors = readAllowedErrors ()

// Error messages are not required to be directly the same between
// old and new implementations. However, this can hide errors, so we
// manually verify them all to make sure we didn't miss any.
let errorIsAllowed
  (fn : RT.FQFnName.StdlibFnName)
  (debug : bool)
  (actualMsg : string)
  =
  allowedErrors.knownErrors
  |> List.any (fun knownError ->
    match knownError with
    | [ namePat; actualPat; expectedPat ] ->
      let regexMatch str regex = Regex.Match(str, regex, RegexOptions.Singleline)

      let nameMatches =
        (regexMatch (fn |> RT.FQFnName.StdlibFnName.toString) namePat).Success
      let actualMatch = regexMatch actualMsg actualPat

      let actualGroupMatches = Dictionary.empty ()

      if actualMatch.Groups.Count > 1 then
        // start at 1, because 0 is the whole match
        for i = 1 to actualMatch.Groups.Count - 1 do
          let group = actualMatch.Groups[i]
          actualGroupMatches.Add(group.Name, group.Value)

      let dToL d = Dictionary.toList d |> List.sortBy Tuple2.first

      if nameMatches && debug then
        print "\n\n\n======================"
        print (if nameMatches then "✅" else "❌")
        print (if actualMatch.Success then "✅" else "❌")
        print $"{string fn}"
        print $"{actualMsg}"
        print $"{namePat}"
        print $"{actualPat}"
        print $"{expectedPat}"
        print $"actualGroupMatches: {dToL actualGroupMatches}"

      nameMatches && actualMatch.Success
    | _ ->
      Exception.raiseInternal "Invalid json in tests/allowedFuzzerErrors.json" [])
