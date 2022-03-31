/// Error messages are not required to be directly the same between
/// old and new implementations.
module FuzzTests.AllowedFuzzerErrors

open System.Threading.Tasks
open FSharp.Control.Tasks
open System.Text.RegularExpressions

open Prelude
open Prelude.Tablecloth
open Tablecloth
open TestUtils.TestUtils

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
  (fn : PT.FQFnName.StdlibFnName)
  (debug : bool)
  (actualMsg : string)
  (expectedMsg : string)
  =
  let expectedMsg =
    // Some OCaml errors are in a JSON struct, so get the message and compare that
    try
      let mutable options = System.Text.Json.JsonDocumentOptions()
      options.CommentHandling <- System.Text.Json.JsonCommentHandling.Skip

      let jsonDocument = System.Text.Json.JsonDocument.Parse(expectedMsg, options)

      let mutable elem = System.Text.Json.JsonElement()

      if jsonDocument.RootElement.TryGetProperty("short", &elem) then
        elem.GetString()
      else
        expectedMsg
    with
    | _ -> expectedMsg

  if actualMsg = expectedMsg then
    true
  else
    List.any
      (function
      | [ namePat; actualPat; expectedPat ] ->
        let regexMatch str regex = Regex.Match(str, regex, RegexOptions.Singleline)

        let nameMatches =
          (regexMatch
            (fn
             |> PT2RT.FQFnName.StdlibFnName.toRT
             |> RT.FQFnName.StdlibFnName.toString)
            namePat)
            .Success
        let actualMatch = regexMatch actualMsg actualPat
        let expectedMatch = regexMatch expectedMsg expectedPat

        // Not only should we check that the error message matches,
        // but also that the captures match in both.
        let sameGroups = actualMatch.Groups.Count = expectedMatch.Groups.Count

        let actualGroupMatches = Dictionary.empty ()
        let expectedGroupMatches = Dictionary.empty ()

        if sameGroups && actualMatch.Groups.Count > 1 then
          // start at 1, because 0 is the whole match
          for i = 1 to actualMatch.Groups.Count - 1 do
            let group = actualMatch.Groups[i]
            actualGroupMatches.Add(group.Name, group.Value)
            let group = expectedMatch.Groups[i]
            expectedGroupMatches.Add(group.Name, group.Value)

        let dToL d = Dictionary.toList d |> List.sortBy Tuple2.first

        let groupsMatch = (dToL actualGroupMatches) = (dToL expectedGroupMatches)

        if nameMatches && debug then
          print "\n\n\n======================"
          print (if nameMatches then "✅" else "❌")
          print (if actualMatch.Success then "✅" else "❌")
          print (if expectedMatch.Success then "✅" else "❌")
          print (if groupsMatch then "✅" else "❌")
          print $"{string fn}"
          print $"{actualMsg}"
          print $"{expectedMsg}\n\n"
          print $"{namePat}"
          print $"{actualPat}"
          print $"{expectedPat}"
          print $"actualGroupMatches: {dToL actualGroupMatches}"
          print $"expectedGroupMatches: {dToL expectedGroupMatches}"

        nameMatches && actualMatch.Success && expectedMatch.Success && groupsMatch
      | _ ->
        Exception.raiseInternal "Invalid json in tests/allowedFuzzerErrors.json" [])
      allowedErrors.knownErrors
