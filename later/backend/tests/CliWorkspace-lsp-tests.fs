// RESTORE: backend/tests/Tests/CliWorkspace.Tests.fs
// WAS: the two tests that drove the LSP inlay hints
// ANCHOR: two `let`s in that file, and two names in its test list
// ALSO: later/packages/darklang/languageTools/lsp-server/
// WHEN: the first round of the molt, 2026-10-12
//
// The two CliWorkspace tests that drove the LSP's inlay hints, moved out of
// backend/tests/Tests/CliWorkspace.Tests.fs by the molt. Not compiled.
//
// Both assert the EDITOR's half of live values, which the second one's own comment says:
// "The CLI shows them; this is the editor's half". The CLI half stays asserted in
// CliWorkspace.Tests.fs, where the printer is driven with liveValues in its context, and by
// liveValuesStayBounded.

/// A hint goes on the line its value came from, even when two lines are identical.
///
/// The hints are built by printing the function twice, once plain and once with values, and
/// diffing. Finding where each changed line LIVES used to be a search of the document for a line
/// with the same text, which cannot tell two identical lines apart: a function with two of them
/// put both hints on the first and none on the second, and pushed the line after the pair off
/// the end of the document, losing its hint entirely. Counting from the function's header in
/// both the printed form and the document is exact.
let private hintsLandOnTheRightIdenticalLine =
  cliTestWithFreshTraces
    "a hint lands on its own line when another line is identical to it"
    (fun target ->
      task {
        let state = executionState target
        let author = author target
        do!
          author
            "Tests.Dup.twice"
            ("(n: Int64): Int64 =\n"
             + "  let a =\n"
             + "    let v = (n * 2L)\n"
             + "    v\n"
             + "  let b =\n"
             + "    let v = (n * 2L)\n"
             + "    v\n"
             + "  (a + b)")
        let! ran = runCli target [ "eval"; "Tests.Dup.twice 5L" ]
        Expect.stringContains ran "20" "the run happened"

        let! hints =
          evalUnder
            state
            """let bid = Darklang.SCM.Branch.mainBranchId
let ctx = Darklang.PrettyPrinter.ProgramTypes.Context.forBranch bid
let q = Darklang.LanguageTools.ProgramTypes.Search.SearchQuery { currentModule = ["Tests", "Dup"]; text = ""; searchDepth = Darklang.LanguageTools.ProgramTypes.Search.SearchDepth.AllDescendants; entityTypes = []; exactMatch = false }
let r = Darklang.LanguageTools.PackageManager.Search.search bid q
let defs = Darklang.LanguageTools.ProgramTypes.Definitions { types = []; fns = r.fns |> Darklang.Stdlib.List.map (fun f -> f.entity); values = []; traits = []; impls = []; exprs = [] }
let docLines = (Darklang.PrettyPrinter.definitions ctx defs) |> Darklang.Stdlib.String.split "\n"
r.fns
|> Darklang.Stdlib.List.map (fun item -> Darklang.LanguageTools.LspServer.InlayHints.hintsFor (Darklang.Cli.initState ()).accountID bid docLines item)
|> Darklang.Stdlib.List.flatten
|> Darklang.Stdlib.List.map (fun h -> Darklang.Stdlib.toString h.position.line)"""
        let lines =
          match hints with
          | RT.DList(_, items) ->
            items
            |> List.map (fun i ->
              match i with
              | RT.DString l -> l
              | other -> string other)
            |> List.sort
          | other -> failtest $"expected the hints, got {other}"

        // One line each, so no line carries two and none is missing. Matching by text gave
        // four hints across three lines, with one line holding two of them.
        Expect.equal
          (List.length lines)
          (List.length (List.distinct lines))
          $"every hint is on a line of its own, got {lines}"
      })


let private liveValuesReplayTheLastCall =
  cliTestWithFreshTraces
    "live values replay the last recorded call through the current code"
    (fun target ->
      task {
        let state = executionState target
        let author = author target
        do! author "Tests.LiveVals.double" "(n: Int64): Int64 = (n * 2L)"
        do!
          author
            "Tests.LiveVals.greet"
            // `n + 1L` is here for the hints: an INFIX expression had no value in any trace Dark
            // had ever taken until this branch, because the compiler marks the expression a
            // replay should collect and the infix cases built their call by hand and never
            // emitted the marker. The CLI shows them; this is the editor's half, which until now
            // was only confirmed by the two paths sharing a replay rather than by an assertion.
            ("(name: String): String =\n"
             + "  let up = Stdlib.String.toUppercase name\n"
             + "  let n = Tests.LiveVals.double 21L\n"
             + "  let m = n + 1L\n"
             + "  $\"hi {up} {Stdlib.toString m}\"")

        // Nothing recorded yet: no values, and no error.
        let! before = evalUnder state (annotatedPrint "Tests" "LiveVals" "greet")
        match before with
        | RT.DString printed ->
          Expect.isFalse
            (printed.Contains "// =")
            "no call recorded, so nothing beside the code"
          Expect.stringContains printed "let greet" "the code itself still prints"
        | other -> failtest $"expected the print, got {other}"

        let! out = runCli target [ "eval"; "Tests.LiveVals.greet \"bob\"" ]
        Expect.stringContains out "hi BOB 43" "the call ran"

        let! after = evalUnder state (annotatedPrint "Tests" "LiveVals" "greet")
        match after with
        | RT.DString printed ->
          Expect.stringContains
            printed
            "Stdlib.String.toUppercase name // = \"BOB\""
            "the recorded input flowed through the first call"
          Expect.stringContains
            printed
            "double 21L // = 42"
            "and the callee's result is beside its call"
          Expect.isFalse
            (printed.Contains "toString m // =")
            "a call inside an interpolated string is left bare: a comment there would break the string"
        | other -> failtest $"expected the print, got {other}"

        // The LSP's hints: the same values, placed on the document's lines. The document is
        // the module as the editor reads it (`fileSystem/read`), where the fns sit two columns
        // in; `double` has a recorded call of its own, from the eval above.
        let! hints =
          evalUnder
            state
            """let bid = Darklang.SCM.Branch.mainBranchId
let ctx = Darklang.PrettyPrinter.ProgramTypes.Context.forBranch bid
let q = Darklang.LanguageTools.ProgramTypes.Search.SearchQuery { currentModule = ["Tests", "LiveVals"]; text = ""; searchDepth = Darklang.LanguageTools.ProgramTypes.Search.SearchDepth.AllDescendants; entityTypes = []; exactMatch = false }
let r = Darklang.LanguageTools.PackageManager.Search.search bid q
let defs = Darklang.LanguageTools.ProgramTypes.Definitions { types = []; fns = r.fns |> Darklang.Stdlib.List.map (fun f -> f.entity); values = []; traits = []; impls = []; exprs = [] }
let docLines = (Darklang.PrettyPrinter.definitions ctx defs) |> Darklang.Stdlib.String.split "\n"
r.fns
|> Darklang.Stdlib.List.map (fun item -> Darklang.LanguageTools.LspServer.InlayHints.hintsFor (Darklang.Cli.initState ()).accountID bid docLines item)
|> Darklang.Stdlib.List.flatten
|> Darklang.Stdlib.List.map (fun h -> (Darklang.Stdlib.toString h.position.line) + ":" + (Darklang.Stdlib.toString h.position.character) + " " + h.label)"""
        let hints =
          match hints with
          | RT.DList(_, items) ->
            items
            |> List.map (fun i ->
              match i with
              | RT.DString s -> s
              | other -> string other)
            |> List.sort
          | other -> failtest $"expected the hints, got {other}"
        // The last one is the point of the extra line: `7:18 = 43` is `n + 1L`, an INFIX
        // expression. Every other hint here is a CALL's value, and calls had hints before this
        // branch. Without this row the editor's half of the headline feature is confirmed only
        // by sharing a replay with the CLI rather than by anything asserting it.
        Expect.equal
          hints
          [ "2:10 = 42"; "5:43 = \"BOB\""; "6:31 = 42"; "7:18 = 43" ]
          "one hint per value at a line position, at the end of the document's line"

        // The callee changes; the replay runs the current code on the same recorded input.
        do! author "Tests.LiveVals.double" "(n: Int64): Int64 = (n * 3L)"
        let! edited = evalUnder state (annotatedPrint "Tests" "LiveVals" "greet")
        match edited with
        | RT.DString printed ->
          Expect.stringContains
            printed
            "double 21L // = 63"
            "the edit is in the values, with no new call"
        | other -> failtest $"expected the print, got {other}"

        // A version that fails at run time reports the failure and keeps what ran before it.
        do! author "Tests.LiveVals.double" "(n: Int64): Int64 = (n / 0L)"
        let! failed =
          evalUnder
            state
            """match Darklang.Stdlib.Live.Values.replay (Darklang.Cli.initState ()).accountID Darklang.SCM.Branch.mainBranchId (Darklang.LanguageTools.ProgramTypes.PackageLocation { owner = "Tests"; modules = ["LiveVals"]; name = "greet" }) with
| Some v -> (false, Darklang.Stdlib.Dict.size v.byExpr, v.problem)
| None -> (false, 0, Darklang.Stdlib.Option.Option.Some "no trace")"""
        match failed with
        | RT.DTuple(RT.DBool _placeholder,
                    RT.DInt count,
                    [ RT.DEnum(_, _, _, "Some", [ RT.DString problem ]) ]) ->
          // The first slot is a literal `false` in both Dark arms, so asserting it was false
          // could not fail and was testing nothing. `Values` no longer carries the run's own
          // answer: the preview's business is the values inside the code, and `traces inspect`
          // is where a trace's answer lives. The two assertions below are the real ones.
          Expect.isGreaterThan
            (RT.DarkInt.toBigInt count)
            0I
            "the values up to the failure are kept"
          Expect.stringContains
            problem
            "divide by 0"
            "and the problem is the runtime error"
        | other -> failtest $"expected (false, n, Some problem), got {other}"
      })


