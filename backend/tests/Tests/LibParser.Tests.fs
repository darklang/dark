/// Parser structure tests. The core syntax-case coverage was adapted from
/// pbiggar/darklang-compiler's SyntaxInteropTests.fs (re-implemented against
/// this repo's `LibParser.Parser` + Expecto). The corpus gate, fuzzer,
/// range-invariant / prim-type-drift guards, and diagnostics tests are specific
/// to this parser; pretty-print round-trips live in `packages/darklang/tests/languageTools/roundTrip/`,
/// apart from the few in `LibParser.RoundTrip.Tests.fs`.
///
/// Exercises the hand-written parser (`LibParser.Parser`) — its range-complete
/// `WrittenTypes` output structure, offside rules, type grammar, diagnostics,
/// and recovery.
module Tests.LibParser

open Expecto


type TestResult = Result<unit, string>

module WT = LibParser.WrittenTypes
module WT2PT = LibParser.WrittenTypesToProgramTypes
module Validation = LibParser.Validation
module NR = LibParser.NameResolver
module SourceFile = LibParser.SourceFile
module PT = LibExecution.ProgramTypes
module RTT = LibExecution.RuntimeTypes

// Coverage GATE: the parser must cleanly parse every real .dark package file. Files
// that are legitimately not valid Dark go on the allowlist; anything else failing to
// parse cleanly is a regression and fails this test.
// relative to packages/darklang; empty = every file must parse cleanly. An
// entry that starts parsing cleanly again is flagged stale by the rot-guard.
let private corpusAllowlist : Set<string> = Set.empty

let private corpusRoot : Option<string> =
  [ "../packages/darklang"; "packages/darklang"; "/home/dark/app/packages/darklang" ]
  |> List.tryFind System.IO.Directory.Exists

/// Every `.dark` file under `packages/darklang`, parsed once: `(path, relative path,
/// parse result or the message it threw)`.
///
/// Two tests below walk the whole corpus, one checking that it parses cleanly and one
/// checking range containment, and parsing is the expensive half of both. Sharing the
/// parse halves the work; doing it in parallel means neither test is a single-threaded
/// minute in the middle of an otherwise parallel suite.
let private parsedCorpus =
  lazy
    (match corpusRoot with
     | None -> [||] // no package dir (not a CI environment) -- nothing to gate
     | Some root ->
       System.IO.Directory.GetFiles(
         root,
         "*.dark",
         System.IO.SearchOption.AllDirectories
       )
       |> Array.Parallel.map (fun f ->
         let rel = f.Substring(root.Length).TrimStart('/', '\\').Replace('\\', '/')
         let parsed =
           try
             Ok(LibParser.Parser.parse (System.IO.File.ReadAllText f))
           with e ->
             Error("THREW: " + e.Message)
         (f, rel, parsed)))

let private corpusTests =
  testList
    "parser-corpus"
    [ testCase "the parser cleanly parses every valid .dark package file" (fun _ ->
        match corpusRoot with
        | None -> ()
        | Some _ ->
          // (relPath, first-diagnostic-message) for every file that does NOT parse cleanly
          let failures =
            parsedCorpus.Value
            |> Array.choose (fun (_, rel, parsed) ->
              match parsed with
              | Error msg -> Some(rel, msg)
              | Ok r ->
                match r.diagnostics with
                | [] -> None
                | d :: _ -> Some(rel, d.message))
          // regression gate: every non-allowlisted file must parse cleanly
          let unexpected =
            failures
            |> Array.filter (fun (rel, _) -> not (Set.contains rel corpusAllowlist))
          if not (Array.isEmpty unexpected) then
            let detail =
              unexpected
              |> Array.map (fun (rel, m) -> $"  {rel}: {m}")
              |> String.concat "\n"
            failtest
              $"{unexpected.Length} package file(s) no longer parse cleanly (regression):\n{detail}"
          // rot guard: an allowlisted file that now parses cleanly should be removed
          let failed = failures |> Array.map fst |> Set.ofArray
          let stale =
            corpusAllowlist |> Set.filter (fun a -> not (Set.contains a failed))
          if not (Set.isEmpty stale) then
            let staleList = stale |> String.concat ", "
            failtest
              $"stale corpus allowlist entries (they parse cleanly now — remove them): {staleList}") ]

// The range-complete parser. Its tree shapes, source ranges and diagnostic messages are
// package tests now (packages/darklang/tests/languageTools/parser/), read through
// `Builtin.parse`. What stays here needs something that call does not hand over.
module P = LibParser.Parser
module Tok = LibParser.Tokenizer

let private parserStructureTests =
  testList
    "parser-structure"
    [ // Lexer tokens, and .NET normalization as the reference.
      testCase "quoted graphemes are not mistaken for type variables" (fun _ ->
        let grapheme = "Z̤͔ͧ̑̓"
        let literal = $"'{grapheme}'"
        let sources =
          [ $"({literal})"
            $"('a', {literal})"
            $"['a', {literal}]"
            $"let x = {literal}\nx"
            $"fun x -> {literal}" ]
        for source in sources do
          Expect.isEmpty (P.parse source).diagnostics source
          match LibParser.Lexer.tokenize source with
          | Ok(tokens, diagnostics) ->
            Expect.isEmpty diagnostics source
            let chars =
              tokens
              |> List.choose (fun token ->
                match token.token with
                | Tok.TCharLit value -> Some value
                | _ -> None)
            Expect.contains chars (grapheme.Normalize()) source
          | Error message -> failtest message)

      testCase "combining characters in lists retain their values" (fun _ ->
        let source = "['a', 'é', 'Z̤͔ͧ̑̓', 'ä͖̭̈̇', 'lͮ̒ͫ', 'ǧ̗͚̚', 'o̙̔ͮ̇͐̇']"
        let result = P.parse source
        Expect.isEmpty result.diagnostics "all combining literals parse"
        match result.parsed with
        | Some(WT.SourceFile { exprsToEval = [ WT.EList(_, elems, _, _) ] }) ->
          let chars =
            elems
            |> List.map (fun (expr, _) ->
              match expr with
              | WT.EChar(_, Some(_, value), _, _) -> value
              | other -> failtest $"expected char: {other}")
          let expected =
            [ "a"; "é"; "Z̤͔ͧ̑̓"; "ä͖̭̈̇"; "lͮ̒ͫ"; "ǧ̗͚̚"; "o̙̔ͮ̇͐̇" ]
            |> List.map (fun (value : string) -> value.Normalize())
          Expect.equal chars expected "normalized graphemes are preserved"
        | other -> failtest $"expected list: {other}")

      // The diagnostic code and hint are not part of what Dark gets.
      testCase "module value declarations require val" (fun _ ->
        let result = P.parse "module M =\n  let pi = 3"
        match result.diagnostics with
        | [ diagnostic ] ->
          Expect.equal diagnostic.code P.DiagnosticCode.unexpected "diagnostic code"
          Expect.equal
            diagnostic.hint
            (Some "replace 'let' with 'val'")
            "migration hint"
        | other -> failtest $"expected one diagnostic, got {other}")
      testCase
        "an unknown effect in the row is a diagnostic, not a wildcard"
        (fun _ ->
          let result =
            P.parse "module M =\n  let f (x: Int64) :{Telepathy} Int64 = x"
          match result.diagnostics with
          | [ diagnostic ] ->
            Expect.equal diagnostic.code P.DiagnosticCode.effect "diagnostic code"
            Expect.stringContains diagnostic.message "Telepathy" "names the effect"
          | other -> failtest $"expected one diagnostic, got {other}") ]


/// Parse `src` and return its single trailing expression — the range-complete
/// `WrittenTypes` the parser produces directly.
let private lowerExpr (src : string) : WT.Expr =
  match (P.parse src).parsed with
  | Some(WT.SourceFile { exprsToEval = [ e ] }) -> e
  | other -> failtest $"expected a single trailing expr, got: {other}"

/// The body of a single `let f () … = body` declaration (the range-complete
/// WrittenTypes the parser produces directly).
let private fnBody (src : string) : WT.Expr =
  match (P.parse src).parsed with
  | Some(WT.SourceFile { declarations = [ WT.DFunction f ] }) -> f.body
  | other -> failtest $"expected one function, got: {other}"

/// A type reference parsed via a type-alias declaration (there's no public
/// single-type entry; the alias path exercises the same type grammar).
let private parsedType (tsrc : string) : WT.TypeReference =
  match (P.parse $"type X = {tsrc}").parsed with
  | Some(WT.SourceFile { declarations = [ WT.DType { definition = WT.TDAlias t } ] }) ->
    t
  | other -> failtest $"type parse failed for '{tsrc}': {other}"

/// Lower a rich `WrittenTypes.Expr` all the way to ProgramTypes via WT2PT (empty
/// PM/builtins). Exercises the structural desugars WT2PT folds in (which used to
/// live in the separate R→WT lowering).
let private toPT (e : WT.Expr) : PT.Expr =
  let emptyBuiltins : RTT.Builtins = LibExecution.Builtin.make [] []
  let ctx : WT2PT.Context =
    { currentFnName = None; argMap = Map.empty; localBindings = Set.empty }
  (WT2PT.Expr.toPT emptyBuiltins PT.PackageManager.empty NR.OnMissing.Allow [] ctx e
   |> Ply.toTask)
    .Result

let private toPTWithInModule
  (currentModule : List<string>)
  (pm : PT.PackageManager)
  (context : WT2PT.Context)
  (e : WT.Expr)
  : PT.Expr =
  let emptyBuiltins : RTT.Builtins = LibExecution.Builtin.make [] []
  (WT2PT.Expr.toPT emptyBuiltins pm NR.OnMissing.Allow currentModule context e
   |> Ply.toTask)
    .Result

let private toPTWith
  (pm : PT.PackageManager)
  (context : WT2PT.Context)
  (e : WT.Expr)
  : PT.Expr =
  toPTWithInModule [] pm context e

/// WT2PT structural desugars (fused in from the old R→WT lowering).
let private desugarTests =
  testList
    "wt2pt-desugars"
    [ testCase "a bare uppercase name lowers to EVariable (not EEnum)" (fun _ ->
        // `X` parses as a nullary EEnum; WT2PT rewrites bare EEnum → EVariable so
        // name resolution can pick enum case or variable.
        match toPT (lowerExpr "X") with
        | PT.EVariable(_, "X") -> ()
        | other -> failtest $"expected EVariable X, got: {other}")
      testCase "a statement sequence lowers to EStatement" (fun _ ->
        match toPT (fnBody "let f(): Unit =\n  foo()\n  bar()") with
        | PT.EStatement(_, PT.EApply _, PT.EApply _) -> ()
        | other -> failtest $"expected EStatement(a, b), got: {other}")
      testCase "lowering rejects a WrittenTypes recovery hole" (fun _ ->
        Expect.throws
          (fun () -> toPT (WT.EError WT.synthRange) |> ignore<PT.Expr>)
          "recovery holes must not enter ProgramTypes")
      testCase
        "normal and pipe lambdas drop blank placeholders consistently"
        (fun _ ->
          match toPT (lowerExpr "fun(x, ___) -> x") with
          | PT.ELambda(_, pats, _) ->
            Expect.equal (NEList.length pats) 1 "normal lambda arity"
          | other -> failtest $"expected lambda, got {other}"
          match toPT (lowerExpr "1L |> fun(x, ___) -> x") with
          | PT.EPipe(_, _, [ PT.EPipeLambda(_, pats, _) ]) ->
            Expect.equal (NEList.length pats) 1 "pipe lambda arity"
          | other -> failtest $"expected pipe lambda, got {other}")
      testCase "a type-args-only call seeds a unit placeholder arg" (fun _ ->
        match toPT (lowerExpr "Stdlib.List.empty<String>") with
        | PT.EApply(_, _, [ PT.TString ], args) ->
          match NEList.toList args with
          | [ PT.EUnit _ ] -> ()
          | other -> failtest $"expected a single unit placeholder, got: {other}"
        | other -> failtest $"expected a call with one type arg, got: {other}")
      testCase "`Dict { … }` lowers to EDict, not a record" (fun _ ->
        match toPT (lowerExpr "Dict { \"a\": 1L, \"b\": 2L }") with
        | PT.EDict(_, pairs) ->
          Expect.equal (List.length pairs) 2 "two dict entries"
        | other -> failtest $"expected EDict, got: {other}")
      testCase "record-literal type args survive lowering" (fun _ ->
        match toPT (lowerExpr "MyType<Int64> { x = 1L }") with
        | PT.ERecord(_, _, [ PT.TInt64 ], _) -> ()
        | other ->
          failtest $"expected ERecord with one Int64 type arg, got: {other}")
      testCase "enum-constructor type args survive lowering" (fun _ ->
        match toPT (lowerExpr "MyType<Int64>.Case 1L") with
        | PT.EEnum(_, _, [ PT.TInt64 ], "Case", [ _ ]) -> ()
        | other -> failtest $"expected EEnum with one Int64 type arg, got: {other}") ]

let private loweringRegressionTests =
  let globalMapPM : PT.PackageManager =
    { PT.PackageManager.empty with
        findFn =
          fun _ -> Prelude.uply { return Some(PT.FQFnName.package "global-map") } }
  let context args locals : WT2PT.Context =
    { currentFnName = Some [ "Darklang"; "Test"; "outer" ]
      argMap = args
      localBindings = locals }
  testList
    "lowering-regressions"
    [ testCase "applied argument shadows a same-named package function" (fun _ ->
        match
          toPTWith
            globalMapPM
            (context (Map [ ("map", 0) ]) Set.empty)
            (lowerExpr "map 1L")
        with
        | PT.EApply(_, PT.EArg(_, 0), _, _) -> ()
        | other -> failtest $"expected argument callee, got {other}")
      testCase "applied local shadows a same-named package function" (fun _ ->
        match
          toPTWith
            globalMapPM
            (context Map.empty (Set [ "map" ]))
            (lowerExpr "map 1L")
        with
        | PT.EApply(_, PT.EVariable(_, "map"), _, _) -> ()
        | other -> failtest $"expected local callee, got {other}")
      testCase
        "a lambda captures an enclosing argument before global resolution"
        (fun _ ->
          match
            toPTWith
              globalMapPM
              (context (Map [ ("map", 0) ]) Set.empty)
              (lowerExpr "fun x -> map(x)")
          with
          | PT.ELambda(_, _, PT.EApply(_, PT.EVariable(_, "map"), _, _)) -> ()
          | other -> failtest $"expected captured argument callee, got {other}")
      testCase
        "same-named nested lambda preserves a package collision in its rhs"
        (fun _ ->
          let collisionContext : WT2PT.Context =
            { currentFnName = Some [ "Darklang"; "Test"; "map" ]
              argMap = Map.empty
              localBindings = Set.empty }
          match
            toPTWithInModule
              [ "Darklang"; "Test" ]
              globalMapPM
              collisionContext
              (lowerExpr "let map = (fun x -> map(x)) in map(1L)")
          with
          | PT.ELet(_, _, PT.ELambda(_, _, PT.EApply(_, PT.EFnName _, _, _)), _) ->
            ()
          | other -> failtest $"expected resolved package collision, got {other}")
      testCase "pipe argument shadows a same-named package function" (fun _ ->
        match
          toPTWith
            globalMapPM
            (context (Map [ ("map", 0) ]) Set.empty)
            (lowerExpr "1L |> map")
        with
        | PT.EPipe(_, _, [ PT.EPipeVariable(_, "map", []) ]) -> ()
        | other -> failtest $"expected variable pipe segment, got {other}") ]


let private validationTests =
  let sourceFile source =
    match (P.parse source).parsed with
    | Some(WT.SourceFile sf) -> sf
    | None -> failtest $"parser returned no tree for {source}"
  let validationIssues mode sourceFile =
    match Validation.validate mode sourceFile with
    | Ok _ -> []
    | Error issues -> NEList.toList issues
  testList
    "validation"
    [ testCase "parseFor returns a mode-tagged validated source" (fun _ ->
        match
          P.parseFor
            Validation.Script
            "match x with | Some(value) -> value | None -> 0L"
        with
        | Error diagnostics -> failtest $"unexpected diagnostics: {diagnostics}"
        | Ok validated ->
          Expect.equal
            (Validation.ValidatedSourceFile.mode validated)
            Validation.Script
            "validated mode")
      testCase "validate gates a manually supplied syntax tree" (fun _ ->
        let sf = sourceFile "match x with | Some(value) -> value | None -> 0L"
        match Validation.validate Validation.Script sf with
        | Error issues -> failtest $"unexpected validation issues: {issues}"
        | Ok validated ->
          Expect.equal
            (Validation.ValidatedSourceFile.mode validated)
            Validation.Script
            "validated mode"
          Expect.equal
            (Validation.ValidatedSourceFile.toWrittenTypes validated)
            sf
            "validated tree")
      testCase "parseFor applies purpose rules without changing parse" (fun _ ->
        let assertion = "1L = 1L"
        let packageExpressionCode =
          Validation.IssueCode.toString Validation.PackageExpression
        Expect.isEmpty (P.parse assertion).diagnostics "neutral parse"
        match P.parseFor Validation.Script assertion with
        | Ok _ -> failtest "script accepted a test assertion"
        | Error diagnostics ->
          Expect.exists
            diagnostics
            (fun diagnostic ->
              diagnostic.code = Validation.IssueCode.toString Validation.TestMode)
            "script purpose diagnostic"
        Expect.isOk (P.parseFor Validation.Test assertion) "test purpose accepts it"
        match P.parseFor Validation.Package "1L" with
        | Ok _ -> failtest "package accepted a trailing expression"
        | Error diagnostics ->
          Expect.exists
            diagnostics
            (fun diagnostic -> diagnostic.code = packageExpressionCode)
            "package purpose diagnostic")
      testCase "named tests are package declarations only" (fun _ ->
        let source = "test passes = true"
        Expect.isOk
          (P.parseFor Validation.Package source)
          "package mode accepts a named test"
        match P.parseFor Validation.Script source with
        | Ok _ -> failtest "script mode accepted a named package test"
        | Error diagnostics ->
          Expect.exists
            diagnostics
            (fun diagnostic ->
              diagnostic.code = Validation.IssueCode.toString Validation.TestMode)
            "script purpose diagnostic")
      testCase "parseFor does not validate syntax-recovery trees" (fun _ ->
        let recoveryHoleCode = Validation.IssueCode.toString Validation.RecoveryHole
        match P.parseFor Validation.Script "let x =" with
        | Ok _ -> failtest "invalid syntax passed validation"
        | Error diagnostics ->
          Expect.isNonEmpty diagnostics "syntax diagnostic"
          Expect.isFalse
            (diagnostics
             |> List.exists (fun diagnostic -> diagnostic.code = recoveryHoleCode))
            "no cascaded recovery-hole diagnostic")
      testCase "parser surfaces shared structural validation diagnostics" (fun _ ->
        for (source, expectedCode) in
          [ ("fun(x, x) -> x", Validation.DuplicateBinder)
            ("let (x, x) = (1L, 2L) in x", Validation.DuplicateBinder)
            ("let f(x: Int64, x: Int64): Int64 = x", Validation.DuplicateBinder)
            ("match x with | Some(a) | None -> a", Validation.OrBindingMismatch)
            ("match pair with | ((x | _), y) -> x", Validation.OrBindingMismatch) ] do
          Expect.exists
            (P.parse source).diagnostics
            (fun diagnostic ->
              diagnostic.code = Validation.IssueCode.toString expectedCode)
            $"shared validation diagnostic for {source}")
      testCase "duplicate binders are independently rejected after parsing" (fun _ ->
        let issues =
          sourceFile "let (x, x) = (1L, 2L) in x"
          |> validationIssues Validation.Script
        Expect.exists
          issues
          (fun issue -> issue.code = Validation.DuplicateBinder)
          "duplicate binder issue")
      testCase "deep let patterns preserve duplicate binding order" (fun _ ->
        let firstRange = WT.synthRange
        let duplicateRange =
          { firstRange with
              start = { row = 0; column = 1 }
              end_ = { row = 0; column = 2 } }
        let mutable pattern = WT.LPVariable(firstRange, "x")
        for _ in 1..20000 do
          pattern <-
            WT.LPTuple(
              firstRange,
              pattern,
              firstRange,
              WT.LPWildcard firstRange,
              [],
              firstRange,
              firstRange
            )
        pattern <-
          WT.LPTuple(
            firstRange,
            pattern,
            firstRange,
            WT.LPVariable(duplicateRange, "x"),
            [],
            firstRange,
            firstRange
          )
        let unitExpr = WT.EUnit firstRange
        let sf : WT.SourceFile =
          { range = firstRange
            declarations = []
            exprsToEval =
              [ WT.ELet(
                  firstRange,
                  pattern,
                  unitExpr,
                  unitExpr,
                  firstRange,
                  firstRange
                ) ] }
        let issues = Validation.validateStructure sf
        Expect.equal (List.length issues) 1 "one duplicate"
        Expect.equal issues.Head.code Validation.DuplicateBinder "duplicate binder"
        Expect.equal issues.Head.range duplicateRange "later binding is reported"
        Expect.equal
          issues.Head.related
          [ (firstRange, "'x' was first bound here") ]
          "first binding remains the reference")
      testCase
        "match binding collection preserves order across pattern shapes"
        (fun _ ->
          let sf = sourceFile "match x with | (Some(a), [a, a], [a, ...a], a) -> a"
          let issues =
            Validation.validateStructure sf
            |> List.filter (fun issue -> issue.code = Validation.DuplicateBinder)
          Expect.equal (List.length issues) 5 "all later bindings are duplicates"
          let columns = issues |> List.map (fun issue -> issue.range.start.column)
          Expect.equal columns (List.sort columns) "left-to-right duplicate order"
          let firstRanges = issues |> List.map (fun issue -> fst issue.related.Head)
          Expect.equal
            (List.distinct firstRanges |> List.length)
            1
            "same first binding")
      testCase "package mode rejects trailing expressions" (fun _ ->
        let issues = sourceFile "1L" |> validationIssues Validation.Package
        Expect.exists
          issues
          (fun issue -> issue.code = Validation.PackageExpression)
          "package expression issue")
      testCase "file purpose is classified after a shared parse" (fun _ ->
        let source = "val x = 1L\nx = 1L"
        let normal = P.parse source
        let compatibility = P.parseTestFile source
        Expect.equal normal.parsed compatibility.parsed "one parse shape"
        Expect.equal
          normal.diagnostics
          compatibility.diagnostics
          "one diagnostic shape"
        Expect.isEmpty normal.diagnostics "neutral syntax parses cleanly"
        match normal.parsed with
        | Some(WT.SourceFile sf) ->
          Expect.exists
            (validationIssues Validation.Script sf)
            (fun issue -> issue.code = Validation.TestMode)
            "script classification rejects the assertion"
          Expect.isEmpty
            (validationIssues Validation.Test sf)
            "test classification accepts the same tree"
        | None -> failtest "parser returned no tree")
      testCase
        "test classification rejects bare expressions and script lets"
        (fun _ ->
          for source in [ "1L"; "let x = 1L" ] do
            Expect.exists
              (sourceFile source |> validationIssues Validation.Test)
              (fun issue -> issue.code = Validation.TestAssertion)
              $"test classification rejects {source}")
      testCase "empty record updates are rejected independently" (fun _ ->
        Expect.exists
          (sourceFile "{ value with }" |> validationIssues Validation.Script)
          (fun issue -> issue.code = Validation.EmptyRecordUpdate)
          "empty update issue")
      testCase "empty or-patterns cannot pass the lowering gate" (fun _ ->
        let r = WT.synthRange
        let expr =
          WT.EMatch(
            r,
            WT.EUnit r,
            [ { barRange = r
                pat = WT.MPOr(r, [])
                arrowRange = r
                whenCondition = None
                rhs = WT.EUnit r } ],
            r,
            r
          )
        let sf : WT.SourceFile =
          { range = r; declarations = []; exprsToEval = [ expr ] }
        Expect.exists
          (validationIssues Validation.Script sf)
          (fun issue -> issue.code = Validation.EmptyOrPattern)
          "empty or-pattern issue") ]

/// Literal edge cases: min-magnitude wrap and exponent floats.
let private literalTests =
  testList
    "literals"
    [ testCase "generated binary64 integer boundary literals" (fun _ ->
        for offset in 0L .. 255L do
          for sign in [ ""; "-" ] do
            let source = sign + string (9007199254740992L + offset) + ".0"
            let diagnostics = (P.parse source).diagnostics
            if offset % 2L = 0L then
              Expect.isEmpty diagnostics $"{source}: exactly representable integer"
            else
              Expect.isTrue
                (diagnostics
                 |> List.exists (fun d -> d.code = "PARSE-FLOAT-PRECISION"))
                $"{source}: unrepresentable integer")
      testCase "float literals preserve specified decimal precision" (fun _ ->
        for source in
          [ "10000000000000001.0"
            "10000000000000000.1"
            "0.10000000000000000"
            "9007199254740993.0"
            "9007199254740995.0"
            "1.7976931348623158e308"
            "2.4703282292062328e-324" ] do
          let result = P.parse source
          Expect.isTrue
            (result.diagnostics
             |> List.exists (fun d -> d.code = "PARSE-FLOAT-PRECISION"))
            $"{source}: precision diagnostic"
          let pattern = P.parse $"match x with | {source} -> 1 | _ -> 0"
          Expect.isTrue
            (pattern.diagnostics
             |> List.exists (fun d -> d.code = "PARSE-FLOAT-PRECISION"))
            $"{source}: pattern precision diagnostic"
        for source in [ "0.1"; "0.3"; "0.10000000000000001"; "10000000000000002.0" ] do
          Expect.isEmpty (P.parse source).diagnostics $"{source}: accepted")
      testCase "float range errors are independent of exponent spelling" (fun _ ->
        for source in [ "1e309"; "1e401"; "1e999999999999999999999" ] do
          Expect.isTrue
            ((P.parse source).diagnostics
             |> List.exists (fun d -> d.code = "PARSE-FLOAT-OVERFLOW"))
            $"{source}: overflow diagnostic"
        for source in [ "2e-324"; "1e-401"; "-1e-401"; "1e-999999999999999999999" ] do
          Expect.isTrue
            ((P.parse source).diagnostics
             |> List.exists (fun d -> d.code = "PARSE-FLOAT-UNDERFLOW"))
            $"{source}: underflow diagnostic")
      testCase "float normalization preserves lowering and signed zero" (fun _ ->
        let check source expectedBits =
          let result = P.parse source
          Expect.isEmpty result.diagnostics $"{source}: parses"
          match result.parsed with
          | Some(WT.SourceFile { exprsToEval = [ WT.EFloat(_, neg, whole, fraction) ] }) ->
            let value =
              Prelude.makeFloat
                (if neg then Prelude.Negative else Prelude.Positive)
                whole
                fraction
            Expect.equal
              (System.BitConverter.DoubleToUInt64Bits value)
              expectedBits
              $"{source}: exact runtime bits"
          | other -> failtest $"{source}: {other}"
        check "0.000001e1" 0x3ee4f8b588e368f1UL
        check "0.00000001e1" 0x3e7ad7f29abcaf48UL
        check "00.0000001" 0x3e7ad7f29abcaf48UL
        check "5e-324" 1UL
        check "-0.0" 0x8000000000000000UL
        check "0e999999999999999999999" 0UL
        check ("1" + String.replicate 417 "0" + "e-401") 0x4341c37937e08000UL
        check ("0." + String.replicate 416 "0" + "1e401") 0x3c9cd2b297d889bcUL) ]

/// A paren bomb gets one diagnostic instead of a StackOverflow, which would take the
/// whole process down rather than fail one test.
let private recoveryTests =
  testList
    "recovery"
    [ testCase
        "pathological nesting diagnoses instead of killing the process"
        (fun _ ->
          // recursion-depth guard: a paren bomb must produce ONE diagnostic (the
          // unwind's secondary errors are suppressed), never a StackOverflow
          let bomb = String.replicate 5000 "(" + "1L" + String.replicate 5000 ")"
          let r = P.parse bomb
          match r.diagnostics with
          | [ d ] ->
            Expect.stringContains d.message "nesting too deep" "names the cause"
          | other ->
            failtest
              $"expected exactly the abandon diagnostic, got {List.length other}"
          // …while realistic nesting (50 levels) is untouched
          let ok =
            P.parse (String.replicate 50 "(" + "1L" + String.replicate 50 ")")
          Expect.isEmpty ok.diagnostics "50 levels parse cleanly") ]

/// Lexical-failure handling: a malformed lexeme must surface a diagnostic (never a
/// silent success) and must never throw.
/// `f(a, b)` calls, `let f(a: T, b: U)` declarations, and the old forms as errors.
let private callSyntaxTests =
  let argCount (src : string) =
    match toPT (lowerExpr src) with
    | PT.EApply(_, _, _, args) -> NEList.length args
    | other -> failtest $"expected a call for {src}, got: {other}"
  let hint (src : string) =
    (P.parse src).diagnostics |> List.choose (fun d -> d.hint)
  testList
    "call-syntax"
    [ testCase "commas separate arguments" (fun _ ->
        Expect.equal (argCount "f(1L, 2L)") 2 "two arguments"
        Expect.equal (argCount "Stdlib.List.map(xs, fun x -> x)") 2 "a last lambda")
      testCase "a space before the paren is the same call" (fun _ ->
        Expect.equal (argCount "f (1L, 2L)") 2 "two arguments, not one tuple")
      testCase "one tuple argument takes its own parens" (fun _ ->
        match toPT (lowerExpr "f((1L, 2L))") with
        | PT.EApply(_, _, _, args) ->
          match NEList.toList args with
          | [ PT.ETuple _ ] -> ()
          | other -> failtest $"expected one tuple, got: {other}"
        | other -> failtest $"expected a call, got: {other}")
      testCase "`f()` passes unit" (fun _ ->
        match toPT (lowerExpr "f()") with
        | PT.EApply(_, _, _, args) ->
          match NEList.toList args with
          | [ PT.EUnit _ ] -> ()
          | other -> failtest $"expected the unit argument, got: {other}"
        | other -> failtest $"expected a call, got: {other}")
      testCase "a pipe stage's own call puts the piped value first" (fun _ ->
        match toPT (lowerExpr "xs |> Stdlib.List.map(f)") with
        | PT.EPipe(_, _, [ PT.EPipeFnCall(_, _, _, [ _ ], _) ]) -> ()
        | other -> failtest $"expected one extra argument, got: {other}"
        // an unqualified name in a pipe lowers to a variable stage, which may be a fn
        match toPT (lowerExpr "x |> f()") with
        | PT.EPipe(_, _, [ PT.EPipeVariable(_, "f", []) ]) -> ()
        | other -> failtest $"expected `|> f()` to add nothing, got: {other}"
        match toPT (lowerExpr "x |> f(())") with
        | PT.EPipe(_, _, [ PT.EPipeVariable(_, "f", [ PT.EUnit _ ]) ]) -> ()
        | other -> failtest $"expected `|> f(())` to pass unit, got: {other}"
        match toPT (lowerExpr "x |> Stdlib.List.reverse()") with
        | PT.EPipe(_, _, [ PT.EPipeFnCall(_, _, _, [], _) ]) -> ()
        | other ->
          failtest $"expected a named `|> f()` to add nothing, got: {other}")
      testCase "arguments after a space are an error with a hint" (fun _ ->
        Expect.equal
          (hint "Stdlib.String.length s")
          [ "write `length(a)` rather than `length a`" ]
          "the old form names the new one"
        Expect.equal
          (hint "f 1L 2L")
          [ "write `f(a, b)` rather than `f a b`" ]
          "with as many arguments as were written")
      testCase "a constructor's fields go in parentheses" (fun _ ->
        Expect.equal
          (hint "Ok 5L")
          [ "write `Ok(a)` rather than `Ok a`" ]
          "the old form names the new one"
        Expect.equal
          (hint "Stdlib.Result.Result.Error 1L")
          [ "write `Error(a)` rather than `Error a`" ]
          "qualified too")
      testCase "a constructor pattern's fields go in parentheses" (fun _ ->
        Expect.equal
          (hint "match x with | Ok v -> v | _ -> 0L")
          [ "write `Ok(a)` rather than `Ok a`" ]
          "the old form names the new one"
        Expect.equal
          (hint "match x with | Pair a b -> a | _ -> 0L")
          [ "write `Pair(a, b)` rather than `Pair a b`" ]
          "with as many fields as were written")
      // The qualifier on a constructor pattern is not in the Dark tree.
      testCase "a constructor pattern may be qualified" (fun _ ->
        let r =
          P.parse
            "match x with | Result.Ok(v) -> v | Stdlib.Result.Result.Error(_) -> 0L"
        Expect.isEmpty r.diagnostics "both qualified forms parse"
        match r.parsed with
        | Some(WT.SourceFile { exprsToEval = [ WT.EMatch(_, _, cases, _, _) ] }) ->
          match cases with
          | [ { pat = WT.MPEnum(_, [ "Result" ], (_, "Ok"), [ _ ]) }
              { pat = WT.MPEnum(_,
                                [ "Stdlib"; "Result"; "Result" ],
                                (_, "Error"),
                                [ _ ]) } ] -> ()
          | other -> failtest $"cases: {other}"
        | other -> failtest $"match: {other}")
      testCase "lambda parameters take the shape of a call's arguments" (fun _ ->
        Expect.equal
          (hint "fun a b -> a")
          [ "write `fun(a, b) ->` rather than `fun a b ->`; a tuple parameter is `fun((a, b)) ->`" ]
          "the old form names the new one")
      testCase
        "a function type with several arguments lists them in parentheses"
        (fun _ ->
          Expect.equal
            (hint "let f(g: Int64 -> Int64 -> Int64): Int64 = 1L")
            [ "write `(A, B) -> C` for a function of two arguments, or `A -> (B -> C)` for one that returns a function" ]
            "a chain is an error")
      testCase "a declaration lists its parameters once" (fun _ ->
        Expect.equal
          (hint "let add (a: Int64) (b: Int64) : Int64 = a + b")
          [ "write `let f(a: T, b: U)` rather than `let f (a: T) (b: U)`" ]
          "the old form names the new one") ]

/// Lexical failures. The plain "it diagnoses" cases are package tests; these look at
/// tokens or the LEX code, or nest deep enough to need the stack guard.
let private lexicalFailureTests =
  testList
    "lexical-failures"
    [ testCase "bad integer suffix is consumed as one recovered token" (fun _ ->
        match LibParser.Lexer.tokenize "256uy" with
        | Ok(tokens, diagnostics) ->
          Expect.equal diagnostics.Length 1 "one range diagnostic"
          match tokens with
          | token :: eof :: [] ->
            Expect.equal token.text "256uy" "the recovered token covers the suffix"
            Expect.equal eof.token Tok.TEOF "then EOF"
          | other -> failtest $"unexpected tokens: {other}"
        | Error error -> failtest error)
      testCase
        "lex failures inside interpolation bodies surface with offset positions"
        (fun _ ->
          // sub-tokenizer diagnostics were previously dropped (silent acceptance)
          match (P.parse "$\"a{ 'x }b\"").diagnostics with
          | [ d ] ->
            Expect.equal d.code P.DiagnosticCode.lex "LEX code"
            Expect.equal
              d.range.start
              { Tok.row = 0; Tok.column = 5 }
              "position offset into the outer source"
          | other -> failtest $"expected one diagnostic, got {other}"
          Expect.isEmpty
            (P.parse "$\"a{ 1L }b\"").diagnostics
            "clean interpolation unaffected")
      testCase
        "nested-interpolation bomb diagnoses instead of killing the process"
        (fun _ ->
          // interpolation bodies re-tokenize recursively, so nesting = recursion
          // depth in BOTH tokenizer and parser; uncapped, 5000 levels was an
          // uncatchable StackOverflow (fresh parser state per level bypassed the
          // expression depth guard)
          let bomb =
            String.replicate 5000 "$\"{" + "1L" + String.replicate 5000 "}\""
          let r = P.parse bomb
          Expect.isNonEmpty r.diagnostics "reports the nesting error"
          // …while realistic nesting (5 levels) parses cleanly
          let ok =
            P.parse (String.replicate 5 "$\"{" + "1L" + String.replicate 5 "}\"")
          Expect.isEmpty ok.diagnostics "5 levels parse cleanly") ]

/// What a diagnostic carries besides its message: code, severity, related spans,
/// hint, and the human rendering. The messages themselves are pinned in the package
/// tests.
let private goldenDiagnosticsTests =
  testList
    "diagnostic-details"
    [ testCase "diagnostics carry stable codes, related spans, and hints" (fun _ ->
        match (P.parse "(1L + 2L").diagnostics with
        | [ d ] ->
          Expect.equal d.code P.DiagnosticCode.unclosed "code"
          Expect.equal d.severity P.DiagError "severity"
          match d.related with
          | [ (r, note) ] ->
            Expect.equal r.start { Tok.row = 0; Tok.column = 0 } "opener position"
            Expect.equal note "the '(' opened here" "opener note"
          | other -> failtest $"related: {other}"
        | other -> failtest $"expected one diagnostic, got {other}"
        match (P.parse "128y").diagnostics with
        | [ d ] ->
          Expect.equal d.code P.DiagnosticCode.intRange "code"
          Expect.equal d.hint (Some "write it negated: -128y") "hint"
        | other -> failtest $"expected one diagnostic, got {other}"
        match (P.parse "\"abc").diagnostics with
        | [ d ] ->
          Expect.equal
            d.code
            P.DiagnosticCode.lex
            "lex diagnostics carry the LEX code"
        | other -> failtest $"expected one diagnostic, got {other}"
        match (P.parse "[1L; 2L]").diagnostics with
        | [ d ] ->
          Expect.equal d.code P.DiagnosticCode.expected "code"
          Expect.equal d.hint (Some "use ',' instead") "hint"
        | other -> failtest $"expected one diagnostic, got {other}")
      testCase "renderDiagnostic: caret snippet + related + hint" (fun _ ->
        let src = "let x = (1L +"
        match
          (P.parse src).diagnostics
          |> List.filter (fun d -> d.code = P.DiagnosticCode.unclosed)
        with
        | [ d ] ->
          let rendered = P.renderDiagnostic src d
          let expected =
            "error[PARSE-UNCLOSED] at 1:14: expected ')' to close the '(' at line 1:9, found end of file\n"
            + "  1 | let x = (1L +\n"
            + "    |              ^\n"
            + "  note: the '(' opened here (1:9)\n"
            + "  1 | let x = (1L +\n"
            + "    |         ^"
          Expect.equal rendered expected "full rendering"
        | other -> failtest $"expected one diagnostic, got {other}") ]

/// Seeded mutation fuzzing: the parser must never throw and must be
/// deterministic (same input → same result), whatever we feed it.
let private fuzzTests =
  let seeds =
    [ "let f(x: Int64): Int64 =\n  let y = x + 1L\n  y * 2L"
      "module A.B\ntype T = { a: Int64, b: List<String> }\nlet v = T { a = 1L, b = [] }"
      "match xs with\n| [] -> 0L\n| [h, ...t] -> h + (sum t)\n| _ -> $\"n {h} m\""
      "[1L, 2L] |> Stdlib.List.map(fun x -> x * 2L) |> (fun v -> v + \"s\")"
      "type E = | A of Int64 * String | B\nlet g(): E = A(1L, \"x\")" ]
  testList
    "fuzz"
    [ testCase "mutated inputs never throw; parsing is deterministic" (fun _ ->
        let rng = System.Random 42 // fixed seed — reproducible
        let mutate (s : string) : string =
          if s.Length = 0 then
            s
          else
            match rng.Next 4 with
            | 0 -> s.Substring(0, rng.Next s.Length) // truncate
            | 1 -> s.Remove(rng.Next s.Length, 1) // delete a char
            | 2 -> s.Insert(rng.Next s.Length, string (char (rng.Next(32, 127)))) // insert
            | _ -> // splice a random slice elsewhere
              let a = rng.Next s.Length
              let len = rng.Next(min 8 (s.Length - a))
              s.Insert(rng.Next s.Length, s.Substring(a, len))
        for seed in seeds do
          let mutable cur = seed
          for _ in 1..200 do
            cur <- mutate cur
            let shown = cur.Replace("\n", "\\n")
            let r1 =
              try
                P.parse cur
              with e ->
                failtest $"parser THREW on {shown}: {e.Message}"
            (try
              P.parseTestFile cur |> ignore // compatibility entrypoint must not throw either
             with e ->
               failtest
                 $"compatibility parser entrypoint THREW on {shown}: {e.Message}")
            let r3 = P.parse cur
            Expect.isTrue (Option.isSome r1.parsed) "always returns a tree"
            Expect.equal
              (r3.parsed = r1.parsed && r3.diagnostics = r1.diagnostics)
              true
              "deterministic (no state leaks across parses)") ]

/// Range containment: every child expression's range sits inside its parent's.
/// These ranges are load-bearing for highlighting/hover.
let private rangeInvariantTests =
  let posLeq (a : Tok.Pos) (b : Tok.Pos) =
    a.row < b.row || (a.row = b.row && a.column <= b.column)
  let contains (outer : Tok.TokenRange) (inner : Tok.TokenRange) =
    posLeq outer.start inner.start && posLeq inner.end_ outer.end_
  let rec childExprs (e : WT.Expr) : List<WT.Expr> =
    match e with
    | WT.EUnit _
    | WT.EBool _
    | WT.EInt _
    | WT.EInt64 _
    | WT.EInt8 _
    | WT.EUInt8 _
    | WT.EInt16 _
    | WT.EUInt16 _
    | WT.EInt32 _
    | WT.EUInt32 _
    | WT.EUInt64 _
    | WT.EInt128 _
    | WT.EUInt128 _
    | WT.EFloat _
    | WT.EChar _
    | WT.EVariable _
    | WT.EFnName _
    | WT.EError _ -> []
    | WT.EString(_, _, contents, _, _) ->
      contents
      |> List.choose (function
        | WT.StringInterpolation(_, e, _, _) -> Some e
        | _ -> None)
    | WT.EInfix(_, _, l, r) -> [ l; r ]
    | WT.ELet(_, _, v, b, _, _) -> [ v; b ]
    | WT.EApply(_, lhs, _, args) -> lhs :: args
    | WT.EList(_, elems, _, _) -> elems |> List.map fst
    | WT.ETuple(_, f, _, s, rest, _, _) -> f :: s :: (rest |> List.map snd)
    | WT.EIf(_, c, t, e2, _, _, _) -> [ c; t ] @ Option.toList e2
    | WT.ERecordFieldAccess(_, o, _, _) -> [ o ]
    | WT.ELambda(_, _, b, _, _) -> [ b ]
    | WT.ERecord(_, _, fields, _, _) -> fields |> List.map (fun (_, _, v) -> v)
    | WT.EDict(_, contents, _, _, _) ->
      contents |> List.collect (fun (_, k, _, v) -> [ k; v ])
    | WT.ERecordUpdate(_, r, ups, _, _, _) ->
      r :: (ups |> List.map (fun (_, _, v) -> v))
    | WT.EEnum(_, _, _, fields, _) -> fields
    | WT.EMatch(_, expr, cases, _, _) ->
      expr
      :: (cases
          |> List.collect (fun c ->
            c.rhs :: (c.whenCondition |> Option.map snd |> Option.toList)))
    | WT.EPipe(_, head, pipeExprs) ->
      head
      :: (pipeExprs
          |> List.collect (fun (_, pe) ->
            match pe with
            | WT.EPipeLambda(_, _, b, _, _) -> [ b ]
            | WT.EPipeEnum(_, _, _, fs, _) -> fs
            | WT.EPipeFnCall(_, _, _, args) -> args
            | WT.EPipeVariableOrFnCall _ -> []))
    | WT.EUnwrap(_, operand, _) -> [ operand ]
    | WT.EStatement(_, a, b) -> [ a; b ]
  let rec check (path : string) (violations : ResizeArray<string>) (e : WT.Expr) =
    let er = WT.exprRange e
    for c in childExprs e do
      let cr = WT.exprRange c
      if not (contains er cr) then
        violations.Add
          $"{path}: child at {cr.start.row}:{cr.start.column} escapes parent at {er.start.row}:{er.start.column}"
      check path violations c
  let rec declExprs (d : WT.Declaration) : List<WT.Expr> =
    match d with
    | WT.DFunction f -> [ f.body ]
    | WT.DValue v -> [ v.body ]
    | WT.DTest t -> [ t.body ]
    | WT.DExpr e -> [ e ]
    | WT.DModule m -> m.declarations |> List.collect declExprs
    | WT.DType _
    | WT.DTrait _ -> []
    | WT.DImpl impl ->
      impl.members
      |> List.choose (fun m ->
        match m with
        | WT.IMethod f -> Some f.body
        | WT.IAlias _ -> None)
    | WT.DAssertion t -> [ t.actual ]
  testList
    "range-invariants"
    [ testCase
        "child expr ranges are contained in their parents (whole corpus)"
        (fun _ ->
          match corpusRoot with
          | None -> ()
          | Some _ ->
            let violations = ResizeArray<string>()
            // A file that threw is reported by `parser-corpus` above, not here.
            for (f, _, parsed) in parsedCorpus.Value do
              match parsed with
              | Ok r ->
                match r.parsed with
                | Some(WT.SourceFile sf) ->
                  for e in
                    (sf.declarations |> List.collect declExprs) @ sf.exprsToEval do
                    check f violations e
                | _ -> ()
              | Error _ -> ()
            if violations.Count > 0 then
              let detail = violations |> Seq.truncate 10 |> String.concat "\n"
              failtest $"{violations.Count} range-containment violations:\n{detail}") ]

/// Guard: the parser must map every primitive type name to exactly the `TypeReference`
/// case the `primTypes` table records. (Lowering/serializer coverage is now enforced by
/// the compiler — the primitive cases are matched exhaustively — so this only checks the
/// name→case dispatch the parser does.)
let private primTypeDriftTests =
  testList
    "prim-type-drift"
    [ testCase
        "every primTypes entry parses to exactly its recorded primitive case"
        (fun _ ->
          let lower (t : WT.TypeReference) =
            (WT2PT.TypeReference.toPT
              PT.PackageManager.empty
              NR.OnMissing.Allow
              []
              t
             |> Ply.toTask)
              .Result
          for (name, ctor) in WT.primTypes do
            // lowering erases ranges, so comparing the lowered PT compares the *case*:
            // the parser (via primTypeFromName) must map `name` to the case `ctor` builds.
            Expect.equal
              (lower (parsedType name))
              (lower (ctor WT.synthRange))
              $"{name} did not parse as its recorded primitive case") ]

/// Direct unit tests of parser internals — possible since the decomposition
/// hoisted them to module level.
let private internalUnitTests =
  testList
    "internals"
    [ testCase
        "precedence table: every op has a power; comparisons loosest of arith"
        (fun _ ->
          let bp t = P.infixBindingPower t |> Option.map fst
          Expect.isTrue (bp Tok.TOr < bp Tok.TAnd) "|| looser than &&"
          Expect.isTrue (bp Tok.TAnd < bp Tok.TEqEq) "&& looser than =="
          Expect.isTrue (bp Tok.TEqEq < bp Tok.TPlus) "== looser than +"
          Expect.isTrue (bp Tok.TPlus < bp Tok.TStar) "+ looser than *"
          Expect.isTrue (bp Tok.TStar < bp Tok.TStarStar) "* looser than **"
          // The bitwise levels sit BETWEEN the comparisons and `+`, in Python's
          // order rather than C's: `a & b == c` is `(a & b) == c`.
          Expect.isTrue (bp Tok.TEqEq < bp Tok.TBar) "== looser than |"
          Expect.isTrue (bp Tok.TBar < bp Tok.TBitXor) "| looser than ^"
          Expect.isTrue (bp Tok.TBitXor < bp Tok.TBitAnd) "^ looser than &"
          Expect.isTrue (bp Tok.TBitAnd < bp Tok.TShl) "& looser than <<"
          Expect.equal (bp Tok.TShl) (bp Tok.TShr) "<< and >> bind equally"
          Expect.isTrue (bp Tok.TShl < bp Tok.TPlus) "<< looser than +"
          // right-assoc ops
          Expect.equal
            (P.infixBindingPower Tok.TStarStar |> Option.map snd)
            (Some true)
            "** is right-assoc"
          Expect.equal
            (P.infixBindingPower Tok.TBitXor |> Option.map snd)
            (Some false)
            "^ (xor) is left-assoc") ]

let private unwrapTests =
  testList
    "unwrap"
    [ testCase "rejects unwrap outside a function" (fun _ ->
        for source in
          [ "x?"; "val x = (Some(1))?"; "module M =\n  val x = (Some(1))?" ] do
          let parsed = P.parse source
          Expect.isTrue
            (parsed.diagnostics
             |> List.exists (fun d -> d.code = "VALIDATION-UNWRAP-CONTEXT"))
            source)
      testCase "a ? at the end of a pipe segment points at the parens" (fun _ ->
        let parsed = P.parse "fun s -> s |> Stdlib.Int.parse?"
        Expect.isTrue
          (parsed.diagnostics
           |> List.exists (fun d ->
             d.hint = Some "wrap the whole pipeline: `(value |> transform)?`"))
          $"{parsed.diagnostics}") ]


// Small stacks exercise the guard without enormous fixtures or suite-wide GC pressure.
let private stackSafetyTests =
  testList
    "stack-safety"
    [ for label, source in
        [ "parenthesized types",
          "type T = "
          + String.replicate 2000 "("
          + "Int64"
          + String.replicate 2000 ")"
          "constructor patterns",
          "match value with | " + String.replicate 2000 "Some " + "x -> 1L"
          "elif chains",
          "if true then 1L " + String.replicate 2000 "elif true then 1L " + "else 1L"
          "module headers", String.replicate 2000 "module M\n" + "val x = 1L"
          "parentheses", String.replicate 2000 "(" + "1L" + String.replicate 2000 ")"
          "prefix operators", String.replicate 2000 "!" + "true"
          "right-associated operators", String.replicate 2000 "1L ** " + "1L"
          "validation of left-associated operators",
          String.replicate 2000 "1L + " + "1L"
          "rest patterns",
          "match xs with | [" + String.replicate 2000 "x, " + "...rest] -> 1L"
          "let patterns",
          "let "
          + String.replicate 2000 "(x, "
          + "x"
          + String.replicate 2000 ")"
          + " = v in 1L"
          "type arguments",
          "type T = "
          + String.replicate 2000 "List<"
          + "Int64"
          + String.replicate 2000 ">" ] do
        for execution in [ false; true ] do
          testCase $"{label}, execution={execution}" (fun _ ->
            let mutable result = Ok []
            let thread =
              System.Threading.Thread(
                (fun () ->
                  try
                    let diagnostics =
                      if execution then
                        match P.parseFor Validation.Script source with
                        | Ok _ -> []
                        | Error diagnostics -> diagnostics
                      else
                        (P.parse source).diagnostics
                    result <- Ok diagnostics
                  with ex ->
                    result <- Error ex),
                256 * 1024
              )
            thread.Start()
            thread.Join()
            match result with
            | Error ex -> failtest $"parser threw {ex.GetType().Name}: {ex.Message}"
            | Ok diagnostics ->
              Expect.exists
                diagnostics
                (fun d -> d.code = P.DiagnosticCode.tooDeep)
                "reports excessive nesting") ]
/// `trait` / `impl` declarations and `'a: Show` bounds. The parser keeps them as
/// their own declarations; `SourceFile.items` yields them as items with the impl's
/// method fns beneath its member path (tested here too, since that is what every
/// lowering consumes).
let private traitTests =
  let parseDecls (src : string) : List<WT.Declaration> =
    let r = P.parse src
    Expect.equal
      (r.diagnostics |> List.map (fun d -> d.message))
      []
      $"no diagnostics for: {src}"
    match r.parsed with
    | Some(WT.SourceFile { declarations = decls }) -> decls
    | other -> failtest $"parse: {other}"
  testList
    "traits"
    [ // The ORDER of an impl's members reaches `PT.TraitImpl.methods`, which is hashed in
      // list order. The F# WT used to hold declared methods and aliases in two lists and
      // re-join them declared-first, so a block that interleaved them hashed differently
      // than it was written, and differently from what the Dark parser makes of the same
      // source. One ordered list is what stops that, and this is what says so.
      testCase
        "an impl's members keep source order, declared and aliased interleaved"
        (fun _ ->
          let src =
            "impl Show for Point =\n  let a = Acme.one\n  let b(p: Point): String = \"b\"\n  let c = Acme.two\n  let d(p: Point): String = \"d\"\n"
            + ""
            + ""
            + ""
            + ""
          match parseDecls src with
          | [ WT.DImpl impl ] ->
            let names =
              impl.members
              |> List.map (fun m ->
                match m with
                | WT.IMethod fn -> fn.name.name
                | WT.IAlias a -> a.name.name)
            Expect.equal
              names
              [ "a"; "b"; "c"; "d" ]
              "the block's own order, not grouped by kind"

            // And it survives lowering, which is the half the content hash sees.
            let packaged = WT.packageImpl "Tests" [ "Acme"; "Point"; "Show" ] impl
            Expect.equal
              (packaged.methods |> List.map fst)
              [ "a"; "b"; "c"; "d" ]
              "PT.TraitImpl.methods is in the same order"
          | other -> failtest $"interleaved impl: {other}")

      testCase "a trait is its own item, with one method per signature" (fun _ ->
        let decls =
          parseDecls
            "trait Show<'a> =\n  let show(value: 'a): String\n  let describe(value: 'a, verbose: Bool): String"
        match
          SourceFile.items
            { range = WT.synthRange; declarations = decls; exprsToEval = [] }
        with
        | [ SourceFile.Trait([], t) ] ->
          Expect.equal t.name.name "Show" "trait name"
          let packaged = WT.packageTrait "Tests" [] t
          Expect.equal
            (packaged.methods |> List.map (fun m -> m.name))
            [ "show"; "describe" ]
            "one method per signature"
          Expect.equal (NEList.length packaged.methods[1].parameters) 2 "two params"
        | other -> failtest $"items: {other}")

      testCase
        "an impl is its own item at Type.Trait, with its method fns beneath it"
        (fun _ ->
          let decls =
            parseDecls
              "module Acme\nimpl Show for Point =\n  let show(p: Point): String = \"p\""
          match
            SourceFile.items
              { range = WT.synthRange; declarations = decls; exprsToEval = [] }
          with
          | [ SourceFile.Fn(fnPath, fn); SourceFile.Impl(implPath, impl) ] ->
            Expect.equal fnPath [ "Acme"; "Point"; "Show" ] "method path"
            Expect.equal fn.name.name "show" "method name"
            Expect.equal implPath [ "Acme"; "Point"; "Show" ] "impl path"
            let packaged = WT.packageImpl "Acme" [ "Point"; "Show" ] impl
            Expect.equal packaged.name.modules [ "Point" ] "impl modules"
            Expect.equal packaged.name.name "Show" "impl name"
            Expect.equal (packaged.methods |> List.map fst) [ "show" ] "the method"
            Expect.equal packaged.trait_.typ.name "Show" "the trait"
          | other -> failtest $"items: {other}")

      testCase
        "does not repeat the type segment when the module is named for the type"
        (fun _ ->
          let decls =
            parseDecls
              "module Darklang.Stdlib.Int64\nimpl Add for Int64 =\n  let add(a: Int64, b: Int64): Int64 = a"
          match
            SourceFile.items
              { range = WT.synthRange; declarations = decls; exprsToEval = [] }
          with
          | [ SourceFile.Fn(fnPath, _); SourceFile.Impl(implPath, _) ] ->
            Expect.equal
              fnPath
              [ "Darklang"; "Stdlib"; "Int64"; "Add" ]
              "method path"
            Expect.equal
              implPath
              [ "Darklang"; "Stdlib"; "Int64"; "Add" ]
              "impl path"
          | other -> failtest $"items: {other}")

      testCase
        "an alias member names an existing fn and generates no method fn"
        (fun _ ->
          let decls =
            parseDecls
              "module Darklang.Stdlib.Int64\nimpl Add for Int64 =\n  let add = Stdlib.Int64.remainder"
          match
            SourceFile.items
              { range = WT.synthRange; declarations = decls; exprsToEval = [] }
          with
          | [ SourceFile.Impl(implPath, impl) ] ->
            Expect.equal
              implPath
              [ "Darklang"; "Stdlib"; "Int64"; "Add" ]
              "impl path"
            let packaged =
              WT.packageImpl "Darklang" [ "Stdlib"; "Int64"; "Add" ] impl
            match packaged.methods with
            | [ ("add", WT.Unresolved target) ] ->
              Expect.equal
                (NEList.toList target)
                [ "Stdlib"; "Int64"; "remainder" ]
                "the alias target"
            | other -> failtest $"methods: {other}"
          | other -> failtest $"items: {other}")

      testCase
        "a conditional impl keeps its own type params and bounds on the item and its methods"
        (fun _ ->
          let decls =
            parseDecls
              "impl<'a: Show> Show for List<'a> =\n  let show(xs: List<'a>): String = \"xs\""
          match
            SourceFile.items
              { range = WT.synthRange; declarations = decls; exprsToEval = [] }
          with
          | [ SourceFile.Fn(_, fn); SourceFile.Impl(_, impl) ] ->
            Expect.equal
              (fn.typeParams |> List.map fst)
              [ "a" ]
              "the method fn is generic over the impl's param"
            Expect.equal
              (fn.bounds |> List.map (fun b -> b.param))
              [ "a" ]
              "and carries its bound"
            let packaged = WT.packageImpl "Tests" [ "List"; "Show" ] impl
            Expect.equal packaged.typeParams [ "a" ] "impl type params"
            Expect.equal
              (packaged.bounds |> List.map (fun b -> b.param))
              [ "a" ]
              "impl bounds"
          | other -> failtest $"items: {other}")

      testCase
        "diagnoses a bound that is not a trait, a default body, and a trait with no type param"
        (fun _ ->
          let msgs (src : string) =
            (P.parse src).diagnostics |> List.map (fun d -> d.code)
          Expect.equal
            (msgs "let f<'a: String> (x: 'a) : 'a = x")
            [ P.DiagnosticCode.bound ]
            "'a: String"
          Expect.equal
            (msgs "trait Show<'a> =\n  let show (v: 'a) : String = \"default\"")
            [ P.DiagnosticCode.bound ]
            "default body"
          // A method MAY declare its own type params and bounds; they are discharged where
          // the method is called.
          Expect.equal
            (msgs "trait Fun<'f> =\n  let map<'b>(v: 'f): 'b")
            []
            "method type params are allowed"
          Expect.equal
            (msgs "trait Show =\n  let show (v: Int) : String")
            [ P.DiagnosticCode.bound ]
            "trait without a type param") ]

let tests =
  testList
    "LibParser"
    [ testCase "private names reach package lookup" (fun _ ->
        for name in [ "__helper"; "__helper_v0" ] do
          let result =
            (NR.resolveFnName
              Set.empty
              PT.PackageManager.empty
              NR.OnMissing.Allow
              []
              (WT.Unresolved(NEList.singleton name))
             |> Ply.toTask)
              .Result
          Expect.equal
            result.resolved
            (Error PT.NameResolutionError.NotFound)
            $"valid private function: {name}"
        for name in [ "__Buffer"; "__Buffer_v0" ] do
          let result =
            (NR.resolveTypeName
              PT.PackageManager.empty
              NR.OnMissing.Allow
              []
              (WT.Unresolved(NEList.singleton name))
             |> Ply.toTask)
              .Result
          Expect.equal
            result.resolved
            (Error PT.NameResolutionError.NotFound)
            $"valid private type: {name}"
        let invalid =
          (NR.resolveFnName
            Set.empty
            PT.PackageManager.empty
            NR.OnMissing.Allow
            []
            (WT.Unresolved(NEList.singleton "_helper"))
           |> Ply.toTask)
            .Result
        Expect.equal
          invalid.resolved
          (Error PT.NameResolutionError.InvalidName)
          "a single underscore does not acquire the private-name convention")
      stackSafetyTests
      unwrapTests
      parserStructureTests
      internalUnitTests
      desugarTests
      callSyntaxTests
      loweringRegressionTests
      validationTests
      literalTests
      recoveryTests
      lexicalFailureTests
      goldenDiagnosticsTests
      fuzzTests
      rangeInvariantTests
      primTypeDriftTests
      traitTests
      corpusTests ]
