/// Parser structure tests. The core syntax-case coverage was adapted from
/// pbiggar/darklang-compiler's SyntaxInteropTests.fs (re-implemented against
/// this repo's `LibParser.Parser` + Expecto). The corpus gate, fuzzer,
/// range-invariant / prim-type-drift guards, and diagnostics tests are specific
/// to this parser; pretty-print round-trips live in `LibParser.RoundTrip.Tests.fs`.
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
module PT = LibExecution.ProgramTypes
module RTT = LibExecution.RuntimeTypes

// Coverage GATE: the parser must cleanly parse every real .dark package file. Files
// that are legitimately not valid Dark go on the allowlist; anything else failing to
// parse cleanly is a regression and fails this test.
// relative to packages/darklang; empty = every file must parse cleanly. An
// entry that starts parsing cleanly again is flagged stale by the rot-guard.
let private corpusAllowlist : Set<string> = Set.empty

let private corpusTests =
  testList
    "parser-corpus"
    [ testCase "the parser cleanly parses every valid .dark package file" (fun _ ->
        let root =
          [ "../packages/darklang"
            "packages/darklang"
            "/home/dark/app/packages/darklang" ]
          |> List.tryFind System.IO.Directory.Exists
        match root with
        | None -> () // package dir not found (not a CI environment) — nothing to gate
        | Some root ->
          let relOf (f : string) =
            f.Substring(root.Length).TrimStart('/', '\\').Replace('\\', '/')
          let files =
            System.IO.Directory.GetFiles(
              root,
              "*.dark",
              System.IO.SearchOption.AllDirectories
            )
          // (relPath, first-diagnostic-message) for every file that does NOT parse cleanly
          let failures =
            files
            |> Array.choose (fun f ->
              let rel = relOf f
              try
                match
                  (LibParser.Parser.parse (System.IO.File.ReadAllText f))
                    .diagnostics
                with
                | [] -> None
                | d :: _ -> Some(rel, d.message)
              with e ->
                Some(rel, "THREW: " + e.Message))
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

// The range-complete parser: every keyword/symbol/operand carries its exact source
// range — the positions the semantic-token highlighter needs.
module P = LibParser.Parser
module Tok = LibParser.Tokenizer

let private expectColumns label expected (range : Tok.TokenRange) =
  Expect.equal (range.start.column, range.end_.column) expected label

let private parserStructureTests =
  testList
    "parser-structure"
    [ testCase "captures keyword/symbol ranges for a value declaration" (fun _ ->
        // row 1 `  val x = 1 + 2`: val=2..5  x=6  ==8  1=10  +=12  2=14
        let r = P.parse "module M =\n  val x = 1 + 2"
        Expect.isEmpty r.diagnostics "no diagnostics"
        match r.parsed with
        | Some(WT.SourceFile { declarations = [ WT.DModule m ] }) ->
          match m.declarations with
          | [ WT.DValue v ] ->
            Expect.equal
              v.keywordVal.start
              { Tok.row = 1; Tok.column = 2 }
              "`val` start"
            Expect.equal
              v.keywordVal.end_
              { Tok.row = 1; Tok.column = 5 }
              "`val` end"
            Expect.equal
              v.name.range.start
              { Tok.row = 1; Tok.column = 6 }
              "name start"
            Expect.equal
              v.symbolEquals.start
              { Tok.row = 1; Tok.column = 8 }
              "`=` start"
            match v.body with
            | WT.EInfix(_,
                        (opR, WT.InfixFnCall WT.ArithmeticPlus),
                        WT.EInt(_, (_, n1)),
                        WT.EInt(_, (_, n2))) when n1 = 1I && n2 = 2I ->
              Expect.equal
                opR.start
                { Tok.row = 1; Tok.column = 12 }
                "`+` operator start"
            | other -> failtest $"unexpected value body: {other}"
          | other -> failtest $"unexpected module body: {other}"
        | other -> failtest $"unexpected: {other}")

      testCase
        "top-level `let x = …` is a let-EXPRESSION (sequences with what follows)"
        (fun _ ->
          // Per the Dark grammar, a no-param `let` at a file's top level is a script
          // statement, NOT a value declaration: it sequences with the following
          // statements (which become the let body), so a computed binding runs on
          // the interpreter rather than the constant-only package-value evaluator.
          match (P.parse "let x = 1L\nx").parsed with
          | Some(WT.SourceFile { declarations = []
                                 exprsToEval = [ WT.ELet(_,
                                                         WT.LPVariable(_, "x"),
                                                         WT.EInt64(_, (_, 1L), _),
                                                         WT.EVariable(_, "x"),
                                                         _,
                                                         _) ] }) -> ()
          | other -> failtest $"unexpected: {other}")

      testCase "parses a string literal" (fun _ ->
        match (P.parse "\"hi\"").parsed with
        | Some(WT.SourceFile { exprsToEval = [ WT.EString(_,
                                                          None,
                                                          [ WT.StringText(_, "hi") ],
                                                          _,
                                                          _) ] }) -> ()
        | other -> failtest $"unexpected: {other}")

      testCase "numeric literal ranges split digits from suffixes" (fun _ ->
        match (P.parse "-1L").parsed with
        | Some(WT.SourceFile { exprsToEval = [ WT.EInt64(_, (digits, _), suffix) ] }) ->
          expectColumns "signed digits include '-'" (0, 2) digits
          expectColumns "one-character suffix" (2, 3) suffix
        | other -> failtest $"unexpected signed int: {other}"
        match (P.parse "1uy").parsed with
        | Some(WT.SourceFile { exprsToEval = [ WT.EUInt8(_, (digits, _), suffix) ] }) ->
          expectColumns "unsigned digits" (0, 1) digits
          expectColumns "two-character suffix" (1, 3) suffix
        | other -> failtest $"unexpected unsigned int: {other}"
        match (P.parse "match x with | 1L -> x").parsed with
        | Some(WT.SourceFile { exprsToEval = [ WT.EMatch(_, _, [ arm ], _, _) ] }) ->
          match arm.pat with
          | WT.MPInt64(_, (digits, _), suffix) ->
            expectColumns "pattern digits" (15, 16) digits
            expectColumns "pattern suffix" (16, 17) suffix
          | other -> failtest $"unexpected int pattern: {other}"
        | other -> failtest $"unexpected int pattern: {other}")

      testCase "quoted literal ranges split delimiters from contents" (fun _ ->
        match (P.parse "\"hi\"").parsed with
        | Some(WT.SourceFile { exprsToEval = [ WT.EString(_,
                                                          _,
                                                          [ WT.StringText(contents,
                                                                          _) ],
                                                          openQuote,
                                                          closeQuote) ] }) ->
          expectColumns "string opening quote" (0, 1) openQuote
          expectColumns "string contents" (1, 3) contents
          expectColumns "string closing quote" (3, 4) closeQuote
        | other -> failtest $"unexpected string: {other}"
        match (P.parse "'x'").parsed with
        | Some(WT.SourceFile { exprsToEval = [ WT.EChar(_,
                                                        Some(contents, _),
                                                        openQuote,
                                                        closeQuote) ] }) ->
          expectColumns "char opening quote" (0, 1) openQuote
          expectColumns "char contents" (1, 2) contents
          expectColumns "char closing quote" (2, 3) closeQuote
        | other -> failtest $"unexpected char: {other}"
        match (P.parse "\"\"\"hi\"\"\"").parsed with
        | Some(WT.SourceFile { exprsToEval = [ WT.EString(_,
                                                          _,
                                                          [ WT.StringText(contents,
                                                                          _) ],
                                                          openQuote,
                                                          closeQuote) ] }) ->
          expectColumns "triple-string opening quote" (0, 3) openQuote
          expectColumns "triple-string contents" (3, 5) contents
          expectColumns "triple-string closing quote" (5, 8) closeQuote
        | other -> failtest $"unexpected triple string: {other}"
        match (P.parse "match x with | 'y' -> x").parsed with
        | Some(WT.SourceFile { exprsToEval = [ WT.EMatch(_, _, [ arm ], _, _) ] }) ->
          match arm.pat with
          | WT.MPChar(_, Some(contents, _), openQuote, closeQuote) ->
            expectColumns "pattern opening quote" (15, 16) openQuote
            expectColumns "pattern contents" (16, 17) contents
            expectColumns "pattern closing quote" (17, 18) closeQuote
          | other -> failtest $"unexpected char pattern: {other}"
        | other -> failtest $"unexpected char pattern: {other}")

      testCase "parses a qualified function call" (fun _ ->
        match (P.parse "Stdlib.Int64.add 1L 2L").parsed with
        | Some(WT.SourceFile { exprsToEval = [ WT.EApply(_,
                                                         WT.EFnName(_, q),
                                                         _,
                                                         [ _; _ ]) ] }) ->
          Expect.equal q.fn.name "add" "callee fn name"
          Expect.equal (List.length q.modules) 2 "two module segments"
        | other -> failtest $"unexpected: {other}")

      testCase "parses if / list / tuple" (fun _ ->
        match (P.parse "if true then 1L else 2L").parsed with
        | Some(WT.SourceFile { exprsToEval = [ WT.EIf(_,
                                                      WT.EBool(_, true),
                                                      _,
                                                      Some _,
                                                      _,
                                                      _,
                                                      Some _) ] }) -> ()
        | other -> failtest $"if: {other}"
        match (P.parse "[1L; 2L; 3L]").parsed with
        | Some(WT.SourceFile { exprsToEval = [ WT.EList(_, elems, _, _) ] }) ->
          Expect.equal (List.length elems) 3 "three list elements"
        | other -> failtest $"list: {other}"
        match (P.parse "(1L, 2L)").parsed with
        | Some(WT.SourceFile { exprsToEval = [ WT.ETuple(_, _, _, _, rest, _, _) ] }) ->
          Expect.equal (List.length rest) 0 "2-tuple has empty rest"
        | other -> failtest $"tuple: {other}")

      testCase "lowercase `p.foo` is field access, not a qualified name" (fun _ ->
        match (P.parse "p.foo").parsed with
        | Some(WT.SourceFile { exprsToEval = [ WT.ERecordFieldAccess(_,
                                                                     WT.EVariable(_,
                                                                                  "p"),
                                                                     (_, "foo"),
                                                                     _) ] }) -> ()
        | other -> failtest $"field: {other}")

      testCase "parses a lambda" (fun _ ->
        match (P.parse "fun x y -> x").parsed with
        | Some(WT.SourceFile { exprsToEval = [ WT.ELambda(_,
                                                          [ WT.LPVariable(_, "x")
                                                            WT.LPVariable(_, "y") ],
                                                          WT.EVariable(_, "x"),
                                                          _,
                                                          _) ] }) -> ()
        | other -> failtest $"lambda: {other}")

      testCase "parses a record literal" (fun _ ->
        match (P.parse "Point { x = 1L; y = 2L }").parsed with
        | Some(WT.SourceFile { exprsToEval = [ WT.ERecord(_, typeName, fields, _, _) ] }) ->
          Expect.equal typeName.typ.name "Point" "record type name"
          match fields with
          | [ (_, (_, "x"), _); (_, (_, "y"), _) ] -> ()
          | other -> failtest $"fields: {other}"
        | other -> failtest $"record: {other}")

      testCase "parses a function declaration" (fun _ ->
        match (P.parse "let add (x: Int64) (y: Int64) : Int64 = x + y").parsed with
        | Some(WT.SourceFile { declarations = [ WT.DFunction f ]; exprsToEval = [] }) ->
          Expect.equal f.name.name "add" "fn name"
          Expect.equal (List.length f.parameters) 2 "two params"
          match f.returnType with
          | WT.TInt64 _ -> ()
          | other -> failtest $"return type: {other}"
        | other -> failtest $"fn decl: {other}")

      testCase "parses a value declaration" (fun _ ->
        match (P.parse "module M =\n  val pi = 3").parsed with
        | Some(WT.SourceFile { declarations = [ WT.DModule m ]; exprsToEval = [] }) ->
          match m.declarations with
          | [ WT.DValue v ] -> Expect.equal v.name.name "pi" "value name"
          | other -> failtest $"value decl: {other}"
        | other -> failtest $"value decl: {other}")

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

      testCase "package roots diagnose let values without an EOF cascade" (fun _ ->
        let source =
          "let chars = \"abc\"\n\nlet length (s: String) : Int = Stdlib.String.length s"
        match P.parseFor Validation.Package source with
        | Error [ diagnostic ] ->
          Expect.equal
            diagnostic.message
            "Module value declarations must use 'val'; 'let' is reserved for functions and local bindings"
            "focused package-value diagnostic"
          Expect.isFalse
            (diagnostic.message.Contains "end of file")
            "the package-mode diagnostic identifies the root cause"
        | other -> failtest $"expected one diagnostic, got {other}")

      testCase "val cannot declare a function" (fun _ ->
        let result = P.parse "val f (x: Int64) : Int64 = x"
        Expect.exists
          result.diagnostics
          (fun diagnostic ->
            diagnostic.message = "'val' declares a value and cannot have function parameters")
          "val function diagnostic")

      testCase "file-level module header wraps the file's declarations" (fun _ ->
        match
          (P.parse "module Darklang.Foo\nlet add (x: Int64) : Int64 = x").parsed
        with
        | Some(WT.SourceFile { declarations = [ WT.DModule m ] }) ->
          Expect.equal (snd m.name) "Darklang.Foo" "module name"
          match m.declarations with
          | [ WT.DFunction f ] ->
            Expect.equal f.name.name "add" "fn name nested under module"
          | other -> failtest $"module body: {other}"
        | other -> failtest $"module file: {other}")

      testCase "nested `module X =` block nests its indented declarations" (fun _ ->
        match
          (P.parse "module Darklang.Foo\nmodule Bar =\n  val a = 1L\nval b = 2L")
            .parsed
        with
        | Some(WT.SourceFile { declarations = [ WT.DModule outer ] }) ->
          // outer = Darklang.Foo, containing: submodule Bar (with `a`) + value `b`
          match outer.declarations with
          | [ WT.DModule bar; WT.DValue b ] ->
            Expect.equal (snd bar.name) "Bar" "submodule name"
            Expect.equal b.name.name "b" "sibling value after submodule"
            match bar.declarations with
            | [ WT.DValue a ] ->
              Expect.equal a.name.name "a" "value inside submodule"
            | other -> failtest $"submodule body: {other}"
          | other -> failtest $"outer body: {other}"
        | other -> failtest $"nested module file: {other}")

      testCase "parses type declarations (record / enum / alias)" (fun _ ->
        match (P.parse "type Point = { x: Int64; y: Int64 }").parsed with
        | Some(WT.SourceFile { declarations = [ WT.DType { definition = WT.TDRecord fields } ] }) ->
          Expect.equal (List.length fields) 2 "two record fields"
        | other -> failtest $"record type: {other}"
        match (P.parse "type Color = Red | Green | Blue").parsed with
        | Some(WT.SourceFile { declarations = [ WT.DType { definition = WT.TDEnum cases } ] }) ->
          Expect.equal (List.length cases) 3 "three enum cases"
        | other -> failtest $"enum type: {other}"
        match (P.parse "type Id = Int64").parsed with
        | Some(WT.SourceFile { declarations = [ WT.DType { definition = WT.TDAlias _ } ] }) ->
          ()
        | other -> failtest $"alias: {other}")

      testCase "custom generic type range includes closing angle bracket" (fun _ ->
        match (P.parse "type X = Result<Int64, String>").parsed with
        | Some(WT.SourceFile { declarations = [ WT.DType { definition = definition } ] }) ->
          match definition with
          | WT.TDAlias(WT.TCustom custom) ->
            Expect.equal custom.range.end_.column 30 "range ends after '>'"
          | other -> failtest $"unexpected definition: {other}"
        | other -> failtest $"unexpected type: {other}")

      testCase "parses enum constructors (bare + qualified)" (fun _ ->
        match (P.parse "Some 5L").parsed with
        | Some(WT.SourceFile { exprsToEval = [ WT.EEnum(_, _, (_, "Some"), [ _ ], _) ] }) ->
          ()
        | other -> failtest $"bare ctor: {other}"
        match (P.parse "Stdlib.Result.Result.Ok 5L").parsed with
        | Some(WT.SourceFile { exprsToEval = [ WT.EEnum(_,
                                                        typeName,
                                                        (_, "Ok"),
                                                        [ _ ],
                                                        _) ] }) ->
          Expect.equal typeName.typ.name "Result" "enum type name"
          Expect.equal (List.length typeName.modules) 2 "two module segments"
        | other -> failtest $"qualified ctor: {other}")

      testCase
        "parenthesized constructor range includes closing parenthesis"
        (fun _ ->
          match (P.parse "Pair(1L, 2L)").parsed with
          | Some(WT.SourceFile { exprsToEval = [ expression ] }) ->
            Expect.equal
              (WT.exprRange expression).end_.column
              12
              "range ends after ')'"
          | other -> failtest $"unexpected constructor: {other}")

      testCase "parses match (enum / cons / or patterns)" (fun _ ->
        match (P.parse "match x with | Ok v -> v | Error e -> e").parsed with
        | Some(WT.SourceFile { exprsToEval = [ WT.EMatch(_, _, cases, _, _) ] }) ->
          match cases with
          | [ { pat = WT.MPEnum(_, (_, "Ok"), [ WT.MPVariable(_, "v") ]) }
              { pat = WT.MPEnum(_, (_, "Error"), _) } ] -> ()
          | other -> failtest $"cases: {other}"
        | other -> failtest $"match: {other}"
        match (P.parse "match xs with | [] -> 0L | h :: t -> h").parsed with
        | Some(WT.SourceFile { exprsToEval = [ WT.EMatch(_,
                                                         _,
                                                         [ _
                                                           { pat = WT.MPListCons _ } ],
                                                         _,
                                                         _) ] }) -> ()
        | other -> failtest $"cons match: {other}"
        match (P.parse "match n with | 0L | 1L -> true | _ -> false").parsed with
        | Some(WT.SourceFile { exprsToEval = [ WT.EMatch(_,
                                                         _,
                                                         [ { pat = WT.MPOr(_,
                                                                           [ _; _ ]) }
                                                           _ ],
                                                         _,
                                                         _) ] }) -> ()
        | other -> failtest $"or match: {other}")

      testCase "match pattern precedence: `|` is looser than `,`" (fun _ ->
        // `1, 2 | 3, 4` is `(1,2) | (3,4)` — an or of two tuples, NOT a 3-tuple.
        match
          (P.parse "match p with | 1L, 2L | 3L, 4L -> true | _ -> false").parsed
        with
        | Some(WT.SourceFile { exprsToEval = [ WT.EMatch(_, _, cases, _, _) ] }) ->
          match cases with
          | { pat = WT.MPOr(_,
                            [ WT.MPTuple(_, _, _, _, r1, _, _)
                              WT.MPTuple(_, _, _, _, r2, _, _) ]) } :: _ ->
            Expect.equal (List.length r1) 0 "left alternative is a 2-tuple"
            Expect.equal (List.length r2) 0 "right alternative is a 2-tuple"
          | { pat = other } :: _ ->
            failtest $"`1,2 | 3,4` should be MPOr[MPTuple;MPTuple], got: {other}"
          | [] -> failtest "no cases"
        | other -> failtest $"tuple-or match: {other}"
        // mixed: `1 | 2, 3` is `1 | (2, 3)`
        match
          (P.parse "match p with | 1L | 2L, 3L -> true | _ -> false").parsed
        with
        | Some(WT.SourceFile { exprsToEval = [ WT.EMatch(_, _, cases, _, _) ] }) ->
          match cases with
          | { pat = WT.MPOr(_, [ WT.MPInt64 _; WT.MPTuple _ ]) } :: _ -> ()
          | { pat = other } :: _ ->
            failtest $"`1 | 2,3` should be MPOr[int; tuple], got: {other}"
          | [] -> failtest "no cases"
        | other -> failtest $"mixed match: {other}"
        // parens still make a real tuple (single-element `rest`) — grouping preserved
        match (P.parse "match p with | (1L, 2L) -> true | _ -> false").parsed with
        | Some(WT.SourceFile { exprsToEval = [ WT.EMatch(_,
                                                         _,
                                                         { pat = WT.MPTuple(_,
                                                                            _,
                                                                            _,
                                                                            _,
                                                                            rest,
                                                                            _,
                                                                            _) } :: _,
                                                         _,
                                                         _) ] }) ->
          Expect.equal (List.length rest) 0 "parenthesized 2-tuple"
        | other -> failtest $"paren tuple match: {other}"
        // enum-ctor fields: a bare `,` separates FIELDS, not a tuple (unchanged)
        match (P.parse "match x with | Pair(a, b) -> a | _ -> 0L").parsed with
        | Some(WT.SourceFile { exprsToEval = [ WT.EMatch(_,
                                                         _,
                                                         { pat = WT.MPEnum(_,
                                                                           (_,
                                                                            "Pair"),
                                                                           [ _; _ ]) } :: _,
                                                         _,
                                                         _) ] }) -> ()
        | other -> failtest $"enum-field match: {other}")

      testCase
        "unary minus binds looser than application: `-f x` is `-(f x)`"
        (fun _ ->
          // `-f x` → negate applied to (f x), NOT (negate f) applied to x
          match (P.parse "-f x").parsed with
          | Some(WT.SourceFile { exprsToEval = [ WT.EApply(_,
                                                           WT.EFnName(_, neg),
                                                           _,
                                                           [ WT.EApply(_,
                                                                       _,
                                                                       _,
                                                                       [ _ ]) ]) ] }) ->
            Expect.equal
              neg.fn.name
              "negate"
              "outer call is negate, over the application"
          | other -> failtest $"`-f x` should be negate(f x), got: {other}"
          // `-x` still just negates the variable
          match (P.parse "-x").parsed with
          | Some(WT.SourceFile { exprsToEval = [ WT.EApply(_,
                                                           WT.EFnName(_, neg),
                                                           _,
                                                           [ WT.EVariable(_, "x") ]) ] }) ->
            Expect.equal neg.fn.name "negate" "negate x"
          | other -> failtest $"`-x`: {other}"
          // infix still binds looser than the minus: `-a + b` is `(-a) + b`
          match (P.parse "-a + b").parsed with
          | Some(WT.SourceFile { exprsToEval = [ WT.EInfix(_,
                                                           _,
                                                           WT.EApply(_,
                                                                     WT.EFnName(_, _),
                                                                     _,
                                                                     [ WT.EVariable(_,
                                                                                    "a") ]),
                                                           WT.EVariable(_, "b")) ] }) ->
            ()
          | other -> failtest $"`-a + b` should be (negate a) + b, got: {other}")

      testCase "parses pipe + boolean operators" (fun _ ->
        match (P.parse "xs |> Stdlib.List.map f |> Stdlib.List.length").parsed with
        | Some(WT.SourceFile { exprsToEval = [ WT.EPipe(_,
                                                        WT.EVariable(_, "xs"),
                                                        [ (_, WT.EPipeFnCall _)
                                                          (_, WT.EPipeFnCall _) ]) ] }) ->
          ()
        | other -> failtest $"pipe: {other}"
        match (P.parse "a && b || c").parsed with
        | Some(WT.SourceFile { exprsToEval = [ WT.EInfix(_,
                                                         (_, WT.BinOp WT.BinOpOr),
                                                         WT.EInfix(_,
                                                                   (_,
                                                                    WT.BinOp WT.BinOpAnd),
                                                                   _,
                                                                   _),
                                                         _) ] }) -> ()
        | other -> failtest $"bool: {other}")

      testCase "parses type variables in a generic signature" (fun _ ->
        match (P.parse "let f (x: 'a) : 'a = x").parsed with
        | Some(WT.SourceFile { declarations = [ WT.DFunction f ] }) ->
          match f.returnType, f.parameters with
          | WT.TVariable(_, _, (_, "a")),
            [ WT.FPNormal(_, _, WT.TVariable(_, _, (_, "a")), _, _, _) ] -> ()
          | other -> failtest $"type var: {other}"
        | other -> failtest $"generic fn: {other}")

      testCase "parses function and tuple types" (fun _ ->
        match (P.parse "let f (g: Int64 -> String) : Int64 = 1L").parsed with
        | Some(WT.SourceFile { declarations = [ WT.DFunction { parameters = [ WT.FPNormal(_,
                                                                                          _,
                                                                                          WT.TFn(_,
                                                                                                 [ _ ],
                                                                                                 _),
                                                                                          _,
                                                                                          _,
                                                                                          _) ] } ] }) ->
          ()
        | other -> failtest $"fn type: {other}"
        match (P.parse "let f (p: (String * Int64)) : Int64 = 1L").parsed with
        | Some(WT.SourceFile { declarations = [ WT.DFunction { parameters = [ WT.FPNormal(_,
                                                                                          _,
                                                                                          WT.TTuple _,
                                                                                          _,
                                                                                          _,
                                                                                          _) ] } ] }) ->
          ()
        | other -> failtest $"tuple type: {other}"
        match
          (P.parse "let f (x: Stdlib.Option.Option<String>) : Int64 = 1L").parsed
        with
        | Some(WT.SourceFile { declarations = [ WT.DFunction { parameters = [ WT.FPNormal(_,
                                                                                          _,
                                                                                          WT.TCustom q,
                                                                                          _,
                                                                                          _,
                                                                                          _) ] } ] }) ->
          match q.typeArgs with
          | [ WT.TString _ ] -> ()
          | other -> failtest $"typeArgs: {other}"
        | other -> failtest $"custom type-args: {other}")

      testCase "parses multi-statement blocks (EStatement)" (fun _ ->
        match
          (P.parse "let f () : Unit =\n  Stdlib.printLine \"hi\"\n  ()").parsed
        with
        | Some(WT.SourceFile { declarations = [ WT.DFunction { body = WT.EStatement(_,
                                                                                    _,
                                                                                    WT.EUnit _) } ] }) ->
          ()
        | other -> failtest $"block: {other}")

      testCase "parses string interpolation" (fun _ ->
        match (P.parse "$\"hi {name}!\"").parsed with
        | Some(WT.SourceFile { exprsToEval = [ WT.EString(_, Some _, contents, _, _) ] }) ->
          match contents with
          | [ WT.StringText _
              WT.StringInterpolation(_, WT.EVariable(_, "name"), _, _)
              WT.StringText _ ] -> ()
          | other -> failtest $"contents: {other}"
        | other -> failtest $"interp: {other}") ]

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
  let emptyBuiltins : RTT.Builtins =
    { values = Dictionary.empty (); fns = Dictionary.empty () }
  let ctx : WT2PT.Context =
    { currentFnName = None; argMap = Map.empty; localBindings = Set.empty }
  (WT2PT.Expr.toPT
    emptyBuiltins
    PT.PackageManager.empty
    NR.OnMissing.Allow
    PT.mainBranchId
    []
    ctx
    e
   |> Ply.toTask)
    .Result

let private toPTWithInModule
  (currentModule : List<string>)
  (pm : PT.PackageManager)
  (context : WT2PT.Context)
  (e : WT.Expr)
  : PT.Expr =
  let emptyBuiltins : RTT.Builtins =
    { values = Dictionary.empty (); fns = Dictionary.empty () }
  (WT2PT.Expr.toPT
    emptyBuiltins
    pm
    NR.OnMissing.Allow
    PT.mainBranchId
    currentModule
    context
    e
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
        // `XDB` parses as a nullary EEnum; WT2PT rewrites bare EEnum → EVariable so
        // name resolution can pick enum-case / DB / value.
        match toPT (lowerExpr "XDB") with
        | PT.EVariable(_, "XDB") -> ()
        | other -> failtest $"expected EVariable XDB, got: {other}")
      testCase "a statement sequence lowers to EStatement" (fun _ ->
        match toPT (fnBody "let f () : Unit =\n  foo ()\n  bar ()") with
        | PT.EStatement(_, PT.EApply _, PT.EApply _) -> ()
        | other -> failtest $"expected EStatement(a, b), got: {other}")
      testCase "lowering rejects a WrittenTypes recovery hole" (fun _ ->
        Expect.throws
          (fun () -> toPT (WT.EError WT.synthRange) |> ignore<PT.Expr>)
          "recovery holes must not enter ProgramTypes")
      testCase
        "normal and pipe lambdas drop blank placeholders consistently"
        (fun _ ->
          match toPT (lowerExpr "fun x ___ -> x") with
          | PT.ELambda(_, pats, _) ->
            Expect.equal (NEList.length pats) 1 "normal lambda arity"
          | other -> failtest $"expected lambda, got {other}"
          match toPT (lowerExpr "1L |> fun x ___ -> x") with
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
        match toPT (lowerExpr "Dict { a = 1L; b = 2L }") with
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
          fun (_, _) ->
            Prelude.uply { return Some(PT.FQFnName.package "global-map") } }
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
              (lowerExpr "fun x -> map x")
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
              (lowerExpr "let map = (fun x -> map x) in map 1L")
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

let private valueAnnotationTests =
  testList
    "value-annotations"
    [ testCase "value annotations are rejected instead of discarded" (fun _ ->
        for source in [ "let x : String = 1L in x"; "val x : String = 1L" ] do
          Expect.exists
            (P.parse source).diagnostics
            (fun diagnostic ->
              diagnostic.message = "Value annotations are not supported")
            $"annotation diagnostic for {source}") ]

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
            "match x with | Some value -> value | None -> 0L"
        with
        | Error diagnostics -> failtest $"unexpected diagnostics: {diagnostics}"
        | Ok validated ->
          Expect.equal
            (Validation.ValidatedSourceFile.mode validated)
            Validation.Script
            "validated mode")
      testCase "validate gates a manually supplied syntax tree" (fun _ ->
        let sf = sourceFile "match x with | Some value -> value | None -> 0L"
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
          [ ("fun x x -> x", Validation.DuplicateBinder)
            ("let (x, x) = (1L, 2L) in x", Validation.DuplicateBinder)
            ("let f (x: Int64) (x: Int64) : Int64 = x", Validation.DuplicateBinder)
            ("match x with | Some a | None -> a", Validation.OrBindingMismatch)
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
      testCase "DB syntax is also classified after parsing" (fun _ ->
        let sf = sourceFile "[<DB>] type UserDB = String"
        Expect.exists
          (validationIssues Validation.Script sf)
          (fun issue -> issue.code = Validation.DBMode)
          "script classification rejects DB declarations"
        Expect.isEmpty
          (validationIssues Validation.Test sf)
          "test classification accepts DB declarations")
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
    [ testCase
        "positive min-magnitude sized ints diagnose (no silent wrap)"
        (fun _ ->
          // `128y` is NOT -128; the magnitude only fits when negated
          for src in
            [ "128y"
              "32768s"
              "2147483648l"
              "9223372036854775808L"
              "170141183460469231731687303715884105728Q" ] do
            let r = P.parse src
            Expect.isNonEmpty
              r.diagnostics
              $"expected out-of-range diagnostic for {src}")
      testCase "negated min-magnitude sized ints parse cleanly" (fun _ ->
        let r = P.parse "-128y"
        Expect.isEmpty r.diagnostics "no diagnostics for -128y"
        match r.parsed with
        | Some(WT.SourceFile { exprsToEval = [ WT.EInt8(_, (_, v), _) ] }) ->
          Expect.equal v System.SByte.MinValue "-128y is Int8.MinValue"
        | other -> failtest $"unexpected: {other}"
        Expect.isEmpty (P.parse "-9223372036854775808L").diagnostics "Int64 min"
        Expect.isEmpty
          (P.parse "match x with | -128y -> 1L | _ -> 2L").diagnostics
          "pattern Int64 min")
      testCase
        "empty record / enum type is diagnosed, not silently accepted"
        (fun _ ->
          // these lower to a `"_"` placeholder; the diagnostic makes that honest recovery
          for src in [ "type X = {}"; "type X = |" ] do
            Expect.isNonEmpty
              (P.parse src).diagnostics
              $"expected a diagnostic for `{src}`"
          // a non-empty record/enum still parses cleanly
          Expect.isEmpty
            (P.parse "type X = { a: Int64 }").diagnostics
            "non-empty record"
          Expect.isEmpty (P.parse "type X = A | B").diagnostics "non-empty enum")
      testCase "exponent float literals lower exponent-free" (fun _ ->
        // `1e300` used to put "1E+300" in the whole part → makeFloat crash
        let r = P.parse "1e300"
        Expect.isEmpty r.diagnostics "1e300 parses cleanly"
        match r.parsed with
        | Some(WT.SourceFile { exprsToEval = [ WT.EFloat(_, false, w, "0") ] }) ->
          Expect.equal w.Length 301 "301 decimal digits"
          Expect.isTrue (w |> Seq.forall System.Char.IsDigit) "digits only"
        | other -> failtest $"unexpected: {other}"
        match (P.parse "1e-7").parsed with
        | Some(WT.SourceFile { exprsToEval = [ WT.EFloat(_, false, "0", "0000001") ] }) ->
          ()
        | other -> failtest $"1e-7: {other}"
        // tiny plain-decimal literals need an exponent in the double's R-form too
        match (P.parse "0.00000001").parsed with
        | Some(WT.SourceFile { exprsToEval = [ WT.EFloat(_, false, "0", "00000001") ] }) ->
          ()
        | other -> failtest $"0.00000001: {other}"
        // ordinary floats keep the existing representation
        match (P.parse "0.2").parsed with
        | Some(WT.SourceFile { exprsToEval = [ WT.EFloat(_, false, "0", "2") ] }) ->
          ()
        | other -> failtest $"0.2: {other}")
      testCase "sized-int fields parse in enum patterns" (fun _ ->
        // canStartPattern drift: `Ok 5y` worked as an expr but not as a pattern
        let r = P.parse "match x with | Ok 5y -> 1L | Ok -4y -> 2L | _ -> 3L"
        Expect.isEmpty r.diagnostics "sized-int enum-pattern fields parse cleanly"
        match r.parsed with
        | Some(WT.SourceFile { exprsToEval = [ WT.EMatch(_, _, cases, _, _) ] }) ->
          match cases with
          | [ { pat = WT.MPEnum(_, (_, "Ok"), [ WT.MPInt8(_, (_, 5y), _) ]) }
              { pat = WT.MPEnum(_, (_, "Ok"), [ WT.MPInt8(_, (_, -4y), _) ]) }
              _ ] -> ()
          | other -> failtest $"cases: {other}"
        | other -> failtest $"match: {other}") ]

/// Offside / application-shape rules.
let private offsideTests =
  testList
    "offside"
    [ testCase "explicit type arguments in a call expression" (fun _ ->
        match lowerExpr "Stdlib.Json.parse<Int64> x" with
        | WT.EApply(_, _, [ WT.TInt64 _ ], _) -> ()
        | other -> failtest $"expected a call with one type arg, got: {other}")
      testCase "less-than is still a comparison, not type args" (fun _ ->
        let r = P.parse "a < b"
        Expect.isEmpty r.diagnostics "parses cleanly as a comparison"
        match lowerExpr "a < b" with
        | WT.EInfix(_, (_, WT.InfixFnCall WT.ComparisonLessThan), _, _) -> ()
        | other -> failtest $"expected a comparison, got: {other}")
      testCase "bare statement sequence is an EStatement" (fun _ ->
        // the parse tree keeps `a\n b` as EStatement; the discarding-let folding
        // happens later in WrittenTypesToProgramTypes.
        match fnBody "let f () : Unit =\n  foo ()\n  bar ()" with
        | WT.EStatement(_, WT.EApply _, WT.EApply _) -> ()
        | other ->
          failtest $"expected a statement sequence of two calls, got: {other}")
      testCase "newline-delimited let-chain nests, scoping over the rest" (fun _ ->
        match fnBody "let f () : Int64 =\n  let x = 1L\n  let y = 2L\n  x" with
        | WT.ELet(_,
                  WT.LPVariable(_, "x"),
                  _,
                  WT.ELet(_, WT.LPVariable(_, "y"), _, WT.EVariable(_, "x"), _, _),
                  _,
                  _) -> ()
        | other -> failtest $"expected nested ELet, got: {other}")
      testCase
        "indented continuation stays one application (not a sequence)"
        (fun _ ->
          match fnBody "let f () : Int64 =\n  g\n    a\n    b" with
          | WT.EApply(_, _, _, args) ->
            Expect.equal (List.length args) 2 "g applied to a and b"
          | other -> failtest $"expected a single application, got: {other}")
      testCase "newline-separated list elements" (fun _ ->
        match lowerExpr "[ 1L\n  2L\n  3L ]" with
        | WT.EList(_, elems, _, _) ->
          Expect.equal (List.length elems) 3 "three list elements"
        | other -> failtest $"expected a 3-element list, got: {other}")
      testCase "nested function definition lowers to a name-bound lambda" (fun _ ->
        match
          fnBody "let f () : Int64 =\n  let g (x: Int64) : Int64 = x + 1L\n  g 5L"
        with
        | WT.ELet(_,
                  WT.LPVariable(_, "g"),
                  WT.ELambda(_,
                             _,
                             WT.EInfix(_,
                                       (_, WT.InfixFnCall WT.ArithmeticPlus),
                                       _,
                                       _),
                             _,
                             _),
                  WT.EApply _,
                  _,
                  _) -> ()
        | other -> failtest $"expected let-bound lambda, got: {other}") ]

/// Type grammar (via the type-alias path).
let private typeTests =
  testList
    "types"
    [ testCase "primitives" (fun _ ->
        match parsedType "Int64" with
        | WT.TInt64 _ -> ()
        | o -> failtest $"Int64: {o}"
        match parsedType "String" with
        | WT.TString _ -> ()
        | o -> failtest $"String: {o}"
        match parsedType "Bool" with
        | WT.TBool _ -> ()
        | o -> failtest $"Bool: {o}")
      testCase "List<Int64>" (fun _ ->
        match parsedType "List<Int64>" with
        | WT.TList(_, _, _, WT.TInt64 _, _) -> ()
        | o -> failtest $"{o}")
      testCase "Dict<String>" (fun _ ->
        match parsedType "Dict<String>" with
        | WT.TDict(_, _, _, WT.TString _, _) -> ()
        | o -> failtest $"{o}")
      testCase "tuple type (star-separated)" (fun _ ->
        match parsedType "Int64 * String" with
        | WT.TTuple(_, WT.TInt64 _, _, WT.TString _, [], _, _) -> ()
        | o -> failtest $"{o}")
      testCase "function type" (fun _ ->
        match parsedType "Int64 -> String" with
        | WT.TFn(_, [ (WT.TInt64 _, _) ], WT.TString _) -> ()
        | o -> failtest $"{o}")
      testCase "nested generics close with >>" (fun _ ->
        match parsedType "List<List<Int64>>" with
        | WT.TList(_, _, _, WT.TList(_, _, _, WT.TInt64 _, _), _) -> ()
        | o -> failtest $"{o}")
      testCase "nested generics in a tuple (>> must not swallow the *)" (fun _ ->
        // regression: `List<List<A>> * List<List<B>>` mis-parsed as `List<List<A> * …>`
        let inner (isExpected : WT.TypeReference -> bool) =
          function
          | WT.TList(_, _, _, WT.TList(_, _, _, t, _), _) -> isExpected t
          | _ -> false
        match
          parsedType "List<List<Int64>> * List<List<String>> * List<List<Bool>>"
        with
        | WT.TTuple(_, a, _, b, [ (_, c) ], _, _) when
          inner
            (function
            | WT.TInt64 _ -> true
            | _ -> false)
            a
          && inner
            (function
            | WT.TString _ -> true
            | _ -> false)
            b
          && inner
            (function
            | WT.TBool _ -> true
            | _ -> false)
            c
          ->
          ()
        | o -> failtest $"{o}")
      testCase "generic syntax is explicit and unambiguous" (fun _ ->
        Expect.isEmpty
          (P.parse "type Pair<'a, 'b> = 'a * 'b").diagnostics
          "valid generic declaration"
        for source in
          [ "type Box <'a> = 'a"
            "type Box<a> = a"
            "type Pair<'a 'b> = 'a"
            "type Box<> = Int64"
            "type Box<'a,> = 'a"
            "type Box = List < Int64 >"
            "type Box = Dict < String >"
            "type Box = Stdlib.Option.Option < Int64 >" ] do
          Expect.isNonEmpty
            (P.parse source).diagnostics
            $"expected generic-syntax diagnostic for {source}")
      testCase "qualified custom type with arg" (fun _ ->
        match parsedType "Stdlib.Option.Option<Int64>" with
        | WT.TCustom q ->
          Expect.equal
            (q.modules |> List.map (fun (m, _) -> m.name))
            [ "Stdlib"; "Option" ]
            "modules"
          Expect.equal q.typ.name "Option" "type name"
          match q.typeArgs with
          | [ WT.TInt64 _ ] -> ()
          | o -> failtest $"typeArgs: {o}"
        | other -> failtest $"unexpected: {other}") ]

/// Error recovery: the parser recovers from parse errors without looping or
/// consuming delimiters, filling holes with a placeholder (a bool literal).
let private recoveryTests =
  testList
    "recovery"
    [ testCase "reserved expression syntax diagnoses explicitly" (fun _ ->
        for source in [ "<<"; ">>"; "&"; "|||"; "~~~"; "!"; "..." ] do
          Expect.exists
            (P.parse source).diagnostics
            (fun diagnostic ->
              diagnostic.message.Contains "reserved but not supported")
            $"unsupported diagnostic for {source}")

      testCase "rest patterns diagnose explicitly" (fun _ ->
        Expect.exists
          (P.parse "match xs with | ... -> xs").diagnostics
          (fun diagnostic ->
            diagnostic.message.Contains
              "rest patterns are reserved but not supported")
          "unsupported rest-pattern diagnostic")

      testCase "def remains an ordinary identifier" (fun _ ->
        let result = P.parse "def"
        Expect.isEmpty result.diagnostics "def is not reserved"
        match result.parsed with
        | Some(WT.SourceFile { exprsToEval = [ WT.EVariable(_, "def") ] }) -> ()
        | other -> failtest $"unexpected def parse: {other}")

      testCase
        "rec, private, and internal are identifiers, not let modifiers"
        (fun _ ->
          for source in
            [ "module M =\n  let rec f (x: Int64) : Int64 = x"
              "module M =\n  let private f (x: Int64) : Int64 = x"
              "module M =\n  let internal f (x: Int64) : Int64 = x"
              "if true then let private f (x: Int64) : Int64 = x in f 1L else 0L" ] do
            Expect.isNonEmpty
              (P.parse source).diagnostics
              $"invalid binding form: {source}"

          for source in
            [ "let rec = 1L in rec"
              "let private = 1L in private"
              "let internal = 1L in internal" ] do
            Expect.isEmpty
              (P.parse source).diagnostics
              $"valid identifier: {source}")

      testCase "package function parameters cannot be blank" (fun _ ->
        let invalid = P.parse "let f (___: Int64) (x: Int64) : Int64 = x"
        Expect.exists
          invalid.diagnostics
          (fun diagnostic ->
            diagnostic.message.Contains "Blank parameter '___' is not allowed")
          "blank package parameter diagnostic"

        for source in [ "let f () : Unit = ()"; "fun ___ -> 1L" ] do
          Expect.isEmpty
            (P.parse source).diagnostics
            $"valid ignored parameter form: {source}")

      testCase "same-line collection items require separators" (fun _ ->
        for source in
          [ "[1L 2L]"
            "match xs with | [a b] -> a"
            "Pair(1L 2L)"
            "match pair with | Pair(a b) -> a"
            "Person { a = 1L b = 2L }"
            "Dict { a = 1L b = 2L }"
            "{ value with a = 1L b = 2L }"
            "type R = { a: Int64 b: String }" ] do
          Expect.isNonEmpty
            (P.parse source).diagnostics
            $"expected separator diagnostic for {source}"

        for source in
          [ "[1L, 2L]"
            "match xs with | [a; b] -> a"
            "Pair(1L, 2L)"
            "match pair with | Pair(a, b) -> a"
            "Person { a = 1L; b = 2L }"
            "Dict { a = 1L; b = 2L }"
            "{ value with a = 1L; b = 2L }"
            "type R = { a: Int64; b: String }" ] do
          Expect.isEmpty
            (P.parse source).diagnostics
            $"valid separators parse cleanly for {source}")

      testCase "empty record update `{ r with }` is rejected" (fun _ ->
        // an empty update lowers to a degenerate node in both WT2PTs — reject it
        Expect.isNonEmpty
          (P.parse "{ x with }").diagnostics
          "empty update diagnoses"
        // guard: a non-empty update must still parse cleanly
        Expect.isEmpty
          (P.parse "{ r with f = 1L }").diagnostics
          "non-empty record update is clean")
      testCase "a number glued to identifier chars is rejected" (fun _ ->
        // `123abc` / `12l3` are typos, not two tokens — reject rather than split
        for src in [ "123abc"; "12l3"; "1.5xyz"; "80I" ] do
          Expect.isNonEmpty
            (P.parse src).diagnostics
            $"glued number {src} diagnoses"
        // guard: valid suffixed / bare literals stay clean
        for src in [ "123L"; "1.5"; "1e300"; "[1L; 2L]" ] do
          Expect.isEmpty (P.parse src).diagnostics $"valid literal {src} is clean")
      testCase "two independent errors still recover both list elements" (fun _ ->
        let r = P.parse "[1L + ; 2L * ]"
        Expect.isNonEmpty r.diagnostics "has diagnostics"
        match r.parsed with
        | Some(WT.SourceFile { exprsToEval = [ WT.EList(_, [ _; _ ], _, _) ] }) ->
          ()
        | other -> failtest $"expected a recovered 2-element list, got {other}")
      testCase
        "delimiter not consumed; group still closes; hole is an EError node"
        (fun _ ->
          let r = P.parse "(1L + )"
          Expect.isNonEmpty r.diagnostics "has a diagnostic"
          match r.parsed with
          | Some(WT.SourceFile { exprsToEval = [ WT.EInfix(_,
                                                           _,
                                                           WT.EInt64(_, (_, 1L), _),
                                                           WT.EError _) ] }) -> ()
          | other -> failtest $"expected +(1, EError hole), got {other}")
      testCase
        "a broken declaration does not swallow the declarations after it"
        (fun _ ->
          // decl-level resync: broken1's dangling `+` and broken2's unclosed `[`
          // must both stop at the next `let` at the declaration column
          let src =
            "module Test\n"
            + "let broken1 (x: Int64) : Int64 =\n  x + \n\n"
            + "let broken2 () : Int64 =\n  [1L; 2L\n\n"
            + "let fine (y: Int64) : Int64 =\n  y * 2L\n"
          let r = P.parse src
          Expect.isNonEmpty r.diagnostics "reports the errors"
          match r.parsed with
          | Some(WT.SourceFile { declarations = [ WT.DModule m ] }) ->
            let fnNames =
              m.declarations
              |> List.choose (function
                | WT.DFunction f -> Some f.name.name
                | _ -> None)
            Expect.equal
              fnNames
              [ "broken1"; "broken2"; "fine" ]
              "all three fns survive"
            // and the healthy one parsed intact
            match m.declarations |> List.tryLast with
            | Some(WT.DFunction { body = WT.EInfix(_,
                                                   (_,
                                                    WT.InfixFnCall WT.ArithmeticMultiply),
                                                   _,
                                                   _) }) -> ()
            | other -> failtest $"fine's body: {other}"
          | other -> failtest $"unexpected: {other}")
      testCase "no infinite loop / spam at EOF" (fun _ ->
        let r = P.parse "let x ="
        Expect.isNonEmpty r.diagnostics "reports the error"
        Expect.isTrue (List.length r.diagnostics <= 3) "does not spam")
      testCase
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
let private mustDiagnose (label : string) (src : string) : Test =
  testCase label (fun _ ->
    let res =
      try
        Some(P.parse src)
      with e ->
        failtest $"parse threw on {label}: {e.Message}"
    match res with
    | Some r ->
      Expect.isNonEmpty r.diagnostics $"{label}: expected a diagnostic, got none"
    | None -> ())

let private lexicalFailureTests =
  testList
    "lexical-failures"
    [ mustDiagnose "unknown character" "let x = §"
      mustDiagnose "unterminated string" "\"abc"
      mustDiagnose "unterminated char" "'a"
      mustDiagnose "multi-grapheme escaped char" "'\\na'"
      mustDiagnose "unterminated interpolation" "$\"{1L"
      mustDiagnose "bad expression inside interpolation" "$\"{1L +++ 2L}\""
      mustDiagnose "empty match" "match x with"
      mustDiagnose "empty lambda parameter list" "fun -> 1L"
      mustDiagnose "empty module body" "module X =\nlet y = 1L"
      mustDiagnose "anonymous record" "{ a = 1L }"
      testCase "DB declarations must be aliases" (fun _ ->
        let result = P.parseTestFile "[<DB>] type X = { a: Int64 }"
        Expect.exists
          result.diagnostics
          (fun diagnostic ->
            diagnostic.message = "[<DB>] type must be a type alias")
          "alias diagnostic")
      mustDiagnose
        "misaligned match arm"
        "match x with\n  | Some x -> x\n    | None -> 0L"
      mustDiagnose "malformed float exponent (no digits)" "1e"
      mustDiagnose "malformed float exponent (sign only)" "1.5e+"
      mustDiagnose "oversized float exponent" "1e401"
      mustDiagnose "out-of-range Int64" "99999999999999999999L"
      mustDiagnose "out-of-range Int8" "9000y"
      mustDiagnose "unterminated block comment" "(* never closed"
      testCase "bad integer suffix is consumed as one recovered token" (fun _ ->
        match LibParser.Lexer.tokenize "256uy" with
        | Ok(tokens, diagnostics) ->
          Expect.equal diagnostics.Length 1 "one range diagnostic"
          match tokens with
          | token :: eof :: [] ->
            Expect.equal token.text "256uy" "the recovered token covers the suffix"
            Expect.equal eof.token Tok.TEOF "then EOF"
          | other -> failtest $"unexpected tokens: {other}"
        | Error error -> failtest error)
      mustDiagnose "multiple interpolation expressions" "$\"{1L\n2L}\""
      mustDiagnose "empty interpolation" "$\"{}\""
      mustDiagnose "declaration-only interpolation" "$\"{val x = 1L}\""
      mustDiagnose "single interpolation close brace" "$\"hello } world\""
      testCase "comments and raw strings do not close interpolation" (fun _ ->
        for src in [ "$\"{(* } *) 1L}\""; "$\"{\"\"\" } \"\"\"}\"" ] do
          Expect.isEmpty (P.parse src).diagnostics $"valid interpolation: {src}")
      testCase
        "unterminated interpolation preserves text and synthesizes EOF close"
        (fun _ ->
          match (P.parse "$\"abc").parsed with
          | Some(WT.SourceFile { exprsToEval = [ WT.EString(_,
                                                            _,
                                                            [ WT.StringText(_, "abc") ],
                                                            _,
                                                            close) ] }) ->
            Expect.equal close.start close.end_ "synthetic close is zero-width"
            Expect.equal close.start.column 5 "synthetic close is at EOF"
          | other -> failtest $"unexpected recovered interpolation: {other}")
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

/// Golden diagnostics: the rendered messages are product surface — pin them.
/// (Full-list equality, so a new cascade or a reworded message fails here.)
let private goldenDiagnosticsTests =
  let golden (input : string) (expected : List<string>) : Test =
    testCase (sprintf "golden: %s" (input.Replace("\n", "\\n"))) (fun _ ->
      let actual = (P.parse input).diagnostics |> List.map (fun d -> d.message)
      Expect.equal actual expected $"diagnostics for {input}")
  testList
    "golden-diagnostics"
    [ golden
        "(1L + 2L"
        [ "expected ')' to close the '(' at line 1:1, found end of file" ]
      golden "(1L + )" [ "expected an expression, found ')'" ]
      golden
        "Point { x = 1L"
        [ "expected '}' to close the '{' at line 1:7, found end of file" ]
      golden "if true then 1L else" [ "expected an expression, found end of file" ]
      // the missing `:` is reported once; the param parser then recovers and
      // reads `Int64` as the type, so no cascade follows
      golden "let f (x Int64) : Int64 = x" [ "expected ':', found 'Int64'" ]
      golden
        "128y"
        [ "integer literal 128y is out of range (this magnitude is only valid negated: -128y)" ]
      // the unfinished arm is missing both its arrow and its body
      golden
        "match x with | Ok v -> v | Error"
        [ "expected '->' in match case, found end of file"
          "expected an expression, found end of file" ]
      golden
        "let x = [1L; 2L"
        [ "expected ']' to close the '[' at line 1:9, found end of file"
          "expected an expression, found end of file" ]
      testCase "diagnostics carry stable codes, related spans, and hints" (fun _ ->
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
    [ "let f (x: Int64) : Int64 =\n  let y = x + 1L\n  y * 2L"
      "module A.B\ntype T = { a: Int64; b: List<String> }\nlet v = T { a = 1L; b = [] }"
      "match xs with\n| [] -> 0L\n| h :: t -> h + (sum t)\n| _ -> $\"n {h} m\""
      "[1L; 2L] |> Stdlib.List.map (fun x -> x * 2L) |> (++) \"s\""
      "type E = | A of Int64 * String | B\nlet g () : E = A(1L, \"x\")" ]
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
    | WT.EDict(_, contents, _, _, _) -> contents |> List.map (fun (_, _, v) -> v)
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
            | WT.EPipeInfix(_, _, e) -> [ e ]
            | WT.EPipeLambda(_, _, b, _, _) -> [ b ]
            | WT.EPipeEnum(_, _, _, fs, _) -> fs
            | WT.EPipeFnCall(_, _, _, args) -> args
            | WT.EPipeVariableOrFnCall _ -> []))
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
    | WT.DExpr e -> [ e ]
    | WT.DModule m -> m.declarations |> List.collect declExprs
    | WT.DType _
    | WT.DTypeDB _ -> []
    | WT.DTest t -> [ t.actual ]
  testList
    "range-invariants"
    [ testCase
        "child expr ranges are contained in their parents (whole corpus)"
        (fun _ ->
          let root =
            [ "../packages/darklang"
              "packages/darklang"
              "/home/dark/app/packages/darklang" ]
            |> List.tryFind System.IO.Directory.Exists
          match root with
          | None -> ()
          | Some root ->
            let violations = ResizeArray<string>()
            for f in
              System.IO.Directory.GetFiles(
                root,
                "*.dark",
                System.IO.SearchOption.AllDirectories
              ) do
              match (P.parse (System.IO.File.ReadAllText f)).parsed with
              | Some(WT.SourceFile sf) ->
                for e in
                  (sf.declarations |> List.collect declExprs) @ sf.exprsToEval do
                  check f violations e
              | None -> ()
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
              PT.mainBranchId
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
          Expect.isTrue (bp Tok.TStar < bp Tok.TBitXor) "* looser than ^"
          // right-assoc ops
          Expect.equal
            (P.infixBindingPower Tok.TAt |> Option.map snd)
            (Some true)
            "@ is right-assoc"
          Expect.equal
            (P.infixBindingPower Tok.TBitXor |> Option.map snd)
            (Some true)
            "^ is right-assoc")
      testCase "a parse constructs fresh state (no cross-parse leakage)" (fun _ ->
        // parse something that leaves pendingGt / scopes in interesting states,
        // then confirm an unrelated parse is unaffected
        P.parse "type X = List<List<Int64" |> ignore
        let r = P.parse "type Y = List<Int64>"
        Expect.isEmpty r.diagnostics "clean parse after a broken generic parse") ]

let tests =
  testList
    "LibParser"
    [ parserStructureTests
      internalUnitTests
      offsideTests
      typeTests
      desugarTests
      loweringRegressionTests
      valueAnnotationTests
      validationTests
      literalTests
      recoveryTests
      lexicalFailureTests
      goldenDiagnosticsTests
      fuzzTests
      rangeInvariantTests
      primTypeDriftTests
      corpusTests ]
