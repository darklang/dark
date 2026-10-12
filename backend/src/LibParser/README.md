# LibParser

The hand-written recursive-descent parser for Darklang source. One lexer, one
parser, one syntax tree.

**[GRAMMAR.md](GRAMMAR.md) is the spec for the grammar it accepts** — operator
precedence, offside rules, dialect decisions, and diagnostic codes.

## Pipeline

```
source
  │  Lexer.tokenize                  tokens + trivia (comments and positions
  │                                  preserved; whitespace text is not stored)
  ▼
  │  Parser.parseSyntax              shared private syntax pass
  ▼
  ├─ Parser.parse                    recoverable WrittenTypes tree + syntax and
  │                                  structural diagnostics for tooling
  │  └─ Builtins.Language.WrittenTypesToDarkTypes → Dark WrittenTypes (Dvals)
  │                                                   → Dark WT2PT (highlighter, etc.)
  └─ Parser.parseFor Mode            structural + file-purpose validation
     └─ ValidatedSourceFile          required by Package, TestModule, and CLI
        └─ WrittenTypesToProgramTypes → executable ProgramTypes
```

The two lowerings are kept in agreement by a differential test
(`Tests/WrittenTypesLoweringParity.Tests.fs`) that compares their ProgramTypes
output over the real package corpus; they must be identical (node ids aside).

## Files

- `Tokenizer.fs` — shared token/position types (`Token`, `Pos`, `TokenRange`)
- `Lexer.fs` — the lexer: tokens with ranges, doc comments, trivia
- `Parser.fs` — the parser: `ParserState` + module-level parse functions;
  `parse` for tooling, `parseFor` for execution, offside scopes, recovery, and
  structured diagnostics
- `Validation.fs` — the pre-lowering gate: typed structural and
  Script/Package/Test checks, plus the opaque validated-source wrapper
- `SourceFile.fs` — shared flat view over `WrittenTypes.SourceFile` items
- `WrittenTypes.fs` — the syntax tree (every node carries source ranges) plus
  the normalized package IR
- `WrittenTypesToProgramTypes.fs` — execution lowering + name resolution glue
- `NameResolver.fs` — name → FQName resolution against the package manager
- `Package.fs` — package-file entrypoint used by the loader
- `TestModule.fs` — testfile (`.dark`) classification and execution harness

Value declarations use `val name = expression`. `let` is reserved for function
declarations and local/script bindings.

## Testing

- `Tests/LibParser.Tests.fs` — structure, offside, types, literals,
  recovery, golden diagnostics, fuzzer, range invariants, and the corpus gate
  (every real package file must parse cleanly)
- `Tests/WrittenTypesLoweringParity.Tests.fs` — F# vs Dark lowering agreement
- `Tests/LibParser.RoundTrip.Tests.fs` — source → PT → pretty-print round-trips
