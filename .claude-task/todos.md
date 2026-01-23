# RegEx Support Spike - Plan

## Summary
This spike adds basic regular expression support to Darklang. The implementation includes minimal builtins with stdlib wrappers, and demonstrates the functionality through a parser combinator library that can parse Darklang syntax.

## Completed Work

### 1. RegEx Builtins (F#)
- [x] `Builtin.regexIsMatch` - Check if a string matches a pattern
- [x] `Builtin.regexFind` - Find first match
- [x] `Builtin.regexFindAll` - Find all matches
- [x] `Builtin.regexReplace` - Replace first match
- [x] `Builtin.regexReplaceAll` - Replace all matches
- [x] `Builtin.regexSplit` - Split string by pattern

### 2. Stdlib Wrappers (.dark)
- [x] `Stdlib.Regex.isMatch` - Wrapper for isMatch
- [x] `Stdlib.Regex.find` - Wrapper for find
- [x] `Stdlib.Regex.findAll` - Wrapper for findAll
- [x] `Stdlib.Regex.replace` - Wrapper for replace
- [x] `Stdlib.Regex.replaceAll` - Wrapper for replaceAll
- [x] `Stdlib.Regex.split` - Wrapper for split

### 3. Parser Combinator Library (packages/stachu/parser.dark)
- [x] Core types: ParseResult, Parser
- [x] Basic combinators: succeed, fail, map, apply, andThen, orElse, keepLeft, keepRight
- [x] Regex-based parsers: regex, string, charWhere, digits, whitespace, word, letters
- [x] Repetition: many, many1, optional, sepBy, sepBy1, deferred (for recursion)
- [x] Convenience: between, choice, int64, float, quotedString

### 4. Example Usage - TinyLang (packages/stachu/tinyLang.dark)
- [x] Simple AST types (BinOp, Expr)
- [x] Parser for arithmetic expressions, lists, let bindings
- [x] 34 passing tests

### 5. Full Darklang Parser (packages/stachu/darklangParser.dark)
- [x] Complete AST matching PT.Expr
- [x] All literal types (int8-128, uint8-128, float, bool, char, string, unit)
- [x] Lists, tuples, dicts, records, enums
- [x] Variables and qualified function references
- [x] Infix operators (left-associative, no precedence)
- [x] Let, if, match, lambda expressions
- [x] Pipe expressions
- [x] Field access

## Remaining Work

### 6. Tests
- [ ] Add F# tests for regex builtins (backend/tests)
- [ ] Add .dark tests for Stdlib.Regex functions
- [ ] Add comprehensive tests for parser combinator library
- [ ] Add tests comparing darklangParser output to actual PT.Expr

### 7. Integration
- [ ] Verify tree-sitter grammar doesn't need changes for regex literals
- [ ] Consider if regex literal syntax is needed (e.g., `/pattern/flags`)
- [ ] Document regex support in user docs

### 8. Potential Improvements (Future)
- [ ] Add operator precedence to infix parser (Pratt parsing)
- [ ] Handle indentation-sensitive parsing
- [ ] Add string interpolation parsing
- [ ] Add type annotation parsing

## Notes

- Current regex syntax uses standard regex strings (no special literal syntax)
- Parser combinator library uses `deferred` combinator to handle recursive grammars
- The darklangParser demonstrates regex is powerful enough for real parsing tasks
