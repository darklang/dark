# Darklang Surface Grammar

This describes the syntax accepted by the hand-written parser
(`LibParser.Parser`). The parser is the implementation; this file records the
intended decisions.

## Lexical structure

### Comments and trivia

Supported comments:

- `// line`
- `/// doc`, which attaches to the next declaration as its description
- `////` and deeper, which are plain comments
- `(* block *)`, which nests

`(*)` is the multiply operator section, not a comment. Comments are preserved on
tokens as `leadingTrivia`. Whitespace text is not stored, so the token stream
preserves positions and comments but cannot reconstruct spaces, tabs, line
endings, or trailing whitespace byte-for-byte.

### Identifiers

Normal identifiers start with a Unicode letter or `_`, followed by Unicode
letters/digits, `_`, or `'`. Package names use a separate, stricter naming rule
during name resolution.

Backtick-quoted ``` ``name`` ``` permits anything up to the closing backticks.
`___` is the blank identifier and has the empty name.

A leading `'` in type position lexes a type variable, such as `'a` or `'TModel`.
Elsewhere, `'…'` is a char literal.

### Integer literals

Bare literals (`42`) are arbitrary-precision `Int`. Suffixes select fixed-width
types:

| suffix | type |
|---|---|
| `y` | Int8 |
| `uy` | UInt8 |
| `s` | Int16 |
| `us` | UInt16 |
| `l` | Int32 |
| `ul` | UInt32 |
| `L` | Int64 |
| `UL` | UInt64 |
| `Q` | Int128 |
| `Z` | UInt128 |

(No `I` suffix — bare literals are already `Int`, so `80I` is a number glued to
an identifier and diagnosed like `123abc`.)

A literal whose magnitude equals the type's `|MinValue|` (e.g. `128y`,
`9223372036854775808L`) is valid **only when negated** (`-128y`); bare use is an
out-of-range diagnostic, never a silent wrap.

### Float literals

`digits[.digits][eE[±]exp]`. Lowered to exponent-free whole/fraction decimal
strings (`1e300` → `1` + 300 zeros). Exponents outside `-400..400` are rejected
because ProgramTypes stores expanded decimal strings rather than an exponent.

### String literals

String forms:

- `"…"` with escapes
- `"""…"""` raw, with no escapes
- `$"…{expr}…"` interpolated
- `$"""…"""` raw-interpolated

In interpolated strings, `{{` and `}}` are literal braces. Interpolation bodies
are full expressions and keep real source positions.

Escapes: `\n \t \r \a \b \v \f \\ \" \' \/ \0 \{ \}`, `\xHH`, `\XHHHH`,
`\uHHHH`, `\UHHHHHHHH`. Unicode escapes must be scalar values; surrogates and
values above `0x10FFFF` are invalid.

Invalid escapes are diagnostics. String content is NFC-normalized.

### Character literals

`'c'` — one extended grapheme cluster; escapes as in strings.

### Boolean and unit literals

`true` / `false`; the empty tuple `()` is the unit value.

## Operators and precedence

Loosest to tightest; one table (`infixBindingPower`) drives the parser.

| level | operators | assoc | notes |
|---|---|---|---|
| 0 | `\|>` | left | pipe (below everything) |
| 1 | `\|\|` | left | |
| 2 | `&&` | left | |
| 3 | `== != < > <= >=` | left | `=` is **not** equality (binding only) |
| 4 | `@` | right | list append → `Stdlib.List.append` |
| 5 | `+ - ++` | left | `++` is string concat |
| 6 | `* / %` | left | |
| 7 | `^` | right | exponentiation (`2^3^2 = 2^(3^2)`) |
| 8 | application `f a b` | left | tightest |

`<<`, `>>`, `&`, `|||`, `~~~`, `!`, and `...` are reserved tokens but are not
supported expression operators. `...` is also reserved for unsupported rest
patterns. These forms produce an unsupported-syntax diagnostic. `def` is not
reserved and lexes as a normal identifier.

Operator sections `(op)` are two-arg lambdas.

`x |> (op) y` means `x op y`; the piped value is the *left* operand.

Unary minus on a literal makes a negative literal. On anything else it applies
`Builtin.negate`.

In argument position, a `-` glued to a number with a space before it is a
negative-literal argument:

- `f a -1` means `f a (-1)`
- `f a - 1` means `(f a) - 1`

This follows F#'s rule.

## Offside (indentation) rules

One rule set for all inputs:

- Every statement/declaration/element anchors at its **start column**. A token
  on a new line *indented past the anchor* continues the current construct; at
  the anchor it starts a sibling; left of it, the enclosing construct ends.
- **Inside parentheses** the closing `)` is the real delimiter, so the anchor is
  *exact*: only a token at exactly the inner anchor column starts a new
  statement (paren bodies can be statement blocks); any other column — deeper
  *or shallower* — continues (`x |> (fn\n  args-dedented-below-callee)` works).
- Application arguments continue while past the *statement* anchor even when
  left of a far-right callee (`let r = xs |> List.fold\n  init\n  fn`).
- An operator on a new line continues the statement at-or-past the anchor —
  except `-`, which must be *past* it (`1L` then `-8L` at the anchor is two
  statements, not subtraction).
- `else`/`elif` bind to the `if` chain at-or-left of their column; `match` arms
  align on the first `|`'s column (a shallower `|` belongs to an outer match).

## Expressions

Expression forms include:

- literals and unit `()`
- tuples `(a, b, …)`
- lists `[a, b]`, separated by `,` or newlines
- dicts `Dict { k: v }`, where both halves are expressions -- `:` rather than the
  record literal's `=`, since a dict key is a value, not a field name
- named records `Type { f = v }`; anonymous records are rejected until they
  have a real inferred/represented type
- record updates `{ r with f = v }`
- field access `x.f`
- lambdas `fun p1 p2 -> body`, including tuple patterns
- `let pat = e`, with optional `in`, and tuple/wildcard/unit patterns
- nested function lets, `let f (x: T) : R = …`
- `if c then a [elif …] [else b]`, where `else` is optional
- `match e with | pat [when g] -> body`
- pipes
- statement sequences, separated by newlines or `;`

Adjacent same-line items never imply a separator. Lists and list patterns use
`,`; a `;` there is an error. Dicts, records, record updates, and record-type
fields use `,` or `;`; constructor fields and constructor-pattern fields use
`,`. A newline separates items in all of these forms.

For dotted names, lowercase steps are fields and uppercase steps are module path
segments. A bare dotted name like `Stdlib.List.map` is itself a value/function
reference.

A nested function let lowers to a let-bound lambda. Its annotations are
currently compatibility syntax and are not enforced by this parser/type path.
Local bindings and value declarations do not accept annotations. Write
`let x = value` locally or `val x = value` at declaration scope, without
`: Type`.

### Enum constructors

Constructor forms are `Type.Case`, `Module.Type.Case`, or bare `Case`.

Parenthesized commas are **fields**:

- `Pair(1, 2)` has two fields
- `Pair((1, 2))` has one tuple field
- `Case()` has one unit field

Space-application binds fields only in head position, such as `Ok 5`. It does
not bind in argument position: `f None x` keeps `None` nullary.

A bare nullary uppercase name is resolved contextually at lowering. It may be a
variable, enum case, or DB.

### Pipes

`e |> seg |> seg …` threads the value of `e` through each segment, left to
right. A segment is one of:

- a **function call** — `|> Stdlib.List.map f` — or a bare qualified fn name
  `|> Stdlib.List.reverse` (callee type args are kept: `|> parse<Int64>`);
- a **bare variable / unqualified call** — `|> myFn`;
- a **lambda** — `|> fun x -> …`;
- an **enum constructor** — `|> Ok`, `|> Type.Case`;
- an **operator section with an argument** — `|> (+) 1`, which desugars to
  `e + 1` (the piped value is the *left* operand, not the section's).

A pipe RHS that is none of these is a `PARSE-PIPE-SEGMENT` diagnostic.

### Match patterns

Pattern forms:

- literals, including negative literals
- `_`
- variables
- tuples
- lists
- cons `h :: t`, right-associative
- or-patterns `p1 | p2`
- enum patterns with **unqualified** case names, such as `| Ok x`
- unit

Names in one pattern cannot be repeated. Every or-pattern branch must bind the
same non-ignored names. `_` and names beginning with `_` are ignored bindings.

Qualified enum patterns such as `| Result.Ok x` are rejected.

### Generics

`Name<T, …>` requires `<` to be *adjacent* to the name. A spaced `<` is a
comparison.

Generic type parameters declared after a type or function name must be
apostrophe-prefixed and comma-separated; the list cannot be empty. For example:
`type Pair<'a, 'b> = 'a * 'b`.

Generics work on:

- calls: `parse<Int64> x`
- enum constructors: `Type<T>.Case`
- records: `Type<T> { … }`
- type declarations: `type F<'a> = …`

`>>` closes two levels.

## Types

Primitive types are `Unit Bool Int Int8…UInt128 Float Char String DateTime Uuid
Blob` (from `WrittenTypes.primTypes`). `List<T>` and `Dict<K, V>` have special
syntax. Other uppercase names, including `Stream<T>` and `DB<T>`, are custom
types with optional type arguments.

Type variables are `'a` or bare lowercase names. Tuples use `A * B`. Function
types use `A -> B -> C` as flat multi-arg functions: args `[A, B]`, return `C`,
not curried functions. Enum case fields separate with `*` at atom level:
`Case of A * B` has two fields; a tuple field needs parens, `Case of (A * B)`.

## Declarations

Package function parameters cannot use the blank name `___`; give the parameter
a name, or use `()` when the parameter is unit.

Declaration forms:

- `let f (p: T) … : R = body` defines a function. Parameters are parenthesized
  and annotated. `()` is a unit parameter.
- `val x = e` defines a module or test-setup value.
- `let x = e` is a local/script binding. At declaration scope it is rejected;
  use `val` for a value declaration.
- `module A.B` is a file header and wraps the rest of the file.
- `module X =` starts an indented module body.

`rec`, `private`, and `internal` are ordinary identifier names, not let
modifiers. Functions are automatically self-recursive, so modifier-shaped forms
such as `let rec f ...` are invalid.

Modules nest. The path builds the package location: `owner.modules.name`.

`///` doc comments become descriptions.

**Type declarations** — `type Name<'a> = …` is one of:

- a **record**: `{ f: T; … }`;
- an **enum**: `| A | B of T` (after the first case each case requires a leading
  `|`);
- an **alias** to another type: `type Id = String`, `type Pair = Int * Int`.

### Test classification

The parser represents test assertions and DB declarations in the same syntax
tree for every caller. `Parser.parseFor Validation.Test` accepts them; Script
and Package modes reject them. Successful validation returns a
`ValidatedSourceFile`, which package, test, and CLI entry points require before
lowering. `Parser.parse` remains mode-neutral for tooling.

At the top level:

- `val x = …` declares a test setup value; `let x = …` keeps its normal script
  expression meaning
- `actual = expected` is a test assertion
- expected forms may be `error "msg"` or `sqlerror "msg"`
- `[<DB>] type X = T` declares a user DB

## Diagnostics

Diagnostics are structured: `{ code; severity; range; message; related; hint }`.
Codes are stable identifiers (key on these, never on message text):

| code | meaning |
|---|---|
| `PARSE-EXPECTED` | expected X, found Y |
| `PARSE-UNCLOSED` | missing closing delimiter — the opener is in `related` |
| `PARSE-ESCAPE` | invalid escape/codepoint in a literal |
| `PARSE-INT-RANGE` | integer literal out of range (hint: the negated form) |
| `PARSE-TOO-DEEP` | nesting beyond the recursion cap |
| `PARSE-UNEXPECTED` | stray token inside a construct |
| `PARSE-PIPE-SEGMENT` | pipe RHS isn't a valid segment |
| `PARSE-PATTERN` | invalid match pattern shape |
| `PARSE-INTERPOLATION` | malformed interpolation body or brace boundary |
| `PARSE-INTERNAL-LOOP` | parser step budget exhausted — a parser bug, please report |
| `LEX` | tokenizer-level recovery (unterminated literal, …) |

Post-parse validation uses these stable codes:

| code | meaning |
|---|---|
| `VALIDATION-DUPLICATE-BINDER` | one pattern or parameter list binds the same usable name more than once |
| `VALIDATION-OR-BINDINGS` | or-pattern alternatives bind different usable names |
| `VALIDATION-OR-PATTERN` | an invalid tree contains an empty or-pattern |
| `VALIDATION-RECOVERY-HOLE` | an `EError` or `MPError` recovery node reached validation |
| `VALIDATION-LAMBDA` | a lambda has no parameter |
| `VALIDATION-MATCH` | a match has no case |
| `VALIDATION-ANONYMOUS-RECORD` | the tree contains an unsupported anonymous record |
| `VALIDATION-RECORD-UPDATE` | a record update has no fields |
| `VALIDATION-PACKAGE-EXPR` | a package contains an executable expression |
| `VALIDATION-TEST-ASSERTION` | a test expression is not an assertion |
| `VALIDATION-DB-MODE` | a DB declaration occurs outside Test mode |
| `VALIDATION-DB-SHAPE` | a DB declaration is not a type alias |
| `VALIDATION-TEST-MODE` | a test assertion occurs outside Test mode |

`renderDiagnostic` renders code + position + message + a caret snippet +
related locations + hint; the CLI uses it. The LSP wire (`parserParseDiagnostics`,
range+message tuples) predates the structure — upgrading it to carry
code/related is a pending Dark-side type change.

## Error recovery

- Diagnostics carry ranges. "expected X, found Y" diagnostics point at the
  unexpected token.
- Unclosed delimiters point at their opener.
- Recovery holes are explicit `EError` or `MPError` nodes with a zero-width range
  at the failure point.
- Execution rejects trees with diagnostics. Tooling, such as highlighting and
  hover, consumes recovered trees and paints around holes.
- Recovery never consumes closing/sync tokens or declaration starters:
  `let`, `type`, and `val`.
- A declaration keyword at-or-left of the current declaration's column terminates
  any construct inside it. A broken declaration cannot swallow the declarations
  after it.
- Nesting beyond 200 levels abandons the parse with one diagnostic for stack
  safety.
- String-interpolation nesting beyond 64 levels also stops. Each `{expr}` body
  parses recursively with fresh state.
- Statement sequences are unbounded because they are parsed iteratively.
- A step budget, `4000 + 300·n` parse entries for `n` tokens, backs up the
  per-loop progress guards. A future no-progress loop becomes a
  `PARSE-INTERNAL-LOOP` diagnostic instead of a hang.

## Where this fits

`source ──Lexer──▶ tokens (+trivia) ──Parser──▶ WrittenTypes (range-complete)`

The parser stops at WrittenTypes; lowering to ProgramTypes (`WT2PT`) is downstream.
