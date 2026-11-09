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

### Number literals

A number is digits, an optional fraction (`.digits`) and exponent
(`e[±]digits`), then a suffix glued on that names its type. The type is decided
here, by the literal alone; nothing infers it from context.

| type | literal | | type | literal |
|---|---|---|---|---|
| Int8 | `1s8` | | UInt8 | `1u8` |
| Int16 | `1s16` | | UInt16 | `1u16` |
| Int32 | `1s32` | | UInt32 | `1u32` |
| Int64 | `1`, `1s`, `1s64` | | UInt64 | `1u`, `1u64` |
| Int128 | `1s128` | | UInt128 | `1u128` |
| BigInt | `1z` | | | |
| Float | `1.5`, `1e5`, `1.5f64` | | Float32 | `1.5f32`, `1e5f32` |
| Decimal | `19.99dec` | | | |

`z` is for ℤ, the integers: `1z` is the arbitrary-precision `BigInt`, with no size
limit. A bare integer is an Int64 and a bare fraction or exponent is a Float.
The float suffixes also take a whole number (`1f32`, `1f64`, `1dec`); the
integer suffixes never take a fraction or exponent (`1.5s8` is an error).

A literal ends at a non-identifier character, so any other glued text is one
malformed literal: `123abc`, `1s7`, `1.5f16` and `1.5m` are each diagnosed
whole. A word that starts with a letter, such as `s32`, is an identifier.

A literal whose magnitude equals the type's `|MinValue|` (e.g. `128s8`,
`9223372036854775808`) is valid **only when negated** (`-128s8`); bare use is an
out-of-range diagnostic, never a silent wrap.

### Float literals

`digits[.digits][eE[±]exp]`. Lowered to exponent-free whole/fraction decimal
strings (`1e300` → `1` + 300 zeros). A literal must preserve its specified decimal
precision: converting the exact written value to binary64, then rounding the
exact binary64 value back to the source's last decimal place, must recover the
written value. Both conversions use nearest, ties-to-even rounding. Trailing
zeros specify precision; the exponent shifts that precision position.

`0.1` is accepted, while `10000000000000001.0` and `0.10000000000000000` lose
specified precision and are errors. Overflow to infinity and nonzero underflow
to zero are errors. Negative zero is preserved, and subnormals such as `5e-324`
are accepted when they satisfy the precision rule. There is no arbitrary source
exponent limit: large exponents can cancel large mantissas or fractional zeros.
Normalization removes leading zeros from the whole part and does not depend on
host float formatting. The same rules apply to float patterns. Runtime string
parsing remains a separate operation.

A `Float32` literal (`1.5f32`, `1e3f32`) is checked by the same precision rule
against binary32 instead, so `0.1f32` is accepted and `0.1234567891f32` is an
error.

### Decimal literals

`digits[.digits]dec`, with no exponent (`1e5dec` is an error, since a Decimal is
the digits as written): `19.99dec`, `1dec`, `-0.05dec` (unary minus, as for
floats). The value is a .NET `decimal` and keeps the written scale, so `1.10dec`
is 1.10, not 1.1. A literal is accepted only if it prints back as written once
leading zeros of the whole part are dropped, so anything .NET would round (more
than 28 places, or more significant digits than 96 bits hold) and anything past
79228162514264337593543950335 is a diagnostic rather than a rounded value.

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
| 4 | `\|` | left | bitwise or |
| 5 | `^` | left | bitwise xor |
| 6 | `&` | left | bitwise and |
| 7 | `<< >>` | left | bitwise shifts |
| 8 | `+ -` | left | `+` concatenates Strings too |
| 9 | `* / %` | left | |
| 10 | `**` | right | exponentiation (`2**3**2 = 2**(3**2)`) |
| 11 | call `f(a, b)` | left | `f(a)(b)` calls what `f(a)` returns |
| 12 | field access `x.field`, unwrap `x?` | left | tightest |

The bitwise levels follow Python's order rather than C's: they bind *tighter*
than the comparisons, so `a & b == c` is `(a & b) == c`.

`...` spreads a list into a list literal: `[a, ...b, c]` lowers to
`Stdlib.List.flatten([[a], b, [c]])`, and `[...a, ...b]` to
`Stdlib.List.concat(a, b)`. Anywhere else it is a diagnostic. `::` and `@`
are not tokens: the lexer refuses each with the list form to write instead.
`def` is not reserved and lexes as a normal identifier.

### `|` in a match

`|` is both bitwise-or and the match-arm separator. Inside a `match`, a `|` on
the arm row or at the arm column starts an arm — exactly the tokens the arm
loop would have taken — so every match that parsed before still parses,
single-line ones included. Bitwise-or in an arm body needs parentheses:

```
match x with
| A -> (a | b)
```

Entering any bracket (`( )`, `[ ]`, `{ }`) drops the arm anchors, since no arm
can begin inside one; that is what makes the parenthesised form work even on a
one-line match.

Operator sections `(op)` are two-arg lambdas, called like any other:
`(+)(1, 2)`, `List.fold(xs, 0, (+))`.

Unary minus on a literal makes a negative literal. On anything else it applies
`Builtin.negate`. Prefix `!` (boolean not) and `~` (bitwise not) take the same
shape, applying `Builtin.boolNot` / `Builtin.bitwiseNot` to a whole call:
`!f(x)` is `!(f(x))`, and infix operators still bind looser, so `!a && b` is
`(!a) && b`.

## Calls

A call puts its arguments in parentheses, separated by commas or newlines:
`f(a, b)`. A space before the `(` is allowed, so `f (a, b)` is the same call;
passing one tuple is `f((a, b))`. `f()` passes the unit argument, which is what
a fn declared `let f(): T` takes. A callee is a name, a field (`c.onKey(s, k)`,
`a.add(b)`), a parenthesized lambda or section, or another call (`f(a)(b)`).
Type arguments come before the parentheses: `parse<Int64>(s)`.

Arguments written after a callee with spaces, `f a b`, are a `PARSE-UNEXPECTED`
error with a hint. They are still parsed, as the call they would once have
been, so recovery sees the intended shape.

There is no partial application. A call gives every argument the function
takes; fewer is a runtime error (`TooFewArgsForFn`) and an at-rest checker error
(`TooFewArguments`), not a function waiting for the rest. Write a lambda for
that: `fun x -> f(a, x)`. A function value named without a call (`List.map(xs,
Int64.toFloat)`) is still a value.

Enum case declarations, constructors and lambda parameters take the same shape:
`| Pair(Int64, Int64)`, `Pair(1, 2)`, `fun(acc, x) -> …`. See Enum constructors,
Lambdas and Type declarations below.

## Offside (indentation) rules

One rule set for all inputs:

- Every statement/declaration/element anchors at its **start column**. A token
  on a new line *indented past the anchor* continues the current construct; at
  the anchor it starts a sibling; left of it, the enclosing construct ends.
- **Inside parentheses** the closing `)` is the real delimiter, so the anchor is
  *exact*: only a token at exactly the inner anchor column starts a new
  statement (paren bodies can be statement blocks); any other column — deeper
  *or shallower* — continues (`x |> (fn\n  args-dedented-below-callee)` works).
- Inside a call's parentheses the arguments may sit on their own lines:
  `List.fold(\n  init,\n  fn)`.
- An operator on a new line continues the statement at-or-past the anchor —
  except `-`, which must be *past* it (`1` then `-8` at the anchor is two
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
- calls `f(a, b)` (see Calls)
- postfix unwrap `x?` or `f(x)?`, inside functions/lambdas only
- lambdas `fun x -> body` and `fun(p1, p2) -> body` (see Lambdas)
- `let pat = e`, with optional `in`, and tuple/wildcard/unit patterns
- nested function lets, `let f(x: T, y: U): R = …`
- `if c then a [elif …] [else b]`, where `else` is optional
- `match e with | pat [when g] -> body`, where a guard `when g` is any Bool
  expression over the pattern's names, and an arm whose guard is false falls
  through to the next
- pipes
- statement sequences, separated by newlines; `;` is an error, not a separator

Adjacent same-line items never imply a separator. Lists, list patterns, dicts,
records, record updates, and record-type fields use `,`; a `;` is an error.
Constructor fields and constructor-pattern fields also use `,`. A newline
separates items in all of these forms.

For dotted names, lowercase steps are fields and uppercase steps are module path
segments. A bare dotted name like `Stdlib.List.map` is itself a value/function
reference.

A nested function let lowers to a let-bound lambda. Each parameter must be
annotated, as in a top-level fn; the return annotation is optional. Both are
dropped when it lowers, so neither is checked.
Local bindings and value declarations do not accept annotations. Write
`let x = value` locally or `val x = value` at declaration scope, without
`: Type`.

### Enum constructors

Constructor forms are `Type.Case`, `Module.Type.Case`, or bare `Case`.

Parenthesized commas are **fields**:

- `Pair(1, 2)` has two fields
- `Pair((1, 2))` has one tuple field
- `Case()` has one unit field

Fields go in parentheses, as a call's arguments do, and a space before the `(`
is allowed. Fields written with spaces, `Ok 5`, are a `PARSE-UNEXPECTED` error
with a hint; they are still parsed into the constructor, so recovery sees the
intended shape. A nullary case is written bare: `None`.

A bare nullary uppercase name is resolved contextually at lowering. It may be a
variable or an enum case.

### Lambdas

Parameters have the shape of a call's arguments:

- `fun x -> body` takes one parameter (also `fun _ ->` and `fun () ->`)
- `fun(acc, x) -> body` takes two; a space before the `(` is allowed
- `fun((a, b)) -> body` takes one tuple and takes it apart

Parameters written with spaces, `fun acc x -> …`, are a `PARSE-UNEXPECTED`
error with a hint. They are still parsed as the parameters they list.

### Pipes

`e |> seg |> seg …` threads the value of `e` through each segment, left to
right. A segment is one of:

- a **function call**: `|> Stdlib.List.map(f)`, the piped value first and the
  call's arguments after it, or a bare qualified fn name `|> Stdlib.List.reverse`
  (callee type args are kept: `|> parse<Int64>`). `|> f()` passes the piped value
  alone; `|> f(())` passes it and unit;
- a **bare variable / unqualified call**: `|> myFn`;
- a **lambda**: `|> fun x -> …`;
- an **enum constructor**: `|> Ok`, `|> Type.Case`.

An operator is not a segment: `x |> (+) 1` is an argument error. Write `x + 1`,
or `|> (fun v -> v + 1)` mid-pipe.

A pipe RHS that is none of these is a `PARSE-PIPE-SEGMENT` diagnostic.

### Match patterns

Pattern forms:

- literals, including negative literals
- `_`
- variables
- tuples
- lists
- a rest element, last in a list: `[h, ...t]`, `[a, b, ...]`. It lowers to a
  cons chain, so `[a, b, ...t]` is the same pattern as cons of `a` onto cons of
  `b` onto `t`. `...` takes a name, or nothing to ignore the rest (the same
  pattern as `..._`, and what the printer writes).
- or-patterns `p1 | p2`, also inside a field or a list element: `Some(1 | 2)`,
  `[1 | 2]`
- `p as name`, which matches what `p` matches and also binds the whole value:
  `| Some(_) as o`, `| [x, ...] as all`. It is the loosest pattern operator,
  looser than `|`, so `A | B as x` names the whole or-pattern. Next to a
  tuple's commas it needs parentheses either way: `| ((Some(_) as o), n)` names
  one element, `| (a, b) as t` names the tuple, and `(a as x, b)` and
  `(a, b as t)` are errors. A field or list element is its own slot:
  `Some(x as y)`, `[a as b, c]`. `as` is not a keyword; it is only read this
  way straight after a pattern, so a variable may still be called `as`.
- enum patterns, fields in parentheses: `| Ok(x)`, `| Pair(a, b)`, `| None`. A
  tuple field is `| Some((a, b))`. The case may be qualified, `| Result.Ok(x)`;
  the qualifier must name a type, and the pattern matches the same values as
  `| Ok(x)`
- record patterns, by field: `| { name = n, age = 30 }`. Fields left out are
  ignored, so no `...` is needed (and one is an error). A field with no
  `= pattern` binds a variable named after it: `{ name }` is `{ name = name }`.
  A field takes what a constructor field takes, so a tuple needs parentheses,
  `{ pos = (x, y) }`. At least one field; a field may not appear twice. There is
  no type name in front yet
- unit

Names in one pattern cannot be repeated. Every or-pattern branch must bind the
same non-ignored names. `_` and names beginning with `_` are ignored bindings.

Fields written with spaces, `| Ok x`, are a `PARSE-PATTERN` error with a hint.

### Generics

`Name<T, …>` requires `<` to be *adjacent* to the name. A spaced `<` is a
comparison.

Generic type parameters declared after a type or function name must be
apostrophe-prefixed and comma-separated; the list cannot be empty. For example:
`type Pair<'a, 'b> = 'a * 'b`.

Generics work on:

- calls: `parse<Int64>(x)`
- enum constructors: `Type<T>.Case`
- records: `Type<T> { … }`
- type declarations: `type F<'a> = …`

`>>` closes two levels.

A type parameter may carry **bounds**: `'a: Show`, or several joined with `+`
(`'a: Show + Compare`; `,` separates parameters, so it cannot separate bounds).
A bound names a trait, optionally with type arguments (`'a: Convert<Int64>`).
Bounds are allowed on function and type declarations, and on `impl`. A bound
that does not name a trait is `PARSE-BOUND`.

## Types

Primitive types are `Unit Bool BigInt Int8…UInt128 Float Float32 Decimal Char String DateTime Uuid
Bytes` (from `WrittenTypes.primTypes`). `List<T>` and `Dict<K, V>` have special
syntax. Other uppercase names, including `Stream<T>`, are custom
types with optional type arguments.

Type variables are `'a` or bare lowercase names. Tuples use `A * B`. A function
type with one argument is `A -> B`; with several, the arguments are a
parenthesized list, as in a call: `(A, B) -> C`. A function that returns a
function is `A -> (B -> C)`. A chain `A -> B -> C` is a `PARSE-UNEXPECTED` error
with a hint; it is still parsed as `(A, B) -> C`, for recovery.

## Declarations

Package function parameters cannot use the blank name `___`; give the parameter
a name, or use `()` when the parameter is unit.

Declaration forms:

- `let f(p: T, q: U): R = body` defines a function. Parameters are one
  parenthesized list, each annotated, separated by commas or newlines. `()` is a
  unit parameter. A second list, `let f (p: T) (q: U)`, is an error with a hint.
- `let f(p: T):{Http, Clock} R = body` adds an effect row on the return
  colon: the function's permission ceiling, the most it may ever do. It never
  grants anything; at runtime the row is one more restriction on top of the
  instance, run, and package policies. Names are the effect case names
  (`Http`, `FileRead`, `Clock`, …; an unknown name is a parse error).
  `:{}` declares effect-free; no row declares no ceiling.
- `val x = e` defines a module or test-setup value.
- `test name = body` defines a package test whose body returns
  `Stdlib.Test.Result`. Tests use an allow-all test instance policy; called
  functions retain their approvals and permission ceilings. Tests cannot declare
  an effect row of their own.
  Each execution runs in a disposable process and store snapshot.
- `test name = body => raises "message"` defines a package test that
  passes only when evaluating the whole body raises that exact rendered runtime
  error. The `=> raises` clause is test syntax, not an expression or function
  call.
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

- a **record**: `{ f: T, … }`;
- an **enum**: `| A | B(T)` (after the first case each case requires a leading
  `|`). A case's fields go in parentheses, separated by commas, as a
  constructor's do: `| Seg(Point, Point)` has two fields and
  `| Seg(Point * Point)` one tuple field. A field may be labelled,
  `| KeyPressed(key: Key, modifiers: Modifiers)`; the label is documentation,
  and building and matching stay positional. `| B of T` is a `PARSE-UNEXPECTED`
  error with a hint, still parsed for recovery;
- an **alias** to another type: `type Id = String`, `type Pair = Int64 * Int64`.

**Trait declarations** — `trait Name<'a> =` followed by an indented block of
method signatures, one per line, each a full fn signature without a body:

    trait Show<'a> =
      let show(value: 'a): String

The trait needs at least one type parameter (the first is the self type) and
at least one method; a method body is `PARSE-BOUND`. A method may carry an
effect row (`let fetch(u: 'a):{Http} String`), which is the ceiling every
impl of it must fit under. A trait lowers to its own package item
(`PT.Trait`), with the method signatures and their ceilings on it.

**Impl declarations** — `impl[<'a: Bound>] Trait[<Args>] for Type =` followed
by an indented block with one entry per method, each either a full fn
declaration or an alias of an existing fn:

    impl Show for Point =
      let show(p: Point): String = "..."

    impl Add for String =
      let add = Stdlib.String.append

    impl<'a: Show> Show for List<'a> =
      let show(xs: List<'a>): String = "..."

An entry that is neither (a `val`, a `let` bound to anything but a name) is
`PARSE-BOUND`; an impl with no entries is `PARSE-EXPECTED`. An impl lowers to its
own package item (`PT.TraitImpl`) named `<module>[.<Type>].<Trait>`, its declared
methods becoming ordinary fns beneath that name and an alias naming the fn it
points at. The type segment is dropped when the enclosing module is already named
for the type. Duplicate member names are `VALIDATION-IMPL-METHODS`.

Every package test execution starts in a fresh process and store snapshot.
Isolation is runner behavior, with no declaration modifier or stored setting.
The `=> raises` outcome works across the worker boundary.
Proven-safe passing results may still be cached; `test run --force` executes every
selected test again.

### Test classification

The parser represents test assertions in the same syntax tree for every
caller. `Parser.parseFor Validation.Test` accepts them; Package and Script
modes reject them. Successful validation returns a
`ValidatedSourceFile`, which package, test, and CLI entry points require before
lowering. `Parser.parse` remains mode-neutral for tooling.

At the top level:

- `val x = …` declares a test setup value; `let x = …` keeps its normal script
  expression meaning
- `actual = expected` is a test assertion
- the expected form may be `error "msg"`.

## Diagnostics

Diagnostics are structured: `{ code, severity, range, message, related, hint }`.
Codes are stable identifiers (key on these, never on message text):

| code | meaning |
|---|---|
| `PARSE-EXPECTED` | expected X, found Y |
| `PARSE-UNCLOSED` | missing closing delimiter — the opener is in `related` |
| `PARSE-ESCAPE` | invalid escape/codepoint in a literal |
| `PARSE-INT-RANGE` | integer literal out of range (hint: the negated form) |
| `PARSE-FLOAT-PRECISION` | binary64 (binary32 for `f`) conversion loses specified decimal precision |
| `PARSE-FLOAT-OVERFLOW` | float literal rounds to infinity |
| `PARSE-FLOAT-UNDERFLOW` | nonzero float literal rounds to zero |
| `PARSE-FLOAT-INVALID` | malformed decimal float during recovery |
| `PARSE-TOO-DEEP` | nesting beyond the recursion cap |
| `PARSE-UNEXPECTED` | stray token inside a construct |
| `PARSE-PIPE-SEGMENT` | pipe RHS isn't a valid segment |
| `PARSE-PATTERN` | invalid match pattern shape |
| `PARSE-INTERPOLATION` | malformed interpolation body or brace boundary |
| `PARSE-INTERNAL-LOOP` | parser step budget exhausted — a parser bug, please report |
| `PARSE-BOUND` | a type-param bound that does not name a trait, or a trait/impl member that is not supported |
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
| `VALIDATION-TEST-MODE` | a test assertion occurs outside Test mode |
| `VALIDATION-IMPL-METHODS` | an impl names the same member twice |
| `VALIDATION-BUILTIN-TYPE-NAME` | a type declaration uses a built-in type's name, such as `Unit` |

`renderDiagnostic` renders code + position + message + a caret snippet +
related locations + hint. `renderDiagnostics` renders a whole parse's list for the
CLI: in source order, a repeated mistake once with its other places, at most ten. `Builtin.parse` answers the older
range+message tuples, which predate the structure; upgrading it to carry code/related is a
pending Dark-side type change.

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

### Unwrap

Write `?` directly after its operand, inside a function or lambda.
Field access and `?` group left to right: `x?.field?` unwraps `x`, reads `field`,
then unwraps that field; `nested??` unwraps twice.
`f(x?)` unwraps the argument; `f(x)?` unwraps the call's result. Use
`(value |> transform)?` to unwrap a whole pipeline.

See the [CLI syntax documentation](../../../packages/darklang/cli/docs/syntax.dark)
for return types, function boundaries, and evaluation behavior.
