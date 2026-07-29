/// The hand-written parser: source → range-complete `WrittenTypes` tree, capturing
/// fine-grained keyword/symbol/operator ranges (not just node spans) for the highlighter /
/// LSP. Recovers from errors, returning diagnostics alongside a best-effort tree.
module LibParser.Parser

open LibParser.Tokenizer // Pos, TokenRange, Token
open LibParser.Lexer // SpannedToken, tokenize

module WT = LibParser.WrittenTypes
module Validation = LibParser.Validation

type DiagnosticSeverity =
  | DiagError
  | DiagWarning

/// Stable diagnostic codes — documented in GRAMMAR.md; editors/tests key on
/// these, never on the message text.
module DiagnosticCode =
  let expected = "PARSE-EXPECTED" // expected X, found Y
  let unclosed = "PARSE-UNCLOSED" // missing closing delimiter (opener in `related`)
  let escape = "PARSE-ESCAPE" // invalid escape/codepoint in a literal
  let intRange = "PARSE-INT-RANGE" // integer literal out of range
  let tooDeep = "PARSE-TOO-DEEP" // nesting beyond the recursion cap
  let unexpected = "PARSE-UNEXPECTED" // stray token inside a construct
  let pipeSegment = "PARSE-PIPE-SEGMENT" // pipe RHS isn't a valid segment
  let pattern = "PARSE-PATTERN" // invalid match pattern shape
  let interpolation = "PARSE-INTERPOLATION" // malformed interpolation body/braces
  let internalLoop = "PARSE-INTERNAL-LOOP" // parser step budget exhausted (parser bug)
  let lex = "LEX" // tokenizer-level recovery (unterminated literal, …)

type Diagnostic =
  { code : string // one of DiagnosticCode — stable across releases
    severity : DiagnosticSeverity
    range : TokenRange
    message : string
    // secondary locations, e.g. the opening delimiter of an unclosed pair
    related : List<TokenRange * string>
    hint : Option<string> }

type ParseResult = { parsed : Option<WT.ParsedFile>; diagnostics : List<Diagnostic> }

let diagnosticOfValidationIssue (issue : Validation.Issue) : Diagnostic =
  { code = Validation.IssueCode.toString issue.code
    severity = DiagError
    range = issue.range
    message = issue.message
    related = issue.related
    hint = issue.hint }

/// One offside scope: the current statement's anchor column (`stmtCol`; -1 = none)
/// and `stmtExact`, a flag marking a parenthesized body — there the closing `)`
/// is the real delimiter, so only a token at EXACTLY the anchor column starts a
/// new statement; dedented continuations (`|> (fn\n  args-below-callee)`) stay
/// part of the current one. Constructs enter scopes only through the `with*`
/// helpers in `parseTokens`, so prior state is restored by construction.
type OffsideScope = { mutable stmtCol : int; mutable stmtExact : bool }

let private infixOf (t : Token) : WT.Infix option =
  match t with
  | TPlus -> Some(WT.InfixFnCall WT.ArithmeticPlus)
  | TMinus -> Some(WT.InfixFnCall WT.ArithmeticMinus)
  | TStar -> Some(WT.InfixFnCall WT.ArithmeticMultiply)
  | TSlash -> Some(WT.InfixFnCall WT.ArithmeticDivide)
  | TPercent -> Some(WT.InfixFnCall WT.ArithmeticModulo)
  | TPlusPlus -> Some(WT.InfixFnCall WT.StringConcat)
  | TEqEq -> Some(WT.InfixFnCall WT.ComparisonEquals)
  | TNeq -> Some(WT.InfixFnCall WT.ComparisonNotEquals)
  | TLt -> Some(WT.InfixFnCall WT.ComparisonLessThan)
  | TGt -> Some(WT.InfixFnCall WT.ComparisonGreaterThan)
  | TLte -> Some(WT.InfixFnCall WT.ComparisonLessThanOrEqual)
  | TGte -> Some(WT.InfixFnCall WT.ComparisonGreaterThanOrEqual)
  | TAnd -> Some(WT.BinOp WT.BinOpAnd)
  | TOr -> Some(WT.BinOp WT.BinOpOr)
  | TBitXor -> Some(WT.InfixFnCall WT.ArithmeticPower) // `^` is exponentiation in surface syntax
  | _ -> None

// The one definition of "an integer-literal token" — `canStartAtom` and
// `canStartPattern` both build on it so the lists can't drift from each other.
// Matched EXHAUSTIVELY (no `_`): a new `Token` case won't compile until it's
// classified here, so a new integer type can't silently fall through to `false`
// — the drift that once made `| Ok 5y ->` unparseable while `Ok 5y` worked.
let private isIntLit (t : Token) : bool =
  match t with
  | TInt _
  | TInt64 _
  | TInt8 _
  | TUInt8 _
  | TInt16 _
  | TUInt16 _
  | TInt32 _
  | TUInt32 _
  | TUInt64 _
  | TInt128 _
  | TUInt128 _ -> true
  | TFloat _
  | TStringLit _
  | TCharLit _
  | TInterpString
  | TTrue
  | TFalse
  | TPlus
  | TPlusPlus
  | TMinus
  | TStar
  | TSlash
  | TLParen
  | TRParen
  | TLet
  | TVal
  | TIn
  | TIf
  | TElif
  | TThen
  | TElse
  | TType
  | TCons
  | TColon
  | TComma
  | TSemicolon
  | TDot
  | TLBrace
  | TRBrace
  | TBar
  | TOf
  | TMatch
  | TWith
  | TFun
  | TArrow
  | TUnderscore
  | TWhen
  | TLBracket
  | TRBracket
  | TEquals
  | TEqEq
  | TNeq
  | TLt
  | TGt
  | TLte
  | TGte
  | TAnd
  | TOr
  | TNot
  | TPipe
  | TDotDotDot
  | TPercent
  | TShl
  | TShr
  | TBitAnd
  | TBitOr
  | TBitXor
  | TBitNot
  | TAt
  | TIdent _
  | TEOF -> false

let private canStartAtom (t : Token) : bool =
  isIntLit t
  || (match t with
      | TFloat _
      | TCharLit _
      | TTrue
      | TFalse
      | TStringLit _
      | TInterpString
      | TIdent _
      | TLParen
      | TLBracket
      // a record/anonymous-record/update `{ … }` can be a function argument, e.g.
      // `parseArgs tail { acc with port = p }`
      | TLBrace -> true
      | _ -> false)

// `TMinus` is included so a negative-literal enum-pattern field (`| Ok -4y ->`)
// parses; parsePatternBase's TMinus case handles it.
let private canStartPattern (t : Token) : bool =
  isIntLit t
  || (match t with
      | TUnderscore
      | TMinus
      | TFloat _
      | TCharLit _
      | TTrue
      | TFalse
      | TStringLit _
      | TIdent _
      | TLParen
      | TLBracket -> true
      | _ -> false)

// tokens that close/separate a block — another statement can't start with these
let private closesOrSeparates (t : Token) : bool =
  match t with
  | TRParen
  | TRBracket
  | TRBrace
  | TSemicolon
  | TComma
  | TIn
  | TThen
  | TElse
  | TWith
  | TArrow
  | TEOF -> true
  | _ -> false

// tokens hole-recovery must never consume: closing/separating tokens (the enclosing
// construct needs them to close cleanly) and declaration starters (the next
// declaration must survive a broken one before it)
let private isRecoveryBarrier (t : Token) : bool =
  closesOrSeparates t
  || (match t with
      | TLet
      | TType
      | TVal -> true
      | _ -> false)


/// All parser state, threaded explicitly through every parse function (no
/// closure): the token stream, the diagnostics sink, and the offside/recovery
/// registers. One value per parse; `parseTokens` constructs it.
type ParserState =
  { toks : SpannedToken[] // the token stream being parsed
    tokenCount : int // = toks.Length, cached (read on every bounds check)
    diagnostics : System.Collections.Generic.List<Diagnostic> // parse errors collected during recovery
    scopes : System.Collections.Generic.Stack<OffsideScope> // offside anchor stack (a frame per let/if/match/paren body)
    // Closing a nested generic like `Dict<List<Int>>` ends in `>>`, which the
    // lexer produces as ONE token but which must close TWO levels. Closing the
    // inner `List<Int>` uses only the first `>`, so the second is "left over"
    // for the outer `Dict<…>` to close. `pendingGt` counts those left-over `>`s;
    // `pendingGtRange` is the source range of the next one to spend.
    mutable pendingGt : int
    mutable pendingGtRange : TokenRange
    mutable declAnchor : int // start column of the declaration being parsed (-1 = none); a token at-or-left ends the construct
    mutable depth : int // current recursion depth — stack-overflow guard (see `maxDepth`)
    mutable abandoned : bool // a guard aborted the parse; silences the unwind's cascade of secondary diagnostics
    // parseExpr/pattern/type entry count — the no-progress backstop (see
    // `outOfFuel`); any runaway loop exhausts it and abandons with a
    // diagnostic instead of hanging
    mutable steps : int
    // How deeply string interpolations are nested. Each `{expr}` body is parsed
    // by a FRESH recursive `parseTokens`, whose own `depth` guard restarts at 0 —
    // so `depth` can't see an interpolation bomb like `$"{$"{$"…"}"}"`, where the
    // nesting is in the chain of recursive parses, not one deep expression.
    // Threaded as parent + 1 and capped at `maxInterpNesting` to stay stack-safe.
    interpDepth : int }

let private maxDepth = 200

let tok (state : ParserState) i =
  if i < state.tokenCount then state.toks[i].token else TEOF
let rng (state : ParserState) i =
  if i < state.tokenCount then
    state.toks[i].range
  else
    state.toks[state.tokenCount - 1].range
// raw source text of the token (e.g. a type variable `'a` lexes to an
// ident "a" but its text keeps the leading tick)
let txt (state : ParserState) i =
  if i < state.tokenCount then state.toks[i].text else ""
// `///` doc comment attached to the token at `i` (a declaration keyword), if any
let docOf (state : ParserState) i =
  (if i < state.tokenCount then state.toks[i].docComment else None)
  |> Option.defaultValue ""
// Zero-width range at `r`'s end: the range for a synthetic/missing node (an
// absent `>`, a bare tuple's missing parens, an unsplit int suffix) — points at
// "where it should be" without claiming any real source characters.
let zeroWidthAtEnd (r : TokenRange) : TokenRange = { start = r.end_; end_ = r.end_ }
let span (a : TokenRange) (b : TokenRange) : TokenRange =
  { start = a.start; end_ = b.end_ }

let private advancePos (start : Pos) (text : string) (count : int) : Pos =
  let mutable pos = start
  for index in 0 .. min count text.Length - 1 do
    if text[index] = '\n' then
      pos <- { row = pos.row + 1; column = 0 }
    else
      pos <- { pos with column = pos.column + 1 }
  pos

let private splitTrailingRange
  (state : ParserState)
  (i : int)
  (trailingLength : int)
  : TokenRange * TokenRange =
  let whole = rng state i
  let text = txt state i
  let boundary = advancePos whole.start text (max 0 (text.Length - trailingLength))
  ({ start = whole.start; end_ = boundary }, { start = boundary; end_ = whole.end_ })

let private literalTextRanges
  (state : ParserState)
  (i : int)
  (delimiter : string)
  : TokenRange * TokenRange * TokenRange =
  let whole = rng state i
  let text = txt state i
  let delimiterLength = delimiter.Length
  let hasClose = text.Length >= delimiterLength * 2 && text.EndsWith delimiter
  let openEnd = advancePos whole.start text (min delimiterLength text.Length)
  let contentEndIndex =
    if hasClose then text.Length - delimiterLength else text.Length
  let contentEnd = advancePos whole.start text contentEndIndex
  let closeEnd = if hasClose then whole.end_ else contentEnd
  ({ start = whole.start; end_ = openEnd },
   { start = openEnd; end_ = contentEnd },
   { start = contentEnd; end_ = closeEnd })
// set when a guard abandons the parse: suppresses the cascade of secondary
// diagnostics from the unwinding frames
let errFull
  (state : ParserState)
  (code : string)
  (i : int)
  (m : string)
  (related : List<TokenRange * string>)
  (hint : Option<string>)
  =
  if not state.abandoned then
    state.diagnostics.Add
      { code = code
        severity = DiagError
        range = rng state i
        message = m
        related = related
        hint = hint }

let err (state : ParserState) (code : string) (i : int) (m : string) =
  errFull state code i m [] None
// what the parser is looking at, for "expected X, found Y" messages
let foundDesc (state : ParserState) (i : int) : string =
  if i >= state.tokenCount || tok state i = TEOF then
    "end of file"
  else
    let t = (txt state i).Replace("\n", "\\n")
    if t.Length > 24 then $"'{t.Substring(0, 24)}…'" else $"'{t}'"
let errExpected (state : ParserState) (i : int) (what : string) =
  err state DiagnosticCode.expected i $"expected {what}, found {foundDesc state i}"
// a missing closing delimiter: point back at its opener
let errUnclosed
  (state : ParserState)
  (i : int)
  (closeSym : string)
  (openSym : string)
  (openR : TokenRange)
  =
  errFull
    state
    DiagnosticCode.unclosed
    i
    $"expected '{closeSym}' to close the '{openSym}' at line {openR.start.row + 1}:{openR.start.column + 1}, found {foundDesc state i}"
    [ (openR, $"the '{openSym}' opened here") ]
    None

// --- recursion-depth guard ---
// parseExpr/parseTypeRef/parsePatternBase recurse per nesting level, so a
// pathological `((((…` would overflow the stack — and a .NET StackOverflow is
// UNCATCHABLE (it kills the process). At the cap we diagnose once and skip to
// EOF; 200 levels is far beyond real code.
/// No-progress backstop: each parseExpr/parsePatternBase/parseTypeRef entry
/// spends one step. A real parse of n tokens uses ≪ 300·n (corpus-measured); a
/// loop that stops consuming tokens spends them forever — so exhaustion means a
/// parser bug, and we abandon with a diagnostic instead of hanging the host.
let outOfFuel (state : ParserState) (i : int) : bool =
  state.steps <- state.steps + 1
  if state.steps <= 4000 + state.tokenCount * 300 then
    false
  else
    errFull
      state
      DiagnosticCode.internalLoop
      i
      "internal parser error: step budget exhausted (parser loop?); parsing abandoned"
      []
      (Some
        "please report this — it indicates a parser bug, not a problem with your code")
    state.abandoned <- true
    true

let tooDeep (state : ParserState) (i : int) : bool =
  if state.depth < maxDepth then
    false
  else
    errFull
      state
      DiagnosticCode.tooDeep
      i
      $"nesting too deep (over {maxDepth} levels); parsing abandoned"
      []
      (Some "split the expression with intermediate `let` bindings")
    state.abandoned <- true
    true

// A sized-int literal whose magnitude is the type's |MinValue| lexes to
// MinValue (so the NEGATED literal can exist: `-128y`); consumed WITHOUT the
// minus, the written magnitude is out of range — diagnose instead of silently
// wrapping (`128y` is NOT -128). Only the negating TMinus branches consume
// these tokens without passing through here.
let checkBareMinMagnitude (state : ParserState) (i : int) : unit =
  let isMinMagnitude =
    match tok state i with
    | TInt8 v -> v = System.SByte.MinValue
    | TInt16 v -> v = System.Int16.MinValue
    | TInt32 v -> v = System.Int32.MinValue
    | TInt64 v -> v = System.Int64.MinValue
    | TInt128 v -> v = System.Int128.MinValue
    | _ -> false
  if isMinMagnitude then
    errFull
      state
      DiagnosticCode.intRange
      i
      $"integer literal {txt state i} is out of range (this magnitude is only valid negated: -{txt state i})"
      []
      (Some $"write it negated: -{txt state i}")

// whole/fraction decimal strings of a float literal at token `i`. The
// double's shortest round-trip form is used when it's a plain decimal (the
// usual case); when it needs an exponent (`1e300`, `0.00000001`) or isn't
// finite-decimal at all, the SOURCE text is decimal-shifted instead — the
// PT float representation is exponent-free strings, and an exponent leaking
// into the whole part crashes `makeFloat` downstream.
let floatParts (state : ParserState) (i : int) (v : float) : string * string =
  let r = (abs v).ToString("R", System.Globalization.CultureInfo.InvariantCulture)
  let plainDecimal = r |> Seq.forall (fun c -> System.Char.IsDigit c || c = '.')
  if plainDecimal then
    match r.Split('.') with
    | [| w; f |] -> (w, f)
    | _ -> (r, "0")
  else
    // decimal-shift the literal text `mant[.frac][eE][+-]exp` (exact, no
    // floating-point re-derivation)
    let t = (txt state i).TrimStart('-')
    let (mant, exp) =
      match t.Split([| 'e'; 'E' |]) with
      | [| m; e |] ->
        let exponent =
          match System.Int32.TryParse e with
          | true, x when x >= -400 && x <= 400 -> x
          | true, x ->
            err
              state
              DiagnosticCode.intRange
              i
              $"Float exponent {x} is outside the supported range -400..400"
            max -400 (min 400 x)
          | _ ->
            err
              state
              DiagnosticCode.intRange
              i
              "Float exponent is too large to represent"
            0
        (m, exponent)
      | _ -> (t, 0)
    let (w, f) =
      match mant.Split('.') with
      | [| w; f |] -> (w, f)
      | _ -> (mant, "")
    let digits = w + f
    let pointPos = w.Length + exp
    if pointPos <= 0 then
      ("0", String.replicate (-pointPos) "0" + digits)
    elif pointPos >= digits.Length then
      (digits + String.replicate (pointPos - digits.Length) "0", "0")
    else
      (digits.Substring(0, pointPos), digits.Substring(pointPos))

// Reject invalid escapes / codepoints in string, char, and interpolated-string
// literals (triple-quoted forms are raw, so skipped). A diagnostic here becomes a
// `ParseError.Message` — the escape is otherwise silently error-recovered.
let stripDelims (raw : string) (lead : string) (close : string) : string =
  let a = if raw.StartsWith lead then lead.Length else 0
  let b =
    if raw.Length > a && raw.EndsWith close then
      raw.Length - close.Length
    else
      raw.Length
  if b > a then raw.Substring(a, b - a) else ""
let validateLiterals (state : ParserState) : unit =
  for vi in 0 .. state.tokenCount - 1 do
    match state.toks[vi].token with
    | TStringLit _ when not ((txt state vi).StartsWith "\"\"\"") ->
      if Lexer.hasInvalidEscape (stripDelims (txt state vi) "\"" "\"") then
        err
          state
          DiagnosticCode.escape
          vi
          "Invalid escape sequence or codepoint in string literal"
    | TCharLit _ ->
      if Lexer.hasInvalidEscape (stripDelims (txt state vi) "'" "'") then
        err
          state
          DiagnosticCode.escape
          vi
          "Invalid escape sequence or codepoint in character literal"
      match state.toks[vi].token with
      | TCharLit value when
        System.Globalization.StringInfo.ParseCombiningCharacters(value).Length <> 1
        ->
        err
          state
          DiagnosticCode.escape
          vi
          "Character literal must contain exactly one grapheme"
      | _ -> ()
    | TInterpString when not ((txt state vi).StartsWith "$\"\"\"") ->
      let inner = stripDelims (txt state vi) "$\"" "\""
      if Lexer.hasInvalidEscapeInterp inner then
        err
          state
          DiagnosticCode.escape
          vi
          "Invalid escape sequence or codepoint in interpolated string"
      if Lexer.hasSingleCloseBraceInterp inner false then
        err
          state
          DiagnosticCode.interpolation
          vi
          "Single '}' in interpolated string text; use '}}' or '\\}' for a literal brace"
    | TInterpString ->
      let inner = stripDelims (txt state vi) "$\"\"\"" "\"\"\""
      if Lexer.hasSingleCloseBraceInterp inner true then
        err
          state
          DiagnosticCode.interpolation
          vi
          "Single '}' in raw interpolated string text; use '}}' for a literal brace"
    | _ -> ()

// qualified name: ident (. ident)*  → (modules, finalIdent, nextIndex)
let parseQualified
  (state : ParserState)
  (i : int)
  : (List<WT.Identifier * TokenRange> * WT.Identifier * int) =
  let first : WT.Identifier =
    match tok state i with
    | TIdent s -> { range = rng state i; name = s }
    | _ ->
      errExpected state i "an identifier"
      { range = rng state i; name = "_" }
  let mods = System.Collections.Generic.List<WT.Identifier * TokenRange>()
  let mutable cur = first
  let mutable k = i + 1
  let mutable go = true
  while go do
    // only step into `.seg` as a module path when the CURRENT segment is
    // uppercase (a module); a lowercase ident's `.field` is postfix access,
    // left for parsePostfix to handle.
    match tok state k, tok state (k + 1) with
    | TDot, TIdent s when cur.name.Length > 0 && System.Char.IsUpper cur.name[0] ->
      mods.Add(cur, rng state k) // previous segment becomes a module, with the dot range
      cur <- { range = rng state (k + 1); name = s }
      k <- k + 2
    | _ -> go <- false
  (List.ofSeq mods, cur, k)

// Matches a written type name against the primitive types.
let (|PrimTypeName|_|) (s : string) : (WT.Range -> WT.TypeReference) option =
  WT.primTypeFromName s

// `>>` lexes as one TShr token but closes two generic levels. `state.pendingGt`
// carries the leftover `>` to the enclosing type so `List<List<T>>` parses.

// Close one generic level (`>`), splitting a `>>` (TShr) into one consumed `>`
// and one pending. Returns (close-`>` range, next index).
let expectGt (state : ParserState) (j : int) : TokenRange * int =
  if state.pendingGt > 0 then
    state.pendingGt <- state.pendingGt - 1
    (state.pendingGtRange, j)
  elif tok state j = TGt then
    (rng state j, j + 1)
  elif tok state j = TShr then
    let r = rng state j
    let mid = { row = r.start.row; column = r.start.column + 1 }
    state.pendingGtRange <- { start = mid; end_ = r.end_ }
    state.pendingGt <- state.pendingGt + 1
    ({ start = r.start; end_ = mid }, j + 1)
  else
    errExpected state j "'>'"
    (zeroWidthAtEnd (rng state j), j)

// Skip a `< … >` type-argument list (not modelled yet); a trailing `>>` leaves
// one `>` pending for the enclosing generic.
// declaration type parameters `<'a, 'b>` — collect the (tick-stripped) names
// so generic types/fns keep their params (needed for runtime type unification).
let parseTypeParams
  (state : ParserState)
  (i : int)
  : List<string * TokenRange> * int =
  if tok state i <> TLt then
    ([], i)
  else
    if
      i > 0
      && ((rng state i).start.row <> (rng state (i - 1)).end_.row
          || (rng state i).start.column <> (rng state (i - 1)).end_.column)
    then
      err
        state
        DiagnosticCode.expected
        i
        "Generic type parameters must be adjacent to the declaration name"
    let names = System.Collections.Generic.List<string * TokenRange>()
    let mutable k = i + 1
    let mutable expectingName = true
    while tok state k <> TGt && tok state k <> TShr && tok state k <> TEOF do
      match expectingName, tok state k with
      | true, TIdent name ->
        if not ((txt state k).StartsWith "'") then
          err
            state
            DiagnosticCode.expected
            k
            "Declared type parameters must start with an apostrophe, such as 'a"
        names.Add(name, rng state k)
        expectingName <- false
        k <- k + 1
      | false, TComma ->
        expectingName <- true
        k <- k + 1
      | false, TIdent _ ->
        errExpected state k "a comma between type parameters"
        expectingName <- true
      | _ ->
        errExpected state k "a type parameter"
        k <- k + 1
    if names.Count = 0 then errExpected state (i + 1) "at least one type parameter"
    elif expectingName then errExpected state k "a type parameter after ','"
    let k2 =
      if tok state k = TGt || tok state k = TShr then
        k + 1
      else
        (errExpected state k "'>' to close the type-parameter list"
         k)
    (List.ofSeq names, k2)

// --- offside scope stack ---
// One scope = `stmtCol` (the current statement's anchor column; -1 = none) +
// `stmtExact` (a parenthesized body: only a token at EXACTLY the anchor column
// starts a new statement — the `)` is the real delimiter). let/if/match push a
// fresh scope so their sub-expressions keep normal offside (a let value must
// not swallow the next statement). All state transitions go through the
// `with*` helpers below, which restore the prior scope by construction — a
// leftover flag can't leak into what follows.
// anchor the current scope's statement column (per statement / element)
let setStmtCol (state : ParserState) (col : int) : unit =
  state.scopes.Peek().stmtCol <- col
// fresh sub-statement scope, statement anchored at `col`
let withStmtScope (state : ParserState) (col : int) (f : unit -> 'a) : 'a =
  state.scopes.Push { stmtCol = col; stmtExact = false }
  try
    f ()
  finally
    state.scopes.Pop() |> ignore
// fresh scope; statement anchor inherited (managed per-element by `f`)
let withElementScope (state : ParserState) (f : unit -> 'a) : 'a =
  state.scopes.Push { stmtCol = state.scopes.Peek().stmtCol; stmtExact = false }
  try
    f ()
  finally
    state.scopes.Pop() |> ignore
// re-anchor the statement column within the CURRENT scope as a
// parenthesized (exact-column) anchor, restoring both after
let withStmtColExact (state : ParserState) (col : int) (f : unit -> 'a) : 'a =
  let s = state.scopes.Peek()
  let savedCol = s.stmtCol
  let savedExact = s.stmtExact
  s.stmtCol <- col
  s.stmtExact <- true
  try
    f ()
  finally
    s.stmtCol <- savedCol
    s.stmtExact <- savedExact

// Column of the DECLARATION currently being parsed (set per item by
// parseItems), or -1. Recovery only: a decl-start keyword at or left of this
// column can never belong to a construct inside the declaration, so an
// unclosed delimiter above must stop instead of swallowing the next
// declaration (`let broken = [1L;` must not eat the `let fine …` below it).
let declBarrier (state : ParserState) (k : int) : bool =
  (match tok state k with
   | TLet
   | TType
   | TVal -> true
   | _ -> false)
  && state.declAnchor >= 0
  && (rng state k).start.column <= state.declAnchor

// Offside: a trailing operand (application arg, enum-constructor/pattern field)
// at `k` continues the construct started at `headIdx` only if it's on the same
// line as the head, or indented further (or we're inside parens). A token on a
// new line at the same-or-lower indent starts a new statement.
let offsideContinues (state : ParserState) (headIdx : int) (k : int) : bool =
  (rng state k).start.row = (rng state headIdx).start.row
  || (rng state k).start.column > (rng state headIdx).start.column
  || (let s = state.scopes.Peek()
      s.stmtCol >= 0
      && (if s.stmtExact then
            (rng state k).start.column <> s.stmtCol
          else
            (rng state k).start.column > s.stmtCol))

// A `-` GLUED to a following number, with a space before it, is a negative-literal
// ARGUMENT (`f a -1`), not subtraction (`f a - 1` = `(f a) - 1`). The application
// arg loop accepts it so `Float.multiply a -1.0` / `add 5L -1L` parse correctly
// (matches F#'s high-precedence-application rule).
let isNegLitArg (state : ParserState) (k : int) : bool =
  tok state k = TMinus
  && (match tok state (k + 1) with
      | TInt _
      | TInt64 _
      | TInt8 _
      | TInt16 _
      | TInt32 _
      | TInt128 _
      | TFloat _ -> true
      | _ -> false)
  && (rng state (k + 1)).start.row = (rng state k).end_.row
  && (rng state (k + 1)).start.column = (rng state k).end_.column // no space after `-`
  && (k = 0
      || (rng state k).start.row <> (rng state (k - 1)).end_.row
      || (rng state k).start.column > (rng state (k - 1)).end_.column) // space before `-`

let private requireElementSeparator
  (state : ParserState)
  (previousRange : TokenRange)
  (nextIndex : int)
  (expected : string)
  : unit =
  if
    tok state nextIndex <> TEOF
    && (rng state nextIndex).start.row <= previousRange.end_.row
  then
    errExpected state nextIndex expected

let rec parseExpr (state : ParserState) (i : int) : WT.Expr * int =
  if tooDeep state i || outOfFuel state i then
    (WT.EError(rng state i), state.tokenCount - 1)
  else
    state.depth <- state.depth + 1
    let r =
      match tok state i with
      | TLet
      | TIf
      | TMatch ->
        // anchor arg-offside at this construct's column so a let value / if cond /
        // match expr doesn't grab the following body (`let x = v\n body`)
        withStmtScope state (rng state i).start.column (fun () ->
          match tok state i with
          | TLet -> parseLet state i
          | TIf -> parseIf state (rng state i).start.column i
          | _ -> parseMatch state i)
      | _ -> parsePipe state i
    state.depth <- state.depth - 1
    r

// pipe is the lowest precedence: `expr |> seg |> seg …`
and parsePipe (state : ParserState) (i : int) : WT.Expr * int =
  let (expr, j) = parseInfix state i
  if tok state j <> TPipe then
    (expr, j)
  else
    let pipeExprs = System.Collections.Generic.List<TokenRange * WT.PipeExpr>()
    let mutable k = j
    while tok state k = TPipe do
      let pipeR = rng state k
      // `x |> (op) y` desugars to `x op y` (the piped value is the LEFT operand),
      // so an operator section directly after `|>` with an argument becomes a
      // pipe-infix — not the section lambda applied (which would flip the order).
      if
        tok state (k + 1) = TLParen
        && Option.isSome (infixOf (tok state (k + 2)))
        && tok state (k + 3) = TRParen
        && (canStartAtom (tok state (k + 4)) || isNegLitArg state (k + 4))
      then
        let opR = rng state (k + 2)
        let infix = (infixOf (tok state (k + 2))).Value
        let (arg, k2) = parseInfix state (k + 4)
        pipeExprs.Add(
          pipeR,
          WT.EPipeInfix(span opR (WT.exprRange arg), (opR, infix), arg)
        )
        if k2 > k then k <- k2 else k <- k + 1
      else
        let (rhs, k2) = parseInfix state (k + 1)
        (match toPipeExpr rhs with
         | Some seg -> pipeExprs.Add(pipeR, seg)
         | None ->
           err state DiagnosticCode.pipeSegment (k + 1) "unsupported pipe segment") // → diagnostic
        if k2 > k then k <- k2 else k <- k + 1
    let endR = if k > 0 then rng state (k - 1) else rng state j
    (WT.EPipe(span (WT.exprRange expr) endR, expr, List.ofSeq pipeExprs), k)

// convert a parsed pipe RHS expression into a structured pipe segment
and toPipeExpr (e : WT.Expr) : WT.PipeExpr option =
  match e with
  // keep the callee's type args — `x |> parse<T>` needs `T` (dropping it left the
  // piped fn call with no type args, so `parse` had no target type: "type 'a")
  | WT.EApply(r, WT.EFnName(_, q), typeArgs, args) ->
    Some(WT.EPipeFnCall(r, q, typeArgs, args))
  | WT.EFnName(r, q) -> Some(WT.EPipeFnCall(r, q, [], []))
  | WT.EVariable(r, name) -> Some(WT.EPipeVariableOrFnCall(r, name))
  | WT.ELambda(r, pats, body, kf, ar) -> Some(WT.EPipeLambda(r, pats, body, kf, ar))
  | WT.EEnum(r, tname, cn, fields, dot) ->
    Some(WT.EPipeEnum(r, tname, cn, fields, dot))
  | WT.EApply(r, WT.EVariable(vr, name), typeArgs, args) ->
    Some(
      WT.EPipeFnCall(
        r,
        { range = r; modules = []; fn = { range = vr; name = name } },
        typeArgs,
        args
      )
    )
  | _ -> None

// an indentation-delimited sequence of statements (function / if-branch / match
// arm / lambda body): same-column statements on new lines fold into nested
// EStatement. Fresh scope so statements separate by column.
and parseBlock (state : ParserState) (i : int) : WT.Expr * int =
  withStmtScope state (rng state i).start.column (fun () ->
    parseBlockAt state (rng state i).start.column i)

// iterative (a 10k-statement body must not recurse 10k deep); statements
// fold right-nested into EStatement afterwards
and parseBlockAt (state : ParserState) (col : int) (i : int) : WT.Expr * int =
  let stmts = System.Collections.Generic.List<WT.Expr>()
  let mutable k = i
  let mutable go = true
  while go do
    let (stmt, j) = parseExpr state k
    stmts.Add stmt
    if j = k then
      go <- false // no progress — stop (avoids a spin)
    elif tok state j = TSemicolon then
      k <- j + 1
    elif hasNextStmt state col j then
      k <- j
    else
      k <- j
      go <- false
  let folded =
    stmts
    |> List.ofSeq
    |> List.reduceBack (fun s acc ->
      WT.EStatement(span (WT.exprRange s) (WT.exprRange acc), s, acc))
  (folded, k)

and hasNextStmt (state : ParserState) (col : int) (k : int) : bool =
  tok state k <> TEOF
  && (rng state k).start.column = col
  && tok state k <> TBar
  && not (closesOrSeparates (tok state k))

and parseMatch (state : ParserState) (i : int) : WT.Expr * int =
  let kwMatch = rng state i
  let (first, j0) = parseExpr state (i + 1)
  // bare tuple expr: `match a, b with`
  let (expr, j) =
    if tok state j0 <> TComma then
      (first, j0)
    else
      let comma = rng state j0
      let (second, k) = parseExpr state (j0 + 1)
      let rest = System.Collections.Generic.List<TokenRange * WT.Expr>()
      let mutable m = k
      let mutable go = true
      while go && tok state m = TComma do
        let cr = rng state m
        let (e, m2) = parseExpr state (m + 1)
        rest.Add(cr, e)
        if m2 > m then m <- m2 else go <- false
      let z = zeroWidthAtEnd (WT.exprRange first)
      let endE = if rest.Count > 0 then snd (Seq.last rest) else second
      (WT.ETuple(
        span (WT.exprRange first) (WT.exprRange endE),
        first,
        comma,
        second,
        List.ofSeq rest,
        z,
        z
       ),
       m)
  let (kwWith, afterWith) =
    if tok state j = TWith then
      (rng state j, j + 1)
    else
      errExpected state j "'with' in match"
      (zeroWidthAtEnd (rng state j), j)
  let cases = System.Collections.Generic.List<WT.MatchCase>()
  // arms align on the first `|`'s column; a `|` LESS indented than that belongs
  // to an enclosing match (so a nested match doesn't swallow the outer's arms).
  let armCol =
    if tok state afterWith = TBar then (rng state afterWith).start.column else 0
  let armRow =
    if tok state afterWith = TBar then (rng state afterWith).start.row else -1
  let mutable k = afterWith
  while (tok state k = TBar
         && ((rng state k).start.row = armRow || (rng state k).start.column = armCol)) do
    let barR = rng state k
    let (pat, k2) = parseMatchPattern state (k + 1)
    let (whenCond, k3) =
      if tok state k2 = TWhen then
        let whenR = rng state k2
        let (g, kg) = parseExpr state (k2 + 1)
        (Some(whenR, g), kg)
      else
        (None, k2)
    let (arrowR, k4) =
      if tok state k3 = TArrow then
        (rng state k3, k3 + 1)
      else
        errExpected state k3 "'->' in match case"
        (zeroWidthAtEnd (rng state k3), k3)
    let (rhs, k5) = parseBlock state k4
    cases.Add(
      { barRange = barR
        pat = pat
        arrowRange = arrowR
        whenCondition = whenCond
        rhs = rhs }
    )
    if k5 > k then k <- k5 else k <- k + 1
  if cases.Count = 0 then
    errExpected state afterWith "at least one match case starting with '|'"
  let endR = if cases.Count > 0 then WT.exprRange (Seq.last cases).rhs else kwWith
  (WT.EMatch(span kwMatch endR, expr, List.ofSeq cases, kwMatch, kwWith), k)

// or-level: `p1 | p2 | …` (stops at `->` / `when`)
// top level: a bare tuple `a, b` (comma-separated, no parens); else an or-pattern
// A full match-arm pattern. Precedence, matching F#: `|` (or) is LOOSEST, then
// `,` (tuple), then `::` (cons). So `1, 2 | 3, 4` is `(1,2) | (3,4)` — an or of
// two tuples, NOT a 3-tuple with an or in the middle. Hence `|` is the OUTER
// level here, wrapping tuples (`parsePatternTuple`).
and parseMatchPattern (state : ParserState) (i : int) : WT.MatchPattern * int =
  let (first, j) = parsePatternTuple state i
  if tok state j <> TBar then
    (first, j)
  else
    let pats = System.Collections.Generic.List<WT.MatchPattern>()
    pats.Add first
    let mutable k = j
    while tok state k = TBar do
      let (p, k2) = parsePatternTuple state (k + 1)
      pats.Add p
      if k2 > k then k <- k2 else k <- k + 1
    (WT.MPOr(span (WT.mpRange first) (WT.mpRange (Seq.last pats)), List.ofSeq pats),
     k)

// tuple level: `p1, p2, …` (bare — no parens). Elements are cons-patterns; `|`
// binds looser (handled above) so it can't appear as a bare tuple element.
and parsePatternTuple (state : ParserState) (i : int) : WT.MatchPattern * int =
  let (first, j) = parsePatternCons state i
  if tok state j <> TComma then
    (first, j)
  else
    let comma = rng state j
    let (second, k) = parsePatternCons state (j + 1)
    let rest = System.Collections.Generic.List<TokenRange * WT.MatchPattern>()
    let mutable m = k
    let mutable go = true
    while go && tok state m = TComma do
      let cr = rng state m
      let (p, m2) = parsePatternCons state (m + 1)
      rest.Add(cr, p)
      if m2 > m then m <- m2 else go <- false
    let z = zeroWidthAtEnd (WT.mpRange first) // bare tuple: no parens
    let endP = if rest.Count > 0 then snd (Seq.last rest) else second
    (WT.MPTuple(
      span (WT.mpRange first) (WT.mpRange endP),
      first,
      comma,
      second,
      List.ofSeq rest,
      z,
      z
     ),
     m)

// or of cons-patterns, NO tuple — used for enum-ctor fields, where a bare `,`
// separates FIELDS (`Case(a, b)` = two fields), not tuple elements.
and parsePatternOr (state : ParserState) (i : int) : WT.MatchPattern * int =
  let (first, j) = parsePatternCons state i
  if tok state j = TBar then
    let pats = System.Collections.Generic.List<WT.MatchPattern>()
    pats.Add first
    let mutable k = j
    while tok state k = TBar do
      let (p, k2) = parsePatternCons state (k + 1)
      pats.Add p
      if k2 > k then k <- k2 else k <- k + 1
    (WT.MPOr(span (WT.mpRange first) (WT.mpRange (Seq.last pats)), List.ofSeq pats),
     k)
  else
    (first, j)

// cons-level: `h :: t` (right-assoc)
and parsePatternCons (state : ParserState) (i : int) : WT.MatchPattern * int =
  let (head, j) = parsePatternBase state i
  if tok state j = TCons then
    let consR = rng state j
    let (tail, k) = parsePatternCons state (j + 1)
    (WT.MPListCons(span (WT.mpRange head) (WT.mpRange tail), head, tail, consR), k)
  else
    (head, j)

and parsePatternBase (state : ParserState) (i : int) : WT.MatchPattern * int =
  if tooDeep state i || outOfFuel state i then
    (WT.MPError(rng state i), state.tokenCount - 1)
  else
    state.depth <- state.depth + 1
    let r = parsePatternBaseInner state i
    state.depth <- state.depth - 1
    r

and parsePatternBaseInner (state : ParserState) (i : int) : WT.MatchPattern * int =
  checkBareMinMagnitude state i // `128y` etc. — only valid negated
  match tok state i with
  | TUnderscore -> (WT.MPVariable(rng state i, "_"), i + 1)
  | TInt v -> (WT.MPInt(rng state i, (rng state i, v)), i + 1)
  | TInt64 v ->
    let (digits, suffix) = splitTrailingRange state i 1
    (WT.MPInt64(rng state i, (digits, v), suffix), i + 1)
  | TInt32 v ->
    let (digits, suffix) = splitTrailingRange state i 1
    (WT.MPInt32(rng state i, (digits, v), suffix), i + 1)
  | TInt8 v ->
    let (digits, suffix) = splitTrailingRange state i 1
    (WT.MPInt8(rng state i, (digits, v), suffix), i + 1)
  | TUInt8 v ->
    let (digits, suffix) = splitTrailingRange state i 2
    (WT.MPUInt8(rng state i, (digits, v), suffix), i + 1)
  | TInt16 v ->
    let (digits, suffix) = splitTrailingRange state i 1
    (WT.MPInt16(rng state i, (digits, v), suffix), i + 1)
  | TUInt16 v ->
    let (digits, suffix) = splitTrailingRange state i 2
    (WT.MPUInt16(rng state i, (digits, v), suffix), i + 1)
  | TUInt32 v ->
    let (digits, suffix) = splitTrailingRange state i 2
    (WT.MPUInt32(rng state i, (digits, v), suffix), i + 1)
  | TUInt64 v ->
    let (digits, suffix) = splitTrailingRange state i 2
    (WT.MPUInt64(rng state i, (digits, v), suffix), i + 1)
  | TInt128 v ->
    let (digits, suffix) = splitTrailingRange state i 1
    (WT.MPInt128(rng state i, (digits, v), suffix), i + 1)
  | TUInt128 v ->
    let (digits, suffix) = splitTrailingRange state i 1
    (WT.MPUInt128(rng state i, (digits, v), suffix), i + 1)
  // unary minus on a numeric literal pattern: `-5L`, `-1y`, `-2.0` (unsigned
  // types can't be negative, so only the signed literals + float are handled).
  | TMinus ->
    let r1 = rng state (i + 1)
    let r = span (rng state i) r1
    match tok state (i + 1) with
    | TInt v -> (WT.MPInt(r, (r, -v)), i + 2)
    | TInt64 v ->
      let (digits, suffix) = splitTrailingRange state (i + 1) 1
      (WT.MPInt64(r, (span (rng state i) digits, -v), suffix), i + 2)
    | TInt32 v ->
      let (digits, suffix) = splitTrailingRange state (i + 1) 1
      (WT.MPInt32(r, (span (rng state i) digits, -v), suffix), i + 2)
    | TInt8 v ->
      let (digits, suffix) = splitTrailingRange state (i + 1) 1
      (WT.MPInt8(r, (span (rng state i) digits, -v), suffix), i + 2)
    | TInt16 v ->
      let (digits, suffix) = splitTrailingRange state (i + 1) 1
      (WT.MPInt16(r, (span (rng state i) digits, -v), suffix), i + 2)
    | TInt128 v ->
      let (digits, suffix) = splitTrailingRange state (i + 1) 1
      (WT.MPInt128(r, (span (rng state i) digits, -v), suffix), i + 2)
    | TFloat v ->
      let (whole, frac) = floatParts state (i + 1) v
      (WT.MPFloat(r, true, whole, frac), i + 2)
    | _ ->
      errExpected state i "a pattern"
      (WT.MPError(rng state i), i + 1)
  | TFloat v ->
    let r = rng state i
    let (whole, frac) = floatParts state i v
    (WT.MPFloat(r, v < 0.0, whole, frac), i + 1)
  | TTrue -> (WT.MPBool(rng state i, true), i + 1)
  | TFalse -> (WT.MPBool(rng state i, false), i + 1)
  | TStringLit s ->
    let r = rng state i
    let delimiter = if (txt state i).StartsWith "\"\"\"" then "\"\"\"" else "\""
    let (openQuote, contents, closeQuote) = literalTextRanges state i delimiter
    (WT.MPString(r, Some(contents, s), openQuote, closeQuote), i + 1)
  | TCharLit c ->
    let r = rng state i
    let (openQuote, contents, closeQuote) = literalTextRanges state i "'"
    (WT.MPChar(r, Some(contents, c), openQuote, closeQuote), i + 1)
  | TLParen ->
    if tok state (i + 1) = TRParen then
      (WT.MPUnit(span (rng state i) (rng state (i + 1))), i + 2)
    else
      let openP = rng state i
      // parens hold a full pattern (or > tuple > cons). Parse it, then attach the
      // real paren ranges when it's a bare tuple; otherwise the parens are just
      // grouping (`(a | b)`, `(p)`) and drop away.
      let (inner, j) = parseMatchPattern state (i + 1)
      let (closeP, p2) =
        if tok state j = TRParen then
          (rng state j, j + 1)
        else
          errUnclosed state j ")" "(" openP
          (zeroWidthAtEnd (rng state j), j)
      match inner with
      | WT.MPTuple(_, first, comma, second, rest, _, _) ->
        (WT.MPTuple(span openP closeP, first, comma, second, rest, openP, closeP), p2)
      | _ -> (inner, p2)
  | TLBracket ->
    let openB = rng state i
    let elems =
      System.Collections.Generic.List<WT.MatchPattern * Option<TokenRange>>()
    let mutable k = i + 1
    let mutable go = true
    while go && tok state k <> TRBracket && tok state k <> TEOF do
      let (p, k2) = parsePatternBase state k
      if k2 = k then
        err
          state
          DiagnosticCode.unexpected
          k
          $"unexpected {foundDesc state k} in list pattern"
        go <- false
      elif tok state k2 = TSemicolon || tok state k2 = TComma then
        elems.Add(p, Some(rng state k2))
        k <- k2 + 1
      else
        elems.Add(p, None)
        if tok state k2 <> TRBracket then
          requireElementSeparator
            state
            (WT.mpRange p)
            k2
            "a comma, semicolon, or newline between list-pattern elements"
        k <- k2
    let (closeB, p2) =
      if tok state k = TRBracket then
        (rng state k, k + 1)
      else
        errUnclosed state k "]" "[" openB
        (zeroWidthAtEnd (rng state k), k)
    (WT.MPList(span openB closeB, List.ofSeq elems, openB, closeB), p2)
  | TIdent name when name.Length > 0 && System.Char.IsUpper name[0] ->
    // enum pattern: `[Mod.]Case [fieldPats…]` — last segment is the case
    let (mods, final, j) = parseQualified state i
    // A qualified path (`Result.Ok`, `Stdlib.Result.Result.Ok`) is not a valid enum
    // pattern — patterns use the unqualified case name. Reject rather than silently
    // building a truncated pattern from just the last segment.
    if not (List.isEmpty mods) then
      let fullPath =
        (mods |> List.map (fun (m, _) -> m.name)) @ [ final.name ]
        |> String.concat "."
      err
        state
        DiagnosticCode.pattern
        i
        (sprintf
          "Invalid match pattern. Enum patterns use the unqualified case name (e.g. `| %s n`), not a qualified path like `| %s n`."
          final.name
          fullPath)
    // `Case(p1, p2, …)` is a parenthesized arg list: commas separate FIELDS, so
    // `Pair(a, b)` is two fields — NOT one tuple `Pair((a, b))`. This holds
    // whether or not there's a space before the `(` (matching F#).
    if tok state j = TLParen then
      let openParen = rng state j
      let fieldPats = System.Collections.Generic.List<WT.MatchPattern>()
      let mutable k = j + 1
      let mutable go = true
      while go && tok state k <> TRParen && tok state k <> TEOF do
        let (p, k2) = parsePatternOr state k
        fieldPats.Add p
        if tok state k2 = TComma then
          k <- k2 + 1
        elif k2 > k then
          if tok state k2 <> TRParen then
            requireElementSeparator
              state
              (WT.mpRange p)
              k2
              "a comma or newline between constructor-pattern fields"
          k <- k2
        else
          go <- false
      let k3 =
        if tok state k = TRParen then
          k + 1
        else
          (errUnclosed state k ")" "(" openParen
           k)
      // `Case()` is one unit field (`Case` applied to unit)
      if fieldPats.Count = 0 then
        fieldPats.Add(WT.MPUnit(span openParen (rng state (max j (k3 - 1)))))
      (WT.MPEnum(
        span (rng state i) (rng state (max j (k3 - 1))),
        (final.range, final.name),
        List.ofSeq fieldPats
       ),
       k3)
    else
      let fieldPats = System.Collections.Generic.List<WT.MatchPattern>()
      let mutable k = j
      while canStartPattern (tok state k) && offsideContinues state i k do
        let (p, k2) = parsePatternBase state k
        fieldPats.Add p
        if k2 > k then k <- k2 else k <- k + 1
      let endR =
        if fieldPats.Count > 0 then WT.mpRange (Seq.last fieldPats) else final.range
      (WT.MPEnum(
        span (rng state i) endR,
        (final.range, final.name),
        List.ofSeq fieldPats
       ),
       k)
  | TIdent name -> (WT.MPVariable(rng state i, name), i + 1)
  // TODO: Support `...` list rest patterns once WrittenTypes and ProgramTypes
  // represent their binding and matching semantics.
  | TDotDotDot ->
    err
      state
      DiagnosticCode.unexpected
      i
      "'...' rest patterns are reserved but not supported"
    (WT.MPError(rng state i), i + 1)
  | _ ->
    errExpected state i "a pattern"
    // recovery: an explicit error-hole node; leave closing/separating/decl-start
    // tokens for the enclosing construct
    let holeR = { start = (rng state i).start; end_ = (rng state i).start }
    (WT.MPError holeR,
     (if i < state.tokenCount && not (isRecoveryBarrier (tok state i)) then
        i + 1
      else
        i))

and parseIf (state : ParserState) (minCol : int) (i : int) : WT.Expr * int =
  let kwIf = rng state i
  let (cond, j) = parseExpr state (i + 1)
  let (kwThen, k) =
    if tok state j = TThen then
      (rng state j, j + 1)
    else
      errExpected state j "'then'"
      (zeroWidthAtEnd (rng state j), j)
  let (thenE, m) = parseBlock state k
  // `else`/`elif` binds to the `if` at `minCol` or to the left; a less-indented
  // `else` belongs to an ENCLOSING `if`, so a nested inner `if` in the THEN block
  // must not greedily grab it (`if… then (if… then c) else b` → the `else` is the
  // OUTER's). `minCol` is the column of the FIRST `if` in a chain — so `else if …`
  // chains still bind at the chain's column even though each nested `if` sits
  // further right (after `else `). Same-row `else` always binds (col > minCol).
  let elseBinds = (rng state m).start.column >= minCol
  if tok state m = TElse && elseBinds then
    let kwElse = rng state m
    // same-line `else if …` continues the chain → the nested `if` inherits this
    // chain's `minCol`; any other else-body is a normal block at its own indent.
    if
      tok state (m + 1) = TIf
      && (rng state (m + 1)).start.row = (rng state m).start.row
    then
      let (elseE, p) = parseIf state minCol (m + 1)
      (WT.EIf(
        span kwIf (WT.exprRange elseE),
        cond,
        thenE,
        Some elseE,
        kwIf,
        kwThen,
        Some kwElse
       ),
       p)
    else
      let (elseE, p) = parseBlock state (m + 1)
      (WT.EIf(
        span kwIf (WT.exprRange elseE),
        cond,
        thenE,
        Some elseE,
        kwIf,
        kwThen,
        Some kwElse
       ),
       p)
  elif tok state m = TElif && elseBinds then
    // `elif` is `else (if …)` — the nested EIf is the else branch (its own
    // `if`-keyword range colors the `elif`, so no separate else-keyword range).
    let (elseE, p) = parseIf state minCol m
    (WT.EIf(
      span kwIf (WT.exprRange elseE),
      cond,
      thenE,
      Some elseE,
      kwIf,
      kwThen,
      None
     ),
     p)
  else
    (WT.EIf(span kwIf (WT.exprRange thenE), cond, thenE, None, kwIf, kwThen, None), m)

// a simple binding pattern: variable / wildcard / `()` unit
and parseLetPattern (state : ParserState) (i : int) : WT.LetPattern * int =
  match tok state i with
  | TUnderscore -> (WT.LPWildcard(rng state i), i + 1)
  | TIdent s -> (WT.LPVariable(rng state i, s), i + 1)
  | TLParen when tok state (i + 1) = TRParen ->
    (WT.LPUnit(span (rng state i) (rng state (i + 1))), i + 2)
  | TLParen ->
    // tuple pattern `(a, b, …)` or a parenthesized pattern `(a)`
    let openP = rng state i
    let (first, j) = parseLetPattern state (i + 1)
    if tok state j = TComma then
      let comma = rng state j
      let (second, k) = parseLetPattern state (j + 1)
      let rest = System.Collections.Generic.List<TokenRange * WT.LetPattern>()
      let mutable m = k
      while tok state m = TComma do
        let cr = rng state m
        let (p, m2) = parseLetPattern state (m + 1)
        rest.Add(cr, p)
        m <- if m2 > m then m2 else m + 1
      let (closeP, p2) =
        if tok state m = TRParen then
          (rng state m, m + 1)
        else
          errUnclosed state m ")" "(" openP
          (zeroWidthAtEnd (rng state m), m)
      (WT.LPTuple(
        span openP closeP,
        first,
        comma,
        second,
        List.ofSeq rest,
        openP,
        closeP
       ),
       p2)
    else if tok state j = TRParen then
      (first, j + 1)
    else
      errUnclosed state j ")" "(" openP
      (first, j)
  | _ ->
    errExpected state i "a pattern"
    // recovery: keep a benign binder (LetPattern has no error case yet);
    // leave closing/separating/decl-start tokens for the enclosing construct
    (WT.LPVariable(rng state i, "_"),
     (if i < state.tokenCount && not (isRecoveryBarrier (tok state i)) then
        i + 1
      else
        i))

and parseLet (state : ParserState) (i : int) : WT.Expr * int =
  let keywordLet = rng state i
  let (pat, j) = parseLetPattern state (i + 1)
  match pat with
  | WT.LPVariable _ when tok state j = TLParen ->
    // nested function definition: `let f (x: T) (y) [: R] = body` — bind a lambda
    // to the name (params lowered to untyped lambda patterns, types discarded)
    let pats = System.Collections.Generic.List<WT.LetPattern>()
    let mutable k = j
    let mutable more = true
    while more && tok state k = TLParen do
      if tok state (k + 1) = TRParen then
        pats.Add(WT.LPUnit(span (rng state k) (rng state (k + 1))))
        k <- k + 2
      else
        (match tok state (k + 1) with
         | TIdent nm -> pats.Add(WT.LPVariable(rng state (k + 1), nm))
         | _ -> pats.Add(WT.LPVariable(rng state (k + 1), "_")))
        let c =
          if tok state (k + 2) = TColon then
            snd (parseTypeRef state (k + 3))
          else
            k + 2
        if tok state c = TRParen then
          k <- c + 1
        else
          (errExpected state c "')'"
           more <- false)
    let afterRet =
      if tok state k = TColon then snd (parseTypeRef state (k + 1)) else k
    let (symbolEquals, m0) =
      if tok state afterRet = TEquals then
        (rng state afterRet, afterRet + 1)
      else
        errExpected state afterRet "'=' in function binding"
        (zeroWidthAtEnd (rng state afterRet), afterRet)
    let (fnBody, m1) = parseBlock state m0
    let m = if tok state m1 = TIn then m1 + 1 else m1
    let (body, p) = parseBlock state m
    let z = zeroWidthAtEnd keywordLet // no real `fun`/`->` tokens in this sugar
    let lambda =
      WT.ELambda(
        span (rng state j) (WT.exprRange fnBody),
        List.ofSeq pats,
        fnBody,
        z,
        z
      )
    (WT.ELet(
      span keywordLet (WT.exprRange body),
      pat,
      lambda,
      body,
      keywordLet,
      symbolEquals
     ),
     p)
  | _ ->
    // Value annotations are not part of Dark. Consume the type for recovery,
    // but reject the source instead of silently discarding it.
    let afterAnno =
      if tok state j = TColon then
        state.diagnostics.Add
          { code = DiagnosticCode.unexpected
            severity = DiagError
            range = rng state j
            message = "Value annotations are not supported"
            related = []
            hint = Some "remove ': Type' from this value binding" }
        snd (parseTypeRef state (j + 1))
      else
        j
    let (symbolEquals, k) =
      if tok state afterAnno = TEquals then
        (rng state afterAnno, afterAnno + 1)
      else
        errExpected state afterAnno "'=' in let binding"
        (zeroWidthAtEnd (rng state afterAnno), afterAnno)
    // the value is an offside block, not a single expr, so a multi-statement
    // binding (`let x =\n  doThing ()\n  result`) sequences instead of gluing
    // the following statement onto the first as an application argument.
    let (value, m) = parseBlock state k
    // `in` is optional
    let m = if tok state m = TIn then m + 1 else m
    let (body, p) = parseBlock state m
    let range = span keywordLet (rng state (p - 1))
    (WT.ELet(range, pat, value, body, keywordLet, symbolEquals), p)

// left-assoc binary level
// --- infix expressions: one precedence-climbing loop ---
// Binding powers, loosest → tightest (higher binds tighter); a right-assoc
// op recurses at its own power so it nests to the right.
//   1 `||`   2 `&&`   3 `== != < > <= >=`   4 `@` (right)
//   5 `+ - ++`   6 `* / %`   7 `^` (right)
// `@` desugars to `Stdlib.List.append` (there is no WT infix for it); `^` is
// exponentiation (lexed as TBitXor) and nests right: `2^3^2 = 2^(3^2)`.
and infixBindingPower (t : Token) : (int * bool) option =
  match t with
  | TOr -> Some(1, false)
  | TAnd -> Some(2, false)
  | TEqEq
  | TNeq
  | TLt
  | TGt
  | TLte
  | TGte -> Some(3, false)
  | TAt -> Some(4, true)
  | TPlus
  | TMinus
  | TPlusPlus -> Some(5, false)
  | TStar
  | TSlash
  | TPercent -> Some(6, false)
  | TBitXor -> Some(7, true)
  | _ -> None

and parseInfix (state : ParserState) (i : int) : WT.Expr * int =
  let (left, j) = parseApp state i
  parseInfixRhs state 1 left j

and parseInfixRhs
  (state : ParserState)
  (minBp : int)
  (left0 : WT.Expr)
  (j0 : int)
  : WT.Expr * int =
  let mutable left = left0
  let mutable j = j0
  let mutable go = true
  while go do
    // The operator must belong to THIS statement: same row as the left
    // operand's end, or inside parens, or an indented continuation. Otherwise
    // a following statement that starts with a prefix operator (`1L\n-8L …`)
    // would be wrongly glued on as `1L - 8L …`. On a new line, a pure infix
    // operator at the statement column continues (`x\n++ y` — `++` can't start
    // a statement), but `-` there begins a new statement (a negative literal),
    // so it must be indented PAST it. This rule is identical for every caller.
    let opContinues =
      let s = state.scopes.Peek()
      let so = s.stmtCol
      let col = (rng state j).start.column
      (rng state j).start.row = (WT.exprRange left).end_.row
      || so < 0
      || (if s.stmtExact then tok state j <> TMinus || col <> so
          elif tok state j = TMinus then col > so
          else col >= so)
    match infixBindingPower (tok state j) with
    | Some(bp, rightAssoc) when bp >= minBp && opContinues ->
      let opTok = tok state j
      let opRange = rng state j
      let (rhs0, k0) = parseApp state (j + 1)
      // climb: the RHS folds in everything binding tighter (or equally
      // tight, for a right-assoc op) before this level continues.
      let (right, k) =
        parseInfixRhs state (if rightAssoc then bp else bp + 1) rhs0 k0
      let range = span (WT.exprRange left) (WT.exprRange right)
      left <-
        (match opTok with
         | TAt ->
           let appendFn : WT.QualifiedFnIdentifier =
             { range = opRange
               modules =
                 [ ({ range = opRange; name = "Stdlib" }, opRange)
                   ({ range = opRange; name = "List" }, opRange) ]
               fn = { range = opRange; name = "append" } }
           WT.EApply(range, WT.EFnName(opRange, appendFn), [], [ left; right ])
         | _ -> WT.EInfix(range, (opRange, (infixOf opTok).Value), left, right))
      j <- k
    | _ -> go <- false
  (left, j)

// A parenthesized enum-constructor field list: `(e1, e2, …)` with `i` at the
// `(`. Commas separate FIELDS (so `Pair(a, b)` is two fields; a tuple field
// needs double parens). Fields are anchored exactly like a paren body
// (stmtExact — the `)` is the real delimiter), sunk into `sink`; returns the
// index after the `)`. Shared by the adjacent (`Ctor(…)`) and spaced
// (`Ctor (…)`) forms.
and parseCtorParenFields
  (state : ParserState)
  (i : int)
  (sink : System.Collections.Generic.List<WT.Expr>)
  : int =
  let openParen = rng state i
  withStmtColExact (state) (rng state (i + 1)).start.column (fun () ->
    let mutable m = i + 1
    let mutable go = true
    while go
          && tok state m <> TRParen
          && tok state m <> TEOF
          && not (declBarrier state m) do
      let (a, m2) = parseExpr state m
      sink.Add a
      if tok state m2 = TComma then
        m <- m2 + 1
      elif m2 > m then
        if tok state m2 <> TRParen then
          requireElementSeparator
            state
            (WT.exprRange a)
            m2
            "a comma or newline between constructor fields"
        m <- m2
      else
        go <- false
    let m =
      if tok state m = TRParen then
        m + 1
      else
        (errUnclosed state m ")" "(" openParen
         m)
    // `Ctor()` is `Ctor` applied to unit — one unit field, not zero
    if sink.Count = 0 then sink.Add(WT.EUnit(span openParen (rng state (m - 1))))
    m)

// space application: `f a b`
and parseApp (state : ParserState) (i : int) : WT.Expr * int =
  let (callee, j) = parseAtom state i
  let args = System.Collections.Generic.List<WT.Expr>()
  let mutable k = j
  // A spaced enum ctor followed by a parenthesized list is a FIELD list, not a
  // single tuple arg: `KeyPressed (a, b, c)` → 3 fields.
  // The content is parsed as a comma list, so double parens `Ctor ((a, b))` keep the
  // tuple as ONE field (no top-level comma). Only fires in head position (the ctor IS
  // the callee), so `f None (g)` is unaffected. Adjacent `Ctor(a,b)` was already
  // handled in the enum branch, so a nullary EEnum callee here means a spaced paren.
  (match callee with
   | WT.EEnum(_, _, _, [], _) when
     tok state j = TLParen && offsideContinues state i j
     ->
     k <- parseCtorParenFields state j args
   | _ -> ())
  // only names/applications take args
  // a field access can yield a function too: `c.onKey state key …`
  let acceptsArgs =
    match callee with
    // a lambda literal can be applied directly: `(fun a b -> …) x y`, and an
    // operator section `(op)` lowers to such a lambda; a nullary enum constructor
    // in head position takes its fields as space args (`Ok 5` → `Ok(5)`)
    | WT.EVariable _
    | WT.EFnName _
    | WT.EApply _
    | WT.ERecordFieldAccess _
    | WT.ELambda _ -> true
    | WT.EEnum(_, _, _, [], _) -> true
    | _ -> false
  if acceptsArgs then
    while (canStartAtom (tok state k) || isNegLitArg state k)
          && offsideContinues state i k do
      let (a, k2) = parseAtom state k
      args.Add a
      k <- k2
  if args.Count = 0 then
    (callee, k)
  else
    let endR = WT.exprRange (Seq.last args)
    match callee with
    // callee already carries explicit type args (`parse<T> arg`) — fold the
    // value args into that same EApply rather than nesting.
    | WT.EApply(cr, fn, typeArgs, []) ->
      (WT.EApply(span cr endR, fn, typeArgs, List.ofSeq args), k)
    // a nullary enum constructor in head position: its space args are its FIELDS
    // (`Ok 5` → `EEnum(Ok, [5])`), matching the adjacent-paren `Ok(5)` form.
    | WT.EEnum(er, tname, cn, [], dot) ->
      (WT.EEnum(span er endR, tname, cn, List.ofSeq args, dot), k)
    | _ ->
      let lhs =
        // a bare lowercase variable callee becomes a fn name when applied
        match callee with
        | WT.EVariable(r, name) ->
          WT.EFnName(r, { range = r; modules = []; fn = { range = r; name = name } })
        | other -> other
      (WT.EApply(span (WT.exprRange lhs) endR, lhs, [], List.ofSeq args), k)

and parseAtom (state : ParserState) (i : int) : WT.Expr * int =
  let (baseE, j) = parsePrimary state i
  parsePostfix state baseE j

// postfix `.field` record access (left-assoc, chains)
and parsePostfix (state : ParserState) (e : WT.Expr) (i : int) : WT.Expr * int =
  match tok state i, tok state (i + 1) with
  | TDot, TIdent field ->
    let dotR = rng state i
    let fieldR = rng state (i + 1)
    parsePostfix
      state
      (WT.ERecordFieldAccess(span (WT.exprRange e) fieldR, e, (fieldR, field), dotR))
      (i + 2)
  | _ -> (e, i)

// `$"text {expr} text"` — re-scan the token's source text, deriving exact ranges
// for the literal segments and each `{expr}`; the embedded expression is parsed by
// re-tokenizing its slice and offsetting the sub-token ranges to real positions.
and parseInterpString (state : ParserState) (i : int) : WT.Expr * int =
  let spanned = state.toks[i]
  let fullText = spanned.text // includes `$"` … `"`
  let basePos = spanned.range.start // position of `$`
  let m = fullText.Length
  // offsets of each line start within fullText, computed once — posAt is then
  // a binary search instead of a from-zero rescan (which was O(len²) across a
  // long interpolated string's many segment boundaries)
  let lineStarts =
    let acc = ResizeArray<int>()
    acc.Add 0
    for k in 0 .. m - 1 do
      if fullText[k] = '\n' then acc.Add(k + 1)
    acc
  let posAt (off : int) : Pos =
    let off = min off m
    // last line start <= off
    let mutable lo = 0
    let mutable hi = lineStarts.Count - 1
    while lo < hi do
      let mid = (lo + hi + 1) / 2
      if lineStarts[mid] <= off then lo <- mid else hi <- mid - 1
    let lineOff = off - lineStarts[lo]
    if lo = 0 then
      { row = basePos.row; column = basePos.column + lineOff }
    else
      { row = basePos.row + lo; column = lineOff }
  let rangeAt (a : int) (b : int) : TokenRange = { start = posAt a; end_ = posAt b }
  let triple = m >= 4 && fullText[1] = '"' && fullText[2] = '"' && fullText[3] = '"'
  let dollarR = rangeAt 0 1
  let bodyStart = if triple then 4 else 2
  let closeLen = if triple then 3 else 1
  let openQ = rangeAt 1 bodyStart
  let contents = System.Collections.Generic.List<WT.StringSegment>()
  let mutable textStart = bodyStart
  let mutable k = bodyStart
  let mutable go = true
  let mutable foundClosingQuote = false
  let flushText (endOff : int) =
    if endOff > textStart then
      let raw = fullText.Substring(textStart, endOff - textStart)
      // `{{`/`}}` are the source-level doubling escape for literal braces; resolve
      // them on the RAW text FIRST so braces produced by `\{`/`\}` unescaping
      // below aren't then collapsed (`\{\{` must yield `{{`, not `{`).
      let deDoubled = raw.Replace("{{", "{").Replace("}}", "}")
      // regular `$"…"` literal parts get escapes processed (`\"`, `\n`, `\{`, …);
      // triple-quoted `$"""…"""` stays raw but is NFC-normalized (like `unescape`)
      // so both lowerings see canonical bytes.
      let segText =
        if triple then deDoubled.Normalize() else Lexer.unescape deDoubled
      contents.Add(WT.StringText(rangeAt textStart endOff, segText))
  while go && k < m do
    let atClose =
      if triple then
        k + 2 < m
        && fullText[k] = '"'
        && fullText[k + 1] = '"'
        && fullText[k + 2] = '"'
      else
        fullText[k] = '"'
    if atClose then
      flushText k
      foundClosingQuote <- true
      go <- false
    // skip `\X` so an escaped quote `\"` doesn't end the string (regular only)
    elif fullText[k] = '\\' && not triple && k + 1 < m then
      k <- k + 2
    elif fullText[k] = '{' && k + 1 < m && fullText[k + 1] = '{' then
      k <- k + 2
    elif fullText[k] = '}' && k + 1 < m && fullText[k + 1] = '}' then
      k <- k + 2
    elif fullText[k] = '{' then
      flushText k
      let braceOpen = rangeAt k (k + 1)
      let found = Lexer.findInterpExprClose fullText m (k + 1)
      if found < 0 then
        go <- false
      else
        let exprText = fullText.Substring(k + 1, found - (k + 1))
        let exprStartPos = posAt (k + 1)
        let braceClose = rangeAt found (found + 1)
        let innerExpr =
          // Each `{expr}` body parses via a recursive parseTokensAt with fresh
          // state, so interpolation nesting = recursion depth REGARDLESS of the
          // expression depth guard. Uncapped, a `$"{$"{…}"}"` bomb is an
          // uncatchable StackOverflow that kills the process.
          if state.interpDepth >= maxInterpNesting then
            errFull
              state
              DiagnosticCode.tooDeep
              i
              $"string interpolation nested too deeply (over {maxInterpNesting} levels); parsing abandoned"
              []
              None
            state.abandoned <- true
            WT.EUnit(rangeAt (k + 1) found)
          else
            // sub-token/diagnostic positions are relative to exprText; offset
            // them to real source positions
            let off (rel : Pos) : Pos =
              if rel.row = 0 then
                { row = exprStartPos.row; column = exprStartPos.column + rel.column }
              else
                { row = exprStartPos.row + rel.row; column = rel.column }
            let offRange (r : TokenRange) : TokenRange =
              { start = off r.start; end_ = off r.end_ }
            match tokenize exprText with
            | Ok(innerToks, innerLexDiags) ->
              // lexical-recovery diagnostics from inside `{…}` (unterminated
              // literals etc.) surface like any other — previously dropped
              for (r, msg) in innerLexDiags do
                if not state.abandoned then
                  state.diagnostics.Add
                    { code = DiagnosticCode.lex
                      severity = DiagError
                      range = offRange r
                      message = msg
                      related = []
                      hint = None }
              let offToks =
                innerToks
                |> List.map (fun (t : SpannedToken) ->
                  { t with range = offRange t.range })
                |> List.toArray
              let subResult = parseTokensAt (state.interpDepth + 1) offToks
              // surface parse errors from inside the interpolation `{…}` (their
              // ranges are already offset to the outer source) rather than dropping them
              subResult.diagnostics |> List.iter state.diagnostics.Add
              match subResult.parsed with
              | Some(WT.SourceFile sf) ->
                let bodyRange = rangeAt (k + 1) found
                let interpolationError (message : string) =
                  state.diagnostics.Add
                    { code = DiagnosticCode.interpolation
                      severity = DiagError
                      range = bodyRange
                      message = message
                      related = []
                      hint = None }
                if not (List.isEmpty sf.declarations) then
                  interpolationError
                    "Interpolation body must be one expression, not a declaration"
                match sf.declarations, sf.exprsToEval with
                | [], [ e ] -> e
                | [], e :: _ ->
                  interpolationError
                    "Interpolation body must contain exactly one expression"
                  e
                | _ ->
                  if List.isEmpty sf.declarations then
                    interpolationError "Interpolation body cannot be empty"
                  WT.EUnit bodyRange
              | None -> WT.EUnit(rangeAt (k + 1) found)
            | Error e ->
              // a hard tokenize failure inside `{…}` (e.g. nesting cap) was
              // previously swallowed as a silent unit
              err state DiagnosticCode.lex i e
              WT.EUnit(rangeAt (k + 1) found)
        contents.Add(
          WT.StringInterpolation(
            rangeAt k (found + 1),
            innerExpr,
            braceOpen,
            braceClose
          )
        )
        k <- found + 1
        textStart <- k
    else
      k <- k + 1
  if not foundClosingQuote then flushText m
  let closeQ = if foundClosingQuote then rangeAt (m - closeLen) m else rangeAt m m
  (WT.EString(spanned.range, Some dollarR, List.ofSeq contents, openQ, closeQ), i + 1)

and parsePrimary (state : ParserState) (i : int) : WT.Expr * int =
  checkBareMinMagnitude state i // `128y` etc. — only valid negated
  match tok state i with
  | TInt v -> (WT.EInt(rng state i, (rng state i, v)), i + 1)
  | TInt64 v ->
    let (digits, suffix) = splitTrailingRange state i 1
    (WT.EInt64(rng state i, (digits, v), suffix), i + 1)
  | TInt8 v ->
    let (digits, suffix) = splitTrailingRange state i 1
    (WT.EInt8(rng state i, (digits, v), suffix), i + 1)
  | TUInt8 v ->
    let (digits, suffix) = splitTrailingRange state i 2
    (WT.EUInt8(rng state i, (digits, v), suffix), i + 1)
  | TInt16 v ->
    let (digits, suffix) = splitTrailingRange state i 1
    (WT.EInt16(rng state i, (digits, v), suffix), i + 1)
  | TUInt16 v ->
    let (digits, suffix) = splitTrailingRange state i 2
    (WT.EUInt16(rng state i, (digits, v), suffix), i + 1)
  | TInt32 v ->
    let (digits, suffix) = splitTrailingRange state i 1
    (WT.EInt32(rng state i, (digits, v), suffix), i + 1)
  | TUInt32 v ->
    let (digits, suffix) = splitTrailingRange state i 2
    (WT.EUInt32(rng state i, (digits, v), suffix), i + 1)
  | TUInt64 v ->
    let (digits, suffix) = splitTrailingRange state i 2
    (WT.EUInt64(rng state i, (digits, v), suffix), i + 1)
  | TInt128 v ->
    let (digits, suffix) = splitTrailingRange state i 1
    (WT.EInt128(rng state i, (digits, v), suffix), i + 1)
  | TUInt128 v ->
    let (digits, suffix) = splitTrailingRange state i 1
    (WT.EUInt128(rng state i, (digits, v), suffix), i + 1)
  // unary minus on a numeric literal: `-5L`, `-2.0` (infix `a - b` is handled in
  // parseInfixRhs, so a `-` reaching here always prefixes a literal)
  | TMinus ->
    let r1 = rng state (i + 1)
    let r = span (rng state i) r1
    match tok state (i + 1) with
    | TInt v -> (WT.EInt(r, (r, -v)), i + 2)
    | TInt64 v ->
      let (digits, suffix) = splitTrailingRange state (i + 1) 1
      (WT.EInt64(r, (span (rng state i) digits, -v), suffix), i + 2)
    | TInt8 v ->
      let (digits, suffix) = splitTrailingRange state (i + 1) 1
      (WT.EInt8(r, (span (rng state i) digits, -v), suffix), i + 2)
    | TInt16 v ->
      let (digits, suffix) = splitTrailingRange state (i + 1) 1
      (WT.EInt16(r, (span (rng state i) digits, -v), suffix), i + 2)
    | TInt32 v ->
      let (digits, suffix) = splitTrailingRange state (i + 1) 1
      (WT.EInt32(r, (span (rng state i) digits, -v), suffix), i + 2)
    | TInt128 v ->
      let (digits, suffix) = splitTrailingRange state (i + 1) 1
      (WT.EInt128(r, (span (rng state i) digits, -v), suffix), i + 2)
    | TFloat v ->
      let (whole, frac) = floatParts state (i + 1) v
      (WT.EFloat(r, true, whole, frac), i + 2)
    | _ ->
      // unary minus on a non-literal (`-x`, `-(expr)`, `-f x`) → `Builtin.negate`
      // applied to the whole application, so `-f x` groups as `-(f x)`, not `(-f) x`
      // (minus binds looser than application). Infix ops still bind looser than the
      // minus: `-a + b` is `(-a) + b`, since `+` can't start an application arg.
      let (operand, k) = parseApp state (i + 1)
      let negR = rng state i
      let negateFn : WT.QualifiedFnIdentifier =
        { range = negR
          modules = [ ({ range = negR; name = "Builtin" }, negR) ]
          fn = { range = negR; name = "negate" } }
      (WT.EApply(
        span negR (WT.exprRange operand),
        WT.EFnName(negR, negateFn),
        [],
        [ operand ]
       ),
       k)
  | TFloat v ->
    let r = rng state i
    let (whole, frac) = floatParts state i v
    (WT.EFloat(r, v < 0.0, whole, frac), i + 1)
  | TCharLit c ->
    let r = rng state i
    let (openQuote, contents, closeQuote) = literalTextRanges state i "'"
    (WT.EChar(r, Some(contents, c), openQuote, closeQuote), i + 1)
  | TTrue -> (WT.EBool(rng state i, true), i + 1)
  | TFalse -> (WT.EBool(rng state i, false), i + 1)
  | TStringLit s ->
    let r = rng state i
    let delimiter = if (txt state i).StartsWith "\"\"\"" then "\"\"\"" else "\""
    let (openQuote, contents, closeQuote) = literalTextRanges state i delimiter
    (WT.EString(r, None, [ WT.StringText(contents, s) ], openQuote, closeQuote),
     i + 1)
  | TInterpString -> parseInterpString state i
  | TIdent name ->
    let (mods0, final0, j0) = parseQualified state i
    // Adjacent `<T,…>` type args on the name (no space before `<`): a generic fn
    // call `parse<T> arg`, an enum ctor `Type<T>.Case`, or a record `Type<T> { … }`.
    // A SPACED `<` is a comparison, not type args, so adjacency is required.
    let hasAdjacentLt =
      tok state j0 = TLt
      && (rng state j0).start.row = final0.range.end_.row
      && (rng state j0).start.column = final0.range.end_.column
    let (nameTypeArgs, jTA) =
      if hasAdjacentLt then
        let (args, _, next) = parseTypeArgs state j0
        (args, next)
      else
        ([], j0)
    // `Type<T>.Case`: fold `Type` (which carries the type args) into the module
    // path so the enum branch treats the trailing `.Case` as the case name.
    let (mods, final, j) =
      match nameTypeArgs, tok state jTA, tok state (jTA + 1) with
      | (_ :: _), TDot, TIdent caseName when
        caseName.Length > 0 && System.Char.IsUpper caseName[0]
        ->
        (mods0 @ [ (final0, rng state jTA) ],
         ({ range = rng state (jTA + 1); name = caseName } : WT.Identifier),
         jTA + 2)
      | _ -> (mods0, final0, jTA)
    let fullRange = span (rng state i) final.range
    let finalIsUpper = final.name.Length > 0 && System.Char.IsUpper final.name[0]
    // `Type { … }` record literal (the whole qualified name is the type).
    // A record LITERAL is `{ }` or `{ field = … }`. `{ expr with … }` is NOT a
    // literal of this type — there the brace is a standalone update expression
    // passed as an argument (e.g. `Ctor { r with f = v } x`), so don't consume
    // it as this name's record; let it flow into the payload/arg position.
    // `Type { }` or `Type { field = … }` is unambiguously a record literal — Dark
    // has no `{ }` blocks, and a bare `Type` isn't a statement, so this holds even
    // when the `{` wraps to the next line LESS-indented than a wrapped type name
    // (`… = (Combo\n  { e1 = … })`). `{ expr with … }` is NOT a literal of this
    // type (it's a standalone update passed as an arg), so it's excluded below.
    let braceIsRecordLit =
      finalIsUpper
      && tok state j = TLBrace
      && (match tok state (j + 1), tok state (j + 2) with
          | TRBrace, _ -> true
          | TIdent _, TEquals -> true
          | _ -> false)
    // a bare `Dict { … }` is a dict LITERAL, not a record of a type named `Dict`;
    // `Dict` is a keyword here, so it parses to its own node with the keyword range.
    if braceIsRecordLit && List.isEmpty mods && final.name = "Dict" then
      parseDict state final.range j
    elif braceIsRecordLit then
      parseRecord
        state
        ({ range = fullRange; modules = mods; typ = final; typeArgs = nameTypeArgs }
        : WT.QualifiedTypeIdentifier)
        fullRange
        j
    elif finalIsUpper then
      // enum constructor: `[Mod.Path.]Type.Case fields…` — the LAST segment is
      // the case, the segments before it form the type.
      let (typeName, symbolDot) : WT.QualifiedTypeIdentifier * TokenRange =
        match List.rev mods with
        | (typIdent, dotBeforeCase) :: revTypeMods ->
          let typeMods = List.rev revTypeMods
          let startR =
            match typeMods with
            | (firstMod, _) :: _ -> firstMod.range
            | [] -> typIdent.range
          ({ range = span startR typIdent.range
             modules = typeMods
             typ = typIdent
             typeArgs = nameTypeArgs },
           dotBeforeCase)
        | [] ->
          let z = zeroWidthAtEnd (rng state i)
          ({ range = z; modules = []; typ = { range = z; name = "" }; typeArgs = [] },
           z)
      let fields = System.Collections.Generic.List<WT.Expr>()
      let mutable k = j
      // `Case(e1, e2, …)` — parenthesized arg list ADJACENT to the case name
      // (no space): commas separate FIELDS, so `EInt64(r, n, s)` is three fields,
      // NOT one tuple. 1 item ⇒ 1 field. Adjacency matters: `Case (x) y` (space)
      // is two space-separated args, and `… Ok` then `(x, [])` on the next line is
      // a separate statement — both go through the general offside loop below.
      let adjacentParen =
        tok state j = TLParen
        && (rng state j).start.row = final.range.end_.row
        && (rng state j).start.column = final.range.end_.column
      if adjacentParen then k <- parseCtorParenFields state j fields
      // A non-adjacent-paren constructor is NULLARY here; space-separated fields
      // (`Ok 5`, `Ok -4y`) are folded by parseApp — but ONLY in application-head
      // position, so a constructor used as an ARGUMENT (`f None (g)`) stays nullary
      // instead of over-grabbing its sibling arg.
      let endR =
        if tok state j = TLParen && k > j then rng state (k - 1)
        elif fields.Count > 0 then WT.exprRange (Seq.last fields)
        else final.range
      (WT.EEnum(
        span (rng state i) endR,
        typeName,
        (final.range, final.name),
        List.ofSeq fields,
        symbolDot
       ),
       k)
    else
      // fn / value reference. Any adjacent `<T>` type args were parsed above into
      // `nameTypeArgs`; carry them on a zero-arg EApply so parseApp folds in any
      // value args that follow (`parse<T> arg`).
      let nameExpr =
        if List.isEmpty mods then
          WT.EVariable(rng state i, name)
        else
          WT.EFnName(fullRange, { range = fullRange; modules = mods; fn = final })
      if not (List.isEmpty nameTypeArgs) then
        let fnExpr =
          match nameExpr with
          | WT.EVariable(r, nm) ->
            WT.EFnName(r, { range = r; modules = []; fn = { range = r; name = nm } })
          | other -> other
        (WT.EApply(span fullRange (rng state (j - 1)), fnExpr, nameTypeArgs, []), j)
      else
        (nameExpr, j)
  | TLParen -> parseParen state i
  | TLBracket -> parseList state i
  | TFun ->
    let kwFun = rng state i
    let pats = System.Collections.Generic.List<WT.LetPattern>()
    let mutable k = i + 1
    while (match tok state k with
           | TIdent _
           | TUnderscore
           | TLParen -> true
           | _ -> false) do
      let (p, k2) = parseLetPattern state k
      pats.Add p
      if k2 = k then k <- k + 1 else k <- k2
    if pats.Count = 0 then errExpected state k "at least one lambda parameter"
    let (arrow, m) =
      if tok state k = TArrow then
        (rng state k, k + 1)
      else
        errExpected state k "'->' in lambda"
        (zeroWidthAtEnd (rng state k), k)
    let (body, p) = parseBlock state m
    (WT.ELambda(span kwFun (WT.exprRange body), List.ofSeq pats, body, kwFun, arrow),
     p)
  // bare `{` — anonymous record `{ f = v }` (or `{ }`) vs update `{ r with … }`
  | TLBrace ->
    match tok state (i + 1), tok state (i + 2) with
    | TRBrace, _
    | TIdent _, TEquals ->
      err
        state
        DiagnosticCode.expected
        i
        "Anonymous records are not supported; use a named record type"
      let z = zeroWidthAtEnd (rng state i)
      let emptyType : WT.QualifiedTypeIdentifier =
        { range = z; modules = []; typ = { range = z; name = "" }; typeArgs = [] }
      parseRecord state emptyType (rng state i) i
    | _ -> parseRecordUpdate state i
  // TODO: Support these reserved operators once their precedence and
  // polymorphic numeric/Bool dispatch are defined end to end.
  | TShl
  | TShr
  | TBitAnd
  | TBitOr
  | TBitNot
  | TNot
  | TDotDotDot ->
    err
      state
      DiagnosticCode.unexpected
      i
      $"'{txt state i}' is reserved but not supported by the expression grammar"
    (WT.EError(rng state i), i + 1)
  | _ ->
    errExpected state i "an expression"
    // recovery: an explicit error-hole node; leave closing/separating/decl-start
    // tokens for the enclosing construct (so a group/list still closes and
    // the next declaration survives); skip one token otherwise
    let holeR = { start = (rng state i).start; end_ = (rng state i).start }
    (WT.EError holeR,
     (if i < state.tokenCount && not (isRecoveryBarrier (tok state i)) then
        i + 1
      else
        i))

// `()` unit / `( e )` group / `( a, b, … )` tuple
and parseParen (state : ParserState) (i : int) : WT.Expr * int =
  let openP = rng state i
  if tok state (i + 1) = TRParen then
    (WT.EUnit(span openP (rng state (i + 1))), i + 2)
  elif
    (Option.isSome (infixOf (tok state (i + 1))) || tok state (i + 1) = TAt)
    && tok state (i + 2) = TRParen
  then
    // operator section `(op)` → `fun a b -> a op b` (a 2-arg fn value)
    let opTok = tok state (i + 1)
    let opR = rng state (i + 1)
    let r = span openP (rng state (i + 2))
    let z = zeroWidthAtEnd opR
    let va = WT.EVariable(z, "a")
    let vb = WT.EVariable(z, "b")
    let body =
      match opTok with
      | TAt ->
        let appendFn : WT.QualifiedFnIdentifier =
          { range = opR
            modules =
              [ ({ range = opR; name = "Stdlib" }, opR)
                ({ range = opR; name = "List" }, opR) ]
            fn = { range = opR; name = "append" } }
        WT.EApply(r, WT.EFnName(opR, appendFn), [], [ va; vb ])
      | _ -> WT.EInfix(r, (opR, (infixOf opTok).Value), va, vb)
    (WT.ELambda(r, [ WT.LPVariable(z, "a"); WT.LPVariable(z, "b") ], body, z, z),
     i + 3)
  else
    // Anchor arg-offside at the inner column instead of blanket-suspending
    // it, so a wrapped arg (indented past its callee or the statement)
    // still continues but a sibling statement at the inner column is a NEW
    // statement. This lets a paren-wrapped body be a newline-separated
    // statement BLOCK — `(stmt1 \n stmt2)` — folded into EStatement, not
    // one over-grabbing application.
    withStmtColExact state (rng state (i + 1)).start.column (fun () ->
      let (first, j) = parseExpr state (i + 1)
      if tok state j = TComma then
        let comma = rng state j
        let (second, k) = parseExpr state (j + 1)
        let rest = System.Collections.Generic.List<TokenRange * WT.Expr>()
        let mutable m = k
        let mutable go = true
        while go && tok state m = TComma do
          let cr = rng state m
          let (e, m2) = parseExpr state (m + 1)
          rest.Add(cr, e)
          if m2 = m + 1 && tok state m2 = tok state m then go <- false else m <- m2
        let (closeP, p) =
          if tok state m = TRParen then
            (rng state m, m + 1)
          else
            errUnclosed state m ")" "(" openP
            (zeroWidthAtEnd (rng state m), m)
        (WT.ETuple(
          span openP closeP,
          first,
          comma,
          second,
          List.ofSeq rest,
          openP,
          closeP
         ),
         p)
      else
        // group or statement block: fold newline-separated statements
        let stmts = System.Collections.Generic.List<WT.Expr>()
        stmts.Add first
        let mutable m = j
        let mutable go = true
        while go
              && tok state m <> TRParen
              && tok state m <> TEOF
              && tok state m <> TComma
              && not (declBarrier state m) do
          let bm = m
          let (s, m2) = parseExpr state m
          if m2 > bm then
            stmts.Add s
            m <- m2
          else
            go <- false
        let folded =
          stmts
          |> List.ofSeq
          |> List.reduceBack (fun s acc ->
            WT.EStatement(span (WT.exprRange s) (WT.exprRange acc), s, acc))
        if tok state m = TRParen then
          (folded, m + 1)
        else
          errUnclosed state m ")" "(" openP
          (folded, m))

// `[ e ; e ; … ]` (or comma/newline separators); each element keeps its
// trailing-separator range
and parseList (state : ParserState) (i : int) : WT.Expr * int =
  let openB = rng state i
  // list elements are offside-delimited in their own scope
  // (else an element swallows the next one as an application, e.g. inside `( … )`:
  // `[ [a]\n (f x) ]` must not read as `[a] (f x)`). Mirrors parseRecord.
  withElementScope state (fun () ->
    let elems = System.Collections.Generic.List<WT.Expr * Option<TokenRange>>()
    let mutable k = i + 1
    let mutable go = true
    while go
          && tok state k <> TRBracket
          && tok state k <> TEOF
          && not (declBarrier state k) do
      // each element is its own offside statement, so a wrapped element's args
      // don't grab the NEXT element (`[ f a\n f b ]` stays two elements)
      setStmtCol state (rng state k).start.column
      let (e, k2) = parseExpr state k
      if k2 = k then
        err
          state
          DiagnosticCode.unexpected
          k
          $"unexpected {foundDesc state k} in list"
        go <- false
      elif tok state k2 = TSemicolon || tok state k2 = TComma then
        elems.Add(e, Some(rng state k2))
        k <- k2 + 1
      else
        elems.Add(e, None)
        if tok state k2 <> TRBracket then
          requireElementSeparator
            state
            (WT.exprRange e)
            k2
            "a comma, semicolon, or newline between list elements"
        k <- k2
    let (closeB, p) =
      if tok state k = TRBracket then
        (rng state k, k + 1)
      else
        errUnclosed state k "]" "[" openB
        (zeroWidthAtEnd (rng state k), k)
    (WT.EList(span openB closeB, List.ofSeq elems, openB, closeB), p))

// `Type { name = value ; … }`. `i` is the `{`.
and parseRecord
  (state : ParserState)
  (typeName : WT.QualifiedTypeIdentifier)
  (nameRange : TokenRange)
  (i : int)
  : WT.Expr * int =
  let openB = rng state i
  // record fields are offside-delimited in their own scope
  // (else a field value swallows the next field name, e.g. inside `( … )`)
  withElementScope state (fun () ->
    let fields =
      System.Collections.Generic.List<TokenRange * (TokenRange * string) * WT.Expr>()
    let mutable k = i + 1
    let mutable go = true
    while go && tok state k <> TRBrace && tok state k <> TEOF do
      match tok state k, tok state (k + 1) with
      | TIdent fname, TEquals ->
        let fnameR = rng state k
        // each field is its own offside statement (value args don't grab the next field)
        setStmtCol state (rng state k).start.column
        let (value, k2) = parseExpr state (k + 2)
        fields.Add(span fnameR (WT.exprRange value), (fnameR, fname), value)
        if tok state k2 = TSemicolon || tok state k2 = TComma then
          k <- k2 + 1
        elif k2 > k then
          if tok state k2 <> TRBrace then
            requireElementSeparator
              state
              (WT.exprRange value)
              k2
              "a comma, semicolon, or newline between record fields"
          k <- k2
        else
          go <- false
      | _ ->
        errExpected state k "a record field 'name = value'"
        go <- false
    let (closeB, p) =
      if tok state k = TRBrace then
        (rng state k, k + 1)
      else
        errUnclosed state k "}" "{" openB
        (zeroWidthAtEnd (rng state k), k)
    (WT.ERecord(span nameRange closeB, typeName, List.ofSeq fields, openB, closeB),
     p))

// `Dict { key = value ; … }`. `keywordDict` is the `Dict` range; `i` is the `{`.
// Structurally identical to parseRecord (keys are field-like names), but emits an
// EDict carrying the `Dict` keyword range.
and parseDict
  (state : ParserState)
  (keywordDict : TokenRange)
  (i : int)
  : WT.Expr * int =
  let openB = rng state i
  withElementScope state (fun () ->
    let entries =
      System.Collections.Generic.List<TokenRange * (TokenRange * string) * WT.Expr>()
    let mutable k = i + 1
    let mutable go = true
    while go && tok state k <> TRBrace && tok state k <> TEOF do
      match tok state k, tok state (k + 1) with
      | TIdent kname, TEquals ->
        let knameR = rng state k
        setStmtCol state (rng state k).start.column
        let (value, k2) = parseExpr state (k + 2)
        entries.Add(span knameR (WT.exprRange value), (knameR, kname), value)
        if tok state k2 = TSemicolon || tok state k2 = TComma then
          k <- k2 + 1
        elif k2 > k then
          if tok state k2 <> TRBrace then
            requireElementSeparator
              state
              (WT.exprRange value)
              k2
              "a comma, semicolon, or newline between dict entries"
          k <- k2
        else
          go <- false
      | _ ->
        errExpected state k "a dict entry 'key = value'"
        go <- false
    let (closeB, p) =
      if tok state k = TRBrace then
        (rng state k, k + 1)
      else
        errUnclosed state k "}" "{" openB
        (zeroWidthAtEnd (rng state k), k)
    (WT.EDict(
      span keywordDict closeB,
      List.ofSeq entries,
      keywordDict,
      openB,
      closeB
     ),
     p))

// record update `{ expr with name = value ; … }`. `i` is the `{`.
and parseRecordUpdate (state : ParserState) (i : int) : WT.Expr * int =
  let openB = rng state i
  // fields are offside-delimited, same as parseRecord — suspend paren relaxation
  // and anchor each field value at its own column so it can't grab the next field.
  withElementScope state (fun () ->
    let (recordExpr, j) = parseExpr state (i + 1)
    let (kwWith, afterWith) =
      if tok state j = TWith then
        (rng state j, j + 1)
      else
        errExpected state j "'with' in record update"
        (zeroWidthAtEnd (rng state j), j)
    let updates =
      System.Collections.Generic.List<(TokenRange * string) * TokenRange * WT.Expr>()
    let mutable k = afterWith
    let mutable go = true
    while go && tok state k <> TRBrace && tok state k <> TEOF do
      match tok state k, tok state (k + 1) with
      | TIdent fname, TEquals ->
        let fnameR = rng state k
        let eqR = rng state (k + 1)
        setStmtCol state (rng state k).start.column
        let (value, k2) = parseExpr state (k + 2)
        updates.Add((fnameR, fname), eqR, value)
        if tok state k2 = TSemicolon || tok state k2 = TComma then
          k <- k2 + 1
        elif k2 > k then
          if tok state k2 <> TRBrace then
            requireElementSeparator
              state
              (WT.exprRange value)
              k2
              "a comma, semicolon, or newline between record-update fields"
          k <- k2
        else
          go <- false
      | _ ->
        errExpected state k "a record-update field 'name = value'"
        go <- false
    // `{ r with }` updates nothing — reject it rather than lower a degenerate
    // update (both lowerings otherwise have to special-case the empty list).
    if updates.Count = 0 then
      errExpected state afterWith "at least one 'field = value' in a record update"
    let (closeB, p) =
      if tok state k = TRBrace then
        (rng state k, k + 1)
      else
        errUnclosed state k "}" "{" openB
        (zeroWidthAtEnd (rng state k), k)
    (WT.ERecordUpdate(
      span openB closeB,
      recordExpr,
      List.ofSeq updates,
      openB,
      closeB,
      kwWith
     ),
     p))

// Type references. Precedence (loosest first): function `A -> B`, tuple
// `A * B`, then atoms (prim / List / Dict / custom / `'a` / parenthesized).
and parseTypeRef (state : ParserState) (i : int) : WT.TypeReference * int =
  if tooDeep state i || outOfFuel state i then
    (WT.TUnit(rng state i), state.tokenCount - 1)
  else
    // Defensive: `state.pendingGt` is always 0 here in well-formed input (a `>>`-induced
    // pending is consumed by the enclosing generic before the next parseTypeRef).
    // Clearing it stops a malformed `>>` in a prior parse from leaking a phantom `>`
    // into this one.
    state.pendingGt <- 0
    state.depth <- state.depth + 1
    let r = parseFnType state i
    state.depth <- state.depth - 1
    r

// `A -> B -> C` (right-nested): arguments = [(A,->),(B,->)], ret = C
and parseFnType (state : ParserState) (i : int) : WT.TypeReference * int =
  let (first, j) = parseTupleType state i
  if tok state j <> TArrow then
    (first, j)
  else
    let args = System.Collections.Generic.List<WT.TypeReference * TokenRange>()
    let mutable cur = first
    let mutable k = j
    while tok state k = TArrow do
      args.Add(cur, rng state k)
      let (next, k2) = parseTupleType state (k + 1)
      cur <- next
      k <- if k2 > k then k2 else k + 1
    (WT.TFn(
      span (WT.typeReferenceRange first) (WT.typeReferenceRange cur),
      List.ofSeq args,
      cur
     ),
     k)

// `A * B * C` (bare tuple, e.g. inside `List<…>`); parenthesized tuples fill
// in real paren ranges at the atom level.
and parseTupleType (state : ParserState) (i : int) : WT.TypeReference * int =
  let (first, j) = parseAtomType state i
  // a pending `>` (from splitting a `>>`) means we're still inside an enclosing
  // generic, so a following `*` belongs to an OUTER tuple — don't absorb it here
  // (otherwise `List<List<A>> * B` mis-parses as `List<List<A> * B>`).
  if tok state j <> TStar || state.pendingGt > 0 then
    (first, j)
  else
    let star1 = rng state j
    let (second, k) = parseAtomType state (j + 1)
    let rest = System.Collections.Generic.List<TokenRange * WT.TypeReference>()
    let mutable m = k
    while tok state m = TStar && state.pendingGt = 0 do
      let sr = rng state m
      let (t, m2) = parseAtomType state (m + 1)
      rest.Add(sr, t)
      m <- if m2 > m then m2 else m + 1
    let endT = if rest.Count > 0 then snd (Seq.last rest) else second
    let z = zeroWidthAtEnd (WT.typeReferenceRange first) // bare tuple: no parens
    (WT.TTuple(
      span (WT.typeReferenceRange first) (WT.typeReferenceRange endT),
      first,
      star1,
      second,
      List.ofSeq rest,
      z,
      z
     ),
     m)

// `<T1, T2, …>` generic type-args on a custom type; uses expectGt so a trailing
// `>>` splits correctly. Returns the args, the real or recovered closing `>`
// range, and the index after it.
and parseTypeArgs
  (state : ParserState)
  (i : int)
  : List<WT.TypeReference> * Option<TokenRange> * int =
  if tok state i <> TLt then
    ([], None, i)
  else
    let args = System.Collections.Generic.List<WT.TypeReference>()
    let (first, j) = parseTypeRef state (i + 1)
    args.Add first
    let mutable k = j
    // stop taking args once a `>>` has left a `>` pending for THIS level, else a
    // nested `Option<Result<T,S>>` would swallow the enclosing type's next arg
    // (`Option<Result<T,S>, S>`). Mirrors the tuple loop's `state.pendingGt = 0` guard.
    while tok state k = TComma && state.pendingGt = 0 do
      let (a, k2) = parseTypeRef state (k + 1)
      args.Add a
      k <- if k2 > k then k2 else k + 1
    let (closeR, k3) = expectGt state k
    (List.ofSeq args, Some closeR, k3)

and parseAtomType (state : ParserState) (i : int) : WT.TypeReference * int =
  match tok state i with
  | TLParen ->
    // `(T)` grouping or `(A * B)` parenthesized tuple
    let openP = rng state i
    let (inner, j) = parseFnType state (i + 1)
    if tok state j = TRParen then
      let closeP = rng state j
      match inner with
      | WT.TTuple(_, f, s1, sec, rest, _, _) ->
        (WT.TTuple(span openP closeP, f, s1, sec, rest, openP, closeP), j + 1)
      | other -> (other, j + 1)
    else
      errUnclosed state j ")" "(" openP
      (inner, j)
  | TIdent "List" when tok state (i + 1) = TLt ->
    if
      (rng state (i + 1)).start.row <> (rng state i).end_.row
      || (rng state (i + 1)).start.column <> (rng state i).end_.column
    then
      err
        state
        DiagnosticCode.expected
        (i + 1)
        "Generic type arguments must be adjacent to the type name"
    let kwList = rng state i
    let openB = rng state (i + 1)
    let (inner, j) = parseTypeRef state (i + 2)
    let (closeB, k) = expectGt state j
    (WT.TList(span (rng state i) closeB, kwList, openB, inner, closeB), k)
  | TIdent "Dict" when tok state (i + 1) = TLt ->
    if
      (rng state (i + 1)).start.row <> (rng state i).end_.row
      || (rng state (i + 1)).start.column <> (rng state i).end_.column
    then
      err
        state
        DiagnosticCode.expected
        (i + 1)
        "Generic type arguments must be adjacent to the type name"
    let kwDict = rng state i
    let openB = rng state (i + 1)
    let (inner, j) = parseTypeRef state (i + 2)
    let (closeB, k) = expectGt state j
    (WT.TDict(span (rng state i) closeB, kwDict, openB, inner, closeB), k)
  | TIdent(PrimTypeName ctor) when not ((txt state i).StartsWith "'") ->
    (ctor (rng state i), i + 1)
  // a tick-prefixed name is ALWAYS a type variable, even when uppercase
  // (`'TModel`) — the lexer drops the tick so the case-based check below would
  // otherwise mistake it for a custom type. The token text keeps the tick.
  | TIdent _ when (txt state i).StartsWith "'" ->
    let r = rng state i
    let tickR =
      { start = r.start; end_ = { row = r.start.row; column = r.start.column + 1 } }
    let nameR =
      { start = { row = r.start.row; column = r.start.column + 1 }; end_ = r.end_ }
    (WT.TVariable(
      r,
      tickR,
      (nameR,
       (match tok state i with
        | TIdent nm -> nm
        | _ -> "_"))
     ),
     i + 1)
  | TIdent name when name.Length > 0 && System.Char.IsUpper name[0] ->
    let (mods, final, j) = parseQualified state i
    if
      tok state j = TLt
      && ((rng state j).start.row <> final.range.end_.row
          || (rng state j).start.column <> final.range.end_.column)
    then
      err
        state
        DiagnosticCode.expected
        j
        "Generic type arguments must be adjacent to the type name"
    let (typeArgs, closeGeneric, j2) = parseTypeArgs state j
    let endR =
      match closeGeneric, List.rev typeArgs with
      | Some closeRange, _ -> closeRange
      | None, last :: _ -> WT.typeReferenceRange last
      | None, [] -> final.range
    (WT.TCustom
      { range = span (rng state i) endR
        modules = mods
        typ = final
        typeArgs = typeArgs },
     j2)
  | TIdent name ->
    // a lowercase ident in type position is a type variable: `'a` lexes to the
    // bare name "a" with the token range covering the apostrophe.
    let r = rng state i
    let tickR =
      { start = r.start; end_ = { row = r.start.row; column = r.start.column + 1 } }
    let nameR =
      { start = { row = r.start.row; column = r.start.column + 1 }; end_ = r.end_ }
    (WT.TVariable(r, tickR, (nameR, name)), i + 1)
  | _ ->
    errExpected state i "a type"
    // recovery: leave closing/separating/decl-start tokens for the enclosing construct
    (WT.TUnit(rng state i),
     (if i < state.tokenCount && not (isRecoveryBarrier (tok state i)) then
        i + 1
      else
        i))

// a function parameter `(name: Type)` or `()`
and parseParam (state : ParserState) (i : int) : WT.FnParam * int =
  let lparen = rng state i
  if tok state (i + 1) = TRParen then
    (WT.FPUnit(span lparen (rng state (i + 1))), i + 2)
  else
    let nameId : WT.Identifier =
      match tok state (i + 1) with
      | TIdent nm -> { range = rng state (i + 1); name = nm }
      // `_` names a parameter you don't intend to use. It's also what `()` is stored as, so accepting it
      // here is what makes `(_: Unit)` and `()` both parse to the same thing and either form round-trip.
      | TUnderscore -> { range = rng state (i + 1); name = "_" }
      | _ ->
        errExpected state (i + 1) "a parameter name"
        { range = rng state (i + 1); name = "_" }
    let (colon, afterColon) =
      if tok state (i + 2) = TColon then
        (rng state (i + 2), i + 3)
      else
        errExpected state (i + 2) "':'"
        (zeroWidthAtEnd (rng state (i + 2)), i + 2)
    let (typ, afterTyp) = parseTypeRef state afterColon
    let (rparen, afterRP) =
      if tok state afterTyp = TRParen then
        (rng state afterTyp, afterTyp + 1)
      else
        errUnclosed state afterTyp ")" "(" lparen
        (zeroWidthAtEnd (rng state afterTyp), afterTyp)
    (WT.FPNormal(span lparen rparen, nameId, typ, lparen, colon, rparen), afterRP)

// A declaration-scope function (`let f (p: T) … : R = body`) or value
// (`val x = body`). Legacy module-level `let x = body` also comes through here
// to retain a recovery DValue beside its focused diagnostic.
and parseDecl (state : ParserState) (i : int) : WT.Declaration * int =
  let keywordLet = rng state i
  let nameIdx = i + 1
  let nameId : WT.Identifier =
    match tok state nameIdx with
    | TIdent nm -> { range = rng state nameIdx; name = nm }
    | _ ->
      errExpected state nameIdx "a declaration name"
      { range = rng state nameIdx; name = "_" }
  let (typeParams, afterName) = parseTypeParams state (nameIdx + 1)
  if tok state afterName = TLParen then
    let ps = System.Collections.Generic.List<WT.FnParam>()
    let mutable kk = afterName
    let mutable more = true
    while more && tok state kk = TLParen do
      let (p, kk2) = parseParam state kk
      ps.Add p
      if kk2 = kk then more <- false else kk <- kk2
    for parameter in ps do
      match parameter with
      | WT.FPNormal(_, name, _, _, _, _) when name.name = "" ->
        state.diagnostics.Add
          { code = DiagnosticCode.pattern
            severity = DiagError
            range = name.range
            message = "Blank parameter '___' is not allowed in a package function"
            related = []
            hint = Some "use () for a unit parameter or give the parameter a name" }
      | _ -> ()
    let (colon, afterColon) =
      if tok state kk = TColon then
        (rng state kk, kk + 1)
      else
        errExpected state kk "':' before the return type"
        (zeroWidthAtEnd (rng state kk), kk)
    let (returnType, afterRet) = parseTypeRef state afterColon
    let (eq, afterEq) =
      if tok state afterRet = TEquals then
        (rng state afterRet, afterRet + 1)
      else
        errExpected state afterRet "'='"
        (zeroWidthAtEnd (rng state afterRet), afterRet)
    let (body, afterBody) = parseBlock state afterEq
    (WT.DFunction
      { range = span keywordLet (WT.exprRange body)
        name = nameId
        typeParams = typeParams
        parameters = List.ofSeq ps
        returnType = returnType
        body = body
        keywordLet = keywordLet
        symbolColon = colon
        symbolEquals = eq
        description = docOf state i },
     afterBody)
  else
    let afterAnno =
      if tok state afterName = TColon then
        state.diagnostics.Add
          { code = DiagnosticCode.unexpected
            severity = DiagError
            range = rng state afterName
            message = "Value annotations are not supported"
            related = []
            hint = Some "remove ': Type' from this value declaration" }
        snd (parseTypeRef state (afterName + 1))
      else
        afterName
    let (eq, afterEq) =
      if tok state afterAnno = TEquals then
        (rng state afterAnno, afterAnno + 1)
      else
        errExpected state afterAnno "'='"
        (zeroWidthAtEnd (rng state afterAnno), afterAnno)
    let (body, afterBody) = parseBlock state afterEq
    (WT.DValue
      { range = span keywordLet (WT.exprRange body)
        name = nameId
        body = body
        keywordVal = keywordLet
        symbolEquals = eq
        description = docOf state i },
     afterBody)

// `type Name [<'a>] = Definition`
and parseTypeDecl (state : ParserState) (i : int) : WT.Declaration * int =
  let kwType = rng state i
  let nameId : WT.Identifier =
    match tok state (i + 1) with
    | TIdent nm -> { range = rng state (i + 1); name = nm }
    | _ ->
      errExpected state (i + 1) "a type name"
      { range = rng state (i + 1); name = "_" }
  let (typeParams, afterName) = parseTypeParams state (i + 2)
  let (eq, afterEq) =
    if tok state afterName = TEquals then
      (rng state afterName, afterName + 1)
    else
      errExpected state afterName "'=' in type definition"
      (zeroWidthAtEnd (rng state afterName), afterName)
  let (def, afterDef) = parseTypeDefinition state afterEq
  let endR = if afterDef > 0 then rng state (afterDef - 1) else eq
  (WT.DType
    { range = span kwType endR
      name = nameId
      typeParams = typeParams
      definition = def
      keywordType = kwType
      symbolEquals = eq
      description = docOf state i },
   afterDef)

and parseTypeDefinition (state : ParserState) (i : int) : WT.TypeDefinition * int =
  let isEnumStart =
    tok state i = TBar
    || (match tok state i, tok state (i + 1) with
        | TIdent nm, t2 when nm.Length > 0 && System.Char.IsUpper nm[0] ->
          t2 = TOf || t2 = TBar
        | _ -> false)
  match tok state i with
  | TLBrace -> parseRecordDef state i
  | _ when isEnumStart -> parseEnumDef state i
  | _ -> let (t, j) = parseTypeRef state i in (WT.TDAlias t, j)

and parseRecordDef (state : ParserState) (i : int) : WT.TypeDefinition * int =
  let fields =
    System.Collections.Generic.List<WT.RecordFieldSyntax * Option<TokenRange>>()
  let mutable k = i + 1
  let mutable go = true
  while go && tok state k <> TRBrace && tok state k <> TEOF do
    match tok state k, tok state (k + 1) with
    | TIdent fname, TColon ->
      let fnameR = rng state k
      let colon = rng state (k + 1)
      let (typ, k2) = parseTypeRef state (k + 2)
      let field : WT.RecordFieldSyntax =
        { range = span fnameR (WT.typeReferenceRange typ)
          name = (fnameR, fname)
          typ = typ
          symbolColon = colon }
      if tok state k2 = TSemicolon || tok state k2 = TComma then
        fields.Add(field, Some(rng state k2))
        k <- k2 + 1
      elif k2 > k then
        fields.Add(field, None)
        if tok state k2 <> TRBrace then
          requireElementSeparator
            state
            (WT.typeReferenceRange typ)
            k2
            "a comma, semicolon, or newline between record-type fields"
        k <- k2
      else
        go <- false
    | _ ->
      errExpected state k "a record field 'name : Type'"
      go <- false
  // `type X = {}` — an empty record isn't valid. Diagnose so the `"_"` placeholder
  // the normalizer inserts (records need ≥1 field) is honest recovery, not a
  // silently-accepted phantom field. (A `{ garbage }` already errored in the loop
  // and won't be at `}` here, so this only fires for a genuinely empty record.)
  if fields.Count = 0 && tok state k = TRBrace then
    err state DiagnosticCode.expected k "a record type needs at least one field"
  let kk =
    if tok state k = TRBrace then
      k + 1
    else
      errUnclosed state k "}" "{" (rng state i)
      k
  (WT.TDRecord(List.ofSeq fields), kk)

and parseEnumField (state : ParserState) (i : int) : WT.EnumFieldSyntax * int =
  // an enum case's fields are separated by `*` (`Case of A * B` = two fields), so
  // the field type is parsed at ATOM level — a bare `*` is the separator, not a
  // tuple. A tuple field must be parenthesized (`(A * B)`), which parseAtomType handles.
  match tok state i, tok state (i + 1) with
  | TIdent lbl, TColon ->
    let lblR = rng state i
    let colon = rng state (i + 1)
    let (typ, j) = parseAtomType state (i + 2)
    ({ range = span lblR (WT.typeReferenceRange typ)
       typ = typ
       label = Some(lblR, lbl)
       symbolColon = Some colon },
     j)
  | _ ->
    let (typ, j) = parseAtomType state i
    ({ range = WT.typeReferenceRange typ
       typ = typ
       label = None
       symbolColon = None },
     j)

and parseEnumDef (state : ParserState) (i : int) : WT.TypeDefinition * int =
  let cases = System.Collections.Generic.List<TokenRange * WT.EnumCaseSyntax>()
  let mutable k = i
  let mutable go = true
  let mutable first = true
  while go do
    // Only the first case may omit the leading `|` (`type X = A | B`); every case
    // after it REQUIRES a `|`. Otherwise the following statement — which often
    // starts with an uppercase name (`type X = | A | B | C` then `Foo.bar = …`) —
    // would be swallowed as another case, orphaning the rest of that line.
    if tok state k <> TBar && not first then
      go <- false
    else
      let barRange =
        if tok state k = TBar then
          (let r = rng state k in
           k <- k + 1
           r)
        else
          zeroWidthAtEnd (rng state k)
      match tok state k with
      | TIdent cname when cname.Length > 0 && System.Char.IsUpper cname[0] ->
        first <- false
        let cnameR = rng state k
        k <- k + 1
        let fields = System.Collections.Generic.List<WT.EnumFieldSyntax>()
        let mutable kwOf = None
        if tok state k = TOf then
          kwOf <- Some(rng state k)
          k <- k + 1
          let (f0, k0) = parseEnumField state k
          fields.Add f0
          k <- k0
          while tok state k = TStar do
            k <- k + 1
            let (fi, ki) = parseEnumField state k
            fields.Add fi
            k <- ki
        let lastR = if fields.Count > 0 then (Seq.last fields).range else cnameR
        cases.Add(
          barRange,
          { range = span cnameR lastR
            name = (cnameR, cname)
            fields = List.ofSeq fields
            keywordOf = kwOf }
        )
      | _ -> go <- false
  // `type X = |` with no case — diagnose so the `"_"` placeholder the normalizer
  // inserts (enums need ≥1 case) is honest recovery, not a silent phantom case.
  if cases.Count = 0 then
    err state DiagnosticCode.expected k "an enum type needs at least one case"
  (WT.TDEnum(List.ofSeq cases), k)

// Test-mode only: the expected side of an assertion `actual = expected`. Either
// an `error="msg"` / `sqlerror="msg"` marker (also the bare `error "msg"` shape)
// or a plain value expression. The message string is kept raw (the tokenizer has
// already unescaped it); normalization happens in the lowering.
and parseTestExpected (state : ParserState) (i : int) : WT.TestExpected * int =
  match tok state i, tok state (i + 1), tok state (i + 2) with
  | TIdent "error", TEquals, TStringLit s -> (WT.TEError s, i + 3)
  | TIdent "sqlerror", TEquals, TStringLit s -> (WT.TESqlError s, i + 3)
  | TIdent "error", TStringLit s, _ -> (WT.TEError s, i + 2)
  | TIdent "sqlerror", TStringLit s, _ -> (WT.TESqlError s, i + 2)
  | _ -> let (e, j) = parseExpr state i in (WT.TEExpr e, j)

// `[<DB>]` attribute prefix on a type decl: 5 tokens. Parsing represents it;
// post-parse validation restricts it to Test source.
// `[` `<` `DB` `>` `]`.
and isDbAttr (state : ParserState) (i : int) : bool =
  tok state i = TLBracket
  && tok state (i + 1) = TLt
  && tok state (i + 2) = TIdent "DB"
  && tok state (i + 3) = TGt
  && tok state (i + 4) = TRBracket

// Parse declarations/expressions whose start column is >= minCol (offside): a
// less-indented item ends the scope. Used for the file body and, recursively,
// for nested `module X =` blocks, so module nesting is preserved (FQN paths).
// `insideModule` distinguishes a module body (declarations only — values require
// `val`) from a file's top level (a no-param `let x = …` is a script EXPRESSION
// that sequences with what follows).
// Test assertions and `[<DB>]` declarations are represented in every parse.
// Validation later decides whether Script, Package, or Test source may contain
// them. This keeps file purpose out of expression parsing.
and parseItems
  (state : ParserState)
  (insideModule : bool)
  (start : int)
  (minCol : int)
  : List<WT.Declaration> * List<WT.Expr> * int =
  // each item is its own offside statement (anchored per item in the body);
  // the enclosing scope's anchor + decl anchor are restored on exit
  let savedDecl = state.declAnchor
  let r =
    withElementScope state (fun () -> parseItemsBody state insideModule start minCol)
  state.declAnchor <- savedDecl
  r

and parseItemsBody
  (state : ParserState)
  (insideModule : bool)
  (start : int)
  (minCol : int)
  : List<WT.Declaration> * List<WT.Expr> * int =
  let decls = System.Collections.Generic.List<WT.Declaration>()
  let exprs = System.Collections.Generic.List<WT.Expr>()
  let mutable k = start
  let mutable go = true
  while go && tok state k <> TEOF do
    if (rng state k).start.column < minCol then
      go <- false
    else
      let before = k
      setStmtCol state (rng state k).start.column
      state.declAnchor <- (rng state k).start.column
      (match tok state k, tok state (k + 1) with
       | TLBracket, TLt when isDbAttr state k && tok state (k + 5) = TType ->
         // `[<DB>] type Name = AliasedType` — a user DB declaration
         let (d, k2) = parseTypeDecl state (k + 5)
         (match d with
          | WT.DType t ->
            match t.definition with
            | WT.TDAlias _ -> decls.Add(WT.DTypeDB t)
            | _ ->
              err
                state
                DiagnosticCode.expected
                (k + 5)
                "[<DB>] type must be a type alias"
              decls.Add(WT.DTypeDB t)
          | other -> decls.Add other)
         k <- k2
       | TVal, TIdent _ ->
         // `val x = …` is ALWAYS a value DECLARATION (the explicit value-decl
         // keyword), in a module or at file top level alike.
         let (d, k2) = parseDecl state k
         match d with
         | WT.DFunction _ ->
           err
             state
             DiagnosticCode.expected
             k
             "'val' declares a value and cannot have function parameters"
         | _ -> ()
         decls.Add d
         k <- k2
       | TLet, TIdent _ ->
         // the decl parse below is speculative (a top-level no-param `let` is
         // reparsed as an expression) — drop its diagnostics on reparse so
         // errors aren't reported twice
         let diagnosticsBefore = state.diagnostics.Count
         let (d, k2) = parseDecl state k
         // A no-param `let x = …` is a script EXPRESSION at file top level and
         // with explicit `in`. At module declaration scope it is retained as a
         // DValue recovery node with a diagnostic requiring `val`; `let f (p) …`
         // is a DFunction and is never reparsed.
         let asExpr =
           match d with
           | WT.DValue _ -> tok state k2 = TIn || not insideModule
           | _ -> false
         if asExpr then
           state.diagnostics.RemoveRange(
             diagnosticsBefore,
             state.diagnostics.Count - diagnosticsBefore
           )
           let (e, k3) = parseExpr state k
           exprs.Add e
           k <- k3
         else
           match d with
           | WT.DValue _ when insideModule ->
             state.diagnostics.Add
               { code = DiagnosticCode.unexpected
                 severity = DiagError
                 range = rng state k
                 message =
                   "Module value declarations must use 'val'; 'let' is reserved for functions and local bindings"
                 related = []
                 hint = Some "replace 'let' with 'val'" }
           | _ -> ()
           decls.Add d
           k <- k2
       | TType, TIdent _ ->
         let (d, k2) = parseTypeDecl state k
         decls.Add d
         k <- k2
       | TIdent "module", TIdent _ ->
         let kwModule = rng state k
         let moduleCol = (rng state k).start.column
         let parts = System.Collections.Generic.List<string>()
         (match tok state (k + 1) with
          | TIdent s -> parts.Add s
          | _ -> ())
         let mutable nk = k + 2
         let mutable go2 = true
         while go2 do
           match tok state nk, tok state (nk + 1) with
           | TDot, TIdent s ->
             parts.Add s
             nk <- nk + 2
           | _ -> go2 <- false
         let nameRange = span (rng state (k + 1)) (rng state (nk - 1))
         let nameStr = System.String.Join(".", parts)
         // `module X =`: body is offside-indented under the keyword.
         // `module Darklang.X` (no `=`): file-level header wrapping the rest.
         let (mdecls, mexprs, k2) =
           if tok state nk = TEquals then
             parseItems state true (nk + 1) (moduleCol + 1)
           else
             parseItems state true nk minCol
         if tok state nk = TEquals && List.isEmpty mdecls && List.isEmpty mexprs then
           errExpected state (nk + 1) "an indented module body"
         // A module's trailing expressions belong to the module (as `DExpr`
         // declarations), not the enclosing scope — so they pretty-print nested.
         // span the whole module (header through last child), like other decls —
         // a header-only range makes range-gated walkers (hover) skip the body
         let moduleEndR = if k2 > 0 then rng state (k2 - 1) else nameRange
         decls.Add(
           WT.DModule
             { range = span kwModule moduleEndR
               name = (nameRange, nameStr)
               declarations = mdecls @ (mexprs |> List.map WT.DExpr)
               keywordModule = kwModule }
         )
         k <- k2
         if tok state nk <> TEquals then go <- false // file header consumed the rest
       | _ ->
         let (e, k2) = parseExpr state k
         // A source-level `actual = expected` is represented as a test node.
         // Its validity for this caller is decided after parsing.
         if tok state k2 = TEquals then
           let (expected, k3) = parseTestExpected state (k2 + 1)
           let endR = if k3 > 0 then rng state (k3 - 1) else rng state k2
           decls.Add(
             WT.DTest
               { range = span (WT.exprRange e) endR
                 actual = e
                 expected = expected }
           )
           k <- k3
         else
           exprs.Add e
           k <- k2)
      if k = before then if tok state k = TEOF then go <- false else k <- k + 1
  (List.ofSeq decls, List.ofSeq exprs, k)

and parseFile (state : ParserState) : ParseResult =
  validateLiterals state
  let (topDecls, topExprs, _) = parseItems state false 0 0
  let fileRange =
    if state.tokenCount > 1 then
      span (rng state 0) (rng state (state.tokenCount - 2))
    else
      rng state 0
  let sf : WT.SourceFile =
    { range = fileRange; declarations = topDecls; exprsToEval = topExprs }
  { parsed = Some(WT.SourceFile sf); diagnostics = List.ofSeq state.diagnostics }

/// Parse a pre-tokenized stream. Part of the rec chain so string interpolation
/// can recursively parse the (range-offset) sub-tokens of each `{expr}`.
and parseTokensAt (interpDepth : int) (toks : SpannedToken[]) : ParseResult =
  let scopes = System.Collections.Generic.Stack<OffsideScope>()
  scopes.Push { stmtCol = -1; stmtExact = false }
  let state =
    { toks = toks
      tokenCount = toks.Length
      diagnostics = System.Collections.Generic.List<Diagnostic>()
      scopes = scopes
      pendingGt = 0
      pendingGtRange =
        { start = { row = 0; column = 0 }; end_ = { row = 0; column = 0 } }
      declAnchor = -1
      depth = 0
      abandoned = false
      steps = 0
      interpDepth = interpDepth }
  parseFile state

and parseTokens (toks : SpannedToken[]) : ParseResult = parseTokensAt 0 toks

let private parseSyntax (source : string) : ParseResult =
  match tokenize source with
  | Error e ->
    { parsed = None
      diagnostics =
        [ { code = DiagnosticCode.lex
            severity = DiagError
            range =
              { start = { row = 0; column = 0 }; end_ = { row = 0; column = 0 } }
            message = e
            related = []
            hint = None } ] }
  | Ok(toksList, lexDiags) ->
    // lexical-recovery diagnostics (malformed lexemes the tokenizer recovered from)
    // are surfaced alongside the parser's own diagnostics.
    let result = parseTokens (List.toArray toksList)
    let lexDiagnostics =
      lexDiags
      |> List.map (fun (r, m) ->
        { code = DiagnosticCode.lex
          severity = DiagError
          range = r
          message = m
          related = []
          hint = None })
    { result with diagnostics = lexDiagnostics @ result.diagnostics }

/// Parse for tooling: return a recoverable tree and include mode-independent
/// structural diagnostics after a clean syntax pass.
let parse (source : string) : ParseResult =
  let result = parseSyntax source
  let syntaxDiagnostics = result.diagnostics
  // Tree-wide rules have one implementation in Validation. Run them only
  // after a clean syntax pass so recovery holes do not create cascaded errors.
  let structuralDiagnostics =
    match syntaxDiagnostics, result.parsed with
    | [], Some(WT.SourceFile sourceFile) ->
      sourceFile
      |> Validation.validateStructure
      |> List.map diagnosticOfValidationIssue
    | _ -> []
  { result with diagnostics = syntaxDiagnostics @ structuralDiagnostics }

/// Parse for execution: syntax, structural, and file-purpose validation run
/// once, and only a validated source file can be returned on success.
let parseFor
  (mode : Validation.Mode)
  (source : string)
  : Result<Validation.ValidatedSourceFile, List<Diagnostic>> =
  let result = parseSyntax source
  match result.diagnostics, result.parsed with
  | [], Some(WT.SourceFile sourceFile) ->
    match Validation.validate mode sourceFile with
    | Ok validated -> Ok validated
    | Error issues ->
      issues |> NEList.toList |> List.map diagnosticOfValidationIssue |> Error
  | (_ :: _ as diagnostics), _ -> Error diagnostics
  | [], None ->
    Error
      [ { code = DiagnosticCode.unexpected
          severity = DiagError
          range = { start = { row = 0; column = 0 }; end_ = { row = 0; column = 0 } }
          message = "Parser did not produce a source tree"
          related = []
          hint = None } ]

/// Kept as a compatibility entrypoint. Test syntax has the same parse shape as
/// all other source; parseFor Validation.Test applies the Test purpose rules.
let parseTestFile (source : string) : ParseResult = parse source

/// Render a diagnostic for humans: code, position, message, a source snippet
/// with caret markers, related locations, and the hint if any. E.g.
///   error[PARSE-UNCLOSED] at 1:9: expected ']' to close the '[' at line 1:9, found end of file
///     1 | let x = [1L; 2L
///       |         ^
///     note: the '[' opened here (1:9)
let renderDiagnostic (source : string) (d : Diagnostic) : string =
  let lines = source.Split '\n'
  let snippet (r : TokenRange) : List<string> =
    if r.start.row >= 0 && r.start.row < lines.Length then
      let line = (lines[r.start.row]).TrimEnd('\r')
      let lineNo = string (r.start.row + 1)
      let caretCol = max 0 (min r.start.column line.Length)
      let width =
        if r.start.row = r.end_.row then
          max
            1
            (min (r.end_.column - r.start.column) (max 1 (line.Length - caretCol)))
        else
          1
      [ $"  {lineNo} | {line}"
        "  "
        + String.replicate lineNo.Length " "
        + " | "
        + String.replicate caretCol " "
        + String.replicate width "^" ]
    else
      []
  [ yield
      $"error[{d.code}] at {d.range.start.row + 1}:{d.range.start.column + 1}: {d.message}"
    yield! snippet d.range
    for (r, note) in d.related do
      yield $"  note: {note} ({r.start.row + 1}:{r.start.column + 1})"
      yield! snippet r
    match d.hint with
    | Some h -> yield $"  hint: {h}"
    | None -> () ]
  |> String.concat "\n"
