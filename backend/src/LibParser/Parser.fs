/// The hand-written parser: source → range-complete `WrittenTypes` tree, capturing
/// fine-grained keyword/symbol/operator ranges (not just node spans) for the highlighter /
/// LSP. Recovers from errors, returning diagnostics alongside a best-effort tree.
module LibParser.Parser

open LibParser.Tokenizer // Pos, TokenRange, Token
open LibParser.Lexer // SpannedToken, tokenize

module WT = LibParser.WrittenTypes
module Validation = LibParser.Validation
module PT = LibExecution.ProgramTypes

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
  let effect = "PARSE-EFFECT" // unknown effect name in a `:{…}` row
  let bound = "PARSE-BOUND" // a type-param bound that does not name a trait, or a trait member that is not supported
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

// Build the diagnostic after the exception has exited the recursive parser calls,
// when enough stack space is available to safely construct the error response.
let private stackExhaustionDiagnostic () : Diagnostic =
  { code = DiagnosticCode.tooDeep
    severity = DiagError
    range = { start = { row = 0; column = 0 }; end_ = { row = 0; column = 0 } }
    message = "nesting too deep for the available stack; parsing abandoned"
    related = []
    hint = Some "Split the expression into intermediate bindings." }

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
  | TEqEq -> Some(WT.InfixFnCall WT.ComparisonEquals)
  | TNeq -> Some(WT.InfixFnCall WT.ComparisonNotEquals)
  | TLt -> Some(WT.InfixFnCall WT.ComparisonLessThan)
  | TGt -> Some(WT.InfixFnCall WT.ComparisonGreaterThan)
  | TLte -> Some(WT.InfixFnCall WT.ComparisonLessThanOrEqual)
  | TGte -> Some(WT.InfixFnCall WT.ComparisonGreaterThanOrEqual)
  | TAnd -> Some(WT.BinOp WT.BinOpAnd)
  | TOr -> Some(WT.BinOp WT.BinOpOr)
  | TStarStar -> Some(WT.InfixFnCall WT.ArithmeticPower)
  | TBitAnd -> Some(WT.InfixFnCall WT.BitwiseAnd)
  | TBar -> Some(WT.InfixFnCall WT.BitwiseOr)
  | TBitXor -> Some(WT.InfixFnCall WT.BitwiseXor)
  | TShl -> Some(WT.InfixFnCall WT.ShiftLeft)
  | TShr -> Some(WT.InfixFnCall WT.ShiftRight)
  | _ -> None

// The one definition of "an integer-literal token" — `canStartAtom` and
// `canStartPattern` both build on it so the lists can't drift from each other.
// Matched EXHAUSTIVELY (no `_`): a new `Token` case won't compile until it's
// classified here, so a new integer type can't silently fall through to `false`
// — the drift that once made `| Ok 5s8 ->` unparseable while `Ok 5s8` worked.
let private isIntLit (t : Token) : bool =
  match t with
  | TBigInt _
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
  | TFloat32 _
  | TDecimal _
  | TStringLit _
  | TCharLit _
  | TInterpString
  | TTrue
  | TFalse
  | TPlus
  | TMinus
  | TStar
  | TStarStar
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
  | TTrait
  | TImpl
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
  | TQuestion
  | TPipe
  | TDotDotDot
  | TPercent
  | TShl
  | TShr
  | TBitAnd
  | TBitXor
  | TBitNot
  | TIdent _
  | TEOF -> false

let private canStartAtom (t : Token) : bool =
  isIntLit t
  || (match t with
      | TFloat _
      | TFloat32 _
      | TDecimal _
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
      | TLBrace
      // prefix `!`/`~`: `f !x` is `f (!x)`. Unambiguous — neither token has an
      // infix reading, and `!=` lexes as one token, so `a != b` is untouched.
      | TNot
      | TBitNot -> true
      | _ -> false)

// `TMinus` is included so a negative-literal enum-pattern field (`| Ok -4s8 ->`)
// parses; parsePatternBase's TMinus case handles it.
let private canStartPattern (t : Token) : bool =
  isIntLit t
  || (match t with
      | TUnderscore
      | TMinus
      | TFloat _
      | TFloat32 _
      | TDecimal _
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
      | TTrait
      | TImpl
      | TVal -> true
      | _ -> false)


/// All parser state, threaded explicitly through every parse function (no
/// closure): the token stream, the diagnostics sink, and the offside/recovery
/// registers. One value per parse; `parseTokens` constructs it.
// Mutable on purpose: every parse function reads and writes it, and returning a new one from
// each would add an allocation to every token's parse.
type ParserState =
  { toks : SpannedToken[] // the token stream being parsed
    tokenCount : int // = toks.Length, cached (read on every bounds check)
    diagnostics : System.Collections.Generic.List<Diagnostic> // parse errors collected during recovery
    scopes : System.Collections.Generic.Stack<OffsideScope> // offside anchor stack (a frame per let/if/match/paren body)
    // `|` is both bitwise-or and the match-arm separator. Each anchor records an
    // enclosing match's arm row and column so `barStartsArm` leaves arm-position
    // bars for `parseMatch`. Regions where an arm cannot start clear the anchors.
    mutable matchArms : List<int * int>
    // Closing a nested generic like `Dict<List<Int64>>` ends in `>>`, which the
    // lexer produces as ONE token but which must close TWO levels. Closing the
    // inner `List<Int64>` uses only the first `>`, so the second is "left over"
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

[<RequireQualifiedAccess>]
type ItemScope =
  | Script
  | Module

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

let rec private advancePosFrom
  (pos : Pos)
  (text : string)
  (count : int)
  (index : int)
  : Pos =
  if index >= count || index >= text.Length then
    pos
  else
    let pos =
      if text[index] = '\n' then
        { row = pos.row + 1; column = 0 }
      else
        { pos with column = pos.column + 1 }
    advancePosFrom pos text count (index + 1)

let private advancePos (start : Pos) (text : string) (count : int) : Pos =
  advancePosFrom start text count 0

let private splitTrailingRange
  (state : ParserState)
  (i : int)
  (trailingLength : int)
  : TokenRange * TokenRange =
  let whole = rng state i
  let text = txt state i
  let boundary = advancePos whole.start text (max 0 (text.Length - trailingLength))
  ({ start = whole.start; end_ = boundary }, { start = boundary; end_ = whole.end_ })

// An int literal's digits and its suffix (`s8` in `5s8`; empty for a bare `5`).
let private splitIntSuffix
  (state : ParserState)
  (i : int)
  : TokenRange * TokenRange =
  let text = txt state i
  let digitCount = text |> Seq.takeWhile System.Char.IsDigit |> Seq.length
  splitTrailingRange state i (text.Length - digitCount)

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
// `let rec f`: Dark needs no `rec`. Report it, and go on from the name after it, so
// the binding parses as if `rec` were not there. `rec` alone is still a name: `let rec = 1`.
let skipRec (state : ParserState) (i : int) : int =
  match tok state i, tok state (i + 1) with
  | TIdent "rec", TIdent _ ->
    errFull
      state
      DiagnosticCode.unexpected
      i
      "Dark functions can call themselves; there's no `rec` keyword"
      []
      (Some "remove `rec`")
    i + 1
  | _ -> i
// the words the lexer reserves (`Lexer.keywords`); `true`, `false` and `_` are left
// out, since where a name is expected they are usually a literal pattern
let private isKeyword (t : Token) : bool =
  match t with
  | TTrue
  | TFalse
  | TUnderscore -> false
  | _ -> keywords |> List.exists (fun (_, k) -> k = t)
// "; `match` is a keyword, so to use it as a name write ``match``", or "" when
// the token at `i` is no keyword. `prefix` goes in front of the escaped word. A
// keyword that starts a line is more likely the next statement after a missing
// name, so it gets no hint.
let private keywordHint (state : ParserState) (i : int) (prefix : string) : string =
  if
    i > 0
    && i < state.tokenCount
    && isKeyword (tok state i)
    && (rng state i).start.row = (rng state (i - 1)).end_.row
  then
    let w = txt state i
    $"; `{w}` is a keyword, so to use it as a name write {prefix}``{w}``"
  else
    ""
// `errExpected` where a name is wanted, saying how to escape a keyword
let errExpectedName (state : ParserState) (i : int) (what : string) =
  let hint = keywordHint state i ""
  err
    state
    DiagnosticCode.expected
    i
    $"expected {what}, found {foundDesc state i}{hint}"
// A keyword where a name goes, on the line of what comes before it: reported as
// `errExpectedName` does, then read as the name it was meant to be, so the rest
// parses and nothing more is reported.
let private keywordAsName (state : ParserState) (i : int) (what : string) =
  if keywordHint state i "" = "" then
    None
  else
    errExpectedName state i what
    Some(txt state i)
// The name of a `name = value` or `name : Type` field at `k`, when `sep` follows it
let private fieldName (state : ParserState) (k : int) (sep : Token) (what : string) =
  match tok state k with
  | TIdent name when tok state (k + 1) = sep -> Some name
  | _ when tok state (k + 1) = sep -> keywordAsName state k what
  | _ -> None
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
// MinValue (so the NEGATED literal can exist: `-128s8`); consumed WITHOUT the
// minus, the written magnitude is out of range — diagnose instead of silently
// wrapping (`128s8` is NOT -128). Only the negating TMinus branches consume
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

// Expressions and patterns validate the source's decimal precision before
// constructing written digits. Formatting never determines literal acceptance
// or the decimal representation used during lowering.
let private literalParts
  (state : ParserState)
  (i : int)
  (format : string)
  (parsed : Result<string * string, FloatLiteral.Error>)
  : string * string =
  match parsed with
  | Ok parts -> parts
  // the lexer has already reported a malformed literal, as one diagnostic
  | Error _ when state.toks[i].lexError -> "0", "0"
  | Error error ->
    let code, message = FloatLiteral.diagnostic format error
    err state code i message
    "0", "0"

/// A float token's source minus its `f64`/`f32` suffix, checked against its format.
let private withoutSuffix (suffix : string) (text : string) : string =
  if text.EndsWith suffix then
    text.Substring(0, text.Length - suffix.Length)
  else
    text

let floatParts (state : ParserState) (i : int) (v : float) : string * string =
  literalParts
    state
    i
    "binary64"
    (FloatLiteral.parse (withoutSuffix "f64" (txt state i)) v)

let float32Parts (state : ParserState) (i : int) (v : float32) : string * string =
  literalParts
    state
    i
    "binary32"
    (FloatLiteral.parse32 (withoutSuffix "f32" (txt state i)) v)

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
      | TCharLit value when Option.isNone (String.toEgcChar value) ->
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

// Private type/module names retain their uppercase role after the __ prefix.
// Private lowercase functions still leave .field to postfix parsing.
let private isUpperIdentifier (name : string) =
  let index = if name.StartsWith "__" then 2 else 0
  name.Length > index && System.Char.IsUpper name[index]

// The segments of a qualified name from `k`: `modsRev` holds those so far, last first, and
// `cur` is the one being read. `i` is where the name starts.
let rec private qualifiedSegments
  (state : ParserState)
  (i : int)
  (modsRev : List<WT.Identifier * TokenRange>)
  (cur : WT.Identifier)
  (k : int)
  : (List<WT.Identifier * TokenRange> * WT.Identifier * int) =
  // only step into `.seg` as a module path when the CURRENT segment is
  // uppercase (a module); a lowercase ident's `.field` is postfix access,
  // left for parsePostfix to handle.
  match tok state k, tok state (k + 1) with
  | TDot, TIdent s when isUpperIdentifier cur.name ->
    // previous segment becomes a module, with the dot range
    qualifiedSegments
      state
      i
      ((cur, rng state k) :: modsRev)
      { range = rng state (k + 1); name = s }
      (k + 2)
  // `M.match`: say how to escape it, then read it as the name it was meant to be
  // Only directly after the dot: `M.` then a newline is a name still being typed.
  | TDot, t when
    isKeyword t
    && isUpperIdentifier cur.name
    && (rng state k).end_ = (rng state (k + 1)).start
    ->
    let s = txt state (k + 1)
    // the path as written, backticks included
    let path = [ for j in i..k -> txt state j ] |> String.concat ""
    let hint = keywordHint state (k + 1) path
    err
      state
      DiagnosticCode.expected
      (k + 1)
      $"expected a name after '.', found '{s}'{hint}"
    qualifiedSegments
      state
      i
      ((cur, rng state k) :: modsRev)
      { range = rng state (k + 1); name = s }
      (k + 2)
  | _ -> (List.rev modsRev, cur, k)

// qualified name: ident (. ident)*  → (modules, finalIdent, nextIndex)
let parseQualified
  (state : ParserState)
  (i : int)
  : (List<WT.Identifier * TokenRange> * WT.Identifier * int) =
  let first : WT.Identifier =
    match tok state i with
    | TIdent s -> { range = rng state i; name = s }
    | _ ->
      errExpectedName state i "an identifier"
      { range = rng state i; name = "_" }
  qualifiedSegments state i [] first (i + 1)

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
/// The traits of a `'a: Show + Compare` bound from `k`, after its `:`: each is one bound on
/// `name`, prepended to `boundsRev`.
let rec private typeParamBoundsFrom
  (parseBoundTrait : ParserState -> int -> Option<WT.QualifiedTypeIdentifier> * int)
  (state : ParserState)
  (name : string)
  (colon : TokenRange)
  (boundsRev : List<WT.TypeParamBound>)
  (k : int)
  : List<WT.TypeParamBound> * int =
  let (traitId, k2) = parseBoundTrait state k
  let boundsRev =
    match traitId with
    | Some t ->
      let bound : WT.TypeParamBound =
        { range = span colon t.range; param = name; trait_ = t; symbolColon = colon }
      bound :: boundsRev
    | None -> boundsRev
  let k = if k2 > k then k2 else k + 1
  if tok state k = TPlus then
    typeParamBoundsFrom parseBoundTrait state name colon boundsRev (k + 1)
  else
    (boundsRev, k)

/// The parameters of a `<'a, 'b>` list from `k` up to its `>`: the names and the bounds, each
/// last first, whether a name was still expected, and where the list stopped.
let rec private typeParamsFrom
  (parseBoundTrait : ParserState -> int -> Option<WT.QualifiedTypeIdentifier> * int)
  (state : ParserState)
  (namesRev : List<string * TokenRange>)
  (boundsRev : List<WT.TypeParamBound>)
  (expectingName : bool)
  (k : int)
  : List<string * TokenRange> * List<WT.TypeParamBound> * bool * int =
  // A bound's own type args can end in `>>` (`'a: Convert<Int64>>`), which leaves
  // one `>` pending for this list; see `expectGt`.
  if
    state.pendingGt <> 0
    || tok state k = TGt
    || tok state k = TShr
    || tok state k = TEOF
  then
    (namesRev, boundsRev, expectingName, k)
  else
    match expectingName, tok state k with
    | true, TIdent name ->
      if not ((txt state k).StartsWith "'") then
        err
          state
          DiagnosticCode.expected
          k
          "Declared type parameters must start with an apostrophe, such as 'a"
      let namesRev = (name, rng state k) :: namesRev
      // `'a: Show + Compare`: each trait after the colon is one bound on this param.
      let (boundsRev, k) =
        if tok state (k + 1) = TColon then
          typeParamBoundsFrom
            parseBoundTrait
            state
            name
            (rng state (k + 1))
            boundsRev
            (k + 2)
        else
          (boundsRev, k + 1)
      typeParamsFrom parseBoundTrait state namesRev boundsRev false k
    | false, TComma ->
      typeParamsFrom parseBoundTrait state namesRev boundsRev true (k + 1)
    | false, TIdent _ ->
      errExpected state k "a comma between type parameters"
      typeParamsFrom parseBoundTrait state namesRev boundsRev true k
    | _ ->
      errExpected state k "a type parameter"
      typeParamsFrom parseBoundTrait state namesRev boundsRev expectingName (k + 1)

/// `<'a, 'b: Show + Compare>`: the declared type params, and each `: Trait [+ Trait]`
/// bound as its own entry. `parseBoundTrait` is passed in because the type
/// parser is defined later in the file (mutual recursion via a parameter rather
/// than a `rec` group spanning half the parser).
let parseTypeParamsWith
  (parseBoundTrait : ParserState -> int -> Option<WT.QualifiedTypeIdentifier> * int)
  (state : ParserState)
  (i : int)
  : List<string * TokenRange> * List<WT.TypeParamBound> * int =
  if tok state i <> TLt then
    ([], [], i)
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
    let (namesRev, boundsRev, expectingName, k) =
      typeParamsFrom parseBoundTrait state [] [] true (i + 1)
    if List.isEmpty namesRev then
      errExpected state (i + 1) "at least one type parameter"
    elif expectingName then
      errExpected state k "a type parameter after ','"
    let k2 =
      if state.pendingGt > 0 then
        state.pendingGt <- state.pendingGt - 1
        k
      elif tok state k = TGt then
        k + 1
      elif tok state k = TShr then
        // The second `>` belongs to whatever encloses this declaration; there is
        // nothing that can, so it is a stray token. Report it once, here.
        errExpected state k "'>' to close the type-parameter list"
        k + 1
      else
        (errExpected state k "'>' to close the type-parameter list"
         k)
    (List.rev namesRev, List.rev boundsRev, k2)

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

/// Would a `|` at `i` start a match arm rather than continue an expression?
/// True when it sits on the arm row or at the arm column of ANY enclosing
/// match -- a `|` less indented than the innermost match belongs to an outer
/// one, and must not be eaten as an operator either.
let barStartsArm (state : ParserState) (i : int) : bool =
  match state.matchArms with
  | [] -> false
  | arms ->
    let r = rng state i
    arms
    |> List.exists (fun (armRow, armCol) ->
      r.start.row = armRow || r.start.column = armCol)

/// Run `f` without enclosing match-arm anchors, where `|` can only be an operator.
let withoutMatchArms (state : ParserState) (f : unit -> 'a) : 'a =
  let saved = state.matchArms
  state.matchArms <- []
  try
    f ()
  finally
    state.matchArms <- saved

// fresh scope; statement anchor inherited (managed per-element by `f`)
let withElementScope (state : ParserState) (f : unit -> 'a) : 'a =
  state.scopes.Push { stmtCol = state.scopes.Peek().stmtCol; stmtExact = false }
  try
    withoutMatchArms state f
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
    withoutMatchArms state f
  finally
    s.stmtCol <- savedCol
    s.stmtExact <- savedExact

// Column of the DECLARATION currently being parsed (set per item by
// parseItems), or -1. Recovery only: a decl-start keyword at or left of this
// column can never belong to a construct inside the declaration, so an
// unclosed delimiter above must stop instead of swallowing the next
// declaration (`let broken = [1;` must not eat the `let fine …` below it).
let declBarrier (state : ParserState) (k : int) : bool =
  (match tok state k with
   | TLet
   | TType
   | TTrait
   | TImpl
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
// arg loop accepts it so `f a -1.0` / `add 5 -1` parse correctly
// (matches F#'s high-precedence-application rule).
let isNegLitArg (state : ParserState) (k : int) : bool =
  tok state k = TMinus
  && (match tok state (k + 1) with
      | TBigInt _
      | TInt64 _
      | TInt8 _
      | TInt16 _
      | TInt32 _
      | TInt128 _
      | TFloat _
      | TFloat32 _
      | TDecimal _ -> true
      | _ -> false)
  && (rng state (k + 1)).start.row = (rng state k).end_.row
  && (rng state (k + 1)).start.column = (rng state k).end_.column // no space after `-`
  && (k = 0
      || (rng state k).start.row <> (rng state (k - 1)).end_.row
      || (rng state k).start.column > (rng state (k - 1)).end_.column) // space before `-`

// Report a `;` used in place of `,`, then treat it as a `,` and keep parsing.
let private errCollectionSemicolon (state : ParserState) (i : int) (what : string) =
  errFull
    state
    DiagnosticCode.expected
    i
    $"expected ',' between {what}, found ';'"
    []
    (Some "use ',' instead")

// Report a `;` between statements, then skip it as if it were a newline. Only a
// newline separates statements.
let private errStatementSemicolon (state : ParserState) (i : int) =
  errFull
    state
    DiagnosticCode.expected
    i
    "expected a new line between statements, found ';'"
    []
    (Some "put the next statement on a new line")

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

/// Placeholder names for a hint, one per item: `a, b` for two.
let private placeholders (count : int) : List<string> =
  let names = [ "a"; "b"; "c"; "d"; "e"; "f"; "g"; "h" ] |> List.truncate count
  if count > names.Length then names @ [ "..." ] else names

/// The hint for `Ok 5` / `| Ok x`: write `Ok(a)` rather than `Ok a`.
let private spacedFieldsHint (caseName : string) (count : int) : string =
  let shown = placeholders count
  let commaList = String.concat ", " shown
  let spaceList = String.concat " " shown
  $"write `{caseName}({commaList})` rather than `{caseName} {spaceList}`"

/// `sep item sep item …` from `m`, each item with the range of the `sep` before it. A parse
/// that makes no progress ends the list there.
let rec private separatedFrom
  (sep : Token)
  (parse : ParserState -> int -> 'a * int)
  (state : ParserState)
  (itemsRev : List<TokenRange * 'a>)
  (m : int)
  : List<TokenRange * 'a> * int =
  if tok state m <> sep then
    (List.rev itemsRev, m)
  else
    let (item, m2) = parse state (m + 1)
    let itemsRev = (rng state m, item) :: itemsRev
    if m2 > m then
      separatedFrom sep parse state itemsRev m2
    else
      (List.rev itemsRev, m)

/// Like `separatedFrom`, but a parse that makes no progress skips the separator and goes on,
/// and a `>` left pending by a split `>>` ends the list when `stopAtPendingGt`.
let rec private separatedSkipping
  (sep : Token)
  (stopAtPendingGt : bool)
  (parse : ParserState -> int -> 'a * int)
  (state : ParserState)
  (itemsRev : List<TokenRange * 'a>)
  (m : int)
  : List<TokenRange * 'a> * int =
  if tok state m <> sep || (stopAtPendingGt && state.pendingGt > 0) then
    (List.rev itemsRev, m)
  else
    let (item, m2) = parse state (m + 1)
    separatedSkipping
      sep
      stopAtPendingGt
      parse
      state
      ((rng state m, item) :: itemsRev)
      (if m2 > m then m2 else m + 1)

/// Statements, last first, folded right-nested into one `EStatement` chain.
let private foldStatements (stmtsRev : List<WT.Expr>) : WT.Expr =
  match stmtsRev with
  | [] -> failwith "foldStatements: no statements"
  | last :: earlier ->
    earlier
    |> List.fold
      (fun acc s -> WT.EStatement(span (WT.exprRange s) (WT.exprRange acc), s, acc))
      last

/// The last index in `lo..hi` of the sorted `starts` whose value is at most `off` (binary search).
let rec private lastAtOrBefore
  (starts : ResizeArray<int>)
  (off : int)
  (lo : int)
  (hi : int)
  : int =
  if lo >= hi then
    lo
  else
    let mid = (lo + hi + 1) / 2
    if starts[mid] <= off then
      lastAtOrBefore starts off mid hi
    else
      lastAtOrBefore starts off lo (mid - 1)

/// The `.Name` segments of a module name from `nk`, prepended to `partsRev`.
let rec private moduleNameParts
  (state : ParserState)
  (partsRev : List<string>)
  (nk : int)
  : List<string> * int =
  match tok state nk, tok state (nk + 1) with
  | TDot, TIdent s -> moduleNameParts state (s :: partsRev) (nk + 2)
  | _ -> (partsRev, nk)

let rec parseExpr (state : ParserState) (i : int) : WT.Expr * int =
  System.Runtime.CompilerServices.RuntimeHelpers.EnsureSufficientExecutionStack()
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
    let (pipeExprs, k) = parsePipeSegments state [] j
    let endR = if k > 0 then rng state (k - 1) else rng state j
    (WT.EPipe(span (WT.exprRange expr) endR, expr, pipeExprs), k)

// `|> seg |> seg …` from `k`
and private parsePipeSegments
  (state : ParserState)
  (segmentsRev : List<TokenRange * WT.PipeExpr>)
  (k : int)
  : List<TokenRange * WT.PipeExpr> * int =
  if tok state k <> TPipe then
    (List.rev segmentsRev, k)
  else
    let pipeR = rng state k
    let (rhs, k2) = parseInfix state (k + 1)
    let segmentsRev =
      match toPipeExpr rhs with
      | Some seg -> (pipeR, seg) :: segmentsRev
      | None ->
        (match rhs with
         | WT.EUnwrap _ ->
           errFull
             state
             DiagnosticCode.pipeSegment
             (k + 1)
             "'?' cannot end a pipe segment"
             []
             (Some "wrap the whole pipeline: `(value |> transform)?`")
         | _ ->
           err state DiagnosticCode.pipeSegment (k + 1) "unsupported pipe segment") // → diagnostic
        segmentsRev
    parsePipeSegments state segmentsRev (if k2 > k then k2 else k + 1)

// convert a parsed pipe RHS expression into a structured pipe segment
and toPipeExpr (e : WT.Expr) : WT.PipeExpr option =
  // `x |> f()` is `f(x)`: the empty call's unit (its range is the call's own `()`,
  // ending where the call ends) is not an argument here. `x |> f(())` keeps its unit.
  let pipeArgs (r : TokenRange) (args : List<WT.Expr>) =
    match args with
    | [ WT.EUnit ur ] when ur.end_ = r.end_ -> []
    | _ -> args
  match e with
  // keep the callee's type args — `x |> parse<T>` needs `T` (dropping it left the
  // piped fn call with no type args, so `parse` had no target type: "type 'a")
  | WT.EApply(r, WT.EFnName(_, q), typeArgs, args) ->
    Some(WT.EPipeFnCall(r, q, typeArgs, pipeArgs r args))
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
        pipeArgs r args
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
  let (stmtsRev, k) = parseStatementsAt state col [] i
  (foldStatements stmtsRev, k)

// the statements from `k`, last first (tail-recursive, so a loop)
and private parseStatementsAt
  (state : ParserState)
  (col : int)
  (stmtsRev : List<WT.Expr>)
  (k : int)
  : List<WT.Expr> * int =
  let (stmt, j) = parseExpr state k
  let stmtsRev = stmt :: stmtsRev
  if j = k then
    (stmtsRev, k) // no progress — stop (avoids a spin)
  elif tok state j = TSemicolon then
    errStatementSemicolon state j
    parseStatementsAt state col stmtsRev (j + 1)
  elif hasNextStmt state col j then
    parseStatementsAt state col stmtsRev j
  else
    (stmtsRev, j)

and hasNextStmt (state : ParserState) (col : int) (k : int) : bool =
  tok state k <> TEOF
  && (rng state k).start.column = col
  && tok state k <> TBar
  // `=>` is deliberately lexed as adjacent `=` and `>` tokens. At the end of
  // a package-test body it introduces the expected outcome, rather than a new
  // statement in that body. Outside a test it remains unconsumed and is
  // diagnosed by the enclosing parser.
  && not (
    tok state k = TEquals
    && tok state (k + 1) = TGt
    && (rng state k).end_ = (rng state (k + 1)).start
  )
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
      let (rest, m) = separatedFrom TComma parseExpr state [] k
      let z = zeroWidthAtEnd (WT.exprRange first)
      let endE = if List.isEmpty rest then second else snd (Seq.last rest)
      (WT.ETuple(
        span (WT.exprRange first) (WT.exprRange endE),
        first,
        comma,
        second,
        rest,
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
  // arms align on the first `|`'s column; a `|` LESS indented than that belongs
  // to an enclosing match (so a nested match doesn't swallow the outer's arms).
  let armCol =
    if tok state afterWith = TBar then (rng state afterWith).start.column else 0
  let armRow =
    if tok state afterWith = TBar then (rng state afterWith).start.row else -1
  // While the arm BODIES are parsed, a `|` on this match's arm row or at its
  // arm column is an arm separator, not the bitwise-or operator. The subject
  // expression above is parsed outside the anchor, where `|` is still an
  // operator (`match a | b with …`).
  let savedArms = state.matchArms
  state.matchArms <- (armRow, armCol) :: savedArms
  let (casesRev, k) =
    try
      parseMatchArms state armRow armCol [] afterWith
    finally
      state.matchArms <- savedArms
  let endR =
    match casesRev with
    | last :: _ -> WT.exprRange last.rhs
    | [] ->
      errExpected state afterWith "at least one match case starting with '|'"
      kwWith
  (WT.EMatch(span kwMatch endR, expr, List.rev casesRev, kwMatch, kwWith), k)

// The arms of a match from `k`, last first: each starts with a `|` on the match's
// arm row or at its arm column.
and private parseMatchArms
  (state : ParserState)
  (armRow : int)
  (armCol : int)
  (casesRev : List<WT.MatchCase>)
  (k : int)
  : List<WT.MatchCase> * int =
  if
    tok state k = TBar
    && ((rng state k).start.row = armRow || (rng state k).start.column = armCol)
  then
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
    let case : WT.MatchCase =
      { barRange = barR
        pat = pat
        arrowRange = arrowR
        whenCondition = whenCond
        rhs = rhs }
    parseMatchArms
      state
      armRow
      armCol
      (case :: casesRev)
      (if k5 > k then k5 else k + 1)
  else
    (casesRev, k)

// or-level: `p1 | p2 | …` (stops at `->` / `when`)
// top level: a bare tuple `a, b` (comma-separated, no parens); else an or-pattern
// A full match-arm pattern. Precedence, matching F#: `|` (or) is LOOSEST, then
// `,` (tuple). So `1, 2 | 3, 4` is `(1,2) | (3,4)` — an or of
// two tuples, NOT a 3-tuple with an or in the middle. Hence `|` is the OUTER
// level here, wrapping tuples (`parsePatternTuple`). `as` is looser still, so
// `A | B as x` names the whole or-pattern.
and parseMatchPattern (state : ParserState) (i : int) : WT.MatchPattern * int =
  let (p, j) = parsePatternAs state (parseMatchPatternOr state i)
  // `as` is loosest, so a ',' or '|' after it is a mistake. Report it, then read
  // on as if the named part had been parenthesized, so nothing cascades.
  let rec more (parts : List<WT.MatchPattern>) (k : int) =
    if tok state k = tok state j then
      let (q, k2) = parsePatternAs state (parsePatternTuple state (k + 1))
      if k2 > k + 1 then more (q :: parts) k2 else (List.rev parts, k2)
    else
      (List.rev parts, k)
  match p with
  // `(a, b as t)` reads as naming `b`, but `as` is loosest so it would name the
  // tuple. A bare tuple (no parentheses of its own) has zero-width paren ranges.
  | WT.MPAs(_, WT.MPTuple(_, _, _, _, _, openP, closeP), _, _) when openP = closeP ->
    errFull
      state
      DiagnosticCode.unexpected
      (j - 2)
      "'as' after a tuple's last element is ambiguous"
      []
      (Some
        "write `((a, b) as t)` to name the tuple, or `(a, (b as t))` to name one element")
    (p, j)
  | WT.MPAs _ when tok state j = TComma ->
    errFull
      state
      DiagnosticCode.unexpected
      j
      "'as' names the whole pattern before it, so a ',' cannot follow it here"
      []
      (Some "to name one element of a tuple, wrap it in parentheses: `((p as x), y)`")
    match more [] j with
    | (second :: rest, k) ->
      let z = zeroWidthAtEnd (WT.mpRange p)
      let last = Seq.last (second :: rest)
      (WT.MPTuple(
        span (WT.mpRange p) (WT.mpRange last),
        p,
        rng state j,
        second,
        rest |> List.map (fun r -> (rng state j, r)),
        z,
        z
       ),
       k)
    | ([], k) -> (p, k)
  | WT.MPAs _ when tok state j = TBar ->
    errFull
      state
      DiagnosticCode.unexpected
      j
      "'as' names the whole or-pattern before it, so a '|' cannot follow it"
      []
      (Some "to name one alternative, wrap it in parentheses: `(p as x) | q`")
    match more [] j with
    | ([], k) -> (p, k)
    | (alts, k) ->
      (WT.MPOr(span (WT.mpRange p) (WT.mpRange (Seq.last alts)), p :: alts), k)
  | _ -> (p, j)

// `p as name`: matches what `p` matches and also binds the whole value to `name`.
// `as` is not a keyword, only an identifier straight after a complete pattern, so
// a variable can still be called `as`.
and parsePatternAs
  (state : ParserState)
  ((p, j) : WT.MatchPattern * int)
  : WT.MatchPattern * int =
  match tok state j with
  | TIdent "as" ->
    let asR = rng state j
    match tok state (j + 1) with
    | TIdent name when
      name.Length > 0 && (System.Char.IsLower name[0] || name[0] = '_')
      ->
      let nameR = rng state (j + 1)
      parsePatternAs
        state
        (WT.MPAs(span (WT.mpRange p) nameR, p, asR, (nameR, name)), j + 2)
    | _ ->
      errExpectedName state (j + 1) "a lowercase name after 'as'"
      (p, j + 1)
  | _ -> (p, j)

and parseMatchPatternOr (state : ParserState) (i : int) : WT.MatchPattern * int =
  let (first, j) = parsePatternTuple state i
  if tok state j <> TBar then
    (first, j)
  else
    let (alts, k) = separatedSkipping TBar false parsePatternTuple state [] j
    let pats = first :: List.map snd alts
    (WT.MPOr(span (WT.mpRange first) (WT.mpRange (Seq.last pats)), pats), k)

// tuple level: `p1, p2, …` (bare — no parens). Elements are base patterns; `|`
// binds looser (handled above) so it can't appear as a bare tuple element.
and parsePatternTuple (state : ParserState) (i : int) : WT.MatchPattern * int =
  let (first, j) = parsePatternBase state i
  if tok state j <> TComma then
    (first, j)
  else
    let comma = rng state j
    let (second, k) = parsePatternBase state (j + 1)
    let (rest, m) = separatedFrom TComma parsePatternBase state [] k
    let z = zeroWidthAtEnd (WT.mpRange first) // bare tuple: no parens
    let endP = if List.isEmpty rest then second else snd (Seq.last rest)
    (WT.MPTuple(
      span (WT.mpRange first) (WT.mpRange endP),
      first,
      comma,
      second,
      rest,
      z,
      z
     ),
     m)

// or of base patterns, NO tuple — used for enum-ctor fields and list elements,
// where a bare `,` separates FIELDS (`Case(a, b)` = two fields), not tuple
// elements. A trailing `as name` names the whole field.
and parsePatternOr (state : ParserState) (i : int) : WT.MatchPattern * int =
  parsePatternAs state (parsePatternOrNoAs state i)

and parsePatternOrNoAs (state : ParserState) (i : int) : WT.MatchPattern * int =
  let (first, j) = parsePatternBase state i
  if tok state j = TBar then
    let (alts, k) = separatedSkipping TBar false parsePatternBase state [] j
    let pats = first :: List.map snd alts
    (WT.MPOr(span (WT.mpRange first) (WT.mpRange (Seq.last pats)), pats), k)
  else
    (first, j)

and parsePatternBase (state : ParserState) (i : int) : WT.MatchPattern * int =
  System.Runtime.CompilerServices.RuntimeHelpers.EnsureSufficientExecutionStack()
  if tooDeep state i || outOfFuel state i then
    (WT.MPError(rng state i), state.tokenCount - 1)
  else
    state.depth <- state.depth + 1
    let (p, j) = parsePatternBaseInner state i
    state.depth <- state.depth - 1
    // `head :: tail`: the lexer has already reported the `::`, so skip it and
    // the tail, and the rest of the arm parses as if `[head, ...tail]` were written.
    if j < state.tokenCount && state.toks[j].lexError && txt state j = "::" then
      let (_, k) = parsePatternBase state (j + 1)
      (p, k)
    else
      (p, j)

and parsePatternBaseInner (state : ParserState) (i : int) : WT.MatchPattern * int =
  checkBareMinMagnitude state i // `128s8` etc. — only valid negated
  match tok state i with
  | TUnderscore -> (WT.MPVariable(rng state i, "_"), i + 1)
  | TBigInt v -> (WT.MPBigInt(rng state i, (rng state i, v)), i + 1)
  | TInt64 v ->
    let (digits, suffix) = splitIntSuffix state i
    (WT.MPInt64(rng state i, (digits, v), suffix), i + 1)
  | TInt32 v ->
    let (digits, suffix) = splitIntSuffix state i
    (WT.MPInt32(rng state i, (digits, v), suffix), i + 1)
  | TInt8 v ->
    let (digits, suffix) = splitIntSuffix state i
    (WT.MPInt8(rng state i, (digits, v), suffix), i + 1)
  | TUInt8 v ->
    let (digits, suffix) = splitIntSuffix state i
    (WT.MPUInt8(rng state i, (digits, v), suffix), i + 1)
  | TInt16 v ->
    let (digits, suffix) = splitIntSuffix state i
    (WT.MPInt16(rng state i, (digits, v), suffix), i + 1)
  | TUInt16 v ->
    let (digits, suffix) = splitIntSuffix state i
    (WT.MPUInt16(rng state i, (digits, v), suffix), i + 1)
  | TUInt32 v ->
    let (digits, suffix) = splitIntSuffix state i
    (WT.MPUInt32(rng state i, (digits, v), suffix), i + 1)
  | TUInt64 v ->
    let (digits, suffix) = splitIntSuffix state i
    (WT.MPUInt64(rng state i, (digits, v), suffix), i + 1)
  | TInt128 v ->
    let (digits, suffix) = splitIntSuffix state i
    (WT.MPInt128(rng state i, (digits, v), suffix), i + 1)
  | TUInt128 v ->
    let (digits, suffix) = splitIntSuffix state i
    (WT.MPUInt128(rng state i, (digits, v), suffix), i + 1)
  // unary minus on a numeric literal pattern: `-5L`, `-1y`, `-2.0` (unsigned
  // types can't be negative, so only the signed literals + float are handled).
  | TMinus ->
    let r1 = rng state (i + 1)
    let r = span (rng state i) r1
    match tok state (i + 1) with
    | TBigInt v -> (WT.MPBigInt(r, (r, -v)), i + 2)
    | TInt64 v ->
      let (digits, suffix) = splitIntSuffix state (i + 1)
      (WT.MPInt64(r, (span (rng state i) digits, -v), suffix), i + 2)
    | TInt32 v ->
      let (digits, suffix) = splitIntSuffix state (i + 1)
      (WT.MPInt32(r, (span (rng state i) digits, -v), suffix), i + 2)
    | TInt8 v ->
      let (digits, suffix) = splitIntSuffix state (i + 1)
      (WT.MPInt8(r, (span (rng state i) digits, -v), suffix), i + 2)
    | TInt16 v ->
      let (digits, suffix) = splitIntSuffix state (i + 1)
      (WT.MPInt16(r, (span (rng state i) digits, -v), suffix), i + 2)
    | TInt128 v ->
      let (digits, suffix) = splitIntSuffix state (i + 1)
      (WT.MPInt128(r, (span (rng state i) digits, -v), suffix), i + 2)
    | TFloat v ->
      let (whole, frac) = floatParts state (i + 1) v
      (WT.MPFloat(r, true, whole, frac), i + 2)
    | TFloat32 v ->
      let (whole, frac) = float32Parts state (i + 1) v
      (WT.MPFloat32(r, true, whole, frac), i + 2)
    | TDecimal v -> (WT.MPDecimal(r, -v), i + 2)
    | _ ->
      errExpected state i "a pattern"
      (WT.MPError(rng state i), i + 1)
  | TFloat v ->
    let r = rng state i
    let (whole, frac) = floatParts state i v
    (WT.MPFloat(r, v < 0.0, whole, frac), i + 1)
  | TFloat32 v ->
    let r = rng state i
    let (whole, frac) = float32Parts state i v
    (WT.MPFloat32(r, v < 0.0f, whole, frac), i + 1)
  | TDecimal v -> (WT.MPDecimal(rng state i, v), i + 1)
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
      // `(a, b as t)`, already reported: these parens are the tuple's, so the
      // pattern around it does not report it again
      | WT.MPAs(r, WT.MPTuple(tr, first, comma, second, rest, o, c), asR, name) when
        o = c
        ->
        (WT.MPAs(
          r,
          WT.MPTuple(tr, first, comma, second, rest, openP, closeP),
          asR,
          name
         ),
         p2)
      | _ -> (inner, p2)
  | TLBracket ->
    let openB = rng state i
    let (elemsRev, rest, k) = parseListPatternElems state [] None (-1) (i + 1)
    let (closeB, p2) =
      if tok state k = TRBracket then
        (rng state k, k + 1)
      else
        errUnclosed state k "]" "[" openB
        (zeroWidthAtEnd (rng state k), k)
    match rest with
    | None -> (WT.MPList(span openB closeB, List.rev elemsRev, openB, closeB), p2)
    | Some(dotsR, tailPat) ->
      // `[a, b, ...t]` is `a :: b :: t`, so it lowers through the cons node
      let heads =
        match elemsRev with
        | _ :: earlier -> earlier |> List.rev |> List.map fst
        | [] -> []
      if List.isEmpty heads then
        err
          state
          DiagnosticCode.unexpected
          i
          "`[...t]` matches the same lists as `t`; write `t`"
      let pat =
        List.foldBack
          (fun head tail ->
            WT.MPListCons(
              span (WT.mpRange head) (WT.mpRange tail),
              head,
              tail,
              dotsR
            ))
          heads
          tailPat
      match pat with
      | WT.MPListCons(_, head, tail, consR) ->
        (WT.MPListCons(span openB closeB, head, tail, consR), p2)
      | _ -> (pat, p2)
  // `{ name = p, age }`: a record, by the fields it names. The fields it leaves
  // out are ignored, and a field with no `= p` binds a variable of its own name.
  // A field takes the same patterns as a constructor field, so a tuple needs its
  // own parentheses: `{ pos = (x, y) }`.
  | TLBrace ->
    let openB = rng state i
    let (fieldsRev, k) = parseRecordPatternFields state [] (i + 1)
    let (closeB, p2) =
      if tok state k = TRBrace then
        (rng state k, k + 1)
      else
        errUnclosed state k "}" "{" openB
        (zeroWidthAtEnd (rng state k), k)
    if List.isEmpty fieldsRev && tok state (i + 1) = TRBrace then
      errFull
        state
        DiagnosticCode.pattern
        i
        "a record pattern names at least one field"
        []
        (Some "to match any value, use a variable or `_`")
    (WT.MPRecord(span openB closeB, List.rev fieldsRev, openB, closeB), p2)
  | TIdent name when name.Length > 0 && System.Char.IsUpper name[0] ->
    // enum pattern: `[Mod.Type.]Case(fieldPats…)`; the last segment is the case.
    // A qualified case (`Result.Ok(x)`, `Stdlib.Result.Result.Ok(x)`) matches the
    // same values as the bare case, as building one accepts `Result.Ok(5)`; the
    // path before the case is the type, resolved when the match is lowered.
    let (mods, final, j) = parseQualified state i
    let typePath = mods |> List.map (fun (m, _) -> m.name)
    // `Case(p1, p2, …)` is a parenthesized arg list: commas separate FIELDS, so
    // `Pair(a, b)` is two fields — NOT one tuple `Pair((a, b))`. This holds
    // whether or not there's a space before the `(`, as for a call.
    if tok state j = TLParen then
      let openParen = rng state j
      let (fieldPatsRev, k) = parseCtorPatternFields state [] (j + 1)
      let k3 =
        if tok state k = TRParen then
          k + 1
        else
          (errUnclosed state k ")" "(" openParen
           k)
      let fieldPats =
        match fieldPatsRev with
        // `Case()` is one unit field (`Case` applied to unit)
        | [] -> [ WT.MPUnit(span openParen (rng state (max j (k3 - 1)))) ]
        | _ -> List.rev fieldPatsRev
      (WT.MPEnum(
        span (rng state i) (rng state (max j (k3 - 1))),
        typePath,
        (final.range, final.name),
        fieldPats
       ),
       k3)
    else
      // `| Ok x` is not Dark. The fields are still parsed, so recovery sees the
      // shape the author meant; the diagnostic is what makes it an error.
      let (fieldPatsRev, k) = parseSpacedPatternFields state i [] j
      let endR =
        match fieldPatsRev with
        | last :: _ ->
          let endR = WT.mpRange last
          state.diagnostics.Add
            { code = DiagnosticCode.pattern
              severity = DiagError
              range = span (rng state i) endR
              message =
                "a constructor pattern's fields go in parentheses, separated by commas"
              related = []
              hint = Some(spacedFieldsHint final.name (List.length fieldPatsRev)) }
          endR
        | [] -> final.range
      (WT.MPEnum(
        span (rng state i) endR,
        typePath,
        (final.range, final.name),
        List.rev fieldPatsRev
       ),
       k)
  | TIdent name -> (WT.MPVariable(rng state i, name), i + 1)
  // a rest element belongs to a list pattern, handled above
  | TDotDotDot ->
    err
      state
      DiagnosticCode.unexpected
      i
      "'...' only goes last inside a list pattern, as in `[head, ...tail]`"
    (WT.MPError(rng state i), i + 1)
  | _ when keywordHint state i "" <> "" ->
    errExpectedName state i "a pattern"
    (WT.MPVariable(rng state i, txt state i), i + 1)
  | _ ->
    errExpectedName state i "a pattern"
    // recovery: an explicit error-hole node; leave closing/separating/decl-start
    // tokens for the enclosing construct
    let holeR = { start = (rng state i).start; end_ = (rng state i).start }
    (WT.MPError holeR,
     (if i < state.tokenCount && not (isRecoveryBarrier (tok state i)) then
        i + 1
      else
        i))

// The elements of a list pattern from `k`, last first, and its `...` rest element if it has
// one. Only the last element may be a rest element; `restAt` is where an earlier one is, until
// an element after it has been reported.
and private parseListPatternElems
  (state : ParserState)
  (elemsRev : List<WT.MatchPattern * Option<TokenRange>>)
  (rest : Option<TokenRange * WT.MatchPattern>)
  (restAt : int)
  (k : int)
  : List<WT.MatchPattern * Option<TokenRange>> *
    Option<TokenRange * WT.MatchPattern> *
    int
  =
  if tok state k = TRBracket || tok state k = TEOF then
    (elemsRev, rest, k)
  else
    let isRest = tok state k = TDotDotDot
    if restAt >= 0 then
      err
        state
        DiagnosticCode.unexpected
        restAt
        "'...' must be the last element of a list pattern"
    // a bare `...` ignores the rest, the same as `..._`
    let bareRest =
      isRest && (tok state (k + 1) = TRBracket || tok state (k + 1) = TComma)
    let (p, k2) =
      if bareRest then
        (WT.MPVariable(rng state k, "_"), k + 1)
      elif isRest then
        parsePatternBase state (k + 1)
      else
        // an element is a full field pattern: `[1 | 2]`, `[Some(_) as o]`
        parsePatternOr state k
    let (rest, restAt) =
      if isRest then
        match p with
        | WT.MPVariable _ -> ()
        | _ ->
          err
            state
            DiagnosticCode.unexpected
            (k + 1)
            "'...' takes a name for the rest of the list, or nothing to ignore it"
        (Some(rng state k, p), (if Option.isNone rest then k else -1))
      else
        (rest, -1)
    if k2 = k then
      err
        state
        DiagnosticCode.unexpected
        k
        $"unexpected {foundDesc state k} in list pattern"
      (elemsRev, rest, k)
    elif tok state k2 = TComma then
      parseListPatternElems
        state
        ((p, Some(rng state k2)) :: elemsRev)
        rest
        restAt
        (k2 + 1)
    elif tok state k2 = TSemicolon then
      errCollectionSemicolon state k2 "list-pattern elements"
      parseListPatternElems
        state
        ((p, Some(rng state k2)) :: elemsRev)
        rest
        restAt
        (k2 + 1)
    else
      if tok state k2 <> TRBracket then
        requireElementSeparator
          state
          (WT.mpRange p)
          k2
          "a comma or newline between list-pattern elements"
      parseListPatternElems state ((p, None) :: elemsRev) rest restAt k2

// The fields of a record pattern from `k`, last first.
and private parseRecordPatternFields
  (state : ParserState)
  (fieldsRev : List<(TokenRange * string) * Option<TokenRange * WT.MatchPattern>>)
  (k : int)
  : List<(TokenRange * string) * Option<TokenRange * WT.MatchPattern>> * int =
  if tok state k = TRBrace || tok state k = TEOF then
    (fieldsRev, k)
  else
    match tok state k with
    | TIdent fname ->
      let fnameR = rng state k
      if fieldsRev |> List.exists (fun ((_, n), _) -> n = fname) then
        err
          state
          DiagnosticCode.pattern
          k
          $"the field '{fname}' appears twice in this record pattern"
      let (field, k2) =
        if tok state (k + 1) = TEquals then
          let (p, k2) = parsePatternOr state (k + 2)
          (((fnameR, fname), Some(rng state (k + 1), p)), k2)
        else
          if not (System.Char.IsLower fname[0]) then
            errFull
              state
              DiagnosticCode.pattern
              k
              $"a field with no '= pattern' binds a variable named after it, and '{fname}' cannot name a variable"
              []
              (Some $"write `{fname} = p` to match the field against a pattern")
          (((fnameR, fname), None), k + 1)
      let fieldsRev = field :: fieldsRev
      let lastR =
        match field with
        | (_, Some(_, p)) -> WT.mpRange p
        | ((r, _), None) -> r
      if tok state k2 = TComma then
        parseRecordPatternFields state fieldsRev (k2 + 1)
      elif tok state k2 = TSemicolon then
        errCollectionSemicolon state k2 "record-pattern fields"
        parseRecordPatternFields state fieldsRev (k2 + 1)
      elif k2 > k then
        if tok state k2 <> TRBrace then
          requireElementSeparator
            state
            lastR
            k2
            "a comma or newline between record-pattern fields"
        parseRecordPatternFields state fieldsRev k2
      else
        (fieldsRev, k)
    | TDotDotDot ->
      errFull
        state
        DiagnosticCode.unexpected
        k
        "a record pattern needs no '...': the fields it leaves out are ignored"
        []
        (Some "remove the '...'")
      parseRecordPatternFields
        state
        fieldsRev
        (if tok state (k + 1) = TComma then k + 2 else k + 1)
    | _ ->
      errExpectedName state k "a record field, as in `name` or `name = pattern`"
      (fieldsRev, k)

// The parenthesized fields of a constructor pattern from `k`, last first.
and private parseCtorPatternFields
  (state : ParserState)
  (fieldPatsRev : List<WT.MatchPattern>)
  (k : int)
  : List<WT.MatchPattern> * int =
  if tok state k = TRParen || tok state k = TEOF then
    (fieldPatsRev, k)
  else
    let (p, k2) = parsePatternOr state k
    let fieldPatsRev = p :: fieldPatsRev
    if tok state k2 = TComma then
      parseCtorPatternFields state fieldPatsRev (k2 + 1)
    elif k2 > k then
      if tok state k2 <> TRParen then
        requireElementSeparator
          state
          (WT.mpRange p)
          k2
          "a comma or newline between constructor-pattern fields"
      parseCtorPatternFields state fieldPatsRev k2
    else
      (fieldPatsRev, k)

// Space-separated fields after the constructor at `i` (`| Ok x`, an error), from `k`, last first.
and private parseSpacedPatternFields
  (state : ParserState)
  (i : int)
  (fieldPatsRev : List<WT.MatchPattern>)
  (k : int)
  : List<WT.MatchPattern> * int =
  // `None as n` names the case; it is not a spaced field
  if
    canStartPattern (tok state k)
    && tok state k <> TIdent "as"
    && offsideContinues state i k
  then
    let (p, k2) = parsePatternBase state k
    parseSpacedPatternFields
      state
      i
      (p :: fieldPatsRev)
      (if k2 > k then k2 else k + 1)
  else
    (fieldPatsRev, k)

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
  System.Runtime.CompilerServices.RuntimeHelpers.EnsureSufficientExecutionStack()
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
      let (rest, m) = separatedSkipping TComma false parseLetPattern state [] k
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
  | _ when keywordHint state i "" <> "" ->
    errExpectedName state i "a pattern"
    (WT.LPVariable(rng state i, txt state i), i + 1)
  | _ ->
    errExpectedName state i "a pattern"
    // recovery: keep a benign binder (LetPattern has no error case yet);
    // leave closing/separating/decl-start tokens for the enclosing construct
    (WT.LPVariable(rng state i, "_"),
     (if i < state.tokenCount && not (isRecoveryBarrier (tok state i)) then
        i + 1
      else
        i))

and parseLet (state : ParserState) (i : int) : WT.Expr * int =
  let keywordLet = rng state i
  let (pat, j) = parseLetPattern state (skipRec state (i + 1))
  match pat with
  | WT.LPVariable _ when tok state j = TLParen ->
    // nested function definition: `let f(x: T, y: U)[: R] = body`. Bind a lambda
    // to the name (params lowered to untyped lambda patterns, types discarded)
    let (ps, k) = parseParams state j
    let pats =
      ps
      |> List.map (fun p ->
        match p with
        | WT.FPUnit r -> WT.LPUnit r
        | WT.FPNormal(_, name, _, _, _, _, _) ->
          WT.LPVariable(name.range, name.name))
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
      WT.ELambda(span (rng state j) (WT.exprRange fnBody), pats, fnBody, z, z)
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
//   1 `||`   2 `&&`   3 `== != < > <= >=`   4 `|`   5 `^`   6 `&`
//   7 `<< >>`   8 `@` (right)   9 `+ -`   10 `* / %`   11 `**` (right)
// The bitwise levels follow Python's order rather than C's: they bind TIGHTER
// than the comparisons, so `a & b == c` is `(a & b) == c` and not C's
// `a & (b == c)`. Every pre-existing operator keeps its relative position.
// `@` desugars to `Stdlib.List.concat` (there is no WT infix for it); `**` is
// exponentiation and nests right: `2 ** 3 ** 2 = 2 ** (3 ** 2)`.
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
  | TBar -> Some(4, false)
  | TBitXor -> Some(5, false)
  | TBitAnd -> Some(6, false)
  | TShl
  | TShr -> Some(7, false)
  | TPlus
  | TMinus -> Some(8, false)
  | TStar
  | TSlash
  | TPercent -> Some(9, false)
  | TStarStar -> Some(10, true)
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
  System.Runtime.CompilerServices.RuntimeHelpers.EnsureSufficientExecutionStack()
  parseInfixFrom state minBp left0 j0

// The operators from `j` binding at least `minBp`, folded onto `left`.
and private parseInfixFrom
  (state : ParserState)
  (minBp : int)
  (left : WT.Expr)
  (j : int)
  : WT.Expr * int =
  // The operator must belong to THIS statement: same row as the left
  // operand's end, or inside parens, or an indented continuation. Otherwise
  // a following statement that starts with a prefix operator (`1\n-8 …`)
  // would be wrongly glued on as `1 - 8 …`. On a new line, a pure infix
  // operator at the statement column continues (`x\n* y`: `*` cannot start
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
  // A `|` belonging to an enclosing match is that match's arm separator, not
  // bitwise-or; leave it for `parseMatch`.
  | Some _ when tok state j = TBar && barStartsArm state j -> (left, j)
  | Some(bp, rightAssoc) when bp >= minBp && opContinues ->
    let opTok = tok state j
    let opRange = rng state j
    let (rhs0, k0) = parseApp state (j + 1)
    // climb: the RHS folds in everything binding tighter (or equally
    // tight, for a right-assoc op) before this level continues.
    let (right, k) = parseInfixRhs state (if rightAssoc then bp else bp + 1) rhs0 k0
    let range = span (WT.exprRange left) (WT.exprRange right)
    parseInfixFrom
      state
      minBp
      (WT.EInfix(range, (opRange, (infixOf opTok).Value), left, right))
      k
  | _ -> (left, j)

// A parenthesized list, `(e1, e2, …)` with `i` at the `(`: a call's arguments or
// a constructor's fields. Commas or newlines separate the items (so `f(a, b)` is
// two, and one tuple needs double parens). Items are anchored exactly like a paren
// body (stmtExact: the `)` is the real delimiter). Returns the items and the
// index after the `)`. An empty list gives the one unit item: `f()`, `Ctor()`.
and parseParenList (state : ParserState) (i : int) : List<WT.Expr> * int =
  let openParen = rng state i
  withStmtColExact (state) (rng state (i + 1)).start.column (fun () ->
    let (itemsRev, m) = parseParenItems state [] (i + 1)
    let m =
      if tok state m = TRParen then
        m + 1
      else
        (errUnclosed state m ")" "(" openParen
         m)
    match itemsRev with
    // `Ctor()` is `Ctor` applied to unit — one unit field, not zero
    | [] -> ([ WT.EUnit(span openParen (rng state (m - 1))) ], m)
    | _ -> (List.rev itemsRev, m))

// The items of a parenthesized list from `m`, last first.
and private parseParenItems
  (state : ParserState)
  (itemsRev : List<WT.Expr>)
  (m : int)
  : List<WT.Expr> * int =
  if tok state m = TRParen || tok state m = TEOF || declBarrier state m then
    (itemsRev, m)
  else
    let (a, m2) = parseExpr state m
    if tok state m2 = TComma then
      parseParenItems state (a :: itemsRev) (m2 + 1)
    elif m2 > m then
      if tok state m2 <> TRParen then
        requireElementSeparator
          state
          (WT.exprRange a)
          m2
          "a comma or newline between arguments"
      parseParenItems state (a :: itemsRev) m2
    else
      (a :: itemsRev, m)

// A prefix operator: `op operand` → `<modules>.<name> operand`, with the operand
// parsed as a whole APPLICATION so `op f x` is `op (f x)`. Shared by `!`, `~`
// and the non-literal case of unary `-`.
and parsePrefixCall
  (state : ParserState)
  (i : int)
  (modules : List<string>)
  (fnName : string)
  : WT.Expr * int =
  let (operand, k) = parseApp state (i + 1)
  let opR = rng state i
  let callee : WT.QualifiedFnIdentifier =
    { range = opR
      modules = modules |> List.map (fun m -> ({ range = opR; name = m }, opR))
      fn = { range = opR; name = fnName } }
  (WT.EApply(
    span opR (WT.exprRange operand),
    WT.EFnName(opR, callee),
    [],
    [ operand ]
   ),
   k)

/// What can be called: a name, a field (`c.onKey(s, k)`), a call's result
/// (`f(a)(b)`), an unwrap, or a lambda (an operator section `(+)` is one).
and isCallee (e : WT.Expr) : bool =
  match e with
  | WT.EVariable _
  | WT.EFnName _
  | WT.EApply _
  | WT.EUnwrap _
  | WT.ERecordFieldAccess _
  | WT.ELambda _ -> true
  | _ -> false

// An expression followed by arguments separated by spaces, `f a b`, is not a call:
// calls are `f(a, b)` (see `parseCalls`). The arguments are still parsed into a
// call, so recovery and tooling see the shape the author meant; the diagnostic is
// what makes it an error.
and parseApp (state : ParserState) (i : int) : WT.Expr * int =
  let (callee, j) = parseAtom state i
  // A constructor's parenthesized fields were taken by the enum branch of
  // parsePrimary, so a nullary EEnum followed by atoms is `Ok 5`: an error below,
  // with the fields still folded in so tooling sees the shape the author meant.
  let acceptsArgs =
    match callee with
    | WT.EEnum(_, _, _, [], _) -> true
    | _ -> isCallee callee
  if acceptsArgs && startsSpacedArg state i j then
    let (argsRev, k) = parseSpacedArgs state i [] j
    parseSpacedApp state callee argsRev k
  else
    (callee, j)

// Does the token at `k` start a space-separated argument of the application at `i`?
and private startsSpacedArg (state : ParserState) (i : int) (k : int) : bool =
  (canStartAtom (tok state k) || isNegLitArg state k)
  && offsideContinues state i k

// The space-separated arguments of the application at `i` from `k`, last first.
and private parseSpacedArgs
  (state : ParserState)
  (i : int)
  (argsRev : List<WT.Expr>)
  (k : int)
  : List<WT.Expr> * int =
  if startsSpacedArg state i k then
    let (a, k2) = parseAtom state k
    parseSpacedArgs state i (a :: argsRev) k2
  else
    (argsRev, k)

// `callee` applied to space-separated arguments (`argsRev`, last first, never empty): an
// error, built into the call it was meant to be.
and private parseSpacedApp
  (state : ParserState)
  (callee : WT.Expr)
  (argsRev : List<WT.Expr>)
  (k : int)
  : WT.Expr * int =
  let args = List.rev argsRev
  let argCount = List.length args
  let endR = WT.exprRange (Seq.head argsRev)
  match callee with
  // `Ok 5` is not Dark: a constructor's fields go in parentheses, like a call's
  // arguments.
  | WT.EEnum(er, tname, (cr, cn), [], dot) ->
    state.diagnostics.Add
      { code = DiagnosticCode.unexpected
        severity = DiagError
        range = span er endR
        message = "a constructor's fields go in parentheses, separated by commas"
        related = []
        hint = Some(spacedFieldsHint cn argCount) }
    (WT.EEnum(span er endR, tname, (cr, cn), args, dot), k)
  | _ ->
    let calleeText =
      match callee with
      | WT.EVariable(_, name) -> name
      | WT.EFnName(_, q) -> q.fn.name
      | _ -> "f"
    state.diagnostics.Add
      { code = DiagnosticCode.unexpected
        severity = DiagError
        range = span (WT.exprRange callee) endR
        message = "arguments go in parentheses, separated by commas"
        related = []
        hint =
          // as many placeholder names as there were arguments: `f(a)` for `f a`
          let names =
            [ "a"; "b"; "c"; "d"; "e"; "f"; "g"; "h" ] |> List.truncate argCount
          let shown = if argCount > names.Length then names @ [ "..." ] else names
          let commaList = String.concat ", " shown
          let spaceList = String.concat " " shown
          Some
            $"write `{calleeText}({commaList})` rather than `{calleeText} {spaceList}`" }
    match callee with
    | WT.EApply(cr, fn, typeArgs, []) ->
      (WT.EApply(span cr endR, fn, typeArgs, args), k)
    | _ ->
      let lhs =
        match callee with
        | WT.EVariable(r, name) ->
          WT.EFnName(r, { range = r; modules = []; fn = { range = r; name = name } })
        | other -> other
      (WT.EApply(span (WT.exprRange lhs) endR, lhs, [], args), k)

/// Calls: `callee(a, b)`, as many times as they follow (`f(a)(b)`), each followed by
/// any postfix (`f(x).field`, `f(x)?`). `f()` gives the unit argument, so it calls
/// a fn declared `let f(): T`. A space before the `(` is allowed. Constructors are
/// not calls: their fields are parsed with the constructor (`parsePrimary`, `parseApp`).
and parseCalls
  (state : ParserState)
  (headIdx : int)
  (e : WT.Expr)
  (j : int)
  : WT.Expr * int =
  let glued =
    j > 0
    && (rng state j).start.row = (rng state (j - 1)).end_.row
    && (rng state j).start.column = (rng state (j - 1)).end_.column
  if
    isCallee e
    && tok state j = TLParen
    && (glued || offsideContinues state headIdx j)
  then
    let (args, k) = parseParenList state j
    let closeR = rng state (k - 1)
    let called =
      match e with
      // a callee carrying explicit type args (`parse<T>`) takes the args itself
      | WT.EApply(cr, fn, typeArgs, []) ->
        WT.EApply(span cr closeR, fn, typeArgs, args)
      | WT.EVariable(r, name) ->
        WT.EApply(
          span r closeR,
          WT.EFnName(r, { range = r; modules = []; fn = { range = r; name = name } }),
          [],
          args
        )
      | other -> WT.EApply(span (WT.exprRange other) closeR, other, [], args)
    let (after, k2) = parsePostfix state called k
    parseCalls state headIdx after k2
  else
    (e, j)

and parseAtom (state : ParserState) (i : int) : WT.Expr * int =
  System.Runtime.CompilerServices.RuntimeHelpers.EnsureSufficientExecutionStack()
  let (baseE, j) = parsePrimary state i
  // Any postfix after an unparenthesized lambda belongs to its body.
  // If the body couldn't parse it, leave it for an error rather than attaching
  // it to the lambda itself.
  if tok state i = TFun then
    (baseE, j)
  else
    let (e, k) = parsePostfix state baseE j
    parseCalls state i e k

// Parse `value?.field.1` from left to right: unwrap, field, then tuple element.
// The parser stores both `.field` and `.1` as field accesses.
and parsePostfix (state : ParserState) (e : WT.Expr) (i : int) : WT.Expr * int =
  let access (e : WT.Expr) (dotR : TokenRange) (fieldR : TokenRange) field =
    WT.ERecordFieldAccess(span (WT.exprRange e) fieldR, e, (fieldR, field), dotR)
  let isDigits (s : string) = s <> "" && Seq.forall System.Char.IsDigit s
  match tok state i, tok state (i + 1) with
  | TQuestion, _ ->
    let question = rng state i
    // Report the unwanted whitespace, then continue parsing as if `?`
    // were adjacent to its operand.
    if (rng state (i - 1)).end_ <> question.start then
      errFull
        state
        DiagnosticCode.unexpected
        i
        "'?' must come directly after its operand, with no space"
        []
        (Some "write `value?`, or `(f x)?` for a call")
    parsePostfix
      state
      (WT.EUnwrap(span (WT.exprRange e) question, e, question))
      (i + 1)
  | TDot, TIdent field ->
    parsePostfix state (access e (rng state i) (rng state (i + 1)) field) (i + 2)
  // `r.type`: say how to escape it, then read it as the field it was meant to be
  // Only directly after the dot: `r.` then a newline is a name still being typed.
  | TDot, t when isKeyword t && (rng state i).end_ = (rng state (i + 1)).start ->
    let field = txt state (i + 1)
    // the receiver as written, backticks included
    let receiver =
      match e with
      | WT.EVariable _ -> txt state (i - 1)
      | _ -> ""
    let hint = keywordHint state (i + 1) (receiver + ".")
    err
      state
      DiagnosticCode.expected
      (i + 1)
      $"expected a field name after '.', found '{field}'{hint}"
    parsePostfix state (access e (rng state i) (rng state (i + 1)) field) (i + 2)
  | TDot, TInt64 _ when isDigits (txt state (i + 1)) ->
    let field = txt state (i + 1)
    parsePostfix state (access e (rng state i) (rng state (i + 1)) field) (i + 2)
  // The lexer reads `1.2` in `t.1.2` as a float; split it into two accesses.
  | TDot, TFloat _ when
    (match (txt state (i + 1)).Split('.') with
     | [| a; b |] -> isDigits a && isDigits b
     | _ -> false)
    ->
    let text = txt state (i + 1)
    let a = text.Substring(0, text.IndexOf '.')
    let b = text.Substring(a.Length + 1)
    let r = rng state (i + 1)
    let at (col : int) = { r.start with column = r.start.column + col }
    let aR = { start = r.start; end_ = at a.Length }
    let innerDotR = { start = at a.Length; end_ = at (a.Length + 1) }
    let bR = { start = at (a.Length + 1); end_ = r.end_ }
    let inner = access e (rng state i) aR a
    parsePostfix state (access inner innerDotR bR b) (i + 2)
  // Give invalid indices such as `t.1s8` and `t.1e0` a specific diagnostic.
  | TDot,
    (TBigInt _ | TInt64 _ | TInt128 _ | TInt8 _ | TInt16 _ | TInt32 _ | TUInt8 _ | TUInt16 _ | TUInt32 _ | TUInt64 _ | TUInt128 _ | TFloat _ | TFloat32 _ | TDecimal _) ->
    let text = txt state (i + 1)
    // The lexer already reports malformed literals such as `1.2s8` or `300s8`.
    if not state.toks[i + 1].lexError then
      errFull
        state
        DiagnosticCode.expected
        (i + 1)
        $"expected a tuple element number after '.', found '{text}'"
        []
        (Some "tuple elements are plain numbers counting from 1: t.1, t.2")
    let digits = text |> Seq.takeWhile System.Char.IsDigit |> System.String.Concat
    parsePostfix state (access e (rng state i) (rng state (i + 1)) digits) (i + 2)
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
  // A ResizeArray on purpose: an array comprehension here cost 3 million more instructions on a
  // script of 1,500 interpolated strings.
  let lineStarts =
    let acc = ResizeArray<int>()
    acc.Add 0
    for k in 0 .. m - 1 do
      if fullText[k] = '\n' then acc.Add(k + 1)
    acc
  let posAt (off : int) : Pos =
    let off = min off m
    let lo = lastAtOrBefore lineStarts off 0 (lineStarts.Count - 1)
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
              // Parse `{...}` as an expression. Package declaration scope applies
              // only to the outer file, not to interpolation contents.
              let subResult =
                parseTokensAt (state.interpDepth + 1) ItemScope.Script offToks
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
  checkBareMinMagnitude state i // `128s8` etc. — only valid negated
  match tok state i with
  | TBigInt v -> (WT.EBigInt(rng state i, (rng state i, v)), i + 1)
  | TInt64 v ->
    let (digits, suffix) = splitIntSuffix state i
    (WT.EInt64(rng state i, (digits, v), suffix), i + 1)
  | TInt8 v ->
    let (digits, suffix) = splitIntSuffix state i
    (WT.EInt8(rng state i, (digits, v), suffix), i + 1)
  | TUInt8 v ->
    let (digits, suffix) = splitIntSuffix state i
    (WT.EUInt8(rng state i, (digits, v), suffix), i + 1)
  | TInt16 v ->
    let (digits, suffix) = splitIntSuffix state i
    (WT.EInt16(rng state i, (digits, v), suffix), i + 1)
  | TUInt16 v ->
    let (digits, suffix) = splitIntSuffix state i
    (WT.EUInt16(rng state i, (digits, v), suffix), i + 1)
  | TInt32 v ->
    let (digits, suffix) = splitIntSuffix state i
    (WT.EInt32(rng state i, (digits, v), suffix), i + 1)
  | TUInt32 v ->
    let (digits, suffix) = splitIntSuffix state i
    (WT.EUInt32(rng state i, (digits, v), suffix), i + 1)
  | TUInt64 v ->
    let (digits, suffix) = splitIntSuffix state i
    (WT.EUInt64(rng state i, (digits, v), suffix), i + 1)
  | TInt128 v ->
    let (digits, suffix) = splitIntSuffix state i
    (WT.EInt128(rng state i, (digits, v), suffix), i + 1)
  | TUInt128 v ->
    let (digits, suffix) = splitIntSuffix state i
    (WT.EUInt128(rng state i, (digits, v), suffix), i + 1)
  // unary minus on a numeric literal: `-5L`, `-2.0` (infix `a - b` is handled in
  // parseInfixRhs, so a `-` reaching here always prefixes a literal)
  | TMinus ->
    let r1 = rng state (i + 1)
    let r = span (rng state i) r1
    match tok state (i + 1) with
    | TBigInt v -> (WT.EBigInt(r, (r, -v)), i + 2)
    | TInt64 v ->
      let (digits, suffix) = splitIntSuffix state (i + 1)
      (WT.EInt64(r, (span (rng state i) digits, -v), suffix), i + 2)
    | TInt8 v ->
      let (digits, suffix) = splitIntSuffix state (i + 1)
      (WT.EInt8(r, (span (rng state i) digits, -v), suffix), i + 2)
    | TInt16 v ->
      let (digits, suffix) = splitIntSuffix state (i + 1)
      (WT.EInt16(r, (span (rng state i) digits, -v), suffix), i + 2)
    | TInt32 v ->
      let (digits, suffix) = splitIntSuffix state (i + 1)
      (WT.EInt32(r, (span (rng state i) digits, -v), suffix), i + 2)
    | TInt128 v ->
      let (digits, suffix) = splitIntSuffix state (i + 1)
      (WT.EInt128(r, (span (rng state i) digits, -v), suffix), i + 2)
    | TFloat v ->
      let (whole, frac) = floatParts state (i + 1) v
      (WT.EFloat(r, true, whole, frac), i + 2)
    | TFloat32 v ->
      let (whole, frac) = float32Parts state (i + 1) v
      (WT.EFloat32(r, true, whole, frac), i + 2)
    | TDecimal v -> (WT.EDecimal(r, -v), i + 2)
    | _ ->
      // unary minus on a non-literal (`-x`, `-(expr)`, `-f x`) → `Stdlib.Negate.negate`
      // applied to the whole application, so `-f x` groups as `-(f x)`, not `(-f) x`
      // (minus binds looser than application). Infix ops still bind looser than the
      // minus: `-a + b` is `(-a) + b`, since `+` can't start an application arg.
      parsePrefixCall state i [ "Stdlib"; "Negate" ] "negate"
  | TFloat v ->
    let r = rng state i
    let (whole, frac) = floatParts state i v
    (WT.EFloat(r, v < 0.0, whole, frac), i + 1)
  | TFloat32 v ->
    let r = rng state i
    let (whole, frac) = float32Parts state i v
    (WT.EFloat32(r, v < 0.0f, whole, frac), i + 1)
  | TDecimal v -> (WT.EDecimal(rng state i, v), i + 1)
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
    let finalIsUpper = isUpperIdentifier final.name
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
          | t, TEquals -> isKeyword t
          | _ -> false)
    // a bare `Dict { … }` is a dict LITERAL, not a record of a type named `Dict`;
    // `Dict` is a keyword here, so it parses to its own node with the keyword range.
    if List.isEmpty mods && final.name = "Dict" && tok state j = TLBrace then
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
      // `Case(e1, e2, …)`: fields go in parentheses, the same as a call's
      // arguments, so commas separate FIELDS (`EInt64(r, n, s)` is three fields,
      // not one tuple; a tuple field is `Case((a, b))`). A space before the `(`
      // is allowed, as for a call, but `… Ok` then `(x, [])` on the next line at
      // the same indent is a separate statement. Space-separated fields (`Ok 5`)
      // are an error, reported by parseApp.
      let glued =
        tok state j = TLParen
        && (rng state j).start.row = final.range.end_.row
        && (rng state j).start.column = final.range.end_.column
      let (fields, k) =
        if tok state j = TLParen && (glued || offsideContinues state i j) then
          parseParenList state j
        else
          ([], j)
      let endR =
        if tok state j = TLParen && k > j then rng state (k - 1)
        elif not (List.isEmpty fields) then WT.exprRange (Seq.last fields)
        else final.range
      (WT.EEnum(
        span (rng state i) endR,
        typeName,
        (final.range, final.name),
        fields,
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
    // Parameters have the shape of a call's arguments: `fun x -> …` takes one,
    // `fun(acc, x) -> …` takes two, and a tuple parameter is `fun((a, b)) -> …`.
    // `fun () -> …` takes the unit value.
    let kwFun = rng state i
    let (patsRev, k) =
      match tok state (i + 1) with
      | TLParen when tok state (i + 2) <> TRParen ->
        let openP = rng state (i + 1)
        let (patsRev, m) = parseLambdaParamList state [] (i + 2)
        if tok state m = TRParen then
          (patsRev, m + 1)
        else
          errUnclosed state m ")" "(" openP
          (patsRev, m)
      | TIdent _
      | TUnderscore
      | TLParen ->
        let (p, k2) = parseLetPattern state (i + 1)
        ([ p ], k2)
      | _ when keywordHint state (i + 1) "" <> "" ->
        let (p, k2) = parseLetPattern state (i + 1)
        ([ p ], k2)
      | _ -> ([], i + 1)
    // `fun a b -> …` is not Dark. The parameters are still parsed, so recovery
    // sees the shape the author meant; the diagnostic is what makes it an error.
    let firstExtra = k
    let (patsRev, k) = parseSpacedLambdaParams state patsRev k
    let patCount = List.length patsRev
    if k > firstExtra then
      let shown = placeholders patCount
      let commaList = String.concat ", " shown
      let spaceList = String.concat " " shown
      errFull
        state
        DiagnosticCode.unexpected
        firstExtra
        "a lambda with several parameters lists them in parentheses, separated by commas"
        []
        (Some
          $"write `fun({commaList}) ->` rather than `fun {spaceList} ->`; a tuple parameter is `fun((a, b)) ->`")
    if patCount = 0 then errExpectedName state k "at least one lambda parameter"
    let (arrow, m) =
      if tok state k = TArrow then
        (rng state k, k + 1)
      else
        errExpected state k "'->' in lambda"
        (zeroWidthAtEnd (rng state k), k)
    let (body, p) = parseBlock state m
    (WT.ELambda(span kwFun (WT.exprRange body), List.rev patsRev, body, kwFun, arrow),
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
  // Prefix `!` (boolean NOT) and `~` (bitwise NOT). Shaped exactly like unary
  // minus on a non-literal: the operand is a whole APPLICATION, so `!f x` is
  // `!(f x)`, and infix operators still bind looser (`!a && b` is `(!a) && b`,
  // since `&&` cannot start an application argument).
  | TNot -> parsePrefixCall state i [ "Builtin" ] PT.InfixFnName.boolNotBuiltinName
  | TBitNot -> parsePrefixCall state i [ "Stdlib"; "BitwiseNot" ] "bitwiseNot"
  | TDotDotDot ->
    err
      state
      DiagnosticCode.unexpected
      i
      "'...' only goes inside a list, as in `[...a, ...b]`"
    (WT.EError(rng state i), i + 1)
  | _ ->
    if tok state i = TQuestion then
      // A `?` glued to something that cannot take it, with no whitespace
      // before it. Suggest parentheses around the expression instead.
      let spaced = i = 0 || (rng state (i - 1)).end_ <> (rng state i).start
      if spaced then
        errFull
          state
          DiagnosticCode.unexpected
          i
          "'?' must come directly after its operand, with no space"
          []
          (Some "write `value?`, or `(f x)?` for a call")
      else
        errFull
          state
          DiagnosticCode.unexpected
          i
          "'?' can't follow this expression directly"
          []
          (Some "wrap what you want to unwrap in parentheses: `(expr)?`")
    else
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
    Option.isSome (infixOf (tok state (i + 1))) && tok state (i + 2) = TRParen
  then
    // operator section `(op)` → `fun a b -> a op b` (a 2-arg fn value)
    let opTok = tok state (i + 1)
    let opR = rng state (i + 1)
    let r = span openP (rng state (i + 2))
    let z = zeroWidthAtEnd opR
    let va = WT.EVariable(z, "a")
    let vb = WT.EVariable(z, "b")
    let body = WT.EInfix(r, (opR, (infixOf opTok).Value), va, vb)
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
        let (restRev, m) = parseParenTupleRest state [] k
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
          List.rev restRev,
          openP,
          closeP
         ),
         p)
      else
        // group or statement block: fold newline-separated statements
        let (stmtsRev, m) = parseParenStatements state [ first ] j
        let folded = foldStatements stmtsRev
        if tok state m = TRParen then
          (folded, m + 1)
        else
          errUnclosed state m ")" "(" openP
          (folded, m))

// The parameters in a lambda's parentheses from `m`, last first.
and private parseLambdaParamList
  (state : ParserState)
  (patsRev : List<WT.LetPattern>)
  (m : int)
  : List<WT.LetPattern> * int =
  if tok state m = TRParen || tok state m = TEOF then
    (patsRev, m)
  else
    let (p, m2) = parseLetPattern state m
    let patsRev = p :: patsRev
    if tok state m2 = TComma then
      parseLambdaParamList state patsRev (m2 + 1)
    elif m2 > m then
      if tok state m2 <> TRParen then
        errExpected state m2 "',' or ')' in the lambda's parameters"
        (patsRev, m2)
      else
        parseLambdaParamList state patsRev m2
    else
      (patsRev, m)

// Space-separated lambda parameters from `k` (`fun a b ->`, an error), prepended to `patsRev`.
and private parseSpacedLambdaParams
  (state : ParserState)
  (patsRev : List<WT.LetPattern>)
  (k : int)
  : List<WT.LetPattern> * int =
  match tok state k with
  | TIdent _
  | TUnderscore
  | TLParen ->
    let (p, k2) = parseLetPattern state k
    parseSpacedLambdaParams state (p :: patsRev) (if k2 = k then k + 1 else k2)
  | _ -> (patsRev, k)

// A parenthesized tuple's elements after its second, from `m`, each with the `,` before it,
// last first.
and private parseParenTupleRest
  (state : ParserState)
  (restRev : List<TokenRange * WT.Expr>)
  (m : int)
  : List<TokenRange * WT.Expr> * int =
  if tok state m <> TComma then
    (restRev, m)
  else
    let cr = rng state m
    let (e, m2) = parseExpr state (m + 1)
    let restRev = (cr, e) :: restRev
    if m2 = m + 1 && tok state m2 = tok state m then
      (restRev, m)
    else
      parseParenTupleRest state restRev m2

// The statements of a parenthesized block from `m`, prepended to `stmtsRev`.
and private parseParenStatements
  (state : ParserState)
  (stmtsRev : List<WT.Expr>)
  (m : int)
  : List<WT.Expr> * int =
  if
    tok state m = TRParen
    || tok state m = TEOF
    || tok state m = TComma
    || declBarrier state m
  then
    (stmtsRev, m)
  else
    let m =
      if tok state m = TSemicolon then
        errStatementSemicolon state m
        m + 1
      else
        m
    let (s, m2) = parseExpr state m
    if m2 > m then parseParenStatements state (s :: stmtsRev) m2 else (stmtsRev, m)

// `[ e , e , … ]` (or newline separators); each element keeps its
// trailing-separator range
and parseList (state : ParserState) (i : int) : WT.Expr * int =
  let openB = rng state i
  // list elements are offside-delimited in their own scope
  // (else an element swallows the next one as an application, e.g. inside `( … )`:
  // `[ [a]\n (f x) ]` must not read as `[a] (f x)`). Mirrors parseRecord.
  withElementScope state (fun () ->
    let (elemsRev, spreads, firstDots, k) =
      parseListElems state [] 0 Set.empty None (i + 1)
    let (closeB, p) =
      if tok state k = TRBracket then
        (rng state k, k + 1)
      else
        errUnclosed state k "]" "[" openB
        (zeroWidthAtEnd (rng state k), k)
    let r = span openB closeB
    match firstDots with
    | None -> (WT.EList(r, List.rev elemsRev, openB, closeB), p)
    | Some dotsR -> (spreadList r openB closeB dotsR (List.rev elemsRev) spreads, p))

// The elements of a list from `k`, last first, with how many there are, which are `...e`
// spreads, and the first `...`.
and private parseListElems
  (state : ParserState)
  (elemsRev : List<WT.Expr * Option<TokenRange>>)
  (count : int)
  (spreads : Set<int>)
  (firstDots : Option<TokenRange>)
  (k : int)
  : List<WT.Expr * Option<TokenRange>> * Set<int> * Option<TokenRange> * int =
  if tok state k = TRBracket || tok state k = TEOF || declBarrier state k then
    (elemsRev, spreads, firstDots, k)
  else
    // each element is its own offside statement, so a wrapped element's args
    // don't grab the NEXT element (`[ f a\n f b ]` stays two elements)
    setStmtCol state (rng state k).start.column
    let isSpread = tok state k = TDotDotDot
    let (e, k2) = parseExpr state (if isSpread then k + 1 else k)
    let spreads = if isSpread then Set.add count spreads else spreads
    let firstDots =
      if isSpread && Option.isNone firstDots then Some(rng state k) else firstDots
    if k2 = k then
      err state DiagnosticCode.unexpected k $"unexpected {foundDesc state k} in list"
      (elemsRev, spreads, firstDots, k)
    elif tok state k2 = TComma then
      parseListElems
        state
        ((e, Some(rng state k2)) :: elemsRev)
        (count + 1)
        spreads
        firstDots
        (k2 + 1)
    elif tok state k2 = TSemicolon then
      errCollectionSemicolon state k2 "list elements"
      parseListElems
        state
        ((e, Some(rng state k2)) :: elemsRev)
        (count + 1)
        spreads
        firstDots
        (k2 + 1)
    else
      if tok state k2 <> TRBracket then
        requireElementSeparator
          state
          (WT.exprRange e)
          k2
          "a comma or newline between list elements"
      parseListElems state ((e, None) :: elemsRev) (count + 1) spreads firstDots k2

// `[a, ...b, c]` lowers to `Stdlib.List.flatten([[a], b, [c]])`, and `[...a, ...b]`
// to `Stdlib.List.concat(a, b)`: runs of plain elements become list literals and
// each spread stands for itself.
and spreadList
  (r : TokenRange)
  (openB : TokenRange)
  (closeB : TokenRange)
  (dotsR : TokenRange)
  (elems : List<WT.Expr * Option<TokenRange>>)
  (spreads : Set<int>)
  : WT.Expr =
  // a run of plain elements, collected in reverse, as one list literal
  let literal (revRun : List<WT.Expr * Option<TokenRange>>) : List<WT.Expr> =
    match List.rev revRun with
    | [] -> []
    | items ->
      let first = WT.exprRange (fst (Seq.head items))
      let last = WT.exprRange (fst (Seq.last items))
      [ WT.EList(span first last, items, zeroWidthAtEnd first, last) ]
  let (revSegments, revRun) =
    elems
    |> List.indexed
    |> List.fold
      (fun (revSegs, revRun) (ix, elem) ->
        if Set.contains ix spreads then
          (fst elem :: (literal revRun @ revSegs), [])
        else
          (revSegs, elem :: revRun))
      ([], [])
  let segments = List.rev (literal revRun @ revSegments)
  // the call has no source of its own, so its name sits on the first `...`
  let stdlibList (name : string) : WT.Expr =
    WT.EFnName(
      dotsR,
      { range = dotsR
        modules =
          [ ({ range = dotsR; name = "Stdlib" }, dotsR)
            ({ range = dotsR; name = "List" }, dotsR) ]
        fn = { range = dotsR; name = name } }
    )
  match segments with
  | [ only ] -> only
  | [ a; b ] -> WT.EApply(r, stdlibList "concat", [], [ a; b ])
  | _ ->
    let items = segments |> List.map (fun e -> (e, None))
    WT.EApply(r, stdlibList "flatten", [], [ WT.EList(r, items, openB, closeB) ])

// `Type { name = value, … }`. `i` is the `{`.
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
    let (fieldsRev, k) = parseRecordFields state [] (i + 1)
    let (closeB, p) =
      if tok state k = TRBrace then
        (rng state k, k + 1)
      else
        errUnclosed state k "}" "{" openB
        (zeroWidthAtEnd (rng state k), k)
    (WT.ERecord(span nameRange closeB, typeName, List.rev fieldsRev, openB, closeB),
     p))

// The `name = value` fields of a record from `k`, last first.
and private parseRecordFields
  (state : ParserState)
  (fieldsRev : List<TokenRange * (TokenRange * string) * WT.Expr>)
  (k : int)
  : List<TokenRange * (TokenRange * string) * WT.Expr> * int =
  if tok state k = TRBrace || tok state k = TEOF then
    (fieldsRev, k)
  else
    let what = "a record field 'name = value'"
    match fieldName state k TEquals what with
    | Some fname ->
      let fnameR = rng state k
      // each field is its own offside statement (value args don't grab the next field)
      setStmtCol state (rng state k).start.column
      let (value, k2) = parseExpr state (k + 2)
      let fieldsRev =
        (span fnameR (WT.exprRange value), (fnameR, fname), value) :: fieldsRev
      if tok state k2 = TComma then
        parseRecordFields state fieldsRev (k2 + 1)
      elif tok state k2 = TSemicolon then
        errCollectionSemicolon state k2 "record fields"
        parseRecordFields state fieldsRev (k2 + 1)
      elif k2 > k then
        if tok state k2 <> TRBrace then
          requireElementSeparator
            state
            (WT.exprRange value)
            k2
            "a comma or newline between record fields"
        parseRecordFields state fieldsRev k2
      else
        (fieldsRev, k)
    | None ->
      errExpectedName state k what
      (fieldsRev, k)

and parseDict
  (state : ParserState)
  (keywordDict : TokenRange)
  (i : int)
  : WT.Expr * int =
  let openB = rng state i
  withElementScope state (fun () ->
    let (entriesRev, k) = parseDictEntries state [] (i + 1)
    let (closeB, p) =
      if tok state k = TRBrace then
        (rng state k, k + 1)
      else
        errUnclosed state k "}" "{" openB
        (zeroWidthAtEnd (rng state k), k)
    (WT.EDict(
      span keywordDict closeB,
      List.rev entriesRev,
      keywordDict,
      openB,
      closeB
     ),
     p))

// The `key: value` entries of a dict from `k`, last first.
and private parseDictEntries
  (state : ParserState)
  (entriesRev : List<TokenRange * WT.Expr * TokenRange * WT.Expr>)
  (k : int)
  : List<TokenRange * WT.Expr * TokenRange * WT.Expr> * int =
  if tok state k = TRBrace || tok state k = TEOF then
    (entriesRev, k)
  else
    setStmtCol state (rng state k).start.column
    let (key, kAfterKey) = parseExpr state k
    if kAfterKey <= k then
      errExpected state k "a dict entry 'key: value'"
      (entriesRev, k)
    elif tok state kAfterKey <> TColon then
      errExpected state kAfterKey "`:` between a dict key and its value"
      (entriesRev, k)
    else
      let colonR = rng state kAfterKey
      let (value, k2) = parseExpr state (kAfterKey + 1)
      let entriesRev =
        (span (WT.exprRange key) (WT.exprRange value), key, colonR, value)
        :: entriesRev
      if tok state k2 = TComma then
        parseDictEntries state entriesRev (k2 + 1)
      elif tok state k2 = TSemicolon then
        errCollectionSemicolon state k2 "dict entries"
        parseDictEntries state entriesRev (k2 + 1)
      elif k2 > kAfterKey then
        if tok state k2 <> TRBrace then
          requireElementSeparator
            state
            (WT.exprRange value)
            k2
            "a comma or newline between dict entries"
        parseDictEntries state entriesRev k2
      else
        (entriesRev, k)

// record update `{ expr with name = value, … }`. `i` is the `{`.
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
    let (updatesRev, k) = parseRecordUpdateFields state [] afterWith
    // `{ r with }` updates nothing — reject it rather than lower a degenerate
    // update (both lowerings otherwise have to special-case the empty list).
    if List.isEmpty updatesRev then
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
      List.rev updatesRev,
      openB,
      closeB,
      kwWith
     ),
     p))

// The `name = value` fields of a record update from `k`, last first.
and private parseRecordUpdateFields
  (state : ParserState)
  (updatesRev : List<(TokenRange * string) * TokenRange * WT.Expr>)
  (k : int)
  : List<(TokenRange * string) * TokenRange * WT.Expr> * int =
  if tok state k = TRBrace || tok state k = TEOF then
    (updatesRev, k)
  else
    let what = "a record-update field 'name = value'"
    match fieldName state k TEquals what with
    | Some fname ->
      let fnameR = rng state k
      let eqR = rng state (k + 1)
      setStmtCol state (rng state k).start.column
      let (value, k2) = parseExpr state (k + 2)
      let updatesRev = ((fnameR, fname), eqR, value) :: updatesRev
      if tok state k2 = TComma then
        parseRecordUpdateFields state updatesRev (k2 + 1)
      elif tok state k2 = TSemicolon then
        errCollectionSemicolon state k2 "record-update fields"
        parseRecordUpdateFields state updatesRev (k2 + 1)
      elif k2 > k then
        if tok state k2 <> TRBrace then
          requireElementSeparator
            state
            (WT.exprRange value)
            k2
            "a comma or newline between record-update fields"
        parseRecordUpdateFields state updatesRev k2
      else
        (updatesRev, k)
    | None ->
      errExpectedName state k what
      (updatesRev, k)

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

// `A -> B` takes one argument, and `(A, B) -> C` two (the arguments are a
// parenthesized list, parsed by parseAtomType, which returns the whole TFn). A
// chain `A -> B -> C` is an error: write `(A, B) -> C` for two arguments, or
// `A -> (B -> C)` for one argument returning a function. The chain is still
// parsed as the two-argument function it used to mean, for recovery.
and parseFnType (state : ParserState) (i : int) : WT.TypeReference * int =
  let (first, j) = parseTupleType state i
  // A remaining `>` from a split `>>` must close the outer generic first.
  // In `List<List<A>> -> B`, the arrow belongs outside both Lists.
  if tok state j <> TArrow || state.pendingGt > 0 then
    (first, j)
  else
    let (second, k0) = parseTupleType state (j + 1)
    let k = if k0 > j then k0 else j + 1
    // a second arrow: `A -> B -> C`, or `(A, B) -> C -> D`
    if tok state k = TArrow && state.pendingGt = 0 then
      errFull
        state
        DiagnosticCode.unexpected
        k
        "a function type with several arguments lists them in parentheses"
        []
        (Some
          "write `(A, B) -> C` for a function of two arguments, or `A -> (B -> C)` for one that returns a function")
    let (argsRev, last, k) =
      parseTypeChain
        TArrow
        true
        parseTupleType
        state
        [ (first, rng state j) ]
        second
        k
    (WT.TFn(
      span (WT.typeReferenceRange first) (WT.typeReferenceRange last),
      List.rev argsRev,
      last
     ),
     k)

// `sep T sep T …` from `k`, after the type `cur`: each type but the last with the `sep` after
// it, prepended to `argsRev`, then the last type. A `>` left pending by a split `>>` ends the
// chain when `stopAtPendingGt`.
and private parseTypeChain
  (sep : Token)
  (stopAtPendingGt : bool)
  (parse : ParserState -> int -> WT.TypeReference * int)
  (state : ParserState)
  (argsRev : List<WT.TypeReference * TokenRange>)
  (cur : WT.TypeReference)
  (k : int)
  : List<WT.TypeReference * TokenRange> * WT.TypeReference * int =
  if tok state k = sep && not (stopAtPendingGt && state.pendingGt > 0) then
    let (next, k2) = parse state (k + 1)
    parseTypeChain
      sep
      stopAtPendingGt
      parse
      state
      ((cur, rng state k) :: argsRev)
      next
      (if k2 > k then k2 else k + 1)
  else
    (argsRev, cur, k)

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
    let (rest, m) = separatedSkipping TStar true parseAtomType state [] k
    let endT = if List.isEmpty rest then second else snd (Seq.last rest)
    let z = zeroWidthAtEnd (WT.typeReferenceRange first) // bare tuple: no parens
    (WT.TTuple(
      span (WT.typeReferenceRange first) (WT.typeReferenceRange endT),
      first,
      star1,
      second,
      rest,
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
    let (first, j) = parseTypeRef state (i + 1)
    // stop taking args once a `>>` has left a `>` pending for THIS level, else a
    // nested `Option<Result<T,S>>` would swallow the enclosing type's next arg
    // (`Option<Result<T,S>, S>`). Mirrors the tuple loop's `state.pendingGt = 0` guard.
    let (rest, k) = separatedSkipping TComma true parseTypeRef state [] j
    let (closeR, k3) = expectGt state k
    (first :: List.map snd rest, Some closeR, k3)

and parseAtomType (state : ParserState) (i : int) : WT.TypeReference * int =
  System.Runtime.CompilerServices.RuntimeHelpers.EnsureSufficientExecutionStack()
  match tok state i with
  | TLParen ->
    // `(T)` grouping, `(A * B)` parenthesized tuple, or `(A, B) -> C`: the
    // argument list of a function type, returned here as the whole TFn
    let openP = rng state i
    let (inner, j) = parseFnType state (i + 1)
    if tok state j = TComma then
      // each argument carries the `,` after it; the last one the `->`
      let (argsRev, cur, m) =
        parseTypeChain TComma false parseFnType state [] inner j
      let afterClose =
        if tok state m = TRParen then
          m + 1
        else
          errUnclosed state m ")" "(" openP
          m
      if tok state afterClose = TArrow then
        let args = List.rev ((cur, rng state afterClose) :: argsRev)
        let (ret, k) = parseTupleType state (afterClose + 1)
        (WT.TFn(span openP (WT.typeReferenceRange ret), args, ret),
         (if k > afterClose then k else afterClose + 1))
      else
        errFull
          state
          DiagnosticCode.expected
          afterClose
          "a parenthesized list of types is the argument list of a function type, and needs `->` and a return type after it"
          []
          (Some "write `(A, B) -> C` for a function, or `A * B` for a tuple")
        let ret = WT.TUnit(zeroWidthAtEnd (rng state (afterClose - 1)))
        let args =
          List.rev ((cur, zeroWidthAtEnd (rng state (afterClose - 1))) :: argsRev)
        (WT.TFn(span openP (rng state (afterClose - 1)), args, ret), afterClose)
    elif tok state j = TRParen then
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
    let (key, j) = parseTypeRef state (i + 2)
    let (commaR, j) =
      if tok state j = TComma then
        (rng state j, j + 1)
      else
        errExpected state j "a comma between Dict's key and value types"
        (zeroWidthAtEnd (rng state j), j)
    let (value, j) = parseTypeRef state j
    let (closeB, k) = expectGt state j
    (WT.TDict(span (rng state i) closeB, kwDict, openB, key, commaR, value, closeB),
     k)
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
  | TIdent name when isUpperIdentifier name ->
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

/// The trait in a bound `'a: Show<Int64>`: a qualified name, parsed like a type
/// reference. Anything else (a primitive, a type variable, a fn type) is a
/// diagnostic, since a bound has to name a trait.
and parseBoundTrait
  (state : ParserState)
  (i : int)
  : Option<WT.QualifiedTypeIdentifier> * int =
  let (t, j) = parseAtomType state i
  match t with
  | WT.TCustom qti -> (Some qti, j)
  | other ->
    state.diagnostics.Add
      { code = DiagnosticCode.bound
        severity = DiagError
        range = WT.typeReferenceRange other
        message = "A bound names a trait, not a type"
        related = []
        hint =
          Some
            "write `'a: Show`, where `Show` is a trait; `'a: String` is not a bound" }
    (None, j)

and parseTypeParams
  (state : ParserState)
  (i : int)
  : List<string * TokenRange> * List<WT.TypeParamBound> * int =
  parseTypeParamsWith parseBoundTrait state i

/// A fn's parameter list, `(a: T, b: U)` or `()`, with the blank-name check both a
/// fn declaration and a trait method need. Parameters are separated by commas or
/// newlines. A second list after the first (`(a: T) (b: U)`) is an error, and is
/// read as more parameters so the rest of the declaration still parses.
and parseParams (state : ParserState) (i : int) : List<WT.FnParam> * int =
  let (first, k1) = parseParamList state i
  let (extrasRev, k) = parseExtraParamLists state [] k1
  let ps = first @ List.concat (List.rev extrasRev)
  for parameter in ps do
    match parameter with
    | WT.FPNormal(_, name, _, _, _, _, _) when name.name = "" ->
      state.diagnostics.Add
        { code = DiagnosticCode.pattern
          severity = DiagError
          range = name.range
          message = "Blank parameter '___' is not allowed in a package function"
          related = []
          hint = Some "use () for a unit parameter or give the parameter a name" }
    | _ -> ()
  (ps, k)

// Parameter lists after a fn's first, from `k` (an error), last first.
and private parseExtraParamLists
  (state : ParserState)
  (extrasRev : List<List<WT.FnParam>>)
  (k : int)
  : List<List<WT.FnParam>> * int =
  if tok state k <> TLParen then
    (extrasRev, k)
  else
    let (extra, k2) = parseParamList state k
    errFull
      state
      DiagnosticCode.unexpected
      k
      "a function's parameters go in one list, separated by commas"
      []
      (Some "write `let f(a: T, b: U)` rather than `let f (a: T) (b: U)`")
    if k2 = k then
      (extra :: extrasRev, k)
    else
      parseExtraParamLists state (extra :: extrasRev) k2

/// One parenthesized parameter list starting at the `(` at `i`. Each parameter
/// keeps the list's `(` (the first) or the `,` before it as its left delimiter, and
/// the `,` after it or the list's `)` (the last) as its right one.
and parseParamList (state : ParserState) (i : int) : List<WT.FnParam> * int =
  let lparen = rng state i
  if tok state (i + 1) = TRParen then
    ([ WT.FPUnit(span lparen (rng state (i + 1))) ], i + 2)
  else
    let (psRev, k) = parseParamsFrom state i lparen [] lparen (i + 1)
    (List.rev psRev, k)

// The parameters of the list whose `(` at `i` has range `lparen`, from `k`, last first. `left`
// is the delimiter before the parameter at `k`.
and private parseParamsFrom
  (state : ParserState)
  (i : int)
  (lparen : TokenRange)
  (psRev : List<WT.FnParam>)
  (left : TokenRange)
  (k : int)
  : List<WT.FnParam> * int =
  let nameIdx = k
  let nameId : WT.Identifier =
    match tok state nameIdx with
    | TIdent nm -> { range = rng state nameIdx; name = nm }
    // `_` names a parameter you don't intend to use. It's also what `()` is stored as, so
    // accepting it here is what makes `(_: Unit)` and `()` both parse to the same thing and
    // either form round-trip.
    | TUnderscore -> { range = rng state nameIdx; name = "_" }
    | _ ->
      errExpectedName state nameIdx "a parameter name"
      { range = rng state nameIdx; name = "_" }
  let (colon, afterColon) =
    if tok state (nameIdx + 1) = TColon then
      (rng state (nameIdx + 1), nameIdx + 2)
    else
      errExpected state (nameIdx + 1) "':'"
      (zeroWidthAtEnd (rng state (nameIdx + 1)), nameIdx + 1)
  let (typ, afterTyp) = parseTypeRef state afterColon
  // A `///` for a parameter attaches to the token after it: the `(` or `,` when it is
  // written on the line above, the name when it is written just inside the list.
  let description =
    let atLeft = if nameIdx = i + 1 then docOf state i else ""
    if atLeft <> "" then atLeft else docOf state nameIdx
  let (right, next, finished) =
    match tok state afterTyp with
    | TComma -> (rng state afterTyp, afterTyp + 1, false)
    | TRParen -> (rng state afterTyp, afterTyp + 1, true)
    // a newline separates parameters as a comma does
    | TIdent _
    | TUnderscore when
      (rng state afterTyp).start.row > (rng state (afterTyp - 1)).end_.row
      ->
      (zeroWidthAtEnd (rng state (afterTyp - 1)), afterTyp, false)
    | _ ->
      errUnclosed state afterTyp ")" "(" lparen
      (zeroWidthAtEnd (rng state afterTyp), afterTyp, true)
  let psRev =
    WT.FPNormal(span left right, nameId, typ, left, colon, right, description)
    :: psRev
  // a trailing comma before the `)`
  if not finished && tok state next = TRParen then (psRev, next + 1)
  elif finished || next = nameIdx then (psRev, next)
  else parseParamsFrom state i lparen psRev right next

// A declaration-scope function (`let f (p: T) … : R = body`) or value
// (`val x = body`). Legacy module-level `let x = body` also comes through here
// to retain a recovery DValue beside its focused diagnostic.
and parseDecl (state : ParserState) (i : int) : WT.Declaration * int =
  let keywordLet = rng state i
  let nameIdx = skipRec state (i + 1)
  let nameId : WT.Identifier =
    match tok state nameIdx with
    | TIdent nm -> { range = rng state nameIdx; name = nm }
    | _ ->
      errExpectedName state nameIdx "a declaration name"
      { range = rng state nameIdx; name = "_" }
  let (typeParams, bounds, afterName) = parseTypeParams state (nameIdx + 1)
  if tok state afterName = TLParen then
    let (ps, kk) = parseParams state afterName
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
        bounds = bounds
        parameters = ps
        effects = None
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

// A named package test: `test name = body`, optionally followed by an
// expected outcome: `=> raises "message"`. The word remains an identifier
// outside the outcome separator.
and parseTestDecl (state : ParserState) (i : int) : WT.Declaration * int =
  let keywordTest = rng state i
  let nameId : WT.Identifier =
    match tok state (i + 1) with
    | TIdent name -> { range = rng state (i + 1); name = name }
    | _ ->
      errExpectedName state (i + 1) "a test name"
      { range = rng state (i + 1); name = "_" }
  let beforeEq =
    if tok state (i + 2) = TColon then
      errExpected
        state
        (i + 2)
        "'='; tests use instance and called-function permissions"
      i + 3
    else
      i + 2
  let (eq, afterEq) =
    if tok state beforeEq = TEquals then
      (rng state beforeEq, beforeEq + 1)
    else
      errExpected state beforeEq "'='"
      (zeroWidthAtEnd (rng state beforeEq), beforeEq)
  let (body, afterBody) = parseBlock state afterEq
  let isOutcome =
    tok state afterBody = TEquals
    && tok state (afterBody + 1) = TGt
    && (rng state afterBody).end_ = (rng state (afterBody + 1)).start
  let (symbolOutcome, errorKindRange, expectedError, afterTest, endRange) =
    if isOutcome then
      let outcome = span (rng state afterBody) (rng state (afterBody + 1))
      match tok state (afterBody + 2), tok state (afterBody + 3) with
      | TIdent "raises", TStringLit message ->
        let errorKindRange = rng state (afterBody + 2)
        let messageRange = rng state (afterBody + 3)
        let expected = WT.RuntimeError message
        (Some outcome,
         Some errorKindRange,
         Some(messageRange, expected),
         afterBody + 4,
         messageRange)
      | TIdent "raises", _ ->
        errExpected state (afterBody + 3) "an expected error string"
        (Some outcome, Some(rng state (afterBody + 2)), None, afterBody + 3, outcome)
      | _ ->
        errExpected state (afterBody + 2) "'raises' after '=>'"
        (Some outcome, None, None, afterBody + 2, outcome)
    else
      (None, None, None, afterBody, WT.exprRange body)
  (WT.DTest
    { range = span keywordTest endRange
      name = nameId
      body = body
      expectedError = expectedError
      keywordTest = keywordTest
      errorKindRange = errorKindRange
      symbolOutcome = symbolOutcome
      symbolEquals = eq
      description = docOf state i },
   afterTest)

// `type Name [<'a>] = Definition`
and parseTypeDecl (state : ParserState) (i : int) : WT.Declaration * int =
  let kwType = rng state i
  let nameId : WT.Identifier =
    match tok state (i + 1) with
    | TIdent nm -> { range = rng state (i + 1); name = nm }
    | _ ->
      errExpectedName state (i + 1) "a type name"
      { range = rng state (i + 1); name = "_" }
  let (typeParams, bounds, afterName) = parseTypeParams state (i + 2)
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
      bounds = bounds
      definition = def
      keywordType = kwType
      symbolEquals = eq
      description = docOf state i },
   afterDef)

/// `trait Name<'a> =` followed by an indented block of method signatures, each a
/// fn header without a body: `let show (v: 'a) : String`. A body (`= …`) is parsed
/// and diagnosed: default methods are not supported yet.
and parseTraitDecl (state : ParserState) (i : int) : WT.Declaration * int =
  let kwTrait = rng state i
  let kwCol = kwTrait.start.column
  let nameId : WT.Identifier =
    match tok state (i + 1) with
    | TIdent nm -> { range = rng state (i + 1); name = nm }
    | _ ->
      errExpectedName state (i + 1) "a trait name"
      { range = rng state (i + 1); name = "_" }
  let (typeParams, bounds, afterName) = parseTypeParams state (i + 2)
  if List.isEmpty typeParams then
    state.diagnostics.Add
      { code = DiagnosticCode.bound
        severity = DiagError
        range = nameId.range
        message = "A trait needs a type parameter for the type it is implemented for"
        related = []
        hint = Some "write `trait Show<'a> = ...`; `'a` is the self type" }
  let (eq, afterEq) =
    if tok state afterName = TEquals then
      (rng state afterName, afterName + 1)
    else
      errExpected state afterName "'=' after the trait name"
      (zeroWidthAtEnd (rng state afterName), afterName)
  let (methodsRev, k) = parseTraitMethods state kwCol [] afterEq
  if List.isEmpty methodsRev then
    errExpected
      state
      afterEq
      "at least one method signature, indented under the trait"
  let endR = if k > 0 then rng state (k - 1) else eq
  (WT.DTrait
    { range = span kwTrait endR
      name = nameId
      typeParams = typeParams
      bounds = bounds
      methods = List.rev methodsRev
      keywordTrait = kwTrait
      symbolEquals = eq
      description = docOf state i },
   k)

// The method signatures of a trait from `k`, each a `let` indented past the trait keyword's
// column `kwCol`, last first.
and private parseTraitMethods
  (state : ParserState)
  (kwCol : int)
  (methodsRev : List<WT.TraitMethodDecl>)
  (k : int)
  : List<WT.TraitMethodDecl> * int =
  if tok state k = TLet && (rng state k).start.column > kwCol then
    let kwLet = rng state k
    let mNameIdx = k + 1
    let mName : WT.Identifier =
      match tok state mNameIdx with
      | TIdent nm -> { range = rng state mNameIdx; name = nm }
      | _ ->
        errExpectedName state mNameIdx "a method name"
        { range = rng state mNameIdx; name = "_" }
    let (mTypeParams, mBounds, afterMName) = parseTypeParams state (mNameIdx + 1)
    let (ps, kk) = parseParams state afterMName
    let (colon, afterColon) =
      if tok state kk = TColon then
        (rng state kk, kk + 1)
      else
        errExpected state kk "':' before the method's return type"
        (zeroWidthAtEnd (rng state kk), kk)
    let (returnType, afterRet) = parseTypeRef state afterColon
    let (body, afterBody) =
      if tok state afterRet = TEquals then
        let (b, after) = parseBlock state (afterRet + 1)
        state.diagnostics.Add
          { code = DiagnosticCode.bound
            severity = DiagError
            range = WT.exprRange b
            message =
              "Default method bodies are not supported yet; a trait lists signatures only"
            related = []
            hint = Some "remove the `= …` and implement the method in each `impl`" }
        (Some b, after)
      else
        (None, afterRet)
    let endR =
      match body with
      | Some b -> WT.exprRange b
      | None -> WT.typeReferenceRange returnType
    let methodDecl : WT.TraitMethodDecl =
      { range = span kwLet endR
        name = mName
        typeParams = mTypeParams
        bounds = mBounds
        parameters = ps
        effects = None
        returnType = returnType
        body = body
        keywordLet = kwLet
        symbolColon = colon
        description = docOf state k }
    let methodsRev = methodDecl :: methodsRev
    if afterBody > k then
      parseTraitMethods state kwCol methodsRev afterBody
    else
      (methodsRev, k)
  else
    (methodsRev, k)

/// `impl[<'a: B>] Trait<Args> for Type =` followed by an indented block with one
/// entry per method: a full fn declaration, or `let name = Existing.fn` naming a
/// fn that already exists.
and parseImplDecl (state : ParserState) (i : int) : WT.Declaration * int =
  let kwImpl = rng state i
  let kwCol = kwImpl.start.column
  let (typeParams, bounds, afterParams) = parseTypeParams state (i + 1)
  let (traitId, afterTrait) =
    match parseBoundTrait state afterParams with
    | Some t, j -> (t, j)
    | None, j ->
      ({ range = rng state afterParams
         modules = []
         typ = { range = rng state afterParams; name = "_" }
         typeArgs = [] },
       (if j > afterParams then j else afterParams + 1))
  let (kwFor, afterFor) =
    match tok state afterTrait with
    | TIdent "for" -> (rng state afterTrait, afterTrait + 1)
    | _ ->
      errExpected state afterTrait "'for' between the trait and the type"
      (zeroWidthAtEnd (rng state afterTrait), afterTrait)
  let (forType, afterType) = parseTypeRef state afterFor
  let (eq, afterEq) =
    if tok state afterType = TEquals then
      (rng state afterType, afterType + 1)
    else
      errExpected state afterType "'=' after the type"
      (zeroWidthAtEnd (rng state afterType), afterType)
  // ONE list, in source order: the order reaches the content hash.
  let (membersRev, k) = parseImplMembers state kwCol [] afterEq
  if List.isEmpty membersRev then
    errExpected state afterEq "at least one method, indented under the impl"
  let endR = if k > 0 then rng state (k - 1) else eq
  (WT.DImpl
    { range = span kwImpl endR
      typeParams = typeParams
      bounds = bounds
      trait_ = traitId
      forType = forType
      members = List.rev membersRev
      keywordImpl = kwImpl
      keywordFor = kwFor
      symbolEquals = eq
      description = docOf state i },
   k)

// The members of an impl from `k`, each a `let` indented past the impl keyword's column
// `kwCol`, last first.
and private parseImplMembers
  (state : ParserState)
  (kwCol : int)
  (membersRev : List<WT.ImplMember>)
  (k : int)
  : List<WT.ImplMember> * int =
  if tok state k = TLet && (rng state k).start.column > kwCol then
    let (d, k2) = parseDecl state k
    let membersRev =
      match d with
      | WT.DFunction fn -> WT.IMethod fn :: membersRev
      | WT.DValue({ body = WT.EVariable _ } as v)
      | WT.DValue({ body = WT.EFnName _ } as v) -> WT.IAlias v :: membersRev
      | _ ->
        state.diagnostics.Add
          { code = DiagnosticCode.bound
            severity = DiagError
            range = rng state k
            message =
              "An impl body holds method functions (`let name (p: T) : R = …`) or "
              + "aliases of existing ones (`let name = Module.fn`)"
            related = []
            hint = None }
        membersRev
    if k2 > k then parseImplMembers state kwCol membersRev k2 else (membersRev, k)
  else
    (membersRev, k)

and parseTypeDefinition (state : ParserState) (i : int) : WT.TypeDefinition * int =
  let isEnumStart =
    tok state i = TBar
    || (match tok state i, tok state (i + 1) with
        | TIdent nm, t2 when nm.Length > 0 && System.Char.IsUpper nm[0] ->
          t2 = TLParen || t2 = TOf || t2 = TBar
        | _ -> false)
  match tok state i with
  | TLBrace -> parseRecordDef state i
  | _ when isEnumStart -> parseEnumDef state i
  | _ -> let (t, j) = parseTypeRef state i in (WT.TDAlias t, j)

and parseRecordDef (state : ParserState) (i : int) : WT.TypeDefinition * int =
  let (fieldsRev, k) = parseRecordDefFields state [] (i + 1)
  // `type X = {}` — an empty record isn't valid. Diagnose so the `"_"` placeholder
  // the normalizer inserts (records need ≥1 field) is honest recovery, not a
  // silently-accepted phantom field. (A `{ garbage }` already errored in the loop
  // and won't be at `}` here, so this only fires for a genuinely empty record.)
  if List.isEmpty fieldsRev && tok state k = TRBrace then
    err state DiagnosticCode.expected k "a record type needs at least one field"
  let kk =
    if tok state k = TRBrace then
      k + 1
    else
      errUnclosed state k "}" "{" (rng state i)
      k
  (WT.TDRecord(List.rev fieldsRev), kk)

// The `name: Type` fields of a record type from `k`, last first.
and private parseRecordDefFields
  (state : ParserState)
  (fieldsRev : List<WT.RecordFieldSyntax * Option<TokenRange>>)
  (k : int)
  : List<WT.RecordFieldSyntax * Option<TokenRange>> * int =
  if tok state k = TRBrace || tok state k = TEOF then
    (fieldsRev, k)
  else
    let what = "a record field 'name : Type'"
    match fieldName state k TColon what with
    | Some fname ->
      let fnameR = rng state k
      let colon = rng state (k + 1)
      let (typ, k2) = parseTypeRef state (k + 2)
      let field : WT.RecordFieldSyntax =
        { range = span fnameR (WT.typeReferenceRange typ)
          name = (fnameR, fname)
          typ = typ
          description = docOf state k
          symbolColon = colon }
      if tok state k2 = TComma then
        parseRecordDefFields
          state
          ((field, Some(rng state k2)) :: fieldsRev)
          (k2 + 1)
      elif tok state k2 = TSemicolon then
        errCollectionSemicolon state k2 "record-type fields"
        parseRecordDefFields
          state
          ((field, Some(rng state k2)) :: fieldsRev)
          (k2 + 1)
      elif k2 > k then
        if tok state k2 <> TRBrace then
          requireElementSeparator
            state
            (WT.typeReferenceRange typ)
            k2
            "a comma or newline between record-type fields"
        parseRecordDefFields state ((field, None) :: fieldsRev) k2
      else
        (fieldsRev, k)
    | None ->
      errExpectedName state k what
      (fieldsRev, k)

and parseEnumField
  (state : ParserState)
  (parseType : ParserState -> int -> WT.TypeReference * int)
  (i : int)
  : WT.EnumFieldSyntax * int =
  match tok state i, tok state (i + 1) with
  | TIdent lbl, TColon ->
    let lblR = rng state i
    let colon = rng state (i + 1)
    let (typ, j) = parseType state (i + 2)
    ({ range = span lblR (WT.typeReferenceRange typ)
       typ = typ
       label = Some(lblR, lbl)
       symbolColon = Some colon },
     j)
  | _ ->
    let (typ, j) = parseType state i
    ({ range = WT.typeReferenceRange typ
       typ = typ
       label = None
       symbolColon = None },
     j)

/// A case's fields go in parentheses, separated by commas, as a constructor's do:
/// `Seg(Point, Point)` is two fields and `Seg(Point * Point)` one tuple field.
and parseEnumFields
  (state : ParserState)
  (i : int)
  : List<WT.EnumFieldSyntax> * int =
  let openP = rng state i
  let (fieldsRev, k) = parseEnumFieldsFrom state [] (i + 1)
  if List.isEmpty fieldsRev && tok state k = TRParen then
    errFull
      state
      DiagnosticCode.expected
      k
      "a case with no fields is written without parentheses"
      []
      None
  if tok state k = TRParen then
    (List.rev fieldsRev, k + 1)
  else
    errUnclosed state k ")" "(" openP
    (List.rev fieldsRev, k)

// A `|` or the end of the file ends an enum case even without its `)`, so the
// cases after an unclosed one are still found.
and private endsEnumCase (state : ParserState) (k : int) : bool =
  tok state k = TBar || tok state k = TEOF

// The first index from `k` at a `)` or the end of an enum case.
and private skipToEnumFieldsEnd (state : ParserState) (k : int) : int =
  if tok state k = TRParen || endsEnumCase state k then
    k
  else
    skipToEnumFieldsEnd state (k + 1)

// The parenthesized fields of an enum case from `k`, last first.
and private parseEnumFieldsFrom
  (state : ParserState)
  (fieldsRev : List<WT.EnumFieldSyntax>)
  (k : int)
  : List<WT.EnumFieldSyntax> * int =
  if tok state k = TRParen || endsEnumCase state k then
    (fieldsRev, k)
  else
    let (f, k2) = parseEnumField state parseFnType k
    let fieldsRev = f :: fieldsRev
    if tok state k2 = TComma then
      parseEnumFieldsFrom state fieldsRev (k2 + 1)
    elif tok state k2 = TRParen || endsEnumCase state k2 then
      (fieldsRev, k2)
    else
      errExpected state k2 "',' or ')' between a case's fields"
      (fieldsRev, skipToEnumFieldsEnd state k2)

/// `| Seg of Point * Point` is not Dark. The fields are still parsed (split by
/// `*`, each at atom level), so recovery sees the shape the author meant.
and parseOfFields
  (state : ParserState)
  (caseIdx : int)
  (i : int)
  : List<WT.EnumFieldSyntax> * int =
  let (f0, k0) = parseEnumField state parseAtomType (i + 1)
  let (fieldsRev, k) = parseStarFields state [ f0 ] k0
  let caseName = txt state caseIdx
  let typed =
    placeholders (List.length fieldsRev) |> List.map (fun p -> p.ToUpperInvariant())
  let commaList = String.concat ", " typed
  let starList = String.concat " * " typed
  state.diagnostics.Add
    { code = DiagnosticCode.unexpected
      severity = DiagError
      range = span (rng state caseIdx) (Seq.head fieldsRev).range
      message = "an enum case's fields go in parentheses, separated by commas"
      related = []
      hint =
        Some
          $"write `{caseName}({commaList})` rather than `{caseName} of {starList}`" }
  (List.rev fieldsRev, k)

// `* field * field …` from `k`, prepended to `fieldsRev`.
and private parseStarFields
  (state : ParserState)
  (fieldsRev : List<WT.EnumFieldSyntax>)
  (k : int)
  : List<WT.EnumFieldSyntax> * int =
  if tok state k = TStar then
    let (f, k2) = parseEnumField state parseAtomType (k + 1)
    parseStarFields state (f :: fieldsRev) k2
  else
    (fieldsRev, k)

and parseEnumDef (state : ParserState) (i : int) : WT.TypeDefinition * int =
  let (casesRev, k) = parseEnumCases state [] true i
  // `type X = |` with no case — diagnose so the `"_"` placeholder the normalizer
  // inserts (enums need ≥1 case) is honest recovery, not a silent phantom case.
  if List.isEmpty casesRev then
    // `type E = | when` or `| foo` names a case, just not as one can be named
    let lowerNameHere =
      k > i
      && (rng state k).start.row = (rng state (k - 1)).end_.row
      && (match tok state k with
          | TIdent name -> name <> ""
          | t -> isKeyword t)
    if lowerNameHere then
      err
        state
        DiagnosticCode.expected
        k
        $"an enum case name starts with an uppercase letter, found {foundDesc state k}"
    else
      err state DiagnosticCode.expected k "an enum type needs at least one case"
  (WT.TDEnum(List.rev casesRev), k)

// The cases of an enum type from `k`, last first. `first` is whether no case has been read.
and private parseEnumCases
  (state : ParserState)
  (casesRev : List<TokenRange * WT.EnumCaseSyntax>)
  (first : bool)
  (k : int)
  : List<TokenRange * WT.EnumCaseSyntax> * int =
  // Only the first case may omit the leading `|` (`type X = A | B`); every case
  // after it REQUIRES a `|`. Otherwise the following statement — which often
  // starts with an uppercase name (`type X = | A | B | C` then `Foo.bar = …`) —
  // would be swallowed as another case, orphaning the rest of that line.
  if tok state k <> TBar && not first then
    (casesRev, k)
  else
    // A `///` above a case attaches to whichever token starts it: the leading `|` usually, or the
    // case name itself when the first case omits its bar (`type X = A | B`).
    let barDoc = docOf state k
    let barRange =
      if tok state k = TBar then rng state k else zeroWidthAtEnd (rng state k)
    let k = if tok state k = TBar then k + 1 else k
    match tok state k with
    | TIdent cname when cname.Length > 0 && System.Char.IsUpper cname[0] ->
      let cnameR = rng state k
      let cnameIdx = k
      let k = k + 1
      // A space before the `(` is allowed, as for a constructor. A `(` on a later
      // line, indented at least as far as the case, is still taken as its
      // fields for recovery, but is an error; a less indented one is the next
      // statement.
      let (fields, k) =
        if tok state k = TLParen && (rng state k).start.row = cnameR.end_.row then
          parseEnumFields state k
        elif
          tok state k = TLParen
          && (rng state k).start.column >= barRange.start.column
        then
          err
            state
            DiagnosticCode.unexpected
            k
            "a case's fields start on the same line as its name"
          parseEnumFields state k
        elif tok state k = TOf then
          parseOfFields state cnameIdx k
        else
          ([], k)
      let lastR = if k > cnameIdx + 1 then rng state (k - 1) else cnameR
      let case : WT.EnumCaseSyntax =
        { range = span cnameR lastR
          name = (cnameR, cname)
          fields = fields
          description = (if barDoc <> "" then barDoc else docOf state cnameIdx) }
      parseEnumCases state ((barRange, case) :: casesRev) false k
    | _ -> (casesRev, k)

// Test-mode only: the expected side of an assertion `actual = expected`. Either
// an `error="msg"` marker (also the bare `error "msg"` shape)
// or a plain value expression. The message string is kept raw (the tokenizer has
// already unescaped it); normalization happens in the lowering.
and parseTestExpected (state : ParserState) (i : int) : WT.TestExpected * int =
  match tok state i, tok state (i + 1), tok state (i + 2) with
  | TIdent "error", TEquals, TStringLit s -> (WT.TEError s, i + 3)
  | TIdent "error", TStringLit s, _ -> (WT.TEError s, i + 2)
  | _ -> let (e, j) = parseExpr state i in (WT.TEExpr e, j)

// Parse declarations/expressions whose start column is >= minCol (offside): a
// less-indented item ends the scope. Used for the file body and, recursively,
// for nested `module X =` blocks, so module nesting is preserved (FQN paths).
// `itemScope` distinguishes module declaration rules (values require `val`)
// from script rules (a no-param `let x = …` is an expression that sequences
// with what follows).
// Test assertions are represented in every parse.
// Validation later decides whether Script, Package, or Test source may contain
// them. `itemScope` only distinguishes declaration-scope `let` from a script
// binding.
and parseItems
  (state : ParserState)
  (itemScope : ItemScope)
  (start : int)
  (minCol : int)
  : List<WT.Declaration> * List<WT.Expr> * int =
  System.Runtime.CompilerServices.RuntimeHelpers.EnsureSufficientExecutionStack()
  // each item is its own offside statement (anchored per item in the body);
  // the enclosing scope's anchor + decl anchor are restored on exit
  let savedDecl = state.declAnchor
  let r =
    withElementScope state (fun () -> parseItemsBody state itemScope start minCol)
  state.declAnchor <- savedDecl
  r

and parseItemsBody
  (state : ParserState)
  (itemScope : ItemScope)
  (start : int)
  (minCol : int)
  : List<WT.Declaration> * List<WT.Expr> * int =
  let (declsRev, exprsRev, k) = parseItemsFrom state itemScope minCol [] [] start
  (List.rev declsRev, List.rev exprsRev, k)

// The items from `k` whose start column is at least `minCol`: the declarations and the
// expressions, each prepended to its list.
and private parseItemsFrom
  (state : ParserState)
  (itemScope : ItemScope)
  (minCol : int)
  (declsRev : List<WT.Declaration>)
  (exprsRev : List<WT.Expr>)
  (k : int)
  : List<WT.Declaration> * List<WT.Expr> * int =
  if tok state k = TEOF || (rng state k).start.column < minCol then
    (declsRev, exprsRev, k)
  else
    let before = k
    setStmtCol state (rng state k).start.column
    state.declAnchor <- (rng state k).start.column
    // the item at `k`: the lists with it added, where it ends, and whether it ends the scope
    let (declsRev, exprsRev, k, scopeEnds) =
      match tok state k, tok state (k + 1) with
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
        (d :: declsRev, exprsRev, k2, false)
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
          | WT.DValue _ -> tok state k2 = TIn || itemScope = ItemScope.Script
          | _ -> false
        if asExpr then
          state.diagnostics.RemoveRange(
            diagnosticsBefore,
            state.diagnostics.Count - diagnosticsBefore
          )
          let (e, k3) = parseExpr state k
          (declsRev, e :: exprsRev, k3, false)
        else
          match d with
          | WT.DValue _ when itemScope = ItemScope.Module ->
            state.diagnostics.Add
              { code = DiagnosticCode.unexpected
                severity = DiagError
                range = rng state k
                message =
                  "Module value declarations must use 'val'; 'let' is reserved for functions and local bindings"
                related = []
                hint = Some "replace 'let' with 'val'" }
          | _ -> ()
          (d :: declsRev, exprsRev, k2, false)
      | TType, TIdent _ ->
        let (d, k2) = parseTypeDecl state k
        (d :: declsRev, exprsRev, k2, false)
      | TTrait, TIdent _ ->
        let (d, k2) = parseTraitDecl state k
        (d :: declsRev, exprsRev, k2, false)
      | TImpl, _ ->
        let (d, k2) = parseImplDecl state k
        (d :: declsRev, exprsRev, k2, false)
      | TIdent "test", TIdent _ when
        tok state (k + 2) = TEquals
        || tok state (k + 2) = TColon
        || tok state (k + 2) = TIdent "raises"
        ->
        let (d, k2) = parseTestDecl state k
        (d :: declsRev, exprsRev, k2, false)
      | TIdent "module", TIdent first ->
        let kwModule = rng state k
        let moduleCol = (rng state k).start.column
        let (partsRev, nk) = moduleNameParts state [ first ] (k + 2)
        let nameRange = span (rng state (k + 1)) (rng state (nk - 1))
        let nameStr = String.concat "." (List.rev partsRev)
        // `module X =`: body is offside-indented under the keyword.
        // `module Darklang.X` (no `=`): file-level header wrapping the rest.
        let (mdecls, mexprs, k2) =
          if tok state nk = TEquals then
            parseItems state ItemScope.Module (nk + 1) (moduleCol + 1)
          else
            parseItems state ItemScope.Module nk minCol
        if tok state nk = TEquals && List.isEmpty mdecls && List.isEmpty mexprs then
          errExpected state (nk + 1) "an indented module body"
        // A module's trailing expressions belong to the module (as `DExpr`
        // declarations), not the enclosing scope — so they pretty-print nested.
        // span the whole module (header through last child), like other decls —
        // a header-only range makes range-gated walkers (hover) skip the body
        let moduleEndR = if k2 > 0 then rng state (k2 - 1) else nameRange
        let d =
          WT.DModule
            { range = span kwModule moduleEndR
              name = (nameRange, nameStr)
              declarations = mdecls @ (mexprs |> List.map WT.DExpr)
              keywordModule = kwModule }
        // a file header consumes the rest
        (d :: declsRev, exprsRev, k2, tok state nk <> TEquals)
      | TSemicolon, _ ->
        errStatementSemicolon state k
        (declsRev, exprsRev, k + 1, false)
      | _ ->
        let (e, k2) = parseExpr state k
        // A source-level `actual = expected` is represented as a test node.
        // Its validity for this caller is decided after parsing.
        if tok state k2 = TEquals then
          let (expected, k3) = parseTestExpected state (k2 + 1)
          let endR = if k3 > 0 then rng state (k3 - 1) else rng state k2
          let d =
            WT.DAssertion
              { range = span (WT.exprRange e) endR; actual = e; expected = expected }
          (d :: declsRev, exprsRev, k3, false)
        else
          (declsRev, e :: exprsRev, k2, false)
    if scopeEnds then
      (declsRev, exprsRev, k)
    elif k = before then
      if tok state k = TEOF then
        (declsRev, exprsRev, k)
      else
        parseItemsFrom state itemScope minCol declsRev exprsRev (k + 1)
    else
      parseItemsFrom state itemScope minCol declsRev exprsRev k

and parseFile (rootScope : ItemScope) (state : ParserState) : ParseResult =
  validateLiterals state
  let (topDecls, topExprs, _) = parseItems state rootScope 0 0
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
and parseTokensAt
  (interpDepth : int)
  (rootScope : ItemScope)
  (toks : SpannedToken[])
  : ParseResult =
  let scopes = System.Collections.Generic.Stack<OffsideScope>()
  scopes.Push { stmtCol = -1; stmtExact = false }
  let state =
    { toks = toks
      tokenCount = toks.Length
      diagnostics = System.Collections.Generic.List<Diagnostic>()
      scopes = scopes
      matchArms = []
      pendingGt = 0
      pendingGtRange =
        { start = { row = 0; column = 0 }; end_ = { row = 0; column = 0 } }
      declAnchor = -1
      depth = 0
      abandoned = false
      steps = 0
      interpDepth = interpDepth }
  try
    parseFile rootScope state
  with :? System.InsufficientExecutionStackException when interpDepth = 0 ->
    { parsed = None; diagnostics = [ stackExhaustionDiagnostic () ] }

and parseTokens (toks : SpannedToken[]) : ParseResult =
  parseTokensAt 0 ItemScope.Script toks

let private parseSyntaxWithRootScope
  (rootScope : ItemScope)
  (source : string)
  : ParseResult =
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
    let result = parseTokensAt 0 rootScope (List.toArray toksList)
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

let private parseRecovering
  (rootScope : ItemScope)
  (validate : WT.SourceFile -> List<Validation.Issue>)
  (source : string)
  : ParseResult =
  try
    let result = parseSyntaxWithRootScope rootScope source
    let syntaxDiagnostics = result.diagnostics
    // Validation owns checks that inspect the whole parsed syntax tree.
    // Run them only when parsing reports no errors, because placeholders inserted
    // during error recovery could otherwise produce misleading extra diagnostics.
    let validationDiagnostics =
      match syntaxDiagnostics, result.parsed with
      | [], Some(WT.SourceFile sourceFile) ->
        sourceFile |> validate |> List.map diagnosticOfValidationIssue
      | _ -> []
    { result with diagnostics = syntaxDiagnostics @ validationDiagnostics }
  with :? System.InsufficientExecutionStackException ->
    { parsed = None; diagnostics = [ stackExhaustionDiagnostic () ] }

/// Parse for tooling: return a recoverable tree and include mode-independent
/// structural diagnostics after a clean syntax pass.
let parse (source : string) : ParseResult =
  parseRecovering ItemScope.Script Validation.validateStructure source

let private withPurpose (mode : Validation.Mode) (sourceFile : WT.SourceFile) =
  Validation.validateStructure sourceFile
  @ Validation.validatePurpose mode sourceFile

/// `parse` with the Script purpose rules alongside the structural ones. The tree
/// comes back either way.
let parseScript (source : string) : ParseResult =
  parseRecovering ItemScope.Script (withPurpose Validation.Script) source

/// `parse` for a package file: declarations at the file root, and the Package
/// purpose rules alongside the structural ones. The tree comes back either way.
let parsePackage (source : string) : ParseResult =
  parseRecovering ItemScope.Module (withPurpose Validation.Package) source

/// Parse for execution: syntax, structural, and file-purpose validation run
/// once, and only a validated source file can be returned on success.
let parseFor
  (mode : Validation.Mode)
  (source : string)
  : Result<Validation.ValidatedSourceFile, List<Diagnostic>> =
  try
    let rootScope =
      match mode with
      | Validation.Package -> ItemScope.Module
      | Validation.Script
      | Validation.Test -> ItemScope.Script
    let result = parseSyntaxWithRootScope rootScope source
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
            range =
              { start = { row = 0; column = 0 }; end_ = { row = 0; column = 0 } }
            message = "Parser did not produce a source tree"
            related = []
            hint = None } ]
  with :? System.InsufficientExecutionStackException ->
    Error [ stackExhaustionDiagnostic () ]

/// Kept as a compatibility entrypoint. Test syntax has the same parse shape as
/// all other source; parseFor Validation.Test applies the Test purpose rules.
let parseTestFile (source : string) : ParseResult = parse source

/// Render a diagnostic for humans: code, position, message, a source snippet
/// with caret markers, related locations, and the hint if any. E.g.
///   error[PARSE-UNCLOSED] at 1:9: expected ']' to close the '[' at line 1:9, found end of file
///     1 | let x = [1; 2
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

/// Render all of a parse's diagnostics, so that one run shows every mistake to fix. They
/// go in source order. A mistake made more than once (the same code and message, with
/// numbers ignored, as for `1L` and `2L`) is shown once, with the other places it
/// occurs listed under it. At most `maxShown` mistakes are shown; a last line counts
/// the rest.
let renderDiagnostics (source : string) (diagnostics : List<Diagnostic>) : string =
  let maxShown = 10
  let maxPlaces = 20
  let kind (d : Diagnostic) =
    (d.code, d.message |> String.filter (fun c -> not (System.Char.IsDigit c)))
  let position (d : Diagnostic) =
    $"{d.range.start.row + 1}:{d.range.start.column + 1}"
  let groups =
    diagnostics
    |> List.distinct
    |> List.sortBy (fun d -> (d.range.start.row, d.range.start.column))
    // in order of first occurrence (Prelude's `List.groupBy` gives a Map)
    |> Microsoft.FSharp.Collections.List.groupBy kind
    |> List.map snd
  let renderGroup (group : List<Diagnostic>) =
    match group with
    | [] -> ""
    | [ d ] -> renderDiagnostic source d
    | d :: rest ->
      let places = rest |> List.truncate maxPlaces |> List.map position
      let unlisted = rest.Length - places.Length
      let places =
        if unlisted > 0 then places @ [ $"and {unlisted} more" ] else places
      renderDiagnostic source d + "\n  the same at " + String.concat ", " places
  let shown = List.truncate maxShown groups
  let hidden = groups |> List.skip shown.Length |> List.sumBy List.length
  let more =
    match hidden with
    | 0 -> []
    | 1 -> [ "1 more error not shown" ]
    | n -> [ $"{n} more errors not shown" ]
  (List.map renderGroup shown @ more) |> String.concat "\n"
