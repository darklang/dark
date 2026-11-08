/// Lexer for the Darklang syntax this repo uses. Produces `Tokenizer.Token`s
/// with source ranges for the parser.
///
/// Regular string/char escapes are processed (`unescape`); triple-quoted strings
/// stay raw.
module LibParser.Lexer

open LibParser.Tokenizer // Pos, TokenRange, Token

type TriviaKind =
  | LineComment // `// …` (and `////…`)
  | DocComment // `/// …`
  | BlockComment // `(* … *)`

/// A comment between the previous token and the one carrying it. Whitespace is
/// not stored — blank lines are derivable from the token/trivia range gaps.
type Trivia = { kind : TriviaKind; text : string; range : TokenRange }

type SpannedToken =
  { token : Token
    text : string
    range : TokenRange
    // accumulated `///` doc-comment text immediately preceding this token (the
    // declaration it documents), if any. Used to recover fn/type descriptions.
    docComment : string option
    // comments between the previous token and this one, in source order — kept
    // so tooling (formatter, lossless round-trip) can reproduce the source.
    // Trailing comments at EOF land on the TEOF token.
    leadingTrivia : List<Trivia>
    // Already reported by the lexer; avoid a duplicate parser error.
    lexError : bool }

/// Decode the escape starting at `source[escapeStartIndex]`, which must be `\`.
/// Returns `Some(charsConsumed, decodedText)`, or `None` for an invalid escape:
/// unknown escape letter, short/non-hex Unicode escape, surrogate, or codepoint
/// above `0x10FFFF`. `unescape` and the validators share this function so they
/// cannot drift.
let private decodeEscape
  (source : string)
  (escapeStartIndex : int)
  : Option<int * string> =
  let sourceLength = source.Length
  let hexInt (hexStartIndex : int) (digitCount : int) : int option =
    if hexStartIndex + digitCount <= sourceLength then
      match
        System.Int32.TryParse(
          source.Substring(hexStartIndex, digitCount),
          System.Globalization.NumberStyles.AllowHexSpecifier,
          null
        )
      with
      | true, value -> Some value
      | _ -> None
    else
      None
  // a Unicode scalar value → its UTF-16 string (a surrogate pair above the BMP)
  let scalar (hexStartIndex : int) (digitCount : int) : string option =
    match hexInt hexStartIndex digitCount with
    | Some codepoint when
      codepoint >= 0
      && codepoint <= 0x10FFFF
      && not (codepoint >= 0xD800 && codepoint <= 0xDFFF)
      ->
      Some(System.Char.ConvertFromUtf32 codepoint)
    | _ -> None
  if escapeStartIndex + 1 >= sourceLength then
    None // trailing backslash
  else
    match source[escapeStartIndex + 1] with
    | 'n' -> Some(2, "\n")
    | 't' -> Some(2, "\t")
    | 'r' -> Some(2, "\r")
    | 'a' -> Some(2, string (char 7)) // bell
    | 'b' -> Some(2, string (char 8)) // backspace
    | 'v' -> Some(2, string (char 11)) // vertical tab
    | 'f' -> Some(2, string (char 12)) // form feed
    | '\\' -> Some(2, "\\")
    | '"' -> Some(2, "\"")
    | '\'' -> Some(2, "'")
    | '/' -> Some(2, "/")
    | '0' -> Some(2, "\000")
    | '{' -> Some(2, "{")
    | '}' -> Some(2, "}")
    | 'x' ->
      hexInt (escapeStartIndex + 2) 2
      |> Option.map (fun value -> (4, string (char value))) // \xHH
    // \XHHHH and \uHHHH both denote a Unicode scalar, so both reject surrogates /
    // out-of-range codepoints via `scalar` (a lone `\XD800` is invalid, like `\uD800`)
    | 'X' -> scalar (escapeStartIndex + 2) 4 |> Option.map (fun text -> (6, text)) // \XHHHH
    | 'u' -> scalar (escapeStartIndex + 2) 4 |> Option.map (fun text -> (6, text)) // \uHHHH (BMP)
    | 'U' -> scalar (escapeStartIndex + 2) 8 |> Option.map (fun text -> (10, text)) // \UHHHHHHHH
    | _ -> None

/// Is there an invalid escape in `content` at or after `index`? Tail-recursive, so a loop.
let rec private badEscapeFrom (content : string) (index : int) : bool =
  if index >= content.Length then
    false
  elif content[index] = '\\' then
    match decodeEscape content index with
    | Some(charsConsumed, _) -> badEscapeFrom content (index + charsConsumed)
    | None -> true
  else
    badEscapeFrom content (index + 1)

/// Process escape sequences in regular string/char content. Triple-quoted
/// strings stay raw. Invalid escapes keep the backslash as-is.
/// Module-level so the parser can reuse it for interpolated-string literal parts.
/// The result is NFC-normalized so decomposed and composed graphemes have the
/// same stored form.
let unescape (source : string) : string =
  let decoded =
    if not (source.Contains '\\') then
      source
    else
      // A builder and an index on purpose: this decodes every escaped literal, in one pass.
      let builder = System.Text.StringBuilder(source.Length)
      let sourceLength = source.Length
      let mutable index = 0
      while index < sourceLength do
        if source[index] = '\\' then
          match decodeEscape source index with
          | Some(charsConsumed, text) ->
            builder.Append text |> ignore
            index <- index + charsConsumed
          | None ->
            builder.Append source[index] |> ignore
            index <- index + 1
        else
          builder.Append source[index] |> ignore
          index <- index + 1
      builder.ToString()
  String.normalize decoded

/// Does the raw inner text of a regular string/char contain an invalid escape?
let hasInvalidEscape (content : string) : bool =
  if not (content.Contains '\\') then false else badEscapeFrom content 0

// The scanners below are tail-recursive, so they compile to loops: no mutable, and no
// allocation.

/// The index of the first `\n` at or after `index`, or `endLimit`.
let rec private lineEnd (source : string) (endLimit : int) (index : int) : int =
  if index < endLimit && source[index] <> '\n' then
    lineEnd source endLimit (index + 1)
  else
    index

let private skipLineComment
  (source : string)
  (endLimit : int)
  (startIndex : int)
  : int =
  lineEnd source endLimit (startIndex + 2)

let rec private skipBlockCommentFrom
  (source : string)
  (endLimit : int)
  (index : int)
  (commentDepth : int)
  : int =
  if index >= endLimit || commentDepth <= 0 then
    index
  elif index + 1 < endLimit && source[index] = '(' && source[index + 1] = '*' then
    skipBlockCommentFrom source endLimit (index + 2) (commentDepth + 1)
  elif index + 1 < endLimit && source[index] = '*' && source[index + 1] = ')' then
    skipBlockCommentFrom source endLimit (index + 2) (commentDepth - 1)
  else
    skipBlockCommentFrom source endLimit (index + 1) commentDepth

let private skipBlockComment
  (source : string)
  (endLimit : int)
  (startIndex : int)
  : int =
  skipBlockCommentFrom source endLimit (startIndex + 2) 1

let rec private skipRawStringFrom
  (source : string)
  (endLimit : int)
  (index : int)
  : int =
  if index >= endLimit then
    index
  elif
    index + 2 < endLimit
    && source[index] = '"'
    && source[index + 1] = '"'
    && source[index + 2] = '"'
  then
    index + 3
  else
    skipRawStringFrom source endLimit (index + 1)

let private skipRawString
  (source : string)
  (endLimit : int)
  (startIndex : int)
  : int =
  skipRawStringFrom source endLimit (startIndex + 3)

/// Past the closing `quote` at or after `index`, skipping `\` escapes; `endLimit` (or a
/// little past it, after a trailing `\`) when there is none.
let rec private skipQuotedFrom
  (source : string)
  (endLimit : int)
  (quote : char)
  (index : int)
  : int =
  if index >= endLimit then index
  elif source[index] = '\\' then skipQuotedFrom source endLimit quote (index + 2)
  elif source[index] = quote then index + 1
  else skipQuotedFrom source endLimit quote (index + 1)

let private skipRegularString
  (source : string)
  (endLimit : int)
  (startIndex : int)
  : int =
  skipQuotedFrom source endLimit '"' (startIndex + 1)

/// The end of the identifier characters (letters, digits, `_` and `'`) from `index`.
let rec private identEnd (source : string) (index : int) : int =
  if
    index < source.Length
    && (System.Char.IsLetterOrDigit source[index]
        || source[index] = '_'
        || source[index] = '\'')
  then
    identEnd source (index + 1)
  else
    index

/// The end of a type variable's name (letters, digits and `_`) from `index`.
let rec private typeVariableEnd (source : string) (index : int) : int =
  if
    index < source.Length
    && (System.Char.IsLetterOrDigit source[index] || source[index] = '_')
  then
    typeVariableEnd source (index + 1)
  else
    index

/// The end of the run of digits from `index`.
let rec private digitsEnd (source : string) (index : int) : int =
  if index < source.Length && System.Char.IsDigit source[index] then
    digitsEnd source (index + 1)
  else
    index

/// From `index`, the index of the closing ``` `` ``` of a backtick identifier, or of the line's
/// end (or the source's last char) when there is none.
let rec private backtickClose (source : string) (index : int) : int =
  if
    index + 1 < source.Length
    && not (source[index] = '`' && source[index + 1] = '`')
    && source[index] <> '\n'
  then
    backtickClose source (index + 1)
  else
    index

/// `position` moved over `source[index..endIndex)`.
let rec private advanceOver
  (source : string)
  (position : Pos)
  (index : int)
  (endIndex : int)
  : Pos =
  if index >= endIndex then
    position
  else
    let position =
      if source[index] = '\n' then
        { row = position.row + 1; column = 0 }
      else
        { position with column = position.column + 1 }
    advanceOver source position (index + 1) endIndex


let rec private interpCloseFrom
  (source : string)
  (endLimit : int)
  (scanIndex : int)
  (braceDepth : int)
  : int =
  if scanIndex >= endLimit then
    -1
  // Comments are code trivia, so braces inside them cannot close an
  // interpolation. Block comments nest just like top-level lexer comments.
  elif
    source[scanIndex] = '/'
    && scanIndex + 1 < endLimit
    && source[scanIndex + 1] = '/'
  then
    interpCloseFrom
      source
      endLimit
      (skipLineComment source endLimit scanIndex)
      braceDepth
  elif
    source[scanIndex] = '('
    && scanIndex + 1 < endLimit
    && source[scanIndex + 1] = '*'
  then
    interpCloseFrom
      source
      endLimit
      (skipBlockComment source endLimit scanIndex)
      braceDepth
  // A raw triple-quoted string may contain unescaped quotes and braces.
  elif
    source[scanIndex] = '"'
    && scanIndex + 2 < endLimit
    && source[scanIndex + 1] = '"'
    && source[scanIndex + 2] = '"'
  then
    interpCloseFrom
      source
      endLimit
      (skipRawString source endLimit scanIndex)
      braceDepth
  elif source[scanIndex] = '"' then
    interpCloseFrom
      source
      endLimit
      (skipRegularString source endLimit scanIndex)
      braceDepth
  // Skip char literals so braces inside them do not affect interpolation
  // depth. A leading `'` that is not a char literal, such as type var `'a` or
  // tick-ident tail `x'`, falls through harmlessly.
  elif
    source[scanIndex] = '\''
    && scanIndex + 1 < endLimit
    && source[scanIndex + 1] = '\\'
  then
    // Start at the `\` so `'\''` skips the escape before looking for the
    // closing quote.
    interpCloseFrom
      source
      endLimit
      (skipQuotedFrom source endLimit '\'' (scanIndex + 1))
      braceDepth
  elif
    source[scanIndex] = '\''
    && scanIndex + 2 < endLimit
    && source[scanIndex + 2] = '\''
  then
    interpCloseFrom source endLimit (scanIndex + 3) braceDepth
  elif source[scanIndex] = '{' then
    interpCloseFrom source endLimit (scanIndex + 1) (braceDepth + 1)
  elif source[scanIndex] = '}' && braceDepth = 0 then
    scanIndex
  elif source[scanIndex] = '}' then
    interpCloseFrom source endLimit (scanIndex + 1) (braceDepth - 1)
  else
    interpCloseFrom source endLimit (scanIndex + 1) braceDepth

/// Find the `}` that closes an interpolation expression region.
/// `startIndex` is just past the opening `{`. The scan tracks nested braces and skips
/// embedded string and char literals, so braces inside them do not desync the
/// scan. Returns `-1` if unclosed. The tokenizer, escape validator, and parser
/// all use this scanner so they agree on where interpolation regions end.
let findInterpExprClose (source : string) (endLimit : int) (startIndex : int) : int =
  interpCloseFrom source endLimit startIndex 0


let rec private badEscapeInterpFrom (inner : string) (index : int) : bool =
  let innerLength = inner.Length
  if index >= innerLength then
    false
  elif index + 1 < innerLength && inner[index] = '{' && inner[index + 1] = '{' then
    badEscapeInterpFrom inner (index + 2)
  elif index + 1 < innerLength && inner[index] = '}' && inner[index + 1] = '}' then
    badEscapeInterpFrom inner (index + 2)
  elif inner[index] = '{' then
    // skip the `{ … }` interpolation region
    let closeIndex = findInterpExprClose inner innerLength (index + 1)
    badEscapeInterpFrom
      inner
      (if closeIndex < 0 then innerLength else closeIndex + 1)
  elif inner[index] = '\\' then
    match decodeEscape inner index with
    | Some(charsConsumed, _) -> badEscapeInterpFrom inner (index + charsConsumed)
    | None -> true
  else
    badEscapeInterpFrom inner (index + 1)

/// Like `hasInvalidEscape`, but for regular `$"…"` interpolated strings.
/// `{{`/`}}` are literal braces and `{ … }` regions are code, so only literal
/// string text is escape-checked.
let hasInvalidEscapeInterp (inner : string) : bool = badEscapeInterpFrom inner 0

let rec private singleCloseBraceFrom
  (inner : string)
  (isRaw : bool)
  (index : int)
  : bool =
  let innerLength = inner.Length
  if index >= innerLength then
    false
  elif index + 1 < innerLength && inner[index] = '{' && inner[index + 1] = '{' then
    singleCloseBraceFrom inner isRaw (index + 2)
  elif index + 1 < innerLength && inner[index] = '}' && inner[index + 1] = '}' then
    singleCloseBraceFrom inner isRaw (index + 2)
  elif inner[index] = '{' then
    let closeIndex = findInterpExprClose inner innerLength (index + 1)
    singleCloseBraceFrom
      inner
      isRaw
      (if closeIndex < 0 then innerLength else closeIndex + 1)
  elif inner[index] = '}' then
    true
  elif inner[index] = '\\' && not isRaw && index + 1 < innerLength then
    singleCloseBraceFrom inner isRaw (index + 2)
  else
    singleCloseBraceFrom inner isRaw (index + 1)

/// Does interpolated-string literal text contain a single unescaped `}`?
/// Literal braces must be doubled (`}}`) or escaped (`\}` in regular strings).
/// Braces inside `{ expression }` regions are code and are skipped.
let hasSingleCloseBraceInterp (inner : string) (isRaw : bool) : bool =
  singleCloseBraceFrom inner isRaw 0


/// The parser parses each `{expr}` body recursively. Nested interpolated strings
/// therefore increase recursion depth. Cap it so pathological nesting cannot
/// overflow the process stack. The tokenizer itself does not recurse here;
/// `scanInterp` skips `{expr}` regions iteratively.
let maxInterpNesting = 64

/// The reserved words, each lexed as its own token. A reserved word is a name only
/// when written in double backticks: ``match``. This is the one list: Dark reads it
/// as `Builtin.parserKeywords`, so the pretty printer escapes these words and
/// `docs syntax` lists them.
///
/// `def` is left out on purpose, so `(def: Type)` works. Words that are special in
/// one place only (`module`, `test`, `for`, `as`, `Dict`, `List`) lex as names, and
/// the parser reads them where they matter.
let keywords : List<string * Token> =
  [ "let", TLet
    // `val x = e` is a value declaration. `let` is reserved for functions and
    // local/script bindings. The distinct token lets `parseItems` keep them apart.
    "val", TVal
    "in", TIn
    "if", TIf
    "elif", TElif
    "then", TThen
    "else", TElse
    "type", TType
    "trait", TTrait
    "impl", TImpl
    "of", TOf
    "match", TMatch
    "with", TWith
    "fun", TFun
    "when", TWhen
    "true", TTrue
    "false", TFalse
    "_", TUnderscore ]

let private keywordTokens = readOnlyDict keywords

let tokenize
  (input : string)
  : Result<List<SpannedToken> * List<TokenRange * string>, string> =
  let inputLength = input.Length

  // `///` doc comments lex as trivia, but their text also lands on the next
  // emitted token. `emit` consumes and clears this.
  // Mutable on purpose, like the two below: threading them through `go` costs a tuple per token.
  let mutable pendingDocComment : string option = None

  // Comments scanned since the last emitted token. `emit` drains them into
  // `leadingTrivia`.
  let pendingTrivia = ResizeArray<Trivia>()

  // Lexical diagnostics collected during recovery. Defined before trivia
  // scanning so an unterminated block comment can report its own range.
  let diagnostics = System.Collections.Generic.List<TokenRange * string>()
  let addDiagnostic (range : TokenRange) (message : string) =
    diagnostics.Add((range, message))

  let step (position : Pos) (character : char) : Pos =
    if character = '\n' then
      { row = position.row + 1; column = 0 }
    else
      { position with column = position.column + 1 }

  let advance (position : Pos) (startIndex : int) (endIndex : int) : Pos =
    advanceOver input position startIndex endIndex

  let rec skipTrivia (index : int) (position : Pos) : int * Pos =
    if index >= inputLength then
      (index, position)
    else
      let character = input[index]
      if
        character = ' ' || character = '\t' || character = '\r' || character = '\n'
      then
        skipTrivia (index + 1) (step position character)
      elif character = '/' && index + 1 < inputLength && input[index + 1] = '/' then
        let lineEndIndex = lineEnd input inputLength index
        // `/// …` is a doc comment for the next declaration. `////` and plain
        // `//` are ordinary comments.
        let isDocComment =
          index + 2 < inputLength
          && input[index + 2] = '/'
          && (index + 3 >= inputLength || input[index + 3] <> '/')
        if isDocComment then
          let text = input.Substring(index + 3, lineEndIndex - (index + 3)).Trim()
          pendingDocComment <-
            Some(
              match pendingDocComment with
              | Some existingText -> existingText + " " + text
              | None -> text
            )
        pendingTrivia.Add
          { kind = (if isDocComment then DocComment else LineComment)
            text = input.Substring(index, lineEndIndex - index)
            range = { start = position; end_ = advance position index lineEndIndex } }
        skipTrivia lineEndIndex (advance position index lineEndIndex)
      elif
        character = '('
        && index + 1 < inputLength
        && input[index + 1] = '*'
        && not (index + 2 < inputLength && input[index + 2] = ')')
        && not (
          index + 3 < inputLength && input[index + 2] = '*' && input[index + 3] = ')'
        )
      then
        // Nested block comment delimited by `(*` and `*)`. `(*)` and `(**)` are
        // the multiply and exponentiation operator sections, so they are excluded here.
        let rec skipBlock (scanIndex : int) (commentDepth : int) : int * bool =
          if
            scanIndex + 1 < inputLength
            && input[scanIndex] = '('
            && input[scanIndex + 1] = '*'
          then
            skipBlock (scanIndex + 2) (commentDepth + 1)
          elif
            scanIndex + 1 < inputLength
            && input[scanIndex] = '*'
            && input[scanIndex + 1] = ')'
          then
            if commentDepth = 0 then
              (scanIndex + 2, true)
            else
              skipBlock (scanIndex + 2) (commentDepth - 1)
          elif scanIndex >= inputLength then
            (scanIndex, false)
          else
            skipBlock (scanIndex + 1) commentDepth
        let (endIndex, closed) = skipBlock (index + 2) 0
        let commentRange =
          { start = position; end_ = advance position index endIndex }
        pendingTrivia.Add
          { kind = BlockComment
            text = input.Substring(index, endIndex - index)
            range = commentRange }
        if not closed then addDiagnostic commentRange "unterminated block comment"
        skipTrivia endIndex (advance position index endIndex)
      else
        (index, position)

  // longest-match operators (order matters)
  let operators : (string * Token) list =
    [ "...", TDotDotDot
      "**", TStarStar
      "->", TArrow
      "==", TEqEq
      "!=", TNeq
      "<<", TShl
      "<=", TLte
      ">>", TShr
      ">=", TGte
      "&&", TAnd
      "||", TOr
      "|>", TPipe
      "+", TPlus
      "-", TMinus
      "*", TStar
      "/", TSlash
      "(", TLParen
      ")", TRParen
      "{", TLBrace
      "}", TRBrace
      "[", TLBracket
      "]", TRBracket
      ":", TColon
      ",", TComma
      ";", TSemicolon
      ".", TDot
      "=", TEquals
      "?", TQuestion
      "!", TNot
      "<", TLt
      ">", TGt
      "&", TBitAnd
      "^", TBitXor
      "~", TBitNot
      "%", TPercent
      "|", TBar ]

  // Operators other languages have and Dark does not, refused with what to write. Each
  // still lexes as a `+`, flagged as a lex error, so the parser reads on past it and
  // reports the file's other mistakes, rather than tripping over the gap it left.
  let refused : (string * string) list =
    [ "::", "'::' is not Dark; a list is `[head, ...tail]`"
      "@", "'@' is not Dark; join lists with `[...a, ...b]`"
      "++", "'++' is not Dark; join Strings with `+`" ]

  let keyword (text : string) : Token =
    match keywordTokens.TryGetValue text with
    | true, token -> token
    // `___` is the blank-name placeholder: an identifier with an empty name.
    | false, _ when text = "___" -> TIdent ""
    | false, _ -> TIdent text

  let matchesAt (text : string) (index : int) : bool =
    index + text.Length <= inputLength
    && System.String.CompareOrdinal(input, index, text, 0, text.Length) = 0

  let emit
    (token : Token)
    (startIndex : int)
    (endIndex : int)
    (position : Pos)
    : SpannedToken * int * Pos =
    let endPosition = advance position startIndex endIndex
    let docComment = pendingDocComment
    pendingDocComment <- None
    let leadingTrivia = List.ofSeq pendingTrivia
    pendingTrivia.Clear()
    ({ token = token
       text = input.Substring(startIndex, endIndex - startIndex)
       range = { start = position; end_ = endPosition }
       docComment = docComment
       leadingTrivia = leadingTrivia
       lexError = false },
     endIndex,
     endPosition)

  // An int literal's token, for the type its suffix names (`None`: no such suffix).
  // A signed type's |MinValue| lexes to MinValue so the negated literal can exist
  // (`-128s8`); the parser refuses it un-negated.
  let intToken (digits : string) (suffix : string) : Option<Result<Token, string>> =
    // `minValue` comes before `tryParse` so it fixes 'a, which picks the overload. An
    // unsigned type has no negated literal, so it passes "" as its magnitude.
    let sized
      (typeName : string)
      (minMagnitude : string)
      (minValue : 'a)
      (tryParse : string -> bool * 'a)
      (token : 'a -> Token)
      =
      match tryParse digits with
      | true, value -> Some(Ok(token value))
      | _ when digits = minMagnitude -> Some(Ok(token minValue))
      | _ -> Some(Error $"out of range for {typeName}: {digits}{suffix}")
    match suffix with
    | ""
    | "s"
    | "s64" ->
      sized
        "Int64"
        "9223372036854775808"
        System.Int64.MinValue
        System.Int64.TryParse
        TInt64
    | "u"
    | "u64" -> sized "UInt64" "" 0UL System.UInt64.TryParse TUInt64
    | "s8" -> sized "Int8" "128" System.SByte.MinValue System.SByte.TryParse TInt8
    | "u8" -> sized "UInt8" "" 0uy System.Byte.TryParse TUInt8
    | "s16" ->
      sized "Int16" "32768" System.Int16.MinValue System.Int16.TryParse TInt16
    | "u16" -> sized "UInt16" "" 0us System.UInt16.TryParse TUInt16
    | "s32" ->
      sized "Int32" "2147483648" System.Int32.MinValue System.Int32.TryParse TInt32
    | "u32" -> sized "UInt32" "" 0u System.UInt32.TryParse TUInt32
    | "s128" ->
      sized
        "Int128"
        "170141183460469231731687303715884105728"
        System.Int128.MinValue
        System.Int128.TryParse
        TInt128
    | "u128" ->
      sized "UInt128" "" System.UInt128.Zero System.UInt128.TryParse TUInt128
    | "z" -> Some(Ok(TBigInt(System.Numerics.BigInteger.Parse digits)))
    | _ -> None

  let rec scanString (scanIndex : int) (closing : char) : Result<int, string> =
    if scanIndex >= inputLength then
      Error "Unterminated string literal"
    elif input[scanIndex] = '\\' && scanIndex + 1 < inputLength then
      scanString (scanIndex + 2) closing
    elif input[scanIndex] = closing then
      Ok(scanIndex + 1)
    else
      scanString (scanIndex + 1) closing

  // Recovery for unterminated lexemes: scan to the current line end or EOF so a
  // half-typed string/char does not swallow the rest of the document.
  let rec scanToLineEnd (scanIndex : int) : int =
    if scanIndex >= inputLength || input[scanIndex] = '\n' then
      scanIndex
    else
      scanToLineEnd (scanIndex + 1)

  // Scan `$"text {expr} text"` or `$"""…"""`. The token carries no payload; the
  // parser re-reads the source text and scans `{expr}` bodies itself. This only
  // needs to find the token end while skipping escapes, literal braces, and
  // interpolation regions with `findInterpExprClose`.
  let scanInterp (startIndex : int) : Result<Token * int, string> =
    // triple-quoted `$"""…"""` (raw; a single `"` is literal text)
    let isTripleQuoted =
      startIndex + 3 < inputLength
      && input[startIndex + 1] = '"'
      && input[startIndex + 2] = '"'
      && input[startIndex + 3] = '"'
    let atClose (scanIndex : int) : bool =
      if isTripleQuoted then
        scanIndex + 2 < inputLength
        && input[scanIndex] = '"'
        && input[scanIndex + 1] = '"'
        && input[scanIndex + 2] = '"'
      else
        input[scanIndex] = '"'
    let closeLength = if isTripleQuoted then 3 else 1
    let rec loop (scanIndex : int) : Result<int, string> =
      if scanIndex >= inputLength then
        Error "Unterminated interpolated string"
      elif atClose scanIndex then
        Ok(scanIndex + closeLength)
      elif
        input[scanIndex] = '\\' && scanIndex + 1 < inputLength && not isTripleQuoted
      then
        loop (scanIndex + 2)
      // `{{` / `}}` are escaped literal braces, not an interpolation
      elif
        input[scanIndex] = '{'
        && scanIndex + 1 < inputLength
        && input[scanIndex + 1] = '{'
      then
        loop (scanIndex + 2)
      elif
        input[scanIndex] = '}'
        && scanIndex + 1 < inputLength
        && input[scanIndex + 1] = '}'
      then
        loop (scanIndex + 2)
      elif input[scanIndex] = '{' then
        match findInterpExprClose input inputLength (scanIndex + 1) with
        | -1 -> Error "Unterminated interpolated expression"
        | closeIndex -> loop (closeIndex + 1)
      else
        loop (scanIndex + 1)
    match loop (if isTripleQuoted then startIndex + 4 else startIndex + 2) with
    | Ok endIndex -> Ok(TInterpString, endIndex)
    | Error message -> Error message

  let rec go
    (index : int)
    (position : Pos)
    (tokensRev : List<SpannedToken>)
    : Result<List<SpannedToken>, string> =
    let (index, position) = skipTrivia index position
    if index >= inputLength then
      let (eofToken, _, _) = emit TEOF index index position
      Ok(List.rev (eofToken :: tokensRev))
    else
      let character = input[index]
      // Tolerate compiler-syntax `=>` by lexing `=` then `>`. The parser rejects
      // it later, but tokenization can continue.
      if character = '=' && index + 1 < inputLength && input[index + 1] = '>' then
        let (spannedToken, nextIndex, nextPosition) =
          emit TEquals index (index + 1) position
        go nextIndex nextPosition (spannedToken :: tokensRev)
      // backtick identifiers: ``name``
      elif character = '`' && index + 1 < inputLength && input[index + 1] = '`' then
        let closeIndex = backtickClose input (index + 2)
        if
          closeIndex + 1 < inputLength
          && input[closeIndex] = '`'
          && input[closeIndex + 1] = '`'
        then
          let (spannedToken, nextIndex, nextPosition) =
            emit
              (TIdent(input.Substring(index + 2, closeIndex - index - 2)))
              index
              (closeIndex + 2)
              position
          go nextIndex nextPosition (spannedToken :: tokensRev)
        else
          // unterminated ``…`` — best-effort ident to end of line
          let endIndex = scanToLineEnd (index + 2)
          let (spannedToken, nextIndex, nextPosition) =
            emit
              (TIdent(input.Substring(index + 2, endIndex - index - 2)))
              index
              endIndex
              position
          addDiagnostic spannedToken.range "unterminated backtick identifier"
          go nextIndex nextPosition (spannedToken :: tokensRev)
      // identifiers / keywords
      elif System.Char.IsLetter character || character = '_' then
        let endIndex = identEnd input (index + 1)
        let (spannedToken, nextIndex, nextPosition) =
          emit
            (keyword (input.Substring(index, endIndex - index)))
            index
            endIndex
            position
        go nextIndex nextPosition (spannedToken :: tokensRev)
      // numbers: digits, an optional fraction and exponent, then a suffix naming
      // the type (`1s8`, `1.5f32`, `19.99dec`). No suffix is Int64 or Float.
      elif System.Char.IsDigit character then
        let digitEndIndex = digitsEnd input (index + 1)
        let hasFraction =
          digitEndIndex + 1 < inputLength
          && input[digitEndIndex] = '.'
          && System.Char.IsDigit input[digitEndIndex + 1]
        let fractionEndIndex =
          if hasFraction then digitsEnd input (digitEndIndex + 1) else digitEndIndex
        // `e` is an exponent only when digits follow it, so `1e` stays one bad literal
        let exponentEndIndex =
          let signIndex = fractionEndIndex + 1
          let digitsIndex =
            if
              signIndex < inputLength
              && (input[signIndex] = '+' || input[signIndex] = '-')
            then
              signIndex + 1
            else
              signIndex
          if
            fractionEndIndex < inputLength
            && (input[fractionEndIndex] = 'e' || input[fractionEndIndex] = 'E')
            && digitsIndex < inputLength
            && System.Char.IsDigit input[digitsIndex]
          then
            digitsEnd input digitsIndex
          else
            fractionEndIndex
        let hasExponent = exponentEndIndex > fractionEndIndex
        // A number token ends at a non-identifier boundary: glued text like `123abc`
        // is one malformed literal, not two tokens.
        let endIndex = identEnd input exponentEndIndex
        let suffix = input.Substring(exponentEndIndex, endIndex - exponentEndIndex)
        let body = input.Substring(index, exponentEndIndex - index)
        let invariant = System.Globalization.CultureInfo.InvariantCulture
        let floatStyle = System.Globalization.NumberStyles.Float
        // Past a format's range the value comes back infinite, and the parser reports it
        let float () =
          match System.Double.TryParse(body, floatStyle, invariant) with
          | true, value -> (TFloat value, None)
          | _ -> (TFloat 0.0, Some $"malformed float literal: {body}{suffix}")
        // A suffix another language uses (F#'s `L`, `uy`, `m`, ...) names the Dark spelling.
        let invalid placeholder =
          let written = input.Substring(index, endIndex - index)
          let fraction = hasFraction || hasExponent
          let spelling =
            match suffix with
            | "L"
            | "l" when not fraction -> Some("an Int64", body)
            | "UL"
            | "ul"
            | "uL"
            | "Ul" when not fraction -> Some("a UInt64", body + "u")
            | "y" when not fraction -> Some("an Int8", body + "s8")
            | "uy" when not fraction -> Some("a UInt8", body + "u8")
            | "us" when not fraction -> Some("a UInt16", body + "u16")
            | "I" when not fraction -> Some("a BigInt", body + "z")
            | "m"
            | "M" when not hasExponent -> Some("a Decimal", body + "dec")
            | "f"
            | "F" when fraction -> Some("a Float32", body + "f32")
            | _ -> None
          match spelling with
          | Some(typeName, dark) ->
            (placeholder,
             Some
               $"invalid number literal: {written}; {typeName} is written `{dark}`")
          | None -> (placeholder, Some $"invalid number literal: {written}")
        let (token, diagnostic) =
          match suffix with
          | "dec" when hasExponent ->
            (TDecimal 0m,
             Some $"a Decimal literal is written without an exponent: {body}dec")
          | "dec" ->
            // .NET rounds digits past 28-29 significant ones rather than refusing them,
            // so a literal is accepted only if it prints back as written (leading zeros
            // of the whole part aside).
            let written =
              let trimmed = body.TrimStart '0'
              if trimmed = "" || trimmed.StartsWith "." then
                "0" + trimmed
              else
                trimmed
            match
              System.Decimal.TryParse(
                body,
                System.Globalization.NumberStyles.AllowDecimalPoint,
                invariant
              )
            with
            | true, value when value.ToString(invariant) = written ->
              (TDecimal value, None)
            | _ ->
              (TDecimal 0m,
               Some $"decimal literal out of range or past 28 digits: {body}dec")
          | "f32" ->
            match System.Single.TryParse(body, floatStyle, invariant) with
            | true, value -> (TFloat32 value, None)
            | _ -> (TFloat32 0.0f, Some $"malformed float literal: {body}f32")
          | "f64" -> float ()
          | "" when hasFraction || hasExponent -> float ()
          | _ when hasFraction || hasExponent -> invalid (TFloat 0.0)
          | _ ->
            match intToken body suffix with
            | Some(Ok token) -> (token, None)
            | Some(Error message) -> (TInt64 0L, Some message)
            | None -> invalid (TInt64 0L)
        let (spannedToken, nextIndex, nextPosition) =
          emit token index endIndex position
        diagnostic |> Option.iter (addDiagnostic spannedToken.range)
        go nextIndex nextPosition (spannedToken :: tokensRev)
      // triple-quoted string: """ ... """ (raw, may contain single/double quotes)
      elif
        character = '"'
        && index + 2 < inputLength
        && input[index + 1] = '"'
        && input[index + 2] = '"'
      then
        let rec findTriple (scanIndex : int) : Result<int, string> =
          if
            scanIndex + 2 < inputLength
            && input[scanIndex] = '"'
            && input[scanIndex + 1] = '"'
            && input[scanIndex + 2] = '"'
          then
            Ok(scanIndex + 3)
          elif scanIndex >= inputLength then
            Error "Unterminated triple-quoted string literal"
          else
            findTriple (scanIndex + 1)
        match findTriple (index + 3) with
        | Ok endIndex ->
          // Triple-quoted strings are raw, so normalize here to match the regular
          // literal path through `unescape`.
          let (spannedToken, nextIndex, nextPosition) =
            emit
              (TStringLit(
                String.normalize (input.Substring(index + 3, endIndex - index - 6))
              ))
              index
              endIndex
              position
          go nextIndex nextPosition (spannedToken :: tokensRev)
        | Error _ ->
          // Unterminated `"""…`: take the rest as string content.
          let (spannedToken, nextIndex, nextPosition) =
            emit
              (TStringLit(
                String.normalize (
                  input.Substring(index + 3, inputLength - index - 3)
                )
              ))
              index
              inputLength
              position
          addDiagnostic
            spannedToken.range
            "unterminated triple-quoted string literal"
          go nextIndex nextPosition (spannedToken :: tokensRev)
      // strings / chars (escape processing deferred — raw content)
      elif character = '"' then
        match scanString (index + 1) '"' with
        | Ok endIndex ->
          let (spannedToken, nextIndex, nextPosition) =
            emit
              (TStringLit(
                unescape (input.Substring(index + 1, endIndex - index - 2))
              ))
              index
              endIndex
              position in
          go nextIndex nextPosition (spannedToken :: tokensRev)
        | Error _ ->
          // Unterminated `"…`: take to end of line for mid-typing recovery.
          let endIndex = scanToLineEnd (index + 1)
          let (spannedToken, nextIndex, nextPosition) =
            emit
              (TStringLit(
                unescape (input.Substring(index + 1, endIndex - index - 1))
              ))
              index
              endIndex
              position
          addDiagnostic spannedToken.range "unterminated string literal"
          go nextIndex nextPosition (spannedToken :: tokensRev)
      elif character = '\'' then
        // A leading `'` is a type variable (`'a`) in type context, decided by
        // the previous token. Otherwise it starts a char literal. Type variables
        // lex to the bare name as `TIdent`.
        let inTypeContext =
          match tokensRev with
          | previousToken :: _ ->
            match previousToken.token with
            | TLParen
            | TStar
            | TLt
            | TComma
            | TColon
            | TArrow
            | TEquals
            | TOf -> true
            | _ -> false
          | [] -> false
        if
          inTypeContext
          && index + 1 < inputLength
          && (System.Char.IsLetter input[index + 1] || input[index + 1] = '_')
        then
          let typeVariableEndIndex = typeVariableEnd input (index + 1)
          // A quoted grapheme wins over a type variable. Combining marks are
          // not identifier characters, so the name scan alone misses 'é'.
          let graphemeEndIndex =
            index
            + 1
            + (System.Globalization.StringInfo.GetNextTextElement(input, index + 1))
              .Length
          if
            (typeVariableEndIndex < inputLength && input[typeVariableEndIndex] = '\'')
            || (graphemeEndIndex < inputLength && input[graphemeEndIndex] = '\'')
          then
            match scanString (index + 1) '\'' with
            | Ok endIndex ->
              let (spannedToken, nextIndex, nextPosition) =
                emit
                  (TCharLit(
                    unescape (input.Substring(index + 1, endIndex - index - 2))
                  ))
                  index
                  endIndex
                  position
              go nextIndex nextPosition (spannedToken :: tokensRev)
            | Error _ ->
              let endIndex = scanToLineEnd (index + 1)
              let (spannedToken, nextIndex, nextPosition) =
                emit
                  (TCharLit(
                    unescape (input.Substring(index + 1, endIndex - index - 1))
                  ))
                  index
                  endIndex
                  position
              addDiagnostic spannedToken.range "unterminated char literal"
              go nextIndex nextPosition (spannedToken :: tokensRev)
          else
            let (spannedToken, nextIndex, nextPosition) =
              emit
                (TIdent(input.Substring(index + 1, typeVariableEndIndex - index - 1)))
                index
                typeVariableEndIndex
                position
            go nextIndex nextPosition (spannedToken :: tokensRev)
        // Char literal: read one char, or one escape, then the closing quote.
        // `'''` is the apostrophe char, not an empty literal.
        // Escaped char: scan to the closing quote so multi-char escapes decode,
        // such as `'\x41'` or `'\U0001F600'`.
        else if index + 1 < inputLength && input[index + 1] = '\\' then
          match scanString (index + 1) '\'' with
          | Ok endIndex ->
            let (spannedToken, nextIndex, nextPosition) =
              emit
                (TCharLit(
                  unescape (input.Substring(index + 1, endIndex - index - 2))
                ))
                index
                endIndex
                position
            go nextIndex nextPosition (spannedToken :: tokensRev)
          | Error _ ->
            let endIndex = scanToLineEnd (index + 1)
            let (spannedToken, nextIndex, nextPosition) =
              emit
                (TCharLit(
                  unescape (input.Substring(index + 1, endIndex - index - 1))
                ))
                index
                endIndex
                position
            addDiagnostic spannedToken.range "unterminated char literal"
            go nextIndex nextPosition (spannedToken :: tokensRev)
        else
          // Unescaped char: one extended grapheme cluster, then the closing
          // quote. A grapheme may span multiple UTF-16 code units.
          let contentEndIndex =
            if index + 1 < inputLength then
              index
              + 1
              + (System.Globalization.StringInfo.GetNextTextElement(input, index + 1))
                .Length
            else
              index + 1
          if contentEndIndex < inputLength && input[contentEndIndex] = '\'' then
            let (spannedToken, nextIndex, nextPosition) =
              emit
                (TCharLit(
                  unescape (input.Substring(index + 1, contentEndIndex - index - 1))
                ))
                index
                (contentEndIndex + 1)
                position
            go nextIndex nextPosition (spannedToken :: tokensRev)
          else
            // Unterminated/half-typed char: recover through the grapheme end.
            let endIndex = min inputLength (max (index + 1) contentEndIndex)
            let (spannedToken, nextIndex, nextPosition) =
              emit
                (TCharLit(
                  unescape (input.Substring(index + 1, endIndex - index - 1))
                ))
                index
                endIndex
                position
            addDiagnostic spannedToken.range "unterminated char literal"
            go nextIndex nextPosition (spannedToken :: tokensRev)
      elif character = '$' && index + 1 < inputLength && input[index + 1] = '"' then
        match scanInterp index with
        | Ok(token, endIndex) ->
          let (spannedToken, nextIndex, nextPosition) =
            emit token index endIndex position
          go nextIndex nextPosition (spannedToken :: tokensRev)
        | Error _ ->
          // Unterminated `$"…`: take to end of line.
          let endIndex = scanToLineEnd (index + 2)
          let (spannedToken, nextIndex, nextPosition) =
            emit TInterpString index endIndex position
          addDiagnostic spannedToken.range "unterminated interpolated string"
          go nextIndex nextPosition (spannedToken :: tokensRev)
      else
        match refused |> List.tryFind (fun (text, _) -> matchesAt text index) with
        | Some(text, message) ->
          let (spannedToken, nextIndex, nextPosition) =
            emit TPlus index (index + text.Length) position
          addDiagnostic spannedToken.range message
          go nextIndex nextPosition (spannedToken :: tokensRev)
        | None ->
          match
            operators
            |> List.tryFind (fun (operatorText, _) -> matchesAt operatorText index)
          with
          | Some(operatorText, token) ->
            let (spannedToken, nextIndex, nextPosition) =
              emit token index (index + operatorText.Length) position
            go nextIndex nextPosition (spannedToken :: tokensRev)
          | None ->
            // Unknown character: record it, skip it, and keep lexing.
            addDiagnostic
              { start = position; end_ = advance position index (index + 1) }
              $"unexpected character: '{input[index]}'"
            go (index + 1) (advance position index (index + 1)) tokensRev

  match go 0 { row = 0; column = 0 } [] with
  | Ok tokens ->
    let diagnostics = List.ofSeq diagnostics
    let tokens =
      match diagnostics with
      | [] -> tokens
      | _ ->
        let diagnosed = diagnostics |> List.map fst |> Set.ofList
        tokens
        |> List.map (fun t ->
          if Set.contains t.range diagnosed then { t with lexError = true } else t)
    Ok(tokens, diagnostics)
  | Error message -> Error message
