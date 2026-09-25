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
    leadingTrivia : List<Trivia> }

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
  decoded.Normalize()

/// Does the raw inner text of a regular string/char contain an invalid escape?
let hasInvalidEscape (content : string) : bool =
  if not (content.Contains '\\') then
    false
  else
    let contentLength = content.Length
    let mutable index = 0
    let mutable hasBadEscape = false
    while index < contentLength && not hasBadEscape do
      if content[index] = '\\' then
        match decodeEscape content index with
        | Some(charsConsumed, _) -> index <- index + charsConsumed
        | None -> hasBadEscape <- true
      else
        index <- index + 1
    hasBadEscape

let private skipLineComment
  (source : string)
  (endLimit : int)
  (startIndex : int)
  : int =
  let mutable index = startIndex + 2
  while index < endLimit && source[index] <> '\n' do
    index <- index + 1
  index

let private skipBlockComment
  (source : string)
  (endLimit : int)
  (startIndex : int)
  : int =
  let mutable index = startIndex + 2
  let mutable commentDepth = 1
  while index < endLimit && commentDepth > 0 do
    if index + 1 < endLimit && source[index] = '(' && source[index + 1] = '*' then
      commentDepth <- commentDepth + 1
      index <- index + 2
    elif index + 1 < endLimit && source[index] = '*' && source[index + 1] = ')' then
      commentDepth <- commentDepth - 1
      index <- index + 2
    else
      index <- index + 1
  index

let private skipRawString
  (source : string)
  (endLimit : int)
  (startIndex : int)
  : int =
  let mutable index = startIndex + 3
  let mutable stillScanning = true
  while stillScanning && index < endLimit do
    if
      index + 2 < endLimit
      && source[index] = '"'
      && source[index + 1] = '"'
      && source[index + 2] = '"'
    then
      index <- index + 3
      stillScanning <- false
    else
      index <- index + 1
  index

let private skipRegularString
  (source : string)
  (endLimit : int)
  (startIndex : int)
  : int =
  let mutable index = startIndex + 1
  let mutable stillScanning = true
  while stillScanning && index < endLimit do
    if source[index] = '\\' then
      index <- index + 2
    elif source[index] = '"' then
      index <- index + 1
      stillScanning <- false
    else
      index <- index + 1
  index


/// Find the `}` that closes an interpolation expression region.
/// `startIndex` is just past the opening `{`. The scan tracks nested braces and skips
/// embedded string and char literals, so braces inside them do not desync the
/// scan. Returns `-1` if unclosed. The tokenizer, escape validator, and parser
/// all use this scanner so they agree on where interpolation regions end.
let findInterpExprClose (source : string) (endLimit : int) (startIndex : int) : int =
  let mutable scanIndex = startIndex
  let mutable braceDepth = 0
  let mutable closingBraceIndex = -1
  while closingBraceIndex < 0 && scanIndex < endLimit do
    // Comments are code trivia, so braces inside them cannot close an
    // interpolation. Block comments nest just like top-level lexer comments.
    if
      source[scanIndex] = '/'
      && scanIndex + 1 < endLimit
      && source[scanIndex + 1] = '/'
    then
      scanIndex <- skipLineComment source endLimit scanIndex
    elif
      source[scanIndex] = '('
      && scanIndex + 1 < endLimit
      && source[scanIndex + 1] = '*'
    then
      scanIndex <- skipBlockComment source endLimit scanIndex
    // A raw triple-quoted string may contain unescaped quotes and braces.
    elif
      source[scanIndex] = '"'
      && scanIndex + 2 < endLimit
      && source[scanIndex + 1] = '"'
      && source[scanIndex + 2] = '"'
    then
      scanIndex <- skipRawString source endLimit scanIndex
    elif source[scanIndex] = '"' then
      scanIndex <- skipRegularString source endLimit scanIndex
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
      let mutable charIndex = scanIndex + 1
      let mutable scanningChar = true
      while scanningChar && charIndex < endLimit do
        if source[charIndex] = '\\' then
          charIndex <- charIndex + 2
        elif source[charIndex] = '\'' then
          charIndex <- charIndex + 1
          scanningChar <- false
        else
          charIndex <- charIndex + 1
      scanIndex <- charIndex
    elif
      source[scanIndex] = '\''
      && scanIndex + 2 < endLimit
      && source[scanIndex + 2] = '\''
    then
      scanIndex <- scanIndex + 3
    elif source[scanIndex] = '{' then
      braceDepth <- braceDepth + 1
      scanIndex <- scanIndex + 1
    elif source[scanIndex] = '}' && braceDepth = 0 then
      closingBraceIndex <- scanIndex
    elif source[scanIndex] = '}' then
      braceDepth <- braceDepth - 1
      scanIndex <- scanIndex + 1
    else
      scanIndex <- scanIndex + 1
  closingBraceIndex


/// Like `hasInvalidEscape`, but for regular `$"…"` interpolated strings.
/// `{{`/`}}` are literal braces and `{ … }` regions are code, so only literal
/// string text is escape-checked.
let hasInvalidEscapeInterp (inner : string) : bool =
  let innerLength = inner.Length
  let mutable index = 0
  let mutable hasBadEscape = false
  while index < innerLength && not hasBadEscape do
    if index + 1 < innerLength && inner[index] = '{' && inner[index + 1] = '{' then
      index <- index + 2
    elif index + 1 < innerLength && inner[index] = '}' && inner[index + 1] = '}' then
      index <- index + 2
    elif inner[index] = '{' then
      // skip the `{ … }` interpolation region
      let closeIndex = findInterpExprClose inner innerLength (index + 1)
      index <- if closeIndex < 0 then innerLength else closeIndex + 1
    elif inner[index] = '\\' then
      match decodeEscape inner index with
      | Some(charsConsumed, _) -> index <- index + charsConsumed
      | None -> hasBadEscape <- true
    else
      index <- index + 1
  hasBadEscape

/// Does interpolated-string literal text contain a single unescaped `}`?
/// Literal braces must be doubled (`}}`) or escaped (`\}` in regular strings).
/// Braces inside `{ expression }` regions are code and are skipped.
let hasSingleCloseBraceInterp (inner : string) (isRaw : bool) : bool =
  let innerLength = inner.Length
  let mutable index = 0
  let mutable hasSingleCloseBrace = false
  while index < innerLength && not hasSingleCloseBrace do
    if index + 1 < innerLength && inner[index] = '{' && inner[index + 1] = '{' then
      index <- index + 2
    elif index + 1 < innerLength && inner[index] = '}' && inner[index + 1] = '}' then
      index <- index + 2
    elif inner[index] = '{' then
      let closeIndex = findInterpExprClose inner innerLength (index + 1)
      index <- if closeIndex < 0 then innerLength else closeIndex + 1
    elif inner[index] = '}' then
      hasSingleCloseBrace <- true
    elif inner[index] = '\\' && not isRaw && index + 1 < innerLength then
      index <- index + 2
    else
      index <- index + 1
  hasSingleCloseBrace


/// The parser parses each `{expr}` body recursively. Nested interpolated strings
/// therefore increase recursion depth. Cap it so pathological nesting cannot
/// overflow the process stack. The tokenizer itself does not recurse here;
/// `scanInterp` skips `{expr}` regions iteratively.
let maxInterpNesting = 64

let tokenize
  (input : string)
  : Result<List<SpannedToken> * List<TokenRange * string>, string> =
  let inputLength = input.Length

  // `///` doc comments lex as trivia, but their text also lands on the next
  // emitted token. `emit` consumes and clears this.
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
    let mutable currentPosition = position
    let mutable index = startIndex
    while index < endIndex do
      currentPosition <- step currentPosition input[index]
      index <- index + 1
    currentPosition

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
        let mutable lineEndIndex = index
        while lineEndIndex < inputLength && input[lineEndIndex] <> '\n' do
          lineEndIndex <- lineEndIndex + 1
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
        // F#-style nestable block comment. `(*)` and `(**)` are the multiply
        // and exponentiation operator sections, so they are excluded here.
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
      "++", TPlusPlus
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
      "::", TCons
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
      "@", TAt
      "|", TBar ]

  let keyword (text : string) : Token =
    match text with
    | "let" -> TLet
    // `val x = e` is a value declaration. `let` is reserved for functions and
    // local/script bindings. The distinct token lets `parseItems` keep them apart.
    | "val" -> TVal
    | "in" -> TIn
    | "if" -> TIf
    | "elif" -> TElif
    | "then" -> TThen
    | "else" -> TElse
    // `def` is not a keyword in the interpreter dialect. It remains a valid
    // identifier, for example `(def: Type)`.
    | "type" -> TType
    | "of" -> TOf
    | "match" -> TMatch
    | "with" -> TWith
    | "fun" -> TFun
    | "when" -> TWhen
    | "true" -> TTrue
    | "false" -> TFalse
    | "_" -> TUnderscore
    // `___` is the blank-name placeholder: an identifier with an empty name.
    | "___" -> TIdent ""
    | _ -> TIdent text

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
       leadingTrivia = leadingTrivia },
     endIndex,
     endPosition)

  // integer suffix → token; returns (token, charsConsumedAfterDigits)
  let intToken (digits : string) (suffixIndex : int) : Result<Token * int, string> =
    let parseInt64 () =
      match System.Int64.TryParse digits with
      | true, value -> Ok(TInt64 value)
      | _ when digits = "9223372036854775808" -> Ok(TInt64 System.Int64.MinValue)
      | _ -> Error $"Integer literal too large: {digits}"
    if matchesAt "uy" suffixIndex then
      (match System.Byte.TryParse digits with
       | true, value -> Ok(TUInt8 value, 2)
       | _ -> Error $"out of range for UInt8: {digits}")
    elif matchesAt "us" suffixIndex then
      (match System.UInt16.TryParse digits with
       | true, value -> Ok(TUInt16 value, 2)
       | _ -> Error $"out of range for UInt16: {digits}")
    elif matchesAt "ul" suffixIndex then
      (match System.UInt32.TryParse digits with
       | true, value -> Ok(TUInt32 value, 2)
       | _ -> Error $"out of range for UInt32: {digits}")
    elif matchesAt "UL" suffixIndex then
      (match System.UInt64.TryParse digits with
       | true, value -> Ok(TUInt64 value, 2)
       | _ -> Error $"out of range for UInt64: {digits}")
    elif matchesAt "L" suffixIndex then
      parseInt64 () |> Result.map (fun token -> (token, 1))
    elif matchesAt "Q" suffixIndex then
      (match System.Int128.TryParse digits with
       | true, value -> Ok(TInt128 value, 1)
       | _ when digits = "170141183460469231731687303715884105728" ->
         Ok(TInt128 System.Int128.MinValue, 1)
       | _ -> Error $"out of range for Int128: {digits}")
    elif matchesAt "Z" suffixIndex then
      (match System.UInt128.TryParse digits with
       | true, value -> Ok(TUInt128 value, 1)
       | _ -> Error $"out of range for UInt128: {digits}")
    elif matchesAt "y" suffixIndex then
      (match System.SByte.TryParse digits with
       | true, value -> Ok(TInt8 value, 1)
       | _ when digits = "128" -> Ok(TInt8 System.SByte.MinValue, 1)
       | _ -> Error $"out of range for Int8: {digits}")
    elif matchesAt "s" suffixIndex then
      (match System.Int16.TryParse digits with
       | true, value -> Ok(TInt16 value, 1)
       | _ when digits = "32768" -> Ok(TInt16 System.Int16.MinValue, 1)
       | _ -> Error $"out of range for Int16: {digits}")
    elif matchesAt "l" suffixIndex then
      (match System.Int32.TryParse digits with
       | true, value -> Ok(TInt32 value, 1)
       | _ when digits = "2147483648" -> Ok(TInt32 System.Int32.MinValue, 1)
       | _ -> Error $"out of range for Int32: {digits}")
    // bare literal (no suffix) → arbitrary-precision `Int` (the default).
    else
      (match System.Numerics.BigInteger.TryParse digits with
       | true, value -> Ok(TInt value, 0)
       | _ -> Error $"Invalid integer literal: {digits}")

  let recognizedIntSuffixLength (suffixIndex : int) : int =
    if
      matchesAt "uy" suffixIndex
      || matchesAt "us" suffixIndex
      || matchesAt "ul" suffixIndex
      || matchesAt "UL" suffixIndex
    then
      2
    elif
      matchesAt "L" suffixIndex
      || matchesAt "Q" suffixIndex
      || matchesAt "Z" suffixIndex
      || matchesAt "y" suffixIndex
      || matchesAt "s" suffixIndex
      || matchesAt "l" suffixIndex
    then
      1
    else
      0

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
        let mutable closeIndex = index + 2
        while closeIndex + 1 < inputLength
              && not (input[closeIndex] = '`' && input[closeIndex + 1] = '`')
              && input[closeIndex] <> '\n' do
          closeIndex <- closeIndex + 1
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
        let mutable endIndex = index + 1
        while endIndex < inputLength
              && (System.Char.IsLetterOrDigit input[endIndex]
                  || input[endIndex] = '_'
                  || input[endIndex] = '\'') do
          endIndex <- endIndex + 1
        let (spannedToken, nextIndex, nextPosition) =
          emit
            (keyword (input.Substring(index, endIndex - index)))
            index
            endIndex
            position
        go nextIndex nextPosition (spannedToken :: tokensRev)
      // numbers
      elif System.Char.IsDigit character then
        // A number token must end at a non-identifier boundary. Glued suffix text
        // like `123abc` or `12l3` is a typo, not two tokens.
        let isIdentifierContinue boundaryIndex =
          boundaryIndex < inputLength
          && (System.Char.IsLetterOrDigit input[boundaryIndex]
              || input[boundaryIndex] = '_'
              || input[boundaryIndex] = '\'')
        let mutable digitEndIndex = index + 1
        while digitEndIndex < inputLength && System.Char.IsDigit input[digitEndIndex] do
          digitEndIndex <- digitEndIndex + 1
        // float?
        let hasFraction =
          digitEndIndex + 1 < inputLength
          && input[digitEndIndex] = '.'
          && System.Char.IsDigit input[digitEndIndex + 1]
        let hasExponent =
          digitEndIndex < inputLength
          && (input[digitEndIndex] = 'e' || input[digitEndIndex] = 'E')
        if hasFraction || hasExponent then
          let mutable floatEndIndex = digitEndIndex
          if hasFraction then
            floatEndIndex <- floatEndIndex + 1
            while (floatEndIndex < inputLength
                   && System.Char.IsDigit input[floatEndIndex]) do
              floatEndIndex <- floatEndIndex + 1
          if
            floatEndIndex < inputLength
            && (input[floatEndIndex] = 'e' || input[floatEndIndex] = 'E')
          then
            floatEndIndex <- floatEndIndex + 1
            if
              floatEndIndex < inputLength
              && (input[floatEndIndex] = '+' || input[floatEndIndex] = '-')
            then
              floatEndIndex <- floatEndIndex + 1
            while (floatEndIndex < inputLength
                   && System.Char.IsDigit input[floatEndIndex]) do
              floatEndIndex <- floatEndIndex + 1
          let text = input.Substring(index, floatEndIndex - index)
          match
            System.Double.TryParse(
              text,
              System.Globalization.NumberStyles.Float,
              System.Globalization.CultureInfo.InvariantCulture
            )
          with
          | true, value when not (isIdentifierContinue floatEndIndex) ->
            let (spannedToken, nextIndex, nextPosition) =
              emit (TFloat value) index floatEndIndex position
            go nextIndex nextPosition (spannedToken :: tokensRev)
          | true, _ ->
            // Float glued to identifier chars (`1.5abc`): consume the run and
            // diagnose it as one malformed literal.
            let mutable errorEndIndex = floatEndIndex
            while isIdentifierContinue errorEndIndex do
              errorEndIndex <- errorEndIndex + 1
            let (spannedToken, nextIndex, nextPosition) =
              emit (TFloat 0.0) index errorEndIndex position
            addDiagnostic
              spannedToken.range
              $"invalid number literal: {input.Substring(index, errorEndIndex - index)}"
            go nextIndex nextPosition (spannedToken :: tokensRev)
          | _ ->
            // Emit a placeholder so malformed floats still highlight as numbers.
            let (spannedToken, nextIndex, nextPosition) =
              emit (TFloat 0.0) index floatEndIndex position
            addDiagnostic spannedToken.range $"malformed float literal: {text}"
            go nextIndex nextPosition (spannedToken :: tokensRev)
        else
          let digits = input.Substring(index, digitEndIndex - index)
          match intToken digits digitEndIndex with
          | Ok(token, suffixLength) when
            not (isIdentifierContinue (digitEndIndex + suffixLength))
            ->
            let (spannedToken, nextIndex, nextPosition) =
              emit token index (digitEndIndex + suffixLength) position
            go nextIndex nextPosition (spannedToken :: tokensRev)
          | Ok(_, suffixLength) ->
            // Int glued to identifier chars: consume the run and diagnose it as
            // one malformed literal.
            let mutable errorEndIndex = digitEndIndex + suffixLength
            while isIdentifierContinue errorEndIndex do
              errorEndIndex <- errorEndIndex + 1
            let (spannedToken, nextIndex, nextPosition) =
              emit (TInt64 0L) index errorEndIndex position
            addDiagnostic
              spannedToken.range
              $"invalid number literal: {input.Substring(index, errorEndIndex - index)}"
            go nextIndex nextPosition (spannedToken :: tokensRev)
          | Error message ->
            // Consume a recognized suffix even on range failure so it cannot
            // reappear as a separate identifier in the recovery tree.
            let suffixLength = recognizedIntSuffixLength digitEndIndex
            let (spannedToken, nextIndex, nextPosition) =
              emit (TInt64 0L) index (digitEndIndex + suffixLength) position
            addDiagnostic spannedToken.range message
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
                (input.Substring(index + 3, endIndex - index - 6)).Normalize()
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
                (input.Substring(index + 3, inputLength - index - 3)).Normalize()
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
          let mutable typeVariableEndIndex = index + 1
          while (typeVariableEndIndex < inputLength
                 && (System.Char.IsLetterOrDigit input[typeVariableEndIndex]
                     || input[typeVariableEndIndex] = '_')) do
            typeVariableEndIndex <- typeVariableEndIndex + 1
          // a closing quote right after the name means it was a char literal
          if
            typeVariableEndIndex < inputLength && input[typeVariableEndIndex] = '\''
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
        else if
          // Char literal: read one char, or one escape, then the closing quote.
          // `'''` is the apostrophe char, not an empty literal.
          index + 1 < inputLength && input[index + 1] = '\\'
        then
          // Escaped char: scan to the closing quote so multi-char escapes decode,
          // such as `'\x41'` or `'\U0001F600'`.
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
  | Ok tokens -> Ok(tokens, List.ofSeq diagnostics)
  | Error message -> Error message
