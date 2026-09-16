// 1_Parser.fs - Lexer and Parser (Pass 1)
//
// Transforms source code (string) into an Abstract Syntax Tree (AST).
//
// Lexer: Converts source string into tokens
// Parser: Recursive descent parser with operator precedence
//
// Operator precedence (specific to this parser):
// - Multiplication and division bind tighter than addition and subtraction
// - Operators are left-associative: "1 + 2 + 3" parses as "(1 + 2) + 3"
// - Parentheses for explicit grouping
//
// Example:
//   "2 + 3 * 4" → BinOp(Add, Int64Literal(2), BinOp(Mul, Int64Literal(3), Int64Literal(4)))

module Parser

open AST

/// Part of an interpolated string token
type InterpPart =
    | InterpText of string       // Literal text
    | InterpTokens of Token list // Tokens for an expression (will be parsed later)

/// Token types for lexer
and Token =
    | TInt64 of int64       // Default integer (Int64)
    | TInt128 of System.Int128  // 128-bit signed: 1Q
    | TInt8 of sbyte        // 8-bit signed: 1y
    | TInt16 of int16       // 16-bit signed: 1s
    | TInt32 of int32       // 32-bit signed: 1l
    | TUInt8 of byte        // 8-bit unsigned: 1uy
    | TUInt16 of uint16     // 16-bit unsigned: 1us
    | TUInt32 of uint32     // 32-bit unsigned: 1ul
    | TUInt64 of uint64     // 64-bit unsigned: 1UL
    | TUInt128 of System.UInt128  // 128-bit unsigned: 1Z
    | TFloat of float
    | TStringLit of string  // String literal token (named to avoid conflict with AST.TString type)
    | TCharLit of string    // Char literal: 'x' (stores UTF-8 string for EGC support)
    | TInterpString of InterpPart list  // Interpolated string: $"Hello {name}!"
    | TTrue
    | TFalse
    | TPlus
    | TPlusPlus    // ++ (string concatenation)
    | TMinus
    | TStar
    | TSlash
    | TLParen
    | TRParen
    | TLet
    | TIn
    | TIf          // if
    | TThen        // then
    | TElse        // else
    | TDef         // def (function definition)
    | TType        // type (type definition)
    | TColon       // : (type annotation)
    | TComma       // , (parameter separator)
    | TDot         // . (tuple/record access)
    | TLBrace      // { (record literal)
    | TRBrace      // } (record literal)
    | TBar         // | (sum type variant separator / pattern separator)
    | TOf          // of (sum type payload)
    | TMatch       // match (pattern matching)
    | TWith        // with (pattern matching)
    | TArrow       // -> (pattern matching)
    | TFatArrow    // => (lambda)
    | TUnderscore  // _ (wildcard pattern)
    | TWhen        // when (guard clause in pattern matching)
    | TLBracket    // [ (list literal)
    | TRBracket    // ] (list literal)
    | TEquals      // = (assignment in let)
    | TEqEq        // == (equality comparison)
    | TNeq         // !=
    | TLt          // <
    | TGt          // >
    | TLte         // <=
    | TGte         // >=
    | TAnd         // &&
    | TOr          // ||
    | TNot         // !
    | TPipe        // |> (pipe operator)
    | TDotDotDot    // ... (rest pattern in lists)
    | TPercent     // % (modulo)
    | TShl         // << (left shift)
    | TShr         // >> (right shift)
    | TBitAnd      // & (bitwise and)
    | TBitOr       // ||| (bitwise or)
    | TBitXor      // ^ (bitwise xor)
    | TBitNot      // ~~~ (bitwise not)
    | TIdent of string
    | TEOF

/// Lexer: convert string to list of tokens
let lex (input: string) : Result<Token list, string> =
    let rec lexHelper (chars: char list) (acc: Token list) : Result<Token list, string> =
        match chars with
        | [] -> Ok (List.rev (TEOF :: acc))
        | ' ' :: rest | '\t' :: rest | '\n' :: rest | '\r' :: rest ->
            // Skip whitespace
            lexHelper rest acc
        | '+' :: '+' :: rest -> lexHelper rest (TPlusPlus :: acc)
        | '+' :: rest -> lexHelper rest (TPlus :: acc)
        | '-' :: '>' :: rest -> lexHelper rest (TArrow :: acc)
        | '-' :: rest -> lexHelper rest (TMinus :: acc)
        | '=' :: '>' :: rest -> lexHelper rest (TFatArrow :: acc)
        | '*' :: rest -> lexHelper rest (TStar :: acc)
        | '/' :: '/' :: rest ->
            // Skip line comment: // ... until end of line
            let rec skipToEndOfLine (cs: char list) : char list =
                match cs with
                | [] -> []
                | '\n' :: remaining -> remaining
                | '\r' :: '\n' :: remaining -> remaining
                | '\r' :: remaining -> remaining
                | _ :: remaining -> skipToEndOfLine remaining
            lexHelper (skipToEndOfLine rest) acc
        | '/' :: rest -> lexHelper rest (TSlash :: acc)
        | '(' :: rest -> lexHelper rest (TLParen :: acc)
        | ')' :: rest -> lexHelper rest (TRParen :: acc)
        | '{' :: rest -> lexHelper rest (TLBrace :: acc)
        | '}' :: rest -> lexHelper rest (TRBrace :: acc)
        | '[' :: rest -> lexHelper rest (TLBracket :: acc)
        | ']' :: rest -> lexHelper rest (TRBracket :: acc)
        | ':' :: rest -> lexHelper rest (TColon :: acc)
        | ',' :: rest -> lexHelper rest (TComma :: acc)
        | '.' :: '.' :: '.' :: rest -> lexHelper rest (TDotDotDot :: acc)
        | '.' :: rest -> lexHelper rest (TDot :: acc)
        | '=' :: '=' :: rest -> lexHelper rest (TEqEq :: acc)
        | '=' :: rest -> lexHelper rest (TEquals :: acc)
        | '!' :: '=' :: rest -> lexHelper rest (TNeq :: acc)
        | '!' :: rest -> lexHelper rest (TNot :: acc)
        | '<' :: '<' :: rest -> lexHelper rest (TShl :: acc)
        | '<' :: '=' :: rest -> lexHelper rest (TLte :: acc)
        | '<' :: rest -> lexHelper rest (TLt :: acc)
        | '>' :: '>' :: rest -> lexHelper rest (TShr :: acc)
        | '>' :: '=' :: rest -> lexHelper rest (TGte :: acc)
        | '>' :: rest -> lexHelper rest (TGt :: acc)
        | '&' :: '&' :: rest -> lexHelper rest (TAnd :: acc)
        | '&' :: rest -> lexHelper rest (TBitAnd :: acc)
        | '^' :: rest -> lexHelper rest (TBitXor :: acc)
        | '~' :: '~' :: '~' :: rest -> lexHelper rest (TBitNot :: acc)
        | '%' :: rest -> lexHelper rest (TPercent :: acc)
        | '|' :: '|' :: '|' :: rest -> lexHelper rest (TBitOr :: acc)
        | '|' :: '|' :: rest -> lexHelper rest (TOr :: acc)
        | '|' :: '>' :: rest -> lexHelper rest (TPipe :: acc)
        | '|' :: rest -> lexHelper rest (TBar :: acc)
        | '`' :: '`' :: rest ->
            // Backtick-escaped identifiers: ``name`` (including keyword-like names).
            let rec parseBacktickIdent (cs: char list) (chars: char list) : Result<string * char list, string> =
                match cs with
                | '`' :: '`' :: remaining ->
                    let ident = System.String(List.rev chars |> List.toArray)
                    if ident.Length = 0 then
                        Error "Backtick identifier cannot be empty"
                    else
                        Ok (ident, remaining)
                | '\n' :: _ | '\r' :: _ ->
                    Error "Unterminated backtick identifier"
                | c :: remaining ->
                    parseBacktickIdent remaining (c :: chars)
                | [] ->
                    Error "Unterminated backtick identifier"

            parseBacktickIdent rest []
            |> Result.bind (fun (ident, remaining) ->
                lexHelper remaining (TIdent ident :: acc))
        | '`' :: _ ->
            Error "Backtick identifiers must use double backticks: ``name``"
        | c :: _ when System.Char.IsLetter(c) || c = '_' ->
            // Parse identifier or keyword
            let rec parseIdent (cs: char list) (chars: char list) : string * char list =
                match cs with
                | c :: rest when System.Char.IsLetterOrDigit(c) || c = '_' || c = '\'' ->
                    parseIdent rest (c :: chars)
                | _ ->
                    let ident = System.String(List.rev chars |> List.toArray)
                    (ident, cs)

            let (ident, remaining) = parseIdent chars []
            let token =
                match ident with
                | "let" -> TLet
                | "in" -> TIn
                | "if" -> TIf
                | "then" -> TThen
                | "else" -> TElse
                | "def" -> TDef
                | "type" -> TType
                | "of" -> TOf
                | "match" -> TMatch
                | "with" -> TWith
                | "when" -> TWhen
                | "true" -> TTrue
                | "false" -> TFalse
                | "_" -> TUnderscore
                | _ -> TIdent ident
            lexHelper remaining (token :: acc)
        | c :: _ when System.Char.IsDigit(c) ->
            // Parse number (integer or float)
            // First collect all digits
            let rec collectDigits (cs: char list) (acc: char list) : char list * char list =
                match cs with
                | d :: rest when System.Char.IsDigit(d) -> collectDigits rest (d :: acc)
                | _ -> (List.rev acc, cs)

            let (intDigits, afterInt) = collectDigits chars []

            // Check if this is a float (has decimal point or exponent)
            match afterInt with
            | '.' :: rest when not (List.isEmpty rest) && System.Char.IsDigit(List.head rest) ->
                // Float with decimal point: 3.14
                let (fracDigits, afterFrac) = collectDigits rest []
                // Check for exponent
                match afterFrac with
                | ('e' :: rest' | 'E' :: rest') ->
                    // Scientific notation: 3.14e10 or 3.14e-10
                    let (expSign, afterSign) =
                        match rest' with
                        | '+' :: r -> (['+'], r)
                        | '-' :: r -> (['-'], r)
                        | _ -> ([], rest')
                    let (expDigits, remaining) = collectDigits afterSign []
                    if List.isEmpty expDigits then
                        Error "Expected exponent digits after 'e'"
                    else
                        let numStr = System.String(Array.ofList (intDigits @ ['.'] @ fracDigits @ ['e'] @ expSign @ expDigits))
                        match System.Double.TryParse(numStr, System.Globalization.NumberStyles.Float, System.Globalization.CultureInfo.InvariantCulture) with
                        | (true, value) -> lexHelper remaining (TFloat value :: acc)
                        | (false, _) -> Error $"Invalid float literal: {numStr}"
                | _ ->
                    // Float without exponent: 3.14
                    let numStr = System.String(Array.ofList (intDigits @ ['.'] @ fracDigits))
                    match System.Double.TryParse(numStr, System.Globalization.NumberStyles.Float, System.Globalization.CultureInfo.InvariantCulture) with
                    | (true, value) -> lexHelper afterFrac (TFloat value :: acc)
                    | (false, _) -> Error $"Invalid float literal: {numStr}"
            | ('e' :: rest | 'E' :: rest) ->
                // Scientific notation without decimal: 1e10 or 1e-10
                let (expSign, afterSign) =
                    match rest with
                    | '+' :: r -> (['+'], r)
                    | '-' :: r -> (['-'], r)
                    | _ -> ([], rest)
                let (expDigits, remaining) = collectDigits afterSign []
                if List.isEmpty expDigits then
                    Error "Expected exponent digits after 'e'"
                else
                    let numStr = System.String(Array.ofList (intDigits @ ['e'] @ expSign @ expDigits))
                    match System.Double.TryParse(numStr, System.Globalization.NumberStyles.Float, System.Globalization.CultureInfo.InvariantCulture) with
                    | (true, value) -> lexHelper remaining (TFloat value :: acc)
                    | (false, _) -> Error $"Invalid float literal: {numStr}"
            | _ ->
                // Integer with optional type suffix
                let numStr = System.String(List.toArray intDigits)

                // Check for type suffix: y, uy, s, us, l, ul, L, Q, UL, Z
                match afterInt with
                | 'u' :: 'y' :: rest ->
                    // UInt8 suffix: 1uy
                    match System.Byte.TryParse(numStr) with
                    | (true, value) -> lexHelper rest (TUInt8 value :: acc)
                    | (false, _) -> Error $"Value {numStr} is out of range for UInt8 (0 to 255)"
                | 'y' :: rest ->
                    // Int8 suffix: 1y
                    match System.SByte.TryParse(numStr) with
                    | (true, value) -> lexHelper rest (TInt8 value :: acc)
                    | (false, _) ->
                        if numStr = "128" then
                            lexHelper rest (TInt8 System.SByte.MinValue :: acc)
                        else
                            Error $"Value {numStr} is out of range for Int8 (-128 to 127)"
                | 'u' :: 's' :: rest ->
                    // UInt16 suffix: 1us
                    match System.UInt16.TryParse(numStr) with
                    | (true, value) -> lexHelper rest (TUInt16 value :: acc)
                    | (false, _) -> Error $"Value {numStr} is out of range for UInt16 (0 to 65535)"
                | 's' :: rest ->
                    // Int16 suffix: 1s
                    match System.Int16.TryParse(numStr) with
                    | (true, value) -> lexHelper rest (TInt16 value :: acc)
                    | (false, _) ->
                        if numStr = "32768" then
                            lexHelper rest (TInt16 System.Int16.MinValue :: acc)
                        else
                            Error $"Value {numStr} is out of range for Int16 (-32768 to 32767)"
                | 'u' :: 'l' :: rest ->
                    // UInt32 suffix: 1ul
                    match System.UInt32.TryParse(numStr) with
                    | (true, value) -> lexHelper rest (TUInt32 value :: acc)
                    | (false, _) -> Error $"Value {numStr} is out of range for UInt32 (0 to 4294967295)"
                | 'l' :: rest ->
                    // Int32 suffix: 1l
                    match System.Int32.TryParse(numStr) with
                    | (true, value) -> lexHelper rest (TInt32 value :: acc)
                    | (false, _) ->
                        if numStr = "2147483648" then
                            lexHelper rest (TInt32 System.Int32.MinValue :: acc)
                        else
                            Error $"Value {numStr} is out of range for Int32 (-2147483648 to 2147483647)"
                | 'U' :: 'L' :: rest ->
                    // UInt64 suffix: 1UL
                    match System.UInt64.TryParse(numStr) with
                    | (true, value) -> lexHelper rest (TUInt64 value :: acc)
                    | (false, _) -> Error $"Value {numStr} is out of range for UInt64"
                | 'L' :: rest ->
                    // Int64 explicit suffix: 1L (same as default)
                    match System.Int64.TryParse(numStr) with
                    | (true, value) -> lexHelper rest (TInt64 value :: acc)
                    | (false, _) ->
                        if numStr = "9223372036854775808" then
                            lexHelper rest (TInt64 System.Int64.MinValue :: acc)
                        else
                            Error $"Integer literal too large: {numStr}"
                | 'Q' :: rest ->
                    // Int128 suffix: 1Q
                    match System.Int128.TryParse(numStr) with
                    | (true, value) -> lexHelper rest (TInt128 value :: acc)
                    | (false, _) ->
                        // Keep INT128_MIN absolute-value sentinel behavior consistent with Int64 parsing.
                        if numStr = "170141183460469231731687303715884105728" then
                            lexHelper rest (TInt128 System.Int128.MinValue :: acc)
                        else
                            Error $"Value {numStr} is out of range for Int128"
                | 'Z' :: rest ->
                    // UInt128 suffix: 1Z
                    match System.UInt128.TryParse(numStr) with
                    | (true, value) -> lexHelper rest (TUInt128 value :: acc)
                    | (false, _) -> Error $"Value {numStr} is out of range for UInt128"
                | _ ->
                    // No suffix: default Int64
                    match System.Int64.TryParse(numStr) with
                    | (true, value) -> lexHelper afterInt (TInt64 value :: acc)
                    | (false, _) ->
                        // Check for INT64_MIN special case: "9223372036854775808"
                        // This value is > INT64_MAX but equals |INT64_MIN|
                        // It's only valid when preceded by minus: -9223372036854775808
                        if numStr = "9223372036854775808" then
                            // Return INT64_MIN directly - this is a special sentinel
                            // The parser will only accept this when it's negated
                            lexHelper afterInt (TInt64 System.Int64.MinValue :: acc)
                        else
                            Error $"Integer literal too large: {numStr}"
        | '$' :: '"' :: rest ->
            // Parse interpolated string: $"Hello {name}!"
            // Returns TInterpString token with parts list

            // Helper to parse escape sequences (same as regular strings)
            let parseEscape (cs: char list) : Result<char * char list, string> =
                match cs with
                | 'n' :: remaining -> Ok ('\n', remaining)
                | 't' :: remaining -> Ok ('\t', remaining)
                | 'r' :: remaining -> Ok ('\r', remaining)
                | '\\' :: remaining -> Ok ('\\', remaining)
                | '"' :: remaining -> Ok ('"', remaining)
                | '\'' :: remaining -> Ok ('\'', remaining)
                | '0' :: remaining -> Ok ('\000', remaining)
                | '{' :: remaining -> Ok ('{', remaining)  // Escape { as \{
                | '}' :: remaining -> Ok ('}', remaining)  // Escape } as \}
                | 'x' :: h1 :: h2 :: remaining ->
                    let hexStr = System.String([| h1; h2 |])
                    match System.Int32.TryParse(hexStr, System.Globalization.NumberStyles.HexNumber, null) with
                    | (true, value) -> Ok (char value, remaining)
                    | (false, _) -> Error $"Invalid hex escape sequence: \\x{hexStr}"
                | 'u' :: h1 :: h2 :: h3 :: h4 :: remaining ->
                    let hexStr = System.String([| h1; h2; h3; h4 |])
                    match System.Int32.TryParse(hexStr, System.Globalization.NumberStyles.HexNumber, null) with
                    | (true, value) -> Ok (char value, remaining)
                    | (false, _) -> Error $"Invalid unicode escape sequence: \\u{hexStr}"
                | c :: _ -> Error $"Unknown escape sequence: \\{c}"
                | [] -> Error "Unterminated escape sequence"

            // Helper to collect characters until { or closing "
            let rec collectLiteralPart (cs: char list) (chars: char list) : Result<string * char list, string> =
                match cs with
                | [] -> Error "Unterminated interpolated string"
                | '"' :: remaining ->
                    let str = System.String(List.rev chars |> List.toArray)
                    Ok (str, '"' :: remaining)  // Put " back for caller to detect end
                | '{' :: remaining ->
                    let str = System.String(List.rev chars |> List.toArray)
                    Ok (str, '{' :: remaining)  // Put { back for caller to detect expression
                | '\\' :: escRest ->
                    match parseEscape escRest with
                    | Ok (c, remaining) -> collectLiteralPart remaining (c :: chars)
                    | Error err -> Error err
                | c :: remaining ->
                    collectLiteralPart remaining (c :: chars)

            // Helper to collect expression chars until matching }
            let rec collectExprChars (cs: char list) (depth: int) (chars: char list) : Result<char list * char list, string> =
                match cs with
                | [] -> Error "Unterminated interpolated expression"
                | '}' :: remaining when depth = 0 ->
                    Ok (List.rev chars, remaining)
                | '}' :: remaining ->
                    collectExprChars remaining (depth - 1) ('}' :: chars)
                | '{' :: remaining ->
                    collectExprChars remaining (depth + 1) ('{' :: chars)
                | '"' :: remaining ->
                    // Skip strings inside the expression
                    let rec skipString (cs: char list) (acc: char list) =
                        match cs with
                        | [] -> Error "Unterminated string in interpolated expression"
                        | '"' :: rest -> Ok ('"' :: acc, rest)
                        | '\\' :: c :: rest -> skipString rest (c :: '\\' :: acc)
                        | c :: rest -> skipString rest (c :: acc)
                    match skipString remaining ('"' :: chars) with
                    | Ok (acc', rest') -> collectExprChars rest' depth acc'
                    | Error err -> Error err
                | c :: remaining ->
                    collectExprChars remaining depth (c :: chars)

            // Parse all parts and build InterpPart list
            let rec parseInterpParts (cs: char list) (parts: InterpPart list) : Result<InterpPart list * char list, string> =
                match cs with
                | '"' :: remaining ->
                    // End of interpolated string
                    Ok (List.rev parts, remaining)
                | '{' :: remaining ->
                    // Expression part - collect chars and lex them
                    match collectExprChars remaining 0 [] with
                    | Ok (exprChars, afterExpr) ->
                        let exprStr = System.String(exprChars |> List.toArray)
                        // Lex the expression
                        match lexHelper (exprStr |> Seq.toList) [] with
                        | Ok tokens ->
                            let tokens' = tokens |> List.filter (fun t -> t <> TEOF)
                            parseInterpParts afterExpr (InterpTokens tokens' :: parts)
                        | Error err -> Error $"Error in interpolated expression: {err}"
                    | Error err -> Error err
                | _ ->
                    // Literal part
                    match collectLiteralPart cs [] with
                    | Ok (str, afterLit) ->
                        if str = "" then
                            parseInterpParts afterLit parts
                        else
                            parseInterpParts afterLit (InterpText str :: parts)
                    | Error err -> Error err

            match parseInterpParts rest [] with
            | Ok (parts, remaining) ->
                lexHelper remaining (TInterpString parts :: acc)
            | Error err -> Error err

        | '\'' :: rest ->
            // Parse char literal with escape sequences (single Extended Grapheme Cluster)
            let rec parseCharContent (cs: char list) (chars: char list) : Result<string * char list, string> =
                match cs with
                | [] -> Error "Unterminated char literal"
                | '\'' :: remaining ->
                    // End of char literal
                    let str = System.String(List.rev chars |> List.toArray)
                    if str.Length = 0 then
                        Error "Empty char literal"
                    else
                        // Validate that it's a single Extended Grapheme Cluster using .NET's StringInfo
                        let enumerator = System.Globalization.StringInfo.GetTextElementEnumerator(str)
                        if enumerator.MoveNext() then
                            if enumerator.MoveNext() then
                                Error $"Char literal contains more than one grapheme cluster: '{str}'"
                            else
                                Ok (str, remaining)
                        else
                            Error "Empty char literal"
                | '\\' :: 'n' :: remaining ->
                    parseCharContent remaining ('\n' :: chars)
                | '\\' :: 't' :: remaining ->
                    parseCharContent remaining ('\t' :: chars)
                | '\\' :: 'r' :: remaining ->
                    parseCharContent remaining ('\r' :: chars)
                | '\\' :: '\\' :: remaining ->
                    parseCharContent remaining ('\\' :: chars)
                | '\\' :: '\'' :: remaining ->
                    parseCharContent remaining ('\'' :: chars)
                | '\\' :: '0' :: remaining ->
                    parseCharContent remaining ('\000' :: chars)
                | '\\' :: 'x' :: h1 :: h2 :: remaining ->
                    // Hex escape: \xNN
                    let hexStr = System.String([| h1; h2 |])
                    match System.Int32.TryParse(hexStr, System.Globalization.NumberStyles.HexNumber, null) with
                    | (true, value) ->
                        parseCharContent remaining (char value :: chars)
                    | (false, _) ->
                        Error $"Invalid hex escape sequence: \\x{hexStr}"
                | '\\' :: c :: _ ->
                    Error $"Unknown escape sequence: \\{c}"
                | c :: remaining ->
                    parseCharContent remaining (c :: chars)

            let parseCharLiteral () : Result<Token list, string> =
                match parseCharContent rest [] with
                | Ok (str, remaining) -> lexHelper remaining (TCharLit str :: acc)
                | Error err -> Error err

            let isApostropheTypeVarContext =
                match acc with
                | TLParen :: _  // parenthesized type context: ('a * 'b)
                | TStar :: _  // tuple-type element separator: 'a * 'b
                | TLt :: _  // generic/type arg list start: <'a>
                | TComma :: _  // additional generic/type arg: <'a, 'b>
                | TColon :: _  // type annotation: x: 'a
                | TArrow :: _  // function return type: ('a) -> 'b
                | TOf :: _  // sum payload type: Case of 'a
                | TEquals :: _ ->  // type alias body: type T = 'a
                    true
                | _ ->
                    false

            let rec parseTypeVarName (cs: char list) (chars: char list) : string * char list =
                match cs with
                | c :: remaining when System.Char.IsLetterOrDigit(c) || c = '_' ->
                    parseTypeVarName remaining (c :: chars)
                | _ ->
                    (System.String(List.rev chars |> List.toArray), cs)

            match rest with
            | c :: _ when isApostropheTypeVarContext && (System.Char.IsLetter(c) || c = '_') ->
                let (typeVarName, afterTypeVar) = parseTypeVarName rest []
                match afterTypeVar with
                | '\'' :: _ ->
                    // Still a char literal if we see a closing quote.
                    parseCharLiteral ()
                | _ ->
                    lexHelper afterTypeVar (TIdent typeVarName :: acc)
            | _ ->
                parseCharLiteral ()

        | '"' :: rest ->
            // Parse string literal with escape sequences
            let rec parseString (cs: char list) (chars: char list) : Result<string * char list, string> =
                match cs with
                | [] -> Error "Unterminated string literal"
                | '"' :: remaining ->
                    // End of string
                    let str = System.String(List.rev chars |> List.toArray)
                    Ok (str, remaining)
                | '\\' :: 'n' :: remaining ->
                    parseString remaining ('\n' :: chars)
                | '\\' :: 't' :: remaining ->
                    parseString remaining ('\t' :: chars)
                | '\\' :: 'r' :: remaining ->
                    parseString remaining ('\r' :: chars)
                | '\\' :: '\\' :: remaining ->
                    parseString remaining ('\\' :: chars)
                | '\\' :: '"' :: remaining ->
                    parseString remaining ('"' :: chars)
                | '\\' :: '\'' :: remaining ->
                    parseString remaining ('\'' :: chars)
                | '\\' :: '0' :: remaining ->
                    parseString remaining ('\000' :: chars)
                | '\\' :: 'x' :: h1 :: h2 :: remaining ->
                    // Hex escape: \xNN
                    let hexStr = System.String([| h1; h2 |])
                    match System.Int32.TryParse(hexStr, System.Globalization.NumberStyles.HexNumber, null) with
                    | (true, value) ->
                        parseString remaining (char value :: chars)
                    | (false, _) ->
                        Error $"Invalid hex escape sequence: \\x{hexStr}"
                | '\\' :: 'u' :: h1 :: h2 :: h3 :: h4 :: remaining ->
                    // Unicode escape: \uNNNN
                    let hexStr = System.String([| h1; h2; h3; h4 |])
                    match System.Int32.TryParse(hexStr, System.Globalization.NumberStyles.HexNumber, null) with
                    | (true, value) ->
                        parseString remaining (char value :: chars)
                    | (false, _) ->
                        Error $"Invalid unicode escape sequence: \\u{hexStr}"
                | '\\' :: c :: _ ->
                    Error $"Unknown escape sequence: \\{c}"
                | c :: remaining ->
                    parseString remaining (c :: chars)

            match parseString rest [] with
            | Ok (str, remaining) -> lexHelper remaining (TStringLit str :: acc)
            | Error err -> Error err
        | c :: _ ->
            Error $"Unexpected character: {c}"

    input |> Seq.toList |> fun cs -> lexHelper cs []

/// Parse function type parameters: type, type, ...) returning list and remaining tokens
let rec parseFunctionTypeParams (typeParams: Set<string>) (tokens: Token list) (acc: Type list) : Result<Type list * Token list, string> =
    match tokens with
    | TRParen :: rest ->
        // End of parameter list
        Ok (List.rev acc, rest)
    | _ ->
        // Parse a type
        parseTypeBase typeParams tokens
        |> Result.bind (fun (ty, remaining) ->
            match remaining with
            | TRParen :: rest -> Ok (List.rev (ty :: acc), rest)
            | TComma :: rest -> parseFunctionTypeParams typeParams rest (ty :: acc)
            | _ -> Error "Expected ',' or ')' in function type parameters")

/// Base type parser (no function types - used to parse function type components)
and parseTypeBase (typeParams: Set<string>) (tokens: Token list) : Result<Type * Token list, string> =
    match tokens with
    | TIdent "Int8" :: rest -> Ok (AST.TInt8, rest)
    | TIdent "Int16" :: rest -> Ok (AST.TInt16, rest)
    | TIdent "Int32" :: rest -> Ok (AST.TInt32, rest)
    | TIdent "Int64" :: rest -> Ok (AST.TInt64, rest)
    | TIdent "Int128" :: rest -> Ok (AST.TInt128, rest)
    | TIdent "UInt8" :: rest -> Ok (AST.TUInt8, rest)
    | TIdent "UInt16" :: rest -> Ok (AST.TUInt16, rest)
    | TIdent "UInt32" :: rest -> Ok (AST.TUInt32, rest)
    | TIdent "UInt64" :: rest -> Ok (AST.TUInt64, rest)
    | TIdent "UInt128" :: rest -> Ok (AST.TUInt128, rest)
    | TIdent "Bool" :: rest -> Ok (AST.TBool, rest)
    | TIdent "String" :: rest -> Ok (AST.TString, rest)
    | TIdent "Bytes" :: rest -> Ok (AST.TBytes, rest)
    | TIdent "Char" :: rest -> Ok (AST.TChar, rest)
    | TIdent "Float" :: rest -> Ok (AST.TFloat64, rest)
    | TIdent "Unit" :: rest -> Ok (AST.TUnit, rest)
    | TIdent "RawPtr" :: rest -> Ok (AST.TRawPtr, rest)  // Internal raw pointer type
    | TIdent typeName :: rest when Set.contains typeName typeParams ->
        Ok (TVar typeName, rest)
    | TIdent typeName :: rest when System.Char.IsLower(typeName.[0]) || typeName.StartsWith "_" ->
        // Allow type variables in annotations even when not explicitly listed in a type parameter context.
        // This keeps parser/pretty roundtrips stable for synthesized lambda type variables.
        Ok (TVar typeName, rest)
    | TIdent "List" :: TLt :: rest ->
        // List type: List<ElementType>
        parseTypeWithContext typeParams rest
        |> Result.bind (fun (elemType, afterElem) ->
            match afterElem with
            | TGt :: remaining -> Ok (TList elemType, remaining)
            | TShr :: remaining -> Ok (TList elemType, TGt :: remaining)  // >> is two >'s
            | _ -> Error "Expected '>' after List element type")
    | TIdent "Dict" :: TLt :: rest ->
        // Dict type: Dict<KeyType, ValueType>
        parseTypeWithContext typeParams rest
        |> Result.bind (fun (firstTypeArg, afterFirstArg) ->
            match afterFirstArg with
            | TComma :: valueRest ->
                parseTypeWithContext typeParams valueRest
                |> Result.bind (fun (valueType, afterValue) ->
                    match afterValue with
                    | TGt :: remaining -> Ok (TDict (firstTypeArg, valueType), remaining)
                    | TShr :: remaining -> Ok (TDict (firstTypeArg, valueType), TGt :: remaining)  // >> is two >'s
                    | _ -> Error "Expected '>' after Dict value type")
            | TGt :: remaining ->
                // Upstream syntax also supports Dict<ValueType> with String keys.
                Ok (TDict (AST.TString, firstTypeArg), remaining)
            | TShr :: remaining ->
                Ok (TDict (AST.TString, firstTypeArg), TGt :: remaining)  // >> is two >'s
            | _ -> Error "Expected ',' or '>' after Dict type argument")
    | TIdent typeName :: rest when System.Char.IsUpper(typeName.[0]) ->
        // Could be a simple type or a qualified type like Stdlib.Option.Option
        // First parse the full qualified name
        let rec parseQualTypeName (name: string) (toks: Token list) : string * Token list =
            match toks with
            | TDot :: TIdent nextName :: remaining when System.Char.IsUpper(nextName.[0]) ->
                parseQualTypeName (name + "." + nextName) remaining
            | _ -> (name, toks)
        let (fullTypeName, afterTypeName) = parseQualTypeName typeName rest
        // Check for type arguments <...>
        match afterTypeName with
        | TLt :: typeArgsStart ->
            // Generic type: TypeName<args>
            // Need to parse type args allowing lowercase type variables
            let rec parseTypeArgsInType (toks: Token list) (acc: Type list) : Result<Type list * Token list, string> =
                parseTypeWithContext typeParams toks
                |> Result.bind (fun (ty, remaining) ->
                    match remaining with
                    | TGt :: rest -> Ok (List.rev (ty :: acc), rest)
                    | TShr :: rest -> Ok (List.rev (ty :: acc), TGt :: rest)  // >> is two >'s
                    | TComma :: rest -> parseTypeArgsInType rest (ty :: acc)
                    | _ -> Error "Expected ',' or '>' after type argument in generic type")
            parseTypeArgsInType typeArgsStart []
            |> Result.map (fun (typeArgs, remaining) ->
                // Store as TSum with type arguments - type checker will validate
                (TSum (fullTypeName, typeArgs), remaining))
        | _ ->
            // Simple type without type arguments
            Ok (TRecord (fullTypeName, []), afterTypeName)
    | TLParen :: rest ->
        // Could be a function type: (int, int) -> bool
        // Or a tuple type: (int, int)
        parseFunctionTypeParams typeParams rest []
        |> Result.bind (fun (paramTypes, afterParams) ->
            match afterParams with
            | TArrow :: returnRest ->
                // Function type: (params) -> return
                parseTypeWithContext typeParams returnRest
                |> Result.map (fun (returnType, remaining) ->
                    (TFunction (paramTypes, returnType), remaining))
            | _ ->
                // Tuple type: (type, type, ...)
                if List.length paramTypes < 2 then
                    Error "Tuple type must have at least 2 elements"
                else
                    Ok (TTuple paramTypes, afterParams))
    | _ -> Error "Expected type annotation (Int64, Bool, String, Float, TypeName, type variable, or function type)"

/// Parse a type annotation with context for type parameters in scope
and parseTypeWithContext (typeParams: Set<string>) (tokens: Token list) : Result<Type * Token list, string> =
    parseTypeBase typeParams tokens

/// Parse a type annotation (no type parameters in scope)
let parseType (tokens: Token list) : Result<Type * Token list, string> =
    parseTypeWithContext Set.empty tokens

/// Parse type parameters: <t, u, v> (names only, for function definitions)
let rec parseTypeParams (tokens: Token list) (acc: string list) : Result<string list * Token list, string> =
    match tokens with
    | TIdent name :: TGt :: rest when System.Char.IsLower(name.[0]) ->
        // Last type parameter
        Ok (List.rev (name :: acc), rest)
    | TIdent name :: TComma :: rest when System.Char.IsLower(name.[0]) ->
        // More type parameters to come
        parseTypeParams rest (name :: acc)
    | TIdent name :: _ when not (System.Char.IsLower(name.[0])) ->
        Error $"Type parameter must start with lowercase letter: {name}"
    | TGt :: rest when List.isEmpty acc ->
        // Empty type parameters: <>
        Ok ([], rest)
    | _ -> Error "Expected type parameter name (lowercase identifier)"

/// Parse type for type arguments context (allows lowercase as type variables)
/// This is used when parsing call sites like func<t>(args) where t is a type variable
let rec parseTypeArgType (tokens: Token list) : Result<Type * Token list, string> =
    let withPossibleArrow
        (parsedType: Type)
        (remaining: Token list)
        : Result<Type * Token list, string> =
        match remaining with
        | TArrow :: returnRest ->
            parseTypeArgType returnRest
            |> Result.map (fun (returnType, remaining') ->
                (TFunction ([parsedType], returnType), remaining'))
        | _ ->
            Ok (parsedType, remaining)

    match tokens with
    | TIdent "Int8" :: rest -> withPossibleArrow AST.TInt8 rest
    | TIdent "Int16" :: rest -> withPossibleArrow AST.TInt16 rest
    | TIdent "Int32" :: rest -> withPossibleArrow AST.TInt32 rest
    | TIdent "Int64" :: rest -> withPossibleArrow AST.TInt64 rest
    | TIdent "Int128" :: rest -> withPossibleArrow AST.TInt128 rest
    | TIdent "UInt8" :: rest -> withPossibleArrow AST.TUInt8 rest
    | TIdent "UInt16" :: rest -> withPossibleArrow AST.TUInt16 rest
    | TIdent "UInt32" :: rest -> withPossibleArrow AST.TUInt32 rest
    | TIdent "UInt64" :: rest -> withPossibleArrow AST.TUInt64 rest
    | TIdent "UInt128" :: rest -> withPossibleArrow AST.TUInt128 rest
    | TIdent "Bool" :: rest -> withPossibleArrow AST.TBool rest
    | TIdent "String" :: rest -> withPossibleArrow AST.TString rest
    | TIdent "Bytes" :: rest -> withPossibleArrow AST.TBytes rest
    | TIdent "Char" :: rest -> withPossibleArrow AST.TChar rest
    | TIdent "Float" :: rest -> withPossibleArrow AST.TFloat64 rest
    | TIdent "Unit" :: rest -> withPossibleArrow AST.TUnit rest
    | TIdent "RawPtr" :: rest -> withPossibleArrow AST.TRawPtr rest  // Internal raw pointer type
    | TIdent "List" :: TLt :: rest ->
        // List type: List<ElementType>
        parseTypeArgType rest
        |> Result.bind (fun (elemType, afterElem) ->
            match afterElem with
            | TGt :: remaining -> withPossibleArrow (TList elemType) remaining
            | TShr :: remaining -> withPossibleArrow (TList elemType) (TGt :: remaining)  // >> is two >'s
            | _ -> Error "Expected '>' after List element type in type argument")
    | TIdent "Dict" :: TLt :: rest ->
        // Dict type: Dict<KeyType, ValueType>
        parseTypeArgType rest
        |> Result.bind (fun (firstTypeArg, afterFirstArg) ->
            match afterFirstArg with
            | TComma :: valueRest ->
                parseTypeArgType valueRest
                |> Result.bind (fun (valueType, afterValue) ->
                    match afterValue with
                    | TGt :: remaining -> withPossibleArrow (TDict (firstTypeArg, valueType)) remaining
                    | TShr :: remaining -> withPossibleArrow (TDict (firstTypeArg, valueType)) (TGt :: remaining)  // >> is two >'s
                    | _ -> Error "Expected '>' after Dict value type in type argument")
            | TGt :: remaining ->
                withPossibleArrow (TDict (AST.TString, firstTypeArg)) remaining
            | TShr :: remaining ->
                withPossibleArrow (TDict (AST.TString, firstTypeArg)) (TGt :: remaining)  // >> is two >'s
            | _ -> Error "Expected ',' or '>' after Dict type argument")
    | TIdent typeName :: rest when System.Char.IsLower(typeName.[0]) ->
        // Lowercase identifier is a type variable in type argument context
        withPossibleArrow (TVar typeName) rest
    | TIdent typeName :: rest when System.Char.IsUpper(typeName.[0]) ->
        // Could be a simple type or a qualified type like Stdlib.Option.Option
        // First parse the full qualified name
        let rec parseQualTypeName (name: string) (toks: Token list) : string * Token list =
            match toks with
            | TDot :: TIdent nextName :: remaining when System.Char.IsUpper(nextName.[0]) ->
                parseQualTypeName (name + "." + nextName) remaining
            | _ -> (name, toks)
        let (fullTypeName, afterTypeName) = parseQualTypeName typeName rest
        // Check for type arguments <...>
        match afterTypeName with
        | TLt :: typeArgsStart ->
            // Generic type: TypeName<args> - recursively parse type arguments
            let rec parseNestedTypeArgs (toks: Token list) (acc: Type list) : Result<Type list * Token list, string> =
                parseTypeArgType toks
                |> Result.bind (fun (ty, remaining) ->
                    match remaining with
                    | TGt :: rest -> Ok (List.rev (ty :: acc), rest)
                    | TShr :: rest -> Ok (List.rev (ty :: acc), TGt :: rest)  // >> is two >'s
                    | TComma :: rest -> parseNestedTypeArgs rest (ty :: acc)
                    | _ -> Error "Expected ',' or '>' after type argument in generic type")
            parseNestedTypeArgs typeArgsStart []
            |> Result.bind (fun (typeArgs, remaining) ->
                withPossibleArrow (TSum (fullTypeName, typeArgs)) remaining)
        | _ ->
            // Simple type without type arguments
            withPossibleArrow (TRecord (fullTypeName, [])) afterTypeName
    | TLParen :: rest ->
        // Tuple type or function type: (Type1, Type2, ...) or (Type1, Type2) -> RetType
        parseTypeArgTupleElements rest []
        |> Result.bind (fun (elemTypes, afterElems) ->
            match afterElems with
            | TArrow :: returnRest ->
                // Function type: (params) -> return
                parseTypeArgType returnRest
                |> Result.map (fun (returnType, remaining) ->
                    (TFunction (elemTypes, returnType), remaining))
            | _ ->
                // Tuple type: (type, type, ...)
                if List.length elemTypes < 2 then
                    Error "Tuple type must have at least 2 elements"
                else
                    Ok (TTuple elemTypes, afterElems))
    | _ -> Error "Expected type in type argument"

/// Parse tuple elements in type argument context: Type1, Type2, ... )
and parseTypeArgTupleElements (tokens: Token list) (acc: Type list) : Result<Type list * Token list, string> =
    match tokens with
    | TRParen :: rest ->
        // End of tuple/parameter list
        Ok (List.rev acc, rest)
    | _ ->
        // Parse a type
        parseTypeArgType tokens
        |> Result.bind (fun (ty, remaining) ->
            match remaining with
            | TRParen :: rest -> Ok (List.rev (ty :: acc), rest)
            | TComma :: rest -> parseTypeArgTupleElements rest (ty :: acc)
            | _ -> Error "Expected ',' or ')' in tuple type")

/// Parse type arguments: <Int64, Bool, Point, t> (concrete types or type vars, for call sites)
let rec parseTypeArgs (tokens: Token list) (acc: Type list) : Result<Type list * Token list, string> =
    parseTypeArgType tokens
    |> Result.bind (fun (ty, remaining) ->
        match remaining with
        | TGt :: rest ->
            // Last type argument
            Ok (List.rev (ty :: acc), rest)
        | TShr :: rest ->
            // >> is two >'s - last type argument, put one > back
            Ok (List.rev (ty :: acc), TGt :: rest)
        | TComma :: rest ->
            // More type arguments to come
            parseTypeArgs rest (ty :: acc)
        | _ -> Error "Expected ',' or '>' after type argument")

/// Parse a single parameter: IDENT : type (with type parameter context)
let parseParamWithContext (typeParams: Set<string>) (tokens: Token list) : Result<(string * Type) * Token list, string> =
    match tokens with
    | TIdent name :: TColon :: rest ->
        parseTypeWithContext typeParams rest
        |> Result.map (fun (ty, remaining) -> ((name, ty), remaining))
    | _ -> Error "Expected parameter (name : type)"

/// Parse parameter list: param (, param)* (with type parameter context)
let rec parseParamsWithContext (typeParams: Set<string>) (tokens: Token list) (acc: (string * Type) list) : Result<(string * Type) list * Token list, string> =
    match tokens with
    | TRParen :: _ ->
        // End of parameters
        Ok (List.rev acc, tokens)
    | _ ->
        // Parse a parameter
        parseParamWithContext typeParams tokens
        |> Result.bind (fun (param, remaining) ->
            match remaining with
            | TComma :: rest ->
                // More parameters
                parseParamsWithContext typeParams rest (param :: acc)
            | TRParen :: _ ->
                // End of parameters
                Ok (List.rev (param :: acc), remaining)
            | _ -> Error "Expected ',' or ')' after parameter")

/// Parse parameter list: param (, param)* (no type parameters in scope)
let rec parseParams (tokens: Token list) (acc: (string * Type) list) : Result<(string * Type) list * Token list, string> =
    parseParamsWithContext Set.empty tokens acc

/// Parse record fields in a type definition: { name: Type, name: Type, ... }
/// Uses type parameter context so generic record fields can reference type vars.
let rec parseRecordFieldsWithContext
    (typeParams: Set<string>)
    (tokens: Token list)
    (acc: (string * Type) list)
    : Result<(string * Type) list * Token list, string> =
    match tokens with
    | TRBrace :: rest ->
        // End of fields
        Ok (List.rev acc, rest)
    | TIdent name :: TColon :: rest ->
        parseTypeWithContext typeParams rest
        |> Result.bind (fun (ty, remaining) ->
            match remaining with
            | TComma :: rest' ->
                // More fields
                parseRecordFieldsWithContext typeParams rest' ((name, ty) :: acc)
            | TRBrace :: rest' ->
                // End of fields
                Ok (List.rev ((name, ty) :: acc), rest')
            | _ -> Error "Expected ',' or '}' after record field")
    | _ -> Error "Expected field name in record definition"

/// Parse record fields in a type definition with no type parameter context.
let parseRecordFields (tokens: Token list) (acc: (string * Type) list) : Result<(string * Type) list * Token list, string> =
    parseRecordFieldsWithContext Set.empty tokens acc

/// Parse sum type variants: Variant1 | Variant2 of Type | ...
/// Returns list of variants and remaining tokens
let private parseVariantPayloadType (tokens: Token list) : Result<Type * Token list, string> =
    let rec stripLabels (expectLabel: bool) (remainingTokens: Token list) (acc: Token list) : Token list =
        match remainingTokens with
        | TIdent _ :: TColon :: rest when expectLabel ->
            stripLabels false rest acc
        | TStar :: rest ->
            stripLabels true rest (TStar :: acc)
        | token :: rest ->
            stripLabels false rest (token :: acc)
        | [] ->
            List.rev acc

    let normalizedTokens = stripLabels true tokens []
    parseType normalizedTokens

let private parseVariantPayloadTypeWithContext
    (typeParamSet: Set<string>)
    (tokens: Token list)
    : Result<Type * Token list, string> =
    let rec stripLabels (expectLabel: bool) (remainingTokens: Token list) (acc: Token list) : Token list =
        match remainingTokens with
        | TIdent _ :: TColon :: rest when expectLabel ->
            stripLabels false rest acc
        | TStar :: rest ->
            stripLabels true rest (TStar :: acc)
        | token :: rest ->
            stripLabels false rest (token :: acc)
        | [] ->
            List.rev acc

    let normalizedTokens = stripLabels true tokens []
    parseTypeWithContext typeParamSet normalizedTokens

let rec parseVariants (tokens: Token list) (acc: Variant list) : Result<Variant list * Token list, string> =
    match tokens with
    | TIdent variantName :: TOf :: rest when System.Char.IsUpper(variantName.[0]) ->
        // Variant with payload: Variant of Type
        parseVariantPayloadType rest
        |> Result.bind (fun (payloadType, afterType) ->
            let variant = { Name = variantName; Payload = Some payloadType }
            match afterType with
            | TBar :: rest' ->
                // More variants
                parseVariants rest' (variant :: acc)
            | _ ->
                // End of variants
                Ok (List.rev (variant :: acc), afterType))
    | TIdent variantName :: rest when System.Char.IsUpper(variantName.[0]) ->
        // Simple enum variant (no payload)
        let variant = { Name = variantName; Payload = None }
        match rest with
        | TBar :: rest' ->
            // More variants
            parseVariants rest' (variant :: acc)
        | _ ->
            // End of variants (next token is not a bar)
            Ok (List.rev (variant :: acc), rest)
    | _ -> Error "Expected variant name (must start with uppercase letter)"

/// Parse sum type variants with type parameter context: Variant1 | Variant2 of t | ...
/// Uses parseTypeWithContext to resolve type parameters
let rec parseVariantsWithContext (typeParams: string list) (tokens: Token list) (acc: Variant list) : Result<Variant list * Token list, string> =
    let typeParamSet = Set.ofList typeParams
    match tokens with
    | TIdent variantName :: TOf :: rest when System.Char.IsUpper(variantName.[0]) ->
        // Variant with payload: Variant of Type
        parseVariantPayloadTypeWithContext typeParamSet rest
        |> Result.bind (fun (payloadType, afterType) ->
            let variant = { Name = variantName; Payload = Some payloadType }
            match afterType with
            | TBar :: rest' ->
                // More variants
                parseVariantsWithContext typeParams rest' (variant :: acc)
            | _ ->
                // End of variants
                Ok (List.rev (variant :: acc), afterType))
    | TIdent variantName :: rest when System.Char.IsUpper(variantName.[0]) ->
        // Simple enum variant (no payload)
        let variant = { Name = variantName; Payload = None }
        match rest with
        | TBar :: rest' ->
            // More variants
            parseVariantsWithContext typeParams rest' (variant :: acc)
        | _ ->
            // End of variants (next token is not a bar)
            Ok (List.rev (variant :: acc), rest)
    | _ -> Error "Expected variant name (must start with uppercase letter)"

/// Parse a qualified type name: Name or Stdlib.Result.Result
let rec parseQualifiedTypeName (firstName: string) (tokens: Token list) : string * Token list =
    match tokens with
    | TDot :: TIdent nextName :: rest when System.Char.IsUpper(nextName.[0]) ->
        let (fullName, remaining) = parseQualifiedTypeName nextName rest
        (firstName + "." + fullName, remaining)
    | _ ->
        (firstName, tokens)

/// Parse a type definition: type Name = { fields } or type Name = Variant1 | Variant2 of Type | ...
/// Also supports type aliases: type Id = String, type MyList = List<Int64>
/// Supports qualified type names: type Stdlib.Result.Result = Ok of T | Error of E
/// Supports generic types: type Result<t, e> = Ok of t | Error of e
let parseTypeDef (tokens: Token list) : Result<TypeDef * Token list, string> =
    match tokens with
    | TType :: TIdent firstName :: rest when System.Char.IsUpper(firstName.[0]) ->
        // Parse potentially qualified type name
        let (typeName, afterName) = parseQualifiedTypeName firstName rest
        // Check for type parameters: <t, e>
        let parseBody typeParams afterTypeParams =
            match afterTypeParams with
            | TEquals :: TLBrace :: bodyRest ->
                // Record type: type Name = { field: Type, ... }
                let typeParamSet = Set.ofList typeParams
                parseRecordFieldsWithContext typeParamSet bodyRest []
                |> Result.map (fun (fields, remaining) ->
                    (RecordDef (typeName, typeParams, fields), remaining))
            | TEquals :: TIdent variantName :: TOf :: bodyRest when System.Char.IsUpper(variantName.[0]) ->
                // Sum type with first variant having payload: type Name = Variant of Type | ...
                let typeParamSet = Set.ofList typeParams
                parseVariantPayloadTypeWithContext typeParamSet bodyRest
                |> Result.bind (fun (payloadType, afterType) ->
                    let firstVariant = { Name = variantName; Payload = Some payloadType }
                    match afterType with
                    | TBar :: rest' ->
                        // More variants
                        parseVariantsWithContext typeParams rest' [firstVariant]
                        |> Result.map (fun (variants, remaining) ->
                            (SumTypeDef (typeName, typeParams, variants), remaining))
                    | _ ->
                        // Single variant sum type
                        Ok (SumTypeDef (typeName, typeParams, [firstVariant]), afterType))
            | TEquals :: TIdent variantName :: TBar :: bodyRest when System.Char.IsUpper(variantName.[0]) ->
                // Sum type with multiple variants: type Name = Variant1 | Variant2 | ...
                let firstVariant = { Name = variantName; Payload = None }
                parseVariantsWithContext typeParams bodyRest [firstVariant]
                |> Result.map (fun (variants, remaining) ->
                    (SumTypeDef (typeName, typeParams, variants), remaining))
            | TEquals :: rest' ->
                // Could be a type alias or a single-variant sum type
                // Try to parse as a type first
                let typeParamSet = Set.ofList typeParams
                match parseTypeWithContext typeParamSet rest' with
                | Ok (targetType, remaining) ->
                    // Decide: type alias or single-variant sum type?
                    // Rules:
                    // 1. Primitive types (Int64, String, etc.) → TYPE ALIAS
                    // 2. Generic types (List<T>, Result<T,E>) → TYPE ALIAS
                    // 3. Tuple types ((T, U)) → TYPE ALIAS
                    // 4. Function types ((T) -> U) → TYPE ALIAS
                    // 5. Simple name (TRecord):
                    //    - Same name as type being defined → SUM TYPE (recursive variant)
                    //    - End of input → SUM TYPE (backwards compat for single-variant enums)
                    //    - Otherwise → TYPE ALIAS (reference to existing type)
                    match targetType with
                    | TRecord (potentialVariant, _) when potentialVariant = typeName ->
                        // Same name as type being defined - this is a recursive variant definition
                        // e.g., type Unit2 = Unit2 defines a sum type with variant Unit2
                        let variant = { Name = potentialVariant; Payload = None }
                        Ok (SumTypeDef (typeName, typeParams, [variant]), remaining)
                    | TRecord (potentialVariant, _) when
                        // Not a primitive type and at end of input - treat as sum type for backwards compat
                        potentialVariant <> "Int64" && potentialVariant <> "Int32" && potentialVariant <> "Int16" && potentialVariant <> "Int8" &&
                        potentialVariant <> "UInt64" && potentialVariant <> "UInt32" && potentialVariant <> "UInt16" && potentialVariant <> "UInt8" &&
                        potentialVariant <> "Bool" && potentialVariant <> "String" && potentialVariant <> "Float" &&
                        (match remaining with [] -> true | _ -> false) ->
                        let variant = { Name = potentialVariant; Payload = None }
                        Ok (SumTypeDef (typeName, typeParams, [variant]), remaining)
                    | _ ->
                        // Type alias for:
                        // - Primitive types (parsed as TInt64, TString, etc. directly by parseType)
                        // - Generic types (TSum with type args, TList)
                        // - Tuple types (TTuple)
                        // - Function types (TFunction)
                        // - User types with remaining tokens (assumed to be alias to existing type)
                        Ok (TypeAlias (typeName, typeParams, targetType), remaining)
                | Error _ ->
                    Error "Expected type expression after '=' in type alias or variant name"
            | _ -> Error "Expected '=' after type name in type definition"
        match afterName with
        | TLt :: rest' ->
            // Generic type: type Name<t, e> = ...
            parseTypeParams rest' []
            |> Result.bind (fun (typeParams, afterParams) ->
                parseBody typeParams afterParams)
        | _ ->
            // Non-generic type
            parseBody [] afterName
    | TType :: TIdent name :: _ when not (System.Char.IsUpper(name.[0])) ->
        Error $"Type name must start with uppercase letter: {name}"
    | _ -> Error "Expected type definition: type Name = { fields } or type Name = Variant1 | Variant2"

/// Parse a qualified function name: name or Stdlib.Int64.add
let rec parseQualifiedFuncName (firstName: string) (tokens: Token list) : string * Token list =
    match tokens with
    | TDot :: TIdent nextName :: rest ->
        let (fullName, remaining) = parseQualifiedFuncName nextName rest
        (firstName + "." + fullName, remaining)
    | _ ->
        (firstName, tokens)

let private generatedUnitParam (index: int) : string * Type =
    ($"$unit{index}", TUnit)

let private ensureParamGroupNonEmpty (paramIndex: int) (parameters: (string * Type) list) : (string * Type) list =
    if List.isEmpty parameters then [generatedUnitParam paramIndex] else parameters

/// Parse a function definition: def name<T, U>(params) : type = body
/// Type parameters are optional: def name(params) : type = body is also valid
/// Qualified names supported: def Stdlib.Int64.add(params) : type = body
let parseFunctionDef (tokens: Token list) (parseExpr: Token list -> Result<Expr * Token list, string>) : Result<FunctionDef * Token list, string> =
    match tokens with
    | TDef :: TIdent firstName :: rest ->
        // Parse potentially qualified function name (e.g., Stdlib.Int64.add)
        let (name, afterName) = parseQualifiedFuncName firstName rest
        match afterName with
        | TLt :: rest' ->
            // Generic function: def name<T, U>(...)
            parseTypeParams rest' []
            |> Result.bind (fun (typeParams, afterTypeParams) ->
                // Check for duplicate type parameters
                if List.length typeParams <> (typeParams |> List.distinct |> List.length) then
                    Error "Duplicate type parameter names"
                else
                let typeParamsSet = Set.ofList typeParams
                match afterTypeParams with
                | TLParen :: paramsStart ->
                    // Parse parameters with type params in scope
                    let paramsResult =
                        match paramsStart with
                        | TRParen :: _ -> Ok ([], paramsStart)
                        | _ -> parseParamsWithContext typeParamsSet paramsStart []

                    paramsResult
                    |> Result.bind (fun (parameters, remaining) ->
                        let normalizedParameters = ensureParamGroupNonEmpty 0 parameters
                        match remaining with
                        | TRParen :: TColon :: rest'' ->
                            // Parse return type with type params in scope
                            parseTypeWithContext typeParamsSet rest''
                            |> Result.bind (fun (returnType, remaining') ->
                                match remaining' with
                                | TEquals :: rest''' ->
                                    // Parse body
                                    parseExpr rest'''
                                    |> Result.map (fun (body, remaining'') ->
                                        let funcDef = {
                                            Name = name
                                            TypeParams = typeParams
                                            Params = NonEmptyList.fromList normalizedParameters
                                            ReturnType = returnType
                                            Body = body
                                        }
                                        (funcDef, remaining''))
                                | _ -> Error "Expected '=' after function return type")
                        | _ -> Error "Expected ':' after function parameters")
                | _ -> Error "Expected '(' after type parameters")
        | TLParen :: rest' ->
            // Non-generic function: def name(...)
            let paramsResult =
                match rest' with
                | TRParen :: _ -> Ok ([], rest')
                | _ -> parseParams rest' []

            paramsResult
            |> Result.bind (fun (parameters, remaining) ->
                let normalizedParameters = ensureParamGroupNonEmpty 0 parameters
                match remaining with
                | TRParen :: TColon :: rest'' ->
                    // Parse return type
                    parseType rest''
                    |> Result.bind (fun (returnType, remaining') ->
                        match remaining' with
                        | TEquals :: rest''' ->
                            // Parse body
                            parseExpr rest'''
                            |> Result.map (fun (body, remaining'') ->
                                let funcDef = {
                                    Name = name
                                    TypeParams = []
                                    Params = NonEmptyList.fromList normalizedParameters
                                    ReturnType = returnType
                                    Body = body
                                }
                                (funcDef, remaining''))
                        | _ -> Error "Expected '=' after function return type")
                | _ -> Error "Expected ':' after function parameters")
        | _ -> Error $"Expected '<' or '(' after function name '{name}'"
    | _ -> Error "Expected function definition (def name(params) : type = body)"

/// Parse a pattern for pattern matching
let rec parsePattern (tokens: Token list) : Result<Pattern * Token list, string> =
    match tokens with
    | TUnderscore :: rest ->
        // Wildcard pattern: _
        Ok (PWildcard, rest)
    | TInt64 n :: rest ->
        // Integer literal pattern (Int64)
        Ok (PInt64 n, rest)
    | TInt128 n :: rest ->
        Ok (PInt128Literal n, rest)
    | TInt8 n :: rest ->
        Ok (PInt8Literal n, rest)
    | TInt16 n :: rest ->
        Ok (PInt16Literal n, rest)
    | TInt32 n :: rest ->
        Ok (PInt32Literal n, rest)
    | TUInt8 n :: rest ->
        Ok (PUInt8Literal n, rest)
    | TUInt16 n :: rest ->
        Ok (PUInt16Literal n, rest)
    | TUInt32 n :: rest ->
        Ok (PUInt32Literal n, rest)
    | TUInt64 n :: rest ->
        Ok (PUInt64Literal n, rest)
    | TUInt128 n :: rest ->
        Ok (PUInt128Literal n, rest)
    | TMinus :: TInt64 n :: rest ->
        // Negative integer literal pattern
        Ok (PInt64 (-n), rest)
    | TMinus :: TInt128 n :: rest when n = System.Int128.MinValue ->
        Ok (PInt128Literal System.Int128.MinValue, rest)
    | TMinus :: TInt128 n :: rest ->
        Ok (PInt128Literal (-n), rest)
    | TMinus :: TInt8 n :: rest ->
        Ok (PInt8Literal (sbyte (-int n)), rest)
    | TMinus :: TInt16 n :: rest ->
        Ok (PInt16Literal (int16 (-int n)), rest)
    | TMinus :: TInt32 n :: rest ->
        Ok (PInt32Literal (-n), rest)
    | TMinus :: TFloat f :: rest ->
        Ok (PFloat (-f), rest)
    | TTrue :: rest ->
        // Boolean true pattern
        Ok (PBool true, rest)
    | TFalse :: rest ->
        // Boolean false pattern
        Ok (PBool false, rest)
    | TStringLit s :: rest ->
        // String literal pattern
        Ok (PString s, rest)
    | TCharLit s :: rest ->
        Ok (PChar s, rest)
    | TFloat f :: rest ->
        // Float literal pattern
        Ok (PFloat f, rest)
    | TLParen :: TRParen :: rest ->
        // Unit pattern: ()
        Ok (PUnit, rest)
    | TLParen :: rest ->
        // Tuple pattern: (a, b, c)
        parseTuplePattern rest []
    | TLBrace :: _ ->
        // Anonymous record pattern is no longer supported
        Error "Record pattern requires type name: use 'TypeName { field = pattern, ... }'"
    | TLBracket :: rest ->
        // List pattern: [a, b, c] or []
        parseListPattern rest []
    | TIdent name :: TLParen :: rest when System.Char.IsUpper(name.[0]) ->
        // Constructor with payload pattern: Some(x)
        parsePattern rest
        |> Result.bind (fun (payloadPattern, remaining) ->
            match remaining with
            | TRParen :: rest' ->
                Ok (PConstructor (name, Some payloadPattern), rest')
            | _ -> Error "Expected ')' after constructor pattern payload")
    | TIdent typeName :: TLBrace :: rest when System.Char.IsUpper(typeName.[0]) ->
        // Record pattern with type name: Point { x = a, y = b }
        parseRecordPatternWithTypeName typeName rest []
    | TIdent name :: rest when System.Char.IsUpper(name.[0]) ->
        // Constructor pattern without payload: Red, None
        Ok (PConstructor (name, None), rest)
    | TIdent name :: rest ->
        // Variable pattern: x (binds the value)
        Ok (PVar name, rest)
    | _ -> Error "Expected pattern (_, variable, literal, or constructor)"

and parseTuplePattern (tokens: Token list) (acc: Pattern list) : Result<Pattern * Token list, string> =
    parsePattern tokens
    |> Result.bind (fun (pat, remaining) ->
        match remaining with
        | TRParen :: rest ->
            // End of tuple pattern
            let patterns = List.rev (pat :: acc)
            Ok (PTuple patterns, rest)
        | TComma :: rest ->
            // More elements
            parseTuplePattern rest (pat :: acc)
        | _ -> Error "Expected ',' or ')' in tuple pattern")

and parseRecordPatternWithTypeName (typeName: string) (tokens: Token list) (acc: (string * Pattern) list) : Result<Pattern * Token list, string> =
    // Parse record pattern with explicit type name: TypeName { field = pattern, ... }
    match tokens with
    | TRBrace :: rest ->
        // Empty record or end of fields
        let fields = List.rev acc
        Ok (PRecord (typeName, fields), rest)
    | TIdent fieldName :: TEquals :: rest ->
        parsePattern rest
        |> Result.bind (fun (pat, remaining) ->
            let field = (fieldName, pat)
            match remaining with
            | TRBrace :: rest' ->
                // End of record pattern
                let fields = List.rev (field :: acc)
                Ok (PRecord (typeName, fields), rest')
            | TComma :: rest' ->
                // More fields
                parseRecordPatternWithTypeName typeName rest' (field :: acc)
            | _ -> Error "Expected ',' or '}' in record pattern")
    | _ -> Error "Expected field name in record pattern"

and parseListPattern (tokens: Token list) (acc: Pattern list) : Result<Pattern * Token list, string> =
    match tokens with
    | TRBracket :: rest ->
        // Empty list or end of list pattern
        Ok (PList (List.rev acc), rest)
    | TDotDotDot :: rest ->
        // Rest pattern at start: [...t]
        parsePattern rest
        |> Result.bind (fun (tailPat, remaining) ->
            match remaining with
            | TRBracket :: rest' ->
                Ok (PListCons (List.rev acc, tailPat), rest')
            | _ -> Error "Expected ']' after rest pattern")
    | _ ->
        parsePattern tokens
        |> Result.bind (fun (pat, remaining) ->
            match remaining with
            | TRBracket :: rest ->
                // End of list pattern
                Ok (PList (List.rev (pat :: acc)), rest)
            | TComma :: TDotDotDot :: rest ->
                // Rest pattern after element: [a, b, ...t]
                parsePattern rest
                |> Result.bind (fun (tailPat, remaining') ->
                    match remaining' with
                    | TRBracket :: rest' ->
                        Ok (PListCons (List.rev (pat :: acc), tailPat), rest')
                    | _ -> Error "Expected ']' after rest pattern")
            | TComma :: rest ->
                // More elements
                parseListPattern rest (pat :: acc)
            | _ -> Error "Expected ',' or ']' in list pattern")

/// Parse a single case: | pat1 | pat2 when guard -> expr
/// Supports multiple patterns (pattern grouping) and optional guard clause
let parseCase (tokens: Token list) (parseExprFn: Token list -> Result<Expr * Token list, string>) : Result<MatchCase * Token list, string> =
    // Parse patterns until we see TWhen or TArrow
    let rec parsePatterns (toks: Token list) (acc: Pattern list) : Result<Pattern list * Token list, string> =
        match toks with
        | TBar :: rest ->
            parsePattern rest
            |> Result.bind (fun (pattern, remaining) ->
                // Check what comes next
                match remaining with
                | TBar :: _ ->
                    // Another pattern in the group
                    parsePatterns remaining (pattern :: acc)
                | TWhen :: _ | TArrow :: _ ->
                    // End of patterns, followed by guard or body
                    Ok (List.rev (pattern :: acc), remaining)
                | _ -> Error "Expected '|', 'when', or '->' after pattern")
        | _ -> Error "Expected '|' before pattern"

    parsePatterns tokens []
    |> Result.bind (fun (patterns, remaining) ->
        // Convert patterns list to NonEmptyList (safe since parsePatterns ensures at least one pattern)
        let patternsNel = NonEmptyList.fromList patterns
        // Parse optional guard
        match remaining with
        | TWhen :: rest' ->
            // Parse guard expression
            parseExprFn rest'
            |> Result.bind (fun (guard, remaining') ->
                match remaining' with
                | TArrow :: rest'' ->
                    // Parse body
                    parseExprFn rest''
                    |> Result.map (fun (body, remaining''') ->
                        ({ Patterns = patternsNel; Guard = Some guard; Body = body }, remaining'''))
                | _ -> Error "Expected '->' after guard expression")
        | TArrow :: rest' ->
            // No guard, parse body directly
            parseExprFn rest'
            |> Result.map (fun (body, remaining') ->
                ({ Patterns = patternsNel; Guard = None; Body = body }, remaining'))
        | _ -> Error "Expected 'when' or '->' after pattern")

/// Try to parse lambda parameters: (ident : type, ident : type, ...)
/// Returns Some (params, remaining) if successful, None otherwise
let tryParseLambdaParams (tokens: Token list) : (NonEmptyList<string * Type> * Token list) option =
    let rec parseParams (toks: Token list) (acc: (string * Type) list) : (NonEmptyList<string * Type> * Token list) option =
        match toks with
        | TRParen :: rest ->
            // End of parameters
            match List.rev acc with
            | [] -> Some (NonEmptyList.singleton (generatedUnitParam 0), rest)
            | parameters -> Some (NonEmptyList.fromList parameters, rest)
        | TIdent name :: TColon :: rest when not (System.Char.IsUpper(name.[0])) ->
            // Parameter: name : type
            match parseType rest with
            | Ok (ty, remaining) ->
                match remaining with
                | TComma :: rest' ->
                    // More parameters
                    parseParams rest' ((name, ty) :: acc)
                | TRParen :: rest' ->
                    // End of parameters
                    Some (NonEmptyList.fromList (List.rev ((name, ty) :: acc)), rest')
                | _ -> None  // Not a valid lambda
            | Error _ -> None  // Type parse failed
        | _ -> None  // Not a valid lambda parameter

    parseParams tokens []

/// Parser: convert tokens to AST
let parse (tokens: Token list) : Result<Program, string> =
    // Recursive descent parser with operator precedence
    // Precedence (low to high): or < and < comparison < +/- < */ < unary

    // Compiler syntax normally requires parenthesized call arguments (`f(x)`), but
    // upstream tests also exercise generic calls with a single space-applied argument
    // (`f<'a> "x"`). Keep this narrowly targeted to generic-call parsing.
    let canStartSpaceApplicationArg (toks: Token list) : bool =
        match toks with
        | TInt64 _ :: _
        | TInt128 _ :: _
        | TInt8 _ :: _
        | TInt16 _ :: _
        | TInt32 _ :: _
        | TUInt8 _ :: _
        | TUInt16 _ :: _
        | TUInt32 _ :: _
        | TUInt64 _ :: _
        | TUInt128 _ :: _
        | TFloat _ :: _
        | TStringLit _ :: _
        | TCharLit _ :: _
        | TTrue :: _
        | TFalse :: _
        | TIdent _ :: _
        | TLParen :: _
        | TLBracket :: _ -> true
        | _ -> false

    /// Parse multiple cases for pattern matching: | p1 -> e1 | p2 -> e2 ...
    let rec parseCases (toks: Token list) (acc: MatchCase list) : Result<MatchCase list * Token list, string> =
        match toks with
        | TBar :: _ ->
            // Another case
            parseCase toks parseExpr
            |> Result.bind (fun (case, remaining) ->
                parseCases remaining (case :: acc))
        | _ ->
            // End of cases
            if List.isEmpty acc then
                Error "Match expression must have at least one case"
            else
                Ok (List.rev acc, toks)

    and parseExpr (toks: Token list) : Result<Expr * Token list, string> =
        match toks with
        | TLet :: rest ->
            // Parse: let pattern = value in body
            // Supports simple let (let x = ...) and pattern matching (let (a, b) = ...)
            parsePattern rest
            |> Result.bind (fun (pattern, remaining) ->
                match remaining with
                | TEquals :: rest' ->
                    parseExpr rest'
                    |> Result.bind (fun (value, remaining') ->
                        match remaining' with
                        | TIn :: rest'' ->
                            parseExpr rest''
                            |> Result.map (fun (body, remaining'') ->
                                // If pattern is just PVar, use Let; otherwise desugar to Match
                                match pattern with
                                | PVar name -> (Let (name, value, body), remaining'')
                                | _ -> (Match (value, [{ Patterns = NonEmptyList.singleton pattern; Guard = None; Body = body }]), remaining''))
                        | _ -> Error "Expected 'in' after let binding value")
                | _ -> Error "Expected '=' after let binding pattern")
        | TIf :: rest ->
            // Parse: if cond then thenBranch [else elseBranch]
            // When else is omitted, synthesize unit: if cond then expr  ==>  if cond then expr else ()
            parseExpr rest
            |> Result.bind (fun (cond, remaining) ->
                match remaining with
                | TThen :: rest' ->
                    parseExpr rest'
                    |> Result.bind (fun (thenBranch, remaining') ->
                        match remaining' with
                        | TElse :: rest'' ->
                            parseExpr rest''
                            |> Result.map (fun (elseBranch, remaining'') ->
                                (If (cond, thenBranch, elseBranch), remaining''))
                        | _ ->
                            Ok (If (cond, thenBranch, UnitLiteral), remaining'))
                | _ -> Error "Expected 'then' after if condition")
        | TMatch :: rest ->
            // Parse: match scrutinee with | p1 -> e1 | p2 -> e2
            parseExpr rest
            |> Result.bind (fun (scrutinee, remaining) ->
                match remaining with
                | TWith :: rest' ->
                    parseCases rest' []
                    |> Result.map (fun (cases, remaining') ->
                        (Match (scrutinee, cases), remaining'))
                | _ -> Error "Expected 'with' after match scrutinee")
        | _ ->
            parsePipe toks

    and parsePipe (toks: Token list) : Result<Expr * Token list, string> =
        // Pipe operator |> has lowest precedence, left-associative
        // x |> f desugars to f(x) - Call if f is a name, Apply if f is an expression
        parseOr toks
        |> Result.bind (fun (left, remaining) ->
            let rec parsePipeRest (leftExpr: Expr) (toks: Token list) : Result<Expr * Token list, string> =
                match toks with
                | TPipe :: rest ->
                    parseOr rest
                    |> Result.bind (fun (right, remaining') ->
                        // Desugar: left |> right
                        // Pipe passes left as the FIRST argument to right
                        // This matches Dark/Darklang convention where data comes first
                        let pipedExpr =
                            match right with
                            | Var funcName ->
                                // Simple function reference: f becomes f(left)
                                Call (funcName, NonEmptyList.singleton leftExpr)
                            | Call (funcName, args) ->
                                // Partial application: f(a) becomes f(left, a)
                                match NonEmptyList.toList args with
                                | [UnitLiteral] ->
                                    // Zero-arg call placeholder: f() |> g() => g(f())
                                    Call (funcName, NonEmptyList.singleton leftExpr)
                                | _ ->
                                    Call (funcName, NonEmptyList.cons leftExpr args)
                            | TypeApp (funcName, typeArgs, args) ->
                                // Generic partial application: f<T>(a) becomes f<T>(left, a)
                                match NonEmptyList.toList args with
                                | [UnitLiteral] ->
                                    // Zero-arg call placeholder: f() |> g<T>() => g<T>(f())
                                    TypeApp (funcName, typeArgs, NonEmptyList.singleton leftExpr)
                                | _ ->
                                    TypeApp (funcName, typeArgs, NonEmptyList.cons leftExpr args)
                            | _ ->
                                // Lambda or other expression: apply left to it
                                Apply (right, NonEmptyList.singleton leftExpr)
                        parsePipeRest pipedExpr remaining')
                | _ -> Ok (leftExpr, toks)
            parsePipeRest left remaining)

    and parseOr (toks: Token list) : Result<Expr * Token list, string> =
        parseAnd toks
        |> Result.bind (fun (left, remaining) ->
            let rec parseOrRest (leftExpr: Expr) (toks: Token list) : Result<Expr * Token list, string> =
                match toks with
                | TOr :: rest ->
                    parseAnd rest
                    |> Result.bind (fun (right, remaining') ->
                        parseOrRest (BinOp (Or, leftExpr, right)) remaining')
                | _ -> Ok (leftExpr, toks)
            parseOrRest left remaining)

    and parseAnd (toks: Token list) : Result<Expr * Token list, string> =
        parseBitOr toks
        |> Result.bind (fun (left, remaining) ->
            let rec parseAndRest (leftExpr: Expr) (toks: Token list) : Result<Expr * Token list, string> =
                match toks with
                | TAnd :: rest ->
                    parseBitOr rest
                    |> Result.bind (fun (right, remaining') ->
                        parseAndRest (BinOp (And, leftExpr, right)) remaining')
                | _ -> Ok (leftExpr, toks)
            parseAndRest left remaining)

    and parseBitOr (toks: Token list) : Result<Expr * Token list, string> =
        parseBitXor toks
        |> Result.bind (fun (left, remaining) ->
            let rec parseBitOrRest (leftExpr: Expr) (toks: Token list) : Result<Expr * Token list, string> =
                match toks with
                | TBitOr :: rest ->
                    parseBitXor rest
                    |> Result.bind (fun (right, remaining') ->
                        parseBitOrRest (BinOp (BitOr, leftExpr, right)) remaining')
                | _ -> Ok (leftExpr, toks)
            parseBitOrRest left remaining)

    and parseBitXor (toks: Token list) : Result<Expr * Token list, string> =
        parseBitAnd toks
        |> Result.bind (fun (left, remaining) ->
            let rec parseBitXorRest (leftExpr: Expr) (toks: Token list) : Result<Expr * Token list, string> =
                match toks with
                | TBitXor :: rest ->
                    parseBitAnd rest
                    |> Result.bind (fun (right, remaining') ->
                        parseBitXorRest (BinOp (BitXor, leftExpr, right)) remaining')
                | _ -> Ok (leftExpr, toks)
            parseBitXorRest left remaining)

    and parseBitAnd (toks: Token list) : Result<Expr * Token list, string> =
        parseComparison toks
        |> Result.bind (fun (left, remaining) ->
            let rec parseBitAndRest (leftExpr: Expr) (toks: Token list) : Result<Expr * Token list, string> =
                match toks with
                | TBitAnd :: rest ->
                    parseComparison rest
                    |> Result.bind (fun (right, remaining') ->
                        parseBitAndRest (BinOp (BitAnd, leftExpr, right)) remaining')
                | _ -> Ok (leftExpr, toks)
            parseBitAndRest left remaining)

    and parseComparison (toks: Token list) : Result<Expr * Token list, string> =
        parseShift toks
        |> Result.bind (fun (left, remaining) ->
            // Comparison operators are non-associative (no chaining)
            match remaining with
            | TEqEq :: rest ->
                parseShift rest
                |> Result.map (fun (right, remaining') ->
                    (BinOp (Eq, left, right), remaining'))
            | TNeq :: rest ->
                parseShift rest
                |> Result.map (fun (right, remaining') ->
                    (BinOp (Neq, left, right), remaining'))
            | TLt :: rest ->
                parseShift rest
                |> Result.map (fun (right, remaining') ->
                    (BinOp (Lt, left, right), remaining'))
            | TGt :: rest ->
                parseShift rest
                |> Result.map (fun (right, remaining') ->
                    (BinOp (Gt, left, right), remaining'))
            | TLte :: rest ->
                parseShift rest
                |> Result.map (fun (right, remaining') ->
                    (BinOp (Lte, left, right), remaining'))
            | TGte :: rest ->
                parseShift rest
                |> Result.map (fun (right, remaining') ->
                    (BinOp (Gte, left, right), remaining'))
            | _ -> Ok (left, remaining))

    and parseShift (toks: Token list) : Result<Expr * Token list, string> =
        parseAdditive toks
        |> Result.bind (fun (left, remaining) ->
            let rec parseShiftRest (leftExpr: Expr) (toks: Token list) : Result<Expr * Token list, string> =
                match toks with
                | TShl :: rest ->
                    parseAdditive rest
                    |> Result.bind (fun (right, remaining') ->
                        parseShiftRest (BinOp (Shl, leftExpr, right)) remaining')
                | TShr :: rest ->
                    parseAdditive rest
                    |> Result.bind (fun (right, remaining') ->
                        parseShiftRest (BinOp (Shr, leftExpr, right)) remaining')
                | _ -> Ok (leftExpr, toks)
            parseShiftRest left remaining)

    and parseAdditive (toks: Token list) : Result<Expr * Token list, string> =
        parseMultiplicative toks
        |> Result.bind (fun (left, remaining) ->
            let rec parseAdditiveRest (leftExpr: Expr) (toks: Token list) : Result<Expr * Token list, string> =
                match toks with
                | TPlus :: rest ->
                    parseMultiplicative rest
                    |> Result.bind (fun (right, remaining') ->
                        parseAdditiveRest (BinOp (Add, leftExpr, right)) remaining')
                | TMinus :: rest ->
                    parseMultiplicative rest
                    |> Result.bind (fun (right, remaining') ->
                        parseAdditiveRest (BinOp (Sub, leftExpr, right)) remaining')
                | TPlusPlus :: rest ->
                    parseMultiplicative rest
                    |> Result.bind (fun (right, remaining') ->
                        parseAdditiveRest (BinOp (StringConcat, leftExpr, right)) remaining')
                | _ -> Ok (leftExpr, toks)
            parseAdditiveRest left remaining)

    and parseMultiplicative (toks: Token list) : Result<Expr * Token list, string> =
        parseUnary toks
        |> Result.bind (fun (left, remaining) ->
            let rec parseMultiplicativeRest (leftExpr: Expr) (toks: Token list) : Result<Expr * Token list, string> =
                match toks with
                | TStar :: rest ->
                    parseUnary rest
                    |> Result.bind (fun (right, remaining') ->
                        parseMultiplicativeRest (BinOp (Mul, leftExpr, right)) remaining')
                | TSlash :: rest ->
                    parseUnary rest
                    |> Result.bind (fun (right, remaining') ->
                        parseMultiplicativeRest (BinOp (Div, leftExpr, right)) remaining')
                | TPercent :: rest ->
                    parseUnary rest
                    |> Result.bind (fun (right, remaining') ->
                        parseMultiplicativeRest (BinOp (Mod, leftExpr, right)) remaining')
                | _ -> Ok (leftExpr, toks)
            parseMultiplicativeRest left remaining)

    and parseUnary (toks: Token list) : Result<Expr * Token list, string> =
        match toks with
        // Negative integer literals - parse directly as negative values
        | TMinus :: TInt64 n :: rest -> Ok (Int64Literal (-n), rest)
        | TMinus :: TInt128 n :: rest when n = System.Int128.MinValue ->
            Ok (Int128Literal System.Int128.MinValue, rest)
        | TMinus :: TInt128 n :: rest -> Ok (Int128Literal (-n), rest)
        | TMinus :: TInt8 n :: rest when n = System.SByte.MinValue ->
            Ok (Int8Literal System.SByte.MinValue, rest)
        | TMinus :: TInt8 n :: rest -> Ok (Int8Literal (-n), rest)
        | TMinus :: TInt16 n :: rest when n = System.Int16.MinValue ->
            Ok (Int16Literal System.Int16.MinValue, rest)
        | TMinus :: TInt16 n :: rest -> Ok (Int16Literal (-n), rest)
        | TMinus :: TInt32 n :: rest when n = System.Int32.MinValue ->
            Ok (Int32Literal System.Int32.MinValue, rest)
        | TMinus :: TInt32 n :: rest -> Ok (Int32Literal (-n), rest)
        | TMinus :: TFloat f :: rest -> Ok (FloatLiteral (-f), rest)
        | TMinus :: rest ->
            // For non-literal expressions, use UnaryOp
            parseUnary rest
            |> Result.map (fun (expr, remaining) -> (UnaryOp (Neg, expr), remaining))
        | TNot :: rest ->
            parseUnary rest
            |> Result.map (fun (expr, remaining) -> (UnaryOp (Not, expr), remaining))
        | TBitNot :: rest ->
            parseUnary rest
            |> Result.map (fun (expr, remaining) -> (UnaryOp (BitNot, expr), remaining))
        | _ ->
            parsePrimary toks

    and parsePrimary (toks: Token list) : Result<Expr * Token list, string> =
        // Parse a primary expression, then handle any postfix operations
        parsePrimaryBase toks
        |> Result.bind (fun (expr, remaining) ->
            parsePostfix expr remaining)

    // Parse a qualified identifier chain: Stdlib.Int64.add
    // Returns the full qualified name and remaining tokens
    and parseQualifiedIdent (firstName: string) (toks: Token list) : string * Token list =
        match toks with
        | TDot :: TIdent nextName :: rest ->
            // Continue the chain: firstName.nextName...
            let (fullName, remaining) = parseQualifiedIdent nextName rest
            (firstName + "." + fullName, remaining)
        | _ ->
            // End of chain
            (firstName, toks)

    and parsePrimaryBase (toks: Token list) : Result<Expr * Token list, string> =
        match toks with
        | TInt64 n :: rest -> Ok (Int64Literal n, rest)
        | TInt128 n :: rest -> Ok (Int128Literal n, rest)
        | TInt8 n :: rest -> Ok (Int8Literal n, rest)
        | TInt16 n :: rest -> Ok (Int16Literal n, rest)
        | TInt32 n :: rest -> Ok (Int32Literal n, rest)
        | TUInt8 n :: rest -> Ok (UInt8Literal n, rest)
        | TUInt16 n :: rest -> Ok (UInt16Literal n, rest)
        | TUInt32 n :: rest -> Ok (UInt32Literal n, rest)
        | TUInt64 n :: rest -> Ok (UInt64Literal n, rest)
        | TUInt128 n :: rest -> Ok (UInt128Literal n, rest)
        | TFloat f :: rest -> Ok (FloatLiteral f, rest)
        | TStringLit s :: rest -> Ok (StringLiteral s, rest)
        | TCharLit s :: rest -> Ok (CharLiteral s, rest)
        | TInterpString parts :: rest ->
            // Parse interpolated string into AST.InterpolatedString
            let rec parseInterpParts (parts: InterpPart list) (acc: AST.StringPart list) : Result<AST.StringPart list, string> =
                match parts with
                | [] -> Ok (List.rev acc)
                | InterpText s :: remaining ->
                    parseInterpParts remaining (AST.StringText s :: acc)
                | InterpTokens tokens :: remaining ->
                    // Parse the tokens as an expression
                    match parseExpr (tokens @ [TEOF]) with
                    | Ok (expr, [TEOF]) ->
                        parseInterpParts remaining (AST.StringExpr expr :: acc)
                    | Ok (_, leftover) ->
                        Error $"Unexpected tokens after interpolated expression: {leftover}"
                    | Error err ->
                        Error $"Error parsing interpolated expression: {err}"
            match parseInterpParts parts [] with
            | Ok astParts -> Ok (InterpolatedString astParts, rest)
            | Error err -> Error err
        | TTrue :: rest -> Ok (BoolLiteral true, rest)
        | TFalse :: rest -> Ok (BoolLiteral false, rest)
        // Qualified identifier: Stdlib.Int64.add, Module.func, or Stdlib.Result.Result.Ok
        | TIdent name :: TDot :: TIdent nextName :: rest when System.Char.IsUpper(name.[0]) ->
            // Parse the full qualified name
            let (qualifiedTail, afterQualified) = parseQualifiedIdent nextName rest
            let fullName = name + "." + qualifiedTail
            // Check if the last segment is uppercase (constructor) or lowercase (function)
            let lastDotIdx = fullName.LastIndexOf('.')
            let lastSegment = if lastDotIdx >= 0 then fullName.Substring(lastDotIdx + 1) else fullName
            let isConstructor = System.Char.IsUpper(lastSegment.[0])
            // Check what follows - function call, constructor, or variable reference
            match afterQualified with
            | TLBrace :: recordFieldsStart when isConstructor ->
                // Qualified record literal: Module.TypeName { field = value, ... }
                parseRecordLiteralFieldsWithTypeName fullName recordFieldsStart []
            | TLParen :: argsStart when isConstructor ->
                // Qualified constructor with payload: Stdlib.Result.Result.Ok(5)
                // Split into type name and variant name
                let typeName = fullName.Substring(0, lastDotIdx)
                let variantName = lastSegment
                parseExpr argsStart
                |> Result.bind (fun (payloadExpr, remaining) ->
                    match remaining with
                    | TRParen :: rest' ->
                        Ok (Constructor (typeName, variantName, Some payloadExpr), rest')
                    | _ -> Error "Expected ')' after constructor payload")
            | _ when isConstructor ->
                // Qualified constructor without payload: Stdlib.Color.Red
                let typeName = fullName.Substring(0, lastDotIdx)
                let variantName = lastSegment
                Ok (Constructor (typeName, variantName, None), afterQualified)
            | TLt :: typeArgsStart ->
                // Qualified generic function call: Stdlib.List.length<t>(args)
                let looksLikeTypeArgs tokens =
                    match parseTypeArgs tokens [] with
                    | Ok (_, afterTypes) ->
                        match afterTypes with
                        | TLParen :: _ -> true
                        | _ when canStartSpaceApplicationArg afterTypes -> true
                        | _ -> false
                    | Error _ -> false
                if looksLikeTypeArgs typeArgsStart then
                    parseTypeArgs typeArgsStart []
                    |> Result.bind (fun (typeArgs, afterTypes) ->
                        match afterTypes with
                        | TLParen :: argsStart ->
                            parseCallArgs argsStart []
                            |> Result.map (fun (args, remaining) ->
                                (TypeApp (fullName, typeArgs, args), remaining))
                        | _ when canStartSpaceApplicationArg afterTypes ->
                            parseSpaceApplicationArgs afterTypes []
                            |> Result.map (fun (args, remaining) ->
                                (TypeApp (fullName, typeArgs, NonEmptyList.fromList args), remaining))
                        | _ ->
                            Error $"Expected '(' or space-applied argument after type arguments in generic call to {fullName}")
                else
                    // Not type args, treat as variable reference and leave < for comparison
                    Ok (Var fullName, TLt :: typeArgsStart)
            | TLParen :: argsStart ->
                // Qualified function call: Stdlib.Int64.add(args)
                parseCallArgs argsStart []
                |> Result.map (fun (args, remaining) ->
                    (Call (fullName, args), remaining))
            | _ ->
                // Qualified variable reference (function as value)
                Ok (Var fullName, afterQualified)
        | TIdent name :: TLt :: rest when not (System.Char.IsUpper(name.[0])) ->
            // Could be generic function call: name<type, ...>(args)
            // Or could be comparison: name < expr
            // Disambiguate by looking for pattern: ident (> | ,ident)* > (
            // i.e., a sequence of identifiers/types followed by > and then (
            let rec looksLikeGenericCall tokens =
                match tokens with
                | TIdent _ :: TGt :: TLParen :: _ -> true   // Single type arg: name<t>(
                | TIdent _ :: TComma :: rest -> looksLikeGenericCall rest  // More args: name<t, ...
                | TIdent "List" :: TLt :: rest ->  // Nested List<...>
                    // Skip past nested generic type
                    let rec skipNested toks depth =
                        match toks with
                        | TGt :: remaining when depth = 1 -> Some remaining
                        | TGt :: remaining -> skipNested remaining (depth - 1)
                        | TShr :: remaining when depth = 1 -> Some (TGt :: remaining)  // >> is two >'s
                        | TShr :: remaining when depth = 2 -> Some remaining  // both >'s consumed
                        | TShr :: remaining -> skipNested remaining (depth - 2)  // >> decreases by 2
                        | TLt :: remaining -> skipNested remaining (depth + 1)
                        | _ :: remaining -> skipNested remaining depth
                        | [] -> None
                    match skipNested rest 1 with
                    | Some (TGt :: TLParen :: _) -> true
                    | Some (TShr :: _) -> true  // >> followed by ( - TShr has one > left for outer
                    | Some (TComma :: rest') -> looksLikeGenericCall rest'
                    | _ -> false
                | TIdent "Dict" :: TLt :: rest ->  // Nested Dict<...>
                    // Skip past nested generic type
                    let rec skipNested toks depth =
                        match toks with
                        | TGt :: remaining when depth = 1 -> Some remaining
                        | TGt :: remaining -> skipNested remaining (depth - 1)
                        | TShr :: remaining when depth = 1 -> Some (TGt :: remaining)  // >> is two >'s
                        | TShr :: remaining when depth = 2 -> Some remaining  // both >'s consumed
                        | TShr :: remaining -> skipNested remaining (depth - 2)  // >> decreases by 2
                        | TLt :: remaining -> skipNested remaining (depth + 1)
                        | _ :: remaining -> skipNested remaining depth
                        | [] -> None
                    match skipNested rest 1 with
                    | Some (TGt :: TLParen :: _) -> true
                    | Some (TShr :: _) -> true  // >> followed by ( - TShr has one > left for outer
                    | Some (TComma :: rest') -> looksLikeGenericCall rest'
                    | _ -> false
                | TLParen :: rest ->  // Tuple type: (Type1, Type2, ...)
                    // Skip until matching )
                    let rec skipParens toks depth =
                        match toks with
                        | TRParen :: remaining when depth = 1 -> Some remaining
                        | TRParen :: remaining -> skipParens remaining (depth - 1)
                        | TLParen :: remaining -> skipParens remaining (depth + 1)
                        | _ :: remaining -> skipParens remaining depth
                        | [] -> None
                    match skipParens rest 1 with
                    | Some (TGt :: TLParen :: _) -> true  // (T1, T2)>(
                    | Some (TShr :: _) -> true  // >> followed by ( - TShr has one > left for outer
                    | Some (TComma :: rest') -> looksLikeGenericCall rest'  // (T1, T2), more types
                    | Some (TArrow :: _) -> true  // Function type: (T1, T2) -> R
                    | _ -> false
                | _ -> false
            if looksLikeGenericCall rest then
                // Parse as generic call
                parseTypeArgs rest []
                |> Result.bind (fun (typeArgs, afterTypes) ->
                    match afterTypes with
                    | TLParen :: argsStart ->
                        parseCallArgs argsStart []
                        |> Result.map (fun (args, remaining) ->
                            (TypeApp (name, typeArgs, args), remaining))
                    | _ -> Error "Expected '(' after type arguments in generic call")
            else
                // Not a type application, treat name as variable and let comparison parsing handle <
                Ok (Var name, TLt :: rest)
        | TIdent name :: TLParen :: rest when not (System.Char.IsUpper(name.[0])) ->
            // Function call: name(args) - only for lowercase names
            parseCallArgs rest []
            |> Result.map (fun (args, remaining) ->
                (Call (name, args), remaining))
        | TIdent name :: TLParen :: rest when System.Char.IsUpper(name.[0]) ->
            // Constructor with payload: Constructor(payload)
            parseExpr rest
            |> Result.bind (fun (payloadExpr, remaining) ->
                match remaining with
                | TRParen :: rest' ->
                    Ok (Constructor ("", name, Some payloadExpr), rest')
                | _ -> Error "Expected ')' after constructor payload")
        | TIdent typeName :: TLBrace :: rest when System.Char.IsUpper(typeName.[0]) ->
            // Record literal with type name: Point { x = 1, y = 2 }
            parseRecordLiteralFieldsWithTypeName typeName rest []
        | TIdent name :: rest when System.Char.IsUpper(name.[0]) ->
            // Constructor without payload (enum variant)
            Ok (Constructor ("", name, None), rest)
        | TIdent name :: rest ->
            // Variable reference (lowercase identifier)
            Ok (Var name, rest)
        | TLParen :: TRParen :: rest ->
            // Unit literal: ()
            Ok (UnitLiteral, rest)
        | TLParen :: rest ->
            // Could be lambda, parenthesized expression, tuple literal, or operator section
            // Check for operator section: (&&), (||), (+), (-), (*), (/), etc.
            match rest with
            | TAnd :: TRParen :: afterOp ->
                // (&&) - operator section, parse the right operand
                parseUnary afterOp
                |> Result.map (fun (rightArg, remaining) ->
                    // Create lambda: \$x -> $x && rightArg
                    let lambda =
                        Lambda (NonEmptyList.singleton ("$pipe_arg", TBool), BinOp (And, Var "$pipe_arg", rightArg))
                    (lambda, remaining))
            | TOr :: TRParen :: afterOp ->
                // (||) - operator section
                parseUnary afterOp
                |> Result.map (fun (rightArg, remaining) ->
                    let lambda =
                        Lambda (NonEmptyList.singleton ("$pipe_arg", TBool), BinOp (Or, Var "$pipe_arg", rightArg))
                    (lambda, remaining))
            | _ ->
                // Check if it looks like lambda params: (ident : type, ...) =>
                match tryParseLambdaParams rest with
                | Some (lambdaParams, TFatArrow :: bodyStart) ->
                    // It's a lambda: (params) => body
                    parseExpr bodyStart
                    |> Result.map (fun (body, remaining) ->
                        (Lambda (lambdaParams, body), remaining))
                | _ ->
                    // Not a lambda - parse as expression/tuple
                    parseExpr rest
                    |> Result.bind (fun (firstExpr, remaining) ->
                        match remaining with
                        | TRParen :: rest' ->
                            // Parenthesized expression (single element)
                            Ok (firstExpr, rest')
                        | TComma :: rest' ->
                            // Tuple literal: (expr, expr, ...)
                            parseTupleElements rest' [firstExpr]
                        | _ -> Error "Expected ')' or ',' in tuple/parenthesized expression")
        | TLBrace :: rest ->
            // Distinguish between:
            // - anonymous record literal: { field = value, ... }
            // - record update: { recordExpr with field = value, ... }
            match rest with
            | TRBrace :: _ ->
                parseRecordLiteralFieldsWithTypeName "" rest []
            | TIdent _ :: TEquals :: _ ->
                parseRecordLiteralFieldsWithTypeName "" rest []
            | _ ->
                parseExpr rest
                |> Result.bind (fun (recordExpr, afterExpr) ->
                    match afterExpr with
                    | TWith :: afterWith ->
                        // Record update: { record with field = value, ... }
                        parseRecordUpdateFields afterWith []
                        |> Result.map (fun (updates, remaining) ->
                            (RecordUpdate (recordExpr, updates), remaining))
                    | _ -> Error "Record update requires 'with' keyword: use '{ record with field = value, ... }'")
        | TLBracket :: rest ->
            // List literal: [1, 2, 3] or []
            parseListLiteralElements rest []
        | _ -> Error "Expected expression"

    and parseTupleElements (toks: Token list) (acc: Expr list) : Result<Expr * Token list, string> =
        // Parse remaining tuple elements after the first comma
        parseExpr toks
        |> Result.bind (fun (expr, remaining) ->
            match remaining with
            | TComma :: rest ->
                // More elements
                parseTupleElements rest (expr :: acc)
            | TRParen :: rest ->
                // End of tuple
                let elements = List.rev (expr :: acc)
                Ok (TupleLiteral elements, rest)
            | _ -> Error "Expected ',' or ')' in tuple literal")

    and parseRecordLiteralFieldsWithTypeName (typeName: string) (toks: Token list) (acc: (string * Expr) list) : Result<Expr * Token list, string> =
        // Parse record literal fields with explicit type name: TypeName { name = expr, ... }
        match toks with
        | TRBrace :: rest ->
            // Empty record or end of fields
            Ok (RecordLiteral (typeName, List.rev acc), rest)
        | TIdent fieldName :: TEquals :: rest ->
            parseExpr rest
            |> Result.bind (fun (value, remaining) ->
                match remaining with
                | TComma :: rest' ->
                    // More fields
                    parseRecordLiteralFieldsWithTypeName typeName rest' ((fieldName, value) :: acc)
                | TRBrace :: rest' ->
                    // End of record
                    Ok (RecordLiteral (typeName, List.rev ((fieldName, value) :: acc)), rest')
                | _ -> Error "Expected ',' or '}' after record field value")
        | _ -> Error "Expected field name in record literal"

    and parseRecordUpdateFields (toks: Token list) (acc: (string * Expr) list) : Result<(string * Expr) list * Token list, string> =
        // Parse record update fields: field = expr, field = expr, ... }
        match toks with
        | TRBrace :: rest ->
            // End of fields
            Ok (List.rev acc, rest)
        | TIdent fieldName :: TEquals :: rest ->
            parseExpr rest
            |> Result.bind (fun (value, remaining) ->
                match remaining with
                | TComma :: rest' ->
                    // More fields
                    parseRecordUpdateFields rest' ((fieldName, value) :: acc)
                | TRBrace :: rest' ->
                    // End of record update
                    Ok (List.rev ((fieldName, value) :: acc), rest')
                | _ -> Error "Expected ',' or '}' after record update field value")
        | _ -> Error "Expected field name in record update"

    and parseListLiteralElements (toks: Token list) (acc: Expr list) : Result<Expr * Token list, string> =
        // Parse list literal elements: [expr, expr, ...] or [] or [a, b, ...rest]
        match toks with
        | TRBracket :: rest ->
            // Empty list or end of list
            Ok (ListLiteral (List.rev acc), rest)
        | TDotDotDot :: rest ->
            // Spread at start: [...tail]
            parseExpr rest
            |> Result.bind (fun (tailExpr, remaining) ->
                match remaining with
                | TRBracket :: rest' ->
                    Ok (ListCons (List.rev acc, tailExpr), rest')
                | _ -> Error "Expected ']' after spread expression")
        | _ ->
            parseExpr toks
            |> Result.bind (fun (expr, remaining) ->
                match remaining with
                | TComma :: TDotDotDot :: rest ->
                    // Spread after elements: [a, b, ...tail]
                    parseExpr rest
                    |> Result.bind (fun (tailExpr, remaining') ->
                        match remaining' with
                        | TRBracket :: rest' ->
                            Ok (ListCons (List.rev (expr :: acc), tailExpr), rest')
                        | _ -> Error "Expected ']' after spread expression")
                | TComma :: rest ->
                    // More elements
                    parseListLiteralElements rest (expr :: acc)
                | TRBracket :: rest ->
                    // End of list
                    Ok (ListLiteral (List.rev (expr :: acc)), rest)
                | _ -> Error "Expected ',' or ']' in list literal")

    and parsePostfix (expr: Expr) (toks: Token list) : Result<Expr * Token list, string> =
        // Handle postfix operations: tuple access (.0, .1), field access (.fieldName), or function application (args)
        match toks with
        | TDot :: TInt64 index :: rest ->
            if index < 0L then
                Error "Tuple index cannot be negative"
            else
                let accessExpr = TupleAccess (expr, int index)
                parsePostfix accessExpr rest
        | TDot :: TIdent fieldName :: rest ->
            // Record field access
            let accessExpr = RecordAccess (expr, fieldName)
            parsePostfix accessExpr rest
        | TLParen :: rest ->
            // Function application: expr(args)
            // Only allow if expr is not a simple named variable (those are handled by Call)
            match expr with
            | Var _ ->
                // This is handled by Call in parsePrimaryBase, not Apply
                // But if we get here with a Var, it means the Var came from a postfix
                // chain (e.g., x.field(args)), so we should use Apply
                parseCallArgs rest []
                |> Result.bind (fun (args, remaining) ->
                    let applyExpr = Apply (expr, args)
                    parsePostfix applyExpr remaining)
            | _ ->
                // Lambda or other expression being applied
                parseCallArgs rest []
                |> Result.bind (fun (args, remaining) ->
                    let applyExpr = Apply (expr, args)
                    parsePostfix applyExpr remaining)
        | _ -> Ok (expr, toks)

    and parseSpaceApplicationArgs (toks: Token list) (acc: Expr list) : Result<Expr list * Token list, string> =
        if canStartSpaceApplicationArg toks then
            parsePrimaryBase toks
            |> Result.bind (fun (argBaseExpr, afterArgBase) ->
                parsePostfix argBaseExpr afterArgBase
                |> Result.bind (fun (argExpr, afterArg) ->
                    parseSpaceApplicationArgs afterArg (argExpr :: acc)))
        else
            Ok (List.rev acc, toks)

    and parseCallArgs (toks: Token list) (acc: Expr list) : Result<NonEmptyList<Expr> * Token list, string> =
        match toks with
        | TRParen :: rest ->
            // End of arguments
            let reversed = List.rev acc
            let normalizedArgs =
                match reversed with
                | [] -> NonEmptyList.singleton UnitLiteral
                | _ -> NonEmptyList.fromList reversed
            Ok (normalizedArgs, rest)
        | _ ->
            // Parse an argument expression
            parseExpr toks
            |> Result.bind (fun (expr, remaining) ->
                match remaining with
                | TComma :: rest ->
                    // More arguments
                    parseCallArgs rest (expr :: acc)
                | TRParen :: rest ->
                    // End of arguments
                    Ok (NonEmptyList.fromList (List.rev (expr :: acc)), rest)
                | _ -> Error "Expected ',' or ')' after function argument")

    // Parse top-level elements (functions or expressions)
    let rec parseTopLevels (toks: Token list) (acc: TopLevel list) : Result<Program, string> =
        match toks with
        | TEOF :: [] ->
            // End of input
            if List.isEmpty acc then
                Error "Empty program"
            else
                Ok (Program (List.rev acc))

        | TDef :: _ ->
            // Parse function definition
            parseFunctionDef toks parseExpr
            |> Result.bind (fun (funcDef, remaining) ->
                parseTopLevels remaining (FunctionDef funcDef :: acc))

        | TType :: _ ->
            // Parse type definition
            parseTypeDef toks
            |> Result.bind (fun (typeDef, remaining) ->
                parseTopLevels remaining (TypeDef typeDef :: acc))

        | _ ->
            // Parse expression
            parseExpr toks
            |> Result.bind (fun (expr, remaining) ->
                match remaining with
                | TEOF :: [] ->
                    // Single expression program
                    Ok (Program (List.rev (Expression expr :: acc)))
                | _ ->
                    // More top-level definitions after expression not allowed for now
                    Error "Unexpected tokens after expression (only function definitions can be followed by more definitions)")

    parseTopLevels tokens []

let private isInternalIdentifier (name: string) : bool =
    let isAllUnderscores = name |> Seq.forall (fun c -> c = '_')
    (name.StartsWith("__") && not isAllUnderscores) || name.Contains(".__")

let private validateNoInternalIdentifier (name: string) : Result<unit, string> =
    if isInternalIdentifier name then
        Error $"Internal identifier not allowed in user code: {name}"
    else
        Ok ()

let rec private validatePattern (pattern: Pattern) : Result<unit, string> =
    match pattern with
    | PVar name -> validateNoInternalIdentifier name
    | PConstructor (_, payload) ->
        match payload with
        | None -> Ok ()
        | Some inner -> validatePattern inner
    | PTuple patterns ->
        patterns
        |> List.fold (fun acc p -> Result.bind (fun () -> validatePattern p) acc) (Ok ())
    | PRecord (_, fields) ->
        fields
        |> List.fold (fun acc (_, p) -> Result.bind (fun () -> validatePattern p) acc) (Ok ())
    | PList patterns ->
        patterns
        |> List.fold (fun acc p -> Result.bind (fun () -> validatePattern p) acc) (Ok ())
    | PListCons (head, tail) ->
        let headResult =
            head |> List.fold (fun acc p -> Result.bind (fun () -> validatePattern p) acc) (Ok ())
        Result.bind (fun () -> validatePattern tail) headResult
    | PUnit
    | PWildcard
    | PInt64 _
    | PInt128Literal _
    | PInt8Literal _
    | PInt16Literal _
    | PInt32Literal _
    | PUInt8Literal _
    | PUInt16Literal _
    | PUInt32Literal _
    | PUInt64Literal _
    | PUInt128Literal _
    | PBool _
    | PString _
    | PChar _
    | PFloat _ -> Ok ()

let rec private validateExpr (expr: Expr) : Result<unit, string> =
    match expr with
    | Let (name, value, body) ->
        validateNoInternalIdentifier name
        |> Result.bind (fun () -> validateExpr value)
        |> Result.bind (fun () -> validateExpr body)
    | Var name -> validateNoInternalIdentifier name
    | Call (funcName, args) ->
        validateNoInternalIdentifier funcName
        |> Result.bind (fun () ->
            args
            |> NonEmptyList.toList
            |> List.fold (fun acc arg -> Result.bind (fun () -> validateExpr arg) acc) (Ok ()))
    | TypeApp (funcName, _, args) ->
        validateNoInternalIdentifier funcName
        |> Result.bind (fun () ->
            args
            |> NonEmptyList.toList
            |> List.fold (fun acc arg -> Result.bind (fun () -> validateExpr arg) acc) (Ok ()))
    | InterpolatedString parts ->
        parts
        |> List.fold (fun acc part ->
            match part with
            | StringText _ -> acc
            | StringExpr inner -> Result.bind (fun () -> validateExpr inner) acc) (Ok ())
    | BinOp (_, left, right) ->
        validateExpr left |> Result.bind (fun () -> validateExpr right)
    | UnaryOp (_, inner) -> validateExpr inner
    | If (cond, thenBranch, elseBranch) ->
        validateExpr cond
        |> Result.bind (fun () -> validateExpr thenBranch)
        |> Result.bind (fun () -> validateExpr elseBranch)
    | TupleLiteral elems ->
        elems |> List.fold (fun acc e -> Result.bind (fun () -> validateExpr e) acc) (Ok ())
    | TupleAccess (tupleExpr, _) -> validateExpr tupleExpr
    | RecordLiteral (_, fields) ->
        fields |> List.fold (fun acc (_, e) -> Result.bind (fun () -> validateExpr e) acc) (Ok ())
    | RecordUpdate (recordExpr, updates) ->
        validateExpr recordExpr
        |> Result.bind (fun () ->
            updates |> List.fold (fun acc (_, e) -> Result.bind (fun () -> validateExpr e) acc) (Ok ()))
    | RecordAccess (recordExpr, _) -> validateExpr recordExpr
    | Constructor (_, _, payload) ->
        match payload with
        | None -> Ok ()
        | Some inner -> validateExpr inner
    | Match (scrutinee, cases) ->
        let validateCase (case: MatchCase) : Result<unit, string> =
            let patternsResult =
                case.Patterns
                |> NonEmptyList.toList
                |> List.fold (fun acc p -> Result.bind (fun () -> validatePattern p) acc) (Ok ())
            patternsResult
            |> Result.bind (fun () ->
                match case.Guard with
                | None -> Ok ()
                | Some guardExpr -> validateExpr guardExpr)
            |> Result.bind (fun () -> validateExpr case.Body)
        validateExpr scrutinee
        |> Result.bind (fun () ->
            cases |> List.fold (fun acc case -> Result.bind (fun () -> validateCase case) acc) (Ok ()))
    | ListLiteral elems ->
        elems |> List.fold (fun acc e -> Result.bind (fun () -> validateExpr e) acc) (Ok ())
    | ListCons (head, tail) ->
        let headResult = head |> List.fold (fun acc e -> Result.bind (fun () -> validateExpr e) acc) (Ok ())
        Result.bind (fun () -> validateExpr tail) headResult
    | Lambda (parameters, body) ->
        parameters
        |> NonEmptyList.toList
        |> List.fold (fun acc (name, _) -> Result.bind (fun () -> validateNoInternalIdentifier name) acc) (Ok ())
        |> Result.bind (fun () -> validateExpr body)
    | Apply (funcExpr, args) ->
        validateExpr funcExpr
        |> Result.bind (fun () ->
            args
            |> NonEmptyList.toList
            |> List.fold (fun acc e -> Result.bind (fun () -> validateExpr e) acc) (Ok ()))
    | FuncRef funcName -> validateNoInternalIdentifier funcName
    | Closure (funcName, captures) ->
        validateNoInternalIdentifier funcName
        |> Result.bind (fun () ->
            captures |> List.fold (fun acc e -> Result.bind (fun () -> validateExpr e) acc) (Ok ()))
    | UnitLiteral | Int64Literal _ | Int128Literal _ | Int8Literal _ | Int16Literal _ | Int32Literal _
    | UInt8Literal _ | UInt16Literal _ | UInt32Literal _ | UInt64Literal _ | UInt128Literal _
    | BoolLiteral _ | StringLiteral _ | CharLiteral _ | FloatLiteral _ -> Ok ()

let private validateNoInternalIdentifiers (Program items) : Result<Program, string> =
    let validateTopLevel (item: TopLevel) : Result<unit, string> =
        match item with
        | FunctionDef def ->
            validateNoInternalIdentifier def.Name
            |> Result.bind (fun () ->
                def.Params
                |> NonEmptyList.toList
                |> List.fold (fun acc (name, _) -> Result.bind (fun () -> validateNoInternalIdentifier name) acc) (Ok ()))
            |> Result.bind (fun () -> validateExpr def.Body)
        | TypeDef _ -> Ok ()
        | Expression expr -> validateExpr expr
    items
    |> List.fold (fun acc item -> Result.bind (fun () -> validateTopLevel item) acc) (Ok ())
    |> Result.map (fun () -> Program items)

/// Parse a string directly to AST
let parseString (allowInternal: bool) (input: string) : Result<Program, string> =
    lex input
    |> Result.bind parse
    |> Result.bind (fun program ->
        if allowInternal then Ok program
        else validateNoInternalIdentifiers program)
