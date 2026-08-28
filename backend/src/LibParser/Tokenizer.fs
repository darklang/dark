/// Shared lexer types for the range-complete parser: source positions and the
/// `Token` DU (produced by `Lexer`, consumed by `Parser` and the
/// syntax-highlighter's token-kind classifier).
module LibParser.Tokenizer

type Pos = { row : int; column : int }
type TokenRange = { start : Pos; end_ : Pos }

/// Token types for the lexer.
type Token =
  | TInt of bigint // Default integer — arbitrary-precision `Int` (bare `1`)
  | TInt64 of int64 // 64-bit signed: 1L
  | TInt128 of System.Int128 // 128-bit signed: 1Q
  | TInt8 of sbyte // 8-bit signed: 1y
  | TInt16 of int16 // 16-bit signed: 1s
  | TInt32 of int32 // 32-bit signed: 1l
  | TUInt8 of byte // 8-bit unsigned: 1uy
  | TUInt16 of uint16 // 16-bit unsigned: 1us
  | TUInt32 of uint32 // 32-bit unsigned: 1ul
  | TUInt64 of uint64 // 64-bit unsigned: 1UL
  | TUInt128 of System.UInt128 // 128-bit unsigned: 1Z
  | TFloat of float
  | TStringLit of string // String literal token
  | TCharLit of string // Char literal: 'x' (stores UTF-8 string for EGC support)
  // Interpolated string `$"Hello {name}!"`. No payload: the parser re-reads the
  // token's source text and re-scans the `{expr}` bodies itself.
  | TInterpString
  | TTrue
  | TFalse
  | TPlus
  | TPlusPlus // ++ (string concatenation)
  | TMinus
  | TStar
  | TSlash
  | TLParen
  | TRParen
  | TLet
  | TVal // val (declaration-scope value; never a local let-expression)
  | TIn
  | TIf // if
  | TElif // elif
  | TThen // then
  | TElse // else
  | TType // type (type definition)
  | TCons // :: (list cons pattern)
  | TColon // : (type annotation)
  | TComma // , (parameter separator)
  | TSemicolon // ; (statement separator; record/dict field separator)
  | TDot // . (tuple/record access)
  | TLBrace // { (record literal)
  | TRBrace // } (record literal)
  | TBar // | (sum type variant separator / pattern separator)
  | TOf // of (sum type payload)
  | TMatch // match (pattern matching)
  | TWith // with (pattern matching)
  | TFun // fun (interpreter-style lambda)
  | TArrow // -> (pattern matching)
  | TUnderscore // _ (wildcard pattern)
  | TWhen // when (guard clause in pattern matching)
  | TLBracket // [ (list literal)
  | TRBracket // ] (list literal)
  | TEquals // = (assignment in let)
  | TEqEq // == (equality comparison)
  | TNeq // !=
  | TLt // <
  | TGt // >
  | TLte // <=
  | TGte // >=
  | TAnd // &&
  | TOr // ||
  | TNot // !
  | TPipe // |> (pipe operator)
  | TDotDotDot // ... (rest pattern in lists)
  | TPercent // % (modulo)
  | TShl // << (left shift)
  | TShr // >> (right shift)
  | TBitAnd // & (bitwise and)
  | TBitOr // ||| (bitwise or)
  | TBitXor // ^ (exponentiation in surface syntax; token name is legacy)
  | TBitNot // ~~~ (bitwise not)
  | TAt // @ (list append)
  | TIdent of string
  | TEOF
