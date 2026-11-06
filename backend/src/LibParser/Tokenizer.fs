/// Shared lexer types for the range-complete parser: source positions and the
/// `Token` DU (produced by `Lexer`, consumed by `Parser` and the
/// syntax-highlighter's token-kind classifier).
module LibParser.Tokenizer

type Pos = { row : int; column : int }
type TokenRange = { start : Pos; end_ : Pos }

/// Token types for the lexer.
type Token =
  | TBigInt of bigint // arbitrary-precision: 1z
  | TInt64 of int64 // 1, 1s, 1s64
  | TInt128 of System.Int128 // 1s128
  | TInt8 of sbyte // 1s8
  | TInt16 of int16 // 1s16
  | TInt32 of int32 // 1s32
  | TUInt8 of byte // 1u8
  | TUInt16 of uint16 // 1u16
  | TUInt32 of uint32 // 1u32
  | TUInt64 of uint64 // 1u, 1u64
  | TUInt128 of System.UInt128 // 1u128
  | TFloat of float // 1.5, 1.5f64
  | TFloat32 of float32 // 1.5f32
  | TDecimal of decimal // 19.99dec, scale as written
  | TStringLit of string // String literal token
  | TCharLit of string // Char literal: 'x' (stores UTF-8 string for EGC support)
  // Interpolated string `$"Hello {name}!"`. No payload: the parser re-reads the
  // token's source text and re-scans the `{expr}` bodies itself.
  | TInterpString
  | TTrue
  | TFalse
  | TPlus
  | TMinus
  | TStar
  | TStarStar // ** (exponentiation)
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
  | TTrait // trait (trait declaration: a set of method signatures)
  | TImpl // impl (an implementation of a trait for a type)
  | TColon // : (type annotation)
  | TComma // , (parameter / collection / field separator)
  | TSemicolon // ; (never a separator: always a diagnostic)
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
  | TQuestion // ? (postfix unwrap)
  | TNot // !
  | TPipe // |> (pipe operator)
  | TDotDotDot // ... (rest pattern in lists)
  | TPercent // % (modulo)
  | TShl // << (left shift)
  | TShr // >> (right shift)
  | TBitAnd // & (bitwise and)
  | TBitXor // ^ (bitwise xor)
  | TBitNot // ~ (bitwise not)
  | TIdent of string
  | TEOF
