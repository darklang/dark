/// The types that the user writes. Think of this as the Syntax Tree.
///
/// This is the range-complete syntax tree produced by the hand-written
/// parser. Every node carries the exact source ranges (whole-node plus the
/// fine-grained keyword/symbol ranges) that the editor tooling needs: the
/// semantic-token highlighter, the LSP (hover / diagnostics), and the formatter.
/// The tree is converted 1:1 into the Dark `LanguageTools.WrittenTypes` (as Dvals) by
/// `WrittenTypesToDarkTypes` in `Builtins.Language/Libs/Parser.fs`.
///
/// Execution lowering (`WrittenTypesToProgramTypes`) consumes the same tree,
/// ignoring the ranges and minting fresh node ids as it lowers to ProgramTypes.
/// (Node ids are ephemeral — a `gid()` counter, not source-derived — so they are
/// created at lowering time rather than stored on every node; the Dark WrittenTypes
/// keys on ranges, not ids.)
module LibParser.WrittenTypes

open Prelude

open LibParser.Tokenizer // Pos, TokenRange

type Range = TokenRange

/// A synthetic (zero-width) range for nodes the lowering synthesizes with no
/// source counterpart, such as an implicit unit parameter. Never serialized for
/// highlighting; the package/decl normalization layer is execution-only.
let synthRange : Range =
  { start = { row = 0; column = 0 }; end_ = { row = 0; column = 0 } }

type Name =
  // Used when a syntactic construct turns into a function, such as some operators.
  | KnownBuiltin of string * int
  // Most names are unresolved here and are resolved during WT2PT lowering.
  | Unresolved of NEList<string>

// Enum type names are a plain `List<string>`. An empty list is valid, e.g. an
// unqualified `Ok`, where only the case name is written. See the long note in git
// history for why EEnum doesn't reuse `Name`.
type UnresolvedEnumTypeName = List<string>


type Infix =
  | InfixFnCall of InfixFnName
  | BinOp of BinaryOperation

and InfixFnName =
  | ArithmeticPlus
  | ArithmeticMinus
  | ArithmeticMultiply
  | ArithmeticDivide
  | ArithmeticModulo
  | ArithmeticPower
  | BitwiseAnd
  | BitwiseOr
  | BitwiseXor
  | ShiftLeft
  | ShiftRight
  | ComparisonGreaterThan
  | ComparisonGreaterThanOrEqual
  | ComparisonLessThan
  | ComparisonLessThanOrEqual
  | ComparisonEquals
  | ComparisonNotEquals
  | StringConcat

and BinaryOperation =
  | BinOpAnd
  | BinOpOr

/// A simple `{ range; name }` identifier. The parent field gives it meaning:
/// variable name, function name, type name, etc.
type Identifier = { range : Range; name : string }

/// `Module.Path.fn`; each module segment carries its own range.
type QualifiedFnIdentifier =
  { range : Range
    modules : List<Identifier * Range> // (module ident, trailing-dot range)
    fn : Identifier }

/// `Module.Path.TypeName<args>`; used by record literals, enum constructors, and
/// custom type references.
type QualifiedTypeIdentifier =
  { range : Range
    modules : List<Identifier * Range>
    typ : Identifier
    typeArgs : List<TypeReference> } // `<…>` generic args (e.g. `Option<String>`)

/// Type references on parameters and return types. Each primitive/built-in type
/// has its own case carrying just its range, so WT2PT and the serializer match
/// them exhaustively.
and TypeReference =
  | TUnit of Range
  | TBool of Range
  | TInt of Range
  | TInt8 of Range
  | TUInt8 of Range
  | TInt16 of Range
  | TUInt16 of Range
  | TInt32 of Range
  | TUInt32 of Range
  | TInt64 of Range
  | TUInt64 of Range
  | TInt128 of Range
  | TUInt128 of Range
  | TFloat of Range
  | TChar of Range
  | TString of Range
  | TDateTime of Range
  | TUuid of Range
  | TBlob of Range
  | TList of
    Range *
    keywordList : Range *
    openBracket : Range *
    inner : TypeReference *
    closeBracket : Range
  | TDict of
    Range *
    keywordDict : Range *
    openBracket : Range *
    key : TypeReference *
    symbolComma : Range *
    value : TypeReference *
    closeBracket : Range
  | TCustom of QualifiedTypeIdentifier
  | TVariable of Range * tick : Range * name : (Range * string) // `'a`
  | TTuple of
    Range *
    first : TypeReference *
    symbolAsterisk : Range *
    second : TypeReference *
    rest : List<Range * TypeReference> *  // each item is (`*` range, type)
    openParen : Range *
    closeParen : Range
  | TFn of Range * arguments : List<TypeReference * Range> * ret : TypeReference // each arg: (type, `->` range)

/// Mapping between primitive type names and their `TypeReference` case
/// constructors. The parser resolves names through this list; WT2PT and the
/// serializer then match the primitive cases exhaustively.
let primTypes : List<string * (Range -> TypeReference)> =
  [ "Unit", TUnit
    "Bool", TBool
    "Int", TInt
    "Int8", TInt8
    "UInt8", TUInt8
    "Int16", TInt16
    "UInt16", TUInt16
    "Int32", TInt32
    "UInt32", TUInt32
    "Int64", TInt64
    "UInt64", TUInt64
    "Int128", TInt128
    "UInt128", TUInt128
    "Float", TFloat
    "Char", TChar
    "String", TString
    "DateTime", TDateTime
    "Uuid", TUuid
    "Blob", TBlob ]

let primTypeFromName (s : string) : Option<Range -> TypeReference> =
  primTypes |> List.tryPick (fun (n, ctor) -> if n = s then Some ctor else None)

type LetPattern =
  | LPUnit of Range
  | LPVariable of Range * name : string
  | LPWildcard of Range
  | LPTuple of
    Range *
    first : LetPattern *
    symbolComma : Range *
    second : LetPattern *
    rest : List<Range * LetPattern> *  // each item is (`,` range, pattern)
    symbolOpenParen : Range *
    symbolCloseParen : Range

/// Match patterns.
type MatchPattern =
  | MPVariable of Range * string // also `_` (as "_")
  | MPInt of Range * intPart : (Range * bigint) // arbitrary-precision `Int`
  | MPInt8 of Range * intPart : (Range * int8) * suffixPart : Range
  | MPUInt8 of Range * intPart : (Range * uint8) * suffixPart : Range
  | MPInt16 of Range * intPart : (Range * int16) * suffixPart : Range
  | MPUInt16 of Range * intPart : (Range * uint16) * suffixPart : Range
  | MPInt32 of Range * intPart : (Range * int32) * suffixPart : Range
  | MPUInt32 of Range * intPart : (Range * uint32) * suffixPart : Range
  | MPInt64 of Range * intPart : (Range * int64) * suffixPart : Range
  | MPUInt64 of Range * intPart : (Range * uint64) * suffixPart : Range
  | MPInt128 of Range * intPart : (Range * System.Int128) * suffixPart : Range
  | MPUInt128 of Range * intPart : (Range * System.UInt128) * suffixPart : Range
  | MPFloat of Range * isNegative : bool * whole : string * fraction : string
  | MPBool of Range * bool
  | MPString of
    Range *
    contents : Option<Range * string> *
    symbolOpenQuote : Range *
    symbolCloseQuote : Range
  | MPChar of
    Range *
    contents : Option<Range * string> *
    symbolOpenQuote : Range *
    symbolCloseQuote : Range
  | MPUnit of Range
  | MPEnum of Range * caseName : (Range * string) * fieldPats : List<MatchPattern>
  | MPTuple of
    Range *
    first : MatchPattern *
    symbolComma : Range *
    second : MatchPattern *
    rest : List<Range * MatchPattern> *
    symbolOpenParen : Range *
    symbolCloseParen : Range
  | MPList of
    Range *
    contents : List<MatchPattern * Option<Range>> *
    symbolOpenBracket : Range *
    symbolCloseBracket : Range
  | MPListCons of
    Range *
    head : MatchPattern *
    tail : MatchPattern *
    symbolCons : Range
  | MPOr of Range * List<MatchPattern>
  /// Recovery hole where a pattern was expected but could not be parsed. The
  /// parse has a diagnostic at this range, and execution paths reject files with
  /// diagnostics before lowering.
  | MPError of Range

let rec mpRange (p : MatchPattern) : Range =
  match p with
  | MPVariable(r, _)
  | MPInt(r, _)
  | MPInt8(r, _, _)
  | MPUInt8(r, _, _)
  | MPInt16(r, _, _)
  | MPUInt16(r, _, _)
  | MPInt32(r, _, _)
  | MPUInt32(r, _, _)
  | MPInt64(r, _, _)
  | MPUInt64(r, _, _)
  | MPInt128(r, _, _)
  | MPUInt128(r, _, _)
  | MPFloat(r, _, _, _)
  | MPBool(r, _)
  | MPString(r, _, _, _)
  | MPChar(r, _, _, _)
  | MPUnit r
  | MPEnum(r, _, _)
  | MPTuple(r, _, _, _, _, _, _)
  | MPList(r, _, _, _)
  | MPListCons(r, _, _, _)
  | MPOr(r, _)
  | MPError r -> r

type StringSegment =
  | StringText of Range * string
  | StringInterpolation of
    Range *
    Expr *
    symbolOpenBrace : Range *
    symbolCloseBrace : Range

and Expr =
  | EUnit of Range
  | EBool of Range * bool
  | EInt of Range * intPart : (Range * bigint) // bare arbitrary-precision `Int`
  | EInt64 of Range * intPart : (Range * int64) * suffixPart : Range
  | EInt8 of Range * intPart : (Range * sbyte) * suffixPart : Range
  | EUInt8 of Range * intPart : (Range * byte) * suffixPart : Range
  | EInt16 of Range * intPart : (Range * int16) * suffixPart : Range
  | EUInt16 of Range * intPart : (Range * uint16) * suffixPart : Range
  | EInt32 of Range * intPart : (Range * int32) * suffixPart : Range
  | EUInt32 of Range * intPart : (Range * uint32) * suffixPart : Range
  | EUInt64 of Range * intPart : (Range * uint64) * suffixPart : Range
  | EInt128 of Range * intPart : (Range * System.Int128) * suffixPart : Range
  | EUInt128 of Range * intPart : (Range * System.UInt128) * suffixPart : Range
  | EFloat of Range * isNegative : bool * whole : string * fraction : string
  | EChar of
    Range *
    contents : Option<Range * string> *
    symbolOpenQuote : Range *
    symbolCloseQuote : Range
  | EString of
    Range *
    symbolDollarSign : Option<Range> *
    contents : List<StringSegment> *
    symbolOpenQuote : Range *
    symbolCloseQuote : Range
  | EVariable of Range * string
  | EFnName of Range * QualifiedFnIdentifier
  | EInfix of Range * op : (Range * Infix) * left : Expr * right : Expr
  | ELet of
    Range *
    LetPattern *
    expr : Expr *
    body : Expr *
    keywordLet : Range *
    symbolEquals : Range
  | EApply of Range * lhs : Expr * typeArgs : List<TypeReference> * args : List<Expr>
  // each list element carries its trailing-separator (`;`/`,`) range, if any
  | EList of
    Range *
    contents : List<Expr * Option<Range>> *
    symbolOpenBracket : Range *
    symbolCloseBracket : Range
  | ETuple of
    Range *
    first : Expr *
    symbolComma : Range *
    second : Expr *
    rest : List<Range * Expr> *
    symbolOpenParen : Range *
    symbolCloseParen : Range
  | EIf of
    Range *
    cond : Expr *
    thenExpr : Expr *
    elseExpr : Option<Expr> *
    keywordIf : Range *
    keywordThen : Range *
    keywordElse : Option<Range>
  | ERecordFieldAccess of
    Range *
    Expr *
    fieldName : (Range * string) *
    symbolDot : Range
  | ELambda of
    Range *
    pats : List<LetPattern> *
    body : Expr *
    keywordFun : Range *
    symbolArrow : Range
  | ERecord of
    Range *
    typeName : QualifiedTypeIdentifier *
    fields : List<Range * (Range * string) * Expr> *
    symbolOpenBrace : Range *
    symbolCloseBrace : Range
  // `Dict { k = v; … }`: a dict literal. Syntactically like a record, but `Dict`
  // is a keyword (its own range), not a type name, so it's a distinct node.
  | EDict of
    Range *
    contents : List<Range * Expr * Range * Expr> *  // (entry, key, `:` range, value)
    keywordDict : Range *
    symbolOpenBrace : Range *
    symbolCloseBrace : Range
  | ERecordUpdate of
    Range *
    record : Expr *
    updates : List<(Range * string) * Range * Expr> *  // (field name, `=` range, value)
    symbolOpenBrace : Range *
    symbolCloseBrace : Range *
    keywordWith : Range
  | EEnum of
    Range *
    typeName : QualifiedTypeIdentifier *
    caseName : (Range * string) *
    fields : List<Expr> *
    symbolDot : Range
  | EMatch of
    Range *
    expr : Expr *
    cases : List<MatchCase> *
    keywordMatch : Range *
    keywordWith : Range
  | EPipe of Range * Expr * List<Range * PipeExpr> // each item is (`|>` range, segment)
  | EStatement of Range * first : Expr * next : Expr // `e1 ⏎ e2` (sequence)
  /// Recovery hole where an expression was expected but could not be parsed. The
  /// parse has a diagnostic at this range, and execution paths reject files with
  /// diagnostics before lowering.
  | EError of Range

and MatchCase =
  { barRange : Range
    pat : MatchPattern
    arrowRange : Range
    whenCondition : Option<Range * Expr>
    rhs : Expr }

and PipeExpr =
  | EPipeInfix of Range * op : (Range * Infix) * Expr
  | EPipeLambda of
    Range *
    pats : List<LetPattern> *
    body : Expr *
    keywordFun : Range *
    symbolArrow : Range
  | EPipeEnum of
    Range *
    typeName : QualifiedTypeIdentifier *
    caseName : (Range * string) *
    fields : List<Expr> *
    symbolDot : Range
  | EPipeFnCall of
    Range *
    fnName : QualifiedFnIdentifier *
    typeArgs : List<TypeReference> *
    args : List<Expr>
  | EPipeVariableOrFnCall of Range * string

/// A function parameter: `(name: Type)` or a `()` unit parameter.
///
/// `description` is the `///` written above it. A parameter's doc is not part of the item's identity
/// hash, so it needs an `UpdateDoc` to travel -- but it has to be READ first, and until this field
/// existed the lexer's doc comment was attached to the `(` token and then dropped on the floor.
type FnParam =
  | FPUnit of Range
  | FPNormal of
    Range *
    name : Identifier *
    typ : TypeReference *
    symbolLeftParen : Range *
    symbolColon : Range *
    symbolRightParen : Range *
    description : string

/// `'a: Show<Int>` in a type-param list. The trait is a type name: a bound says
/// "a value of type `Show<'a>` must exist". Several bounds on one param are
/// written `'a: Show + Eq` and stored as separate entries.
type TypeParamBound =
  { range : Range
    param : string
    trait_ : QualifiedTypeIdentifier
    symbolColon : Range }

/// `let name (p: T) … :{Effect, …} Ret = body`
type FnDecl =
  {
    range : Range
    name : Identifier
    typeParams : List<string * Range> // `<'a, 'b>` (name tick-stripped, with range)
    bounds : List<TypeParamBound>
    parameters : List<FnParam>
    /// An optional effect row after the return colon, such as
    /// `:{Http, Clock} Ret`, sets the function's permission ceiling. It limits
    /// the effects used by the body and its calls; it never grants access.
    /// Effect names are resolved, and unknown names reported, by
    /// `WrittenTypesToProgramTypes`.
    /// `: Ret` means no ceiling; `:{}` requires a pure body; a non-empty row
    /// allows only the listed effects.
    ///
    ///   `: String`              None       no row, no promise
    ///   `:{} String`            Some []    effect-free: every host effect inside is denied
    ///   `:{Http, Clock} String` Some [...] only these; anything else inside is denied
    effects : Option<List<Identifier>>
    returnType : TypeReference
    body : Expr
    keywordLet : Range
    symbolColon : Range
    symbolEquals : Range
    description : string
  } // preceding `///` doc comments

/// `let name = body` (no params)
type ValueDecl =
  { range : Range
    name : Identifier
    body : Expr
    keywordVal : Range
    symbolEquals : Range
    description : string }

// --- type declarations ---

type RecordFieldSyntax =
  {
    range : Range
    name : Range * string
    typ : TypeReference
    /// The `///` written above the field. See `FnParam` for why it is kept.
    description : string
    symbolColon : Range
  }

type EnumFieldSyntax =
  { range : Range
    typ : TypeReference
    label : Option<Range * string>
    symbolColon : Option<Range> }

type EnumCaseSyntax =
  {
    range : Range
    name : Range * string
    fields : List<EnumFieldSyntax>
    /// The `///` written above the case. See `FnParam` for why it is kept.
    description : string
    keywordOf : Option<Range>
  }

type TypeDefinition =
  | TDAlias of TypeReference
  | TDRecord of List<RecordFieldSyntax * Option<Range>> // (field, trailing-separator)
  | TDEnum of List<Range * EnumCaseSyntax> // (leading `|` range, case)

/// `type Name [<'a>] = Definition`
type TypeDecl =
  { range : Range
    name : Identifier
    typeParams : List<string * Range> // `<'a, 'b>` (name tick-stripped, with range)
    bounds : List<TypeParamBound>
    definition : TypeDefinition
    keywordType : Range
    symbolEquals : Range
    description : string }

/// One method signature inside a `trait` block: a fn header with no body
/// (`let show (v: 'a) : String`). A body is parsed and kept so the diagnostic can
/// point at it; default methods are not supported yet.
type TraitMethodDecl =
  { range : Range
    name : Identifier
    typeParams : List<string * Range>
    bounds : List<TypeParamBound>
    parameters : List<FnParam>
    effects : Option<List<Identifier>>
    returnType : TypeReference
    body : Option<Expr>
    keywordLet : Range
    symbolColon : Range
    description : string }

/// `trait Name<'a> = <methods>`. Sugar for a record type whose fields are fn
/// types; `SourceFile.items` performs the desugaring, so nothing downstream of
/// the parser sees a trait as its own kind.
type TraitDecl =
  { range : Range
    name : Identifier
    typeParams : List<string * Range>
    bounds : List<TypeParamBound>
    methods : List<TraitMethodDecl>
    keywordTrait : Range
    symbolEquals : Range
    description : string }

/// `impl[<'a: B>] Trait<Args> for Type = <fns>`. Sugar for one package fn per
/// method plus a package value of the trait's record type (or, when the impl has
/// type params, a fn returning that record). See `SourceFile.items`.
type ImplDecl =
  {
    range : Range
    typeParams : List<string * Range>
    bounds : List<TypeParamBound>
    trait_ : QualifiedTypeIdentifier
    forType : TypeReference
    methods : List<FnDecl>
    /// `let add = Stdlib.Int64.add`: a method that is an existing fn, so the
    /// impl names it instead of wrapping it.
    aliases : List<ValueDecl>
    keywordImpl : Range
    keywordFor : Range
    symbolEquals : Range
    description : string
  }

/// A `module Name.Path` header.
type ModuleDecl =
  { range : Range
    name : Range * string
    declarations : List<Declaration>
    keywordModule : Range }

/// A test assertion's expected side: a value expression, or an expected
/// runtime / SQL error message. Validation restricts it to Test source.
and TestExpected =
  | TEExpr of Expr
  | TEError of string
  | TESqlError of string

/// A test assertion `actual = expected`; post-parse validation restricts it to Test source.
and Test = { range : Range; actual : Expr; expected : TestExpected }

and Declaration =
  | DFunction of FnDecl
  | DValue of ValueDecl
  | DModule of ModuleDecl
  | DType of TypeDecl
  | DTrait of TraitDecl
  | DImpl of ImplDecl
  /// A trailing expression inside a module body (`module M = … \n expr`).
  | DExpr of Expr
  /// `[<DB>] type Name = AliasedType` — a Test-only user DB.
  | DTypeDB of TypeDecl
  /// `actual = expected` assertion accepted only by Test validation.
  | DTest of Test

/// The whole file: top-level declarations + trailing expressions to eval.
type SourceFile =
  { range : Range; declarations : List<Declaration>; exprsToEval : List<Expr> }

type ParsedFile = SourceFile of SourceFile

/// Source range covering a whole expression node.
let exprRange (e : Expr) : Range =
  match e with
  | EUnit r -> r
  | EBool(r, _)
  | EInt(r, _)
  | EInt64(r, _, _)
  | EInt8(r, _, _)
  | EUInt8(r, _, _)
  | EInt16(r, _, _)
  | EUInt16(r, _, _)
  | EInt32(r, _, _)
  | EUInt32(r, _, _)
  | EUInt64(r, _, _)
  | EInt128(r, _, _)
  | EUInt128(r, _, _)
  | EFloat(r, _, _, _)
  | EChar(r, _, _, _)
  | EString(r, _, _, _, _)
  | EVariable(r, _)
  | EFnName(r, _)
  | EInfix(r, _, _, _)
  | ELet(r, _, _, _, _, _)
  | EApply(r, _, _, _)
  | EList(r, _, _, _)
  | ETuple(r, _, _, _, _, _, _)
  | EIf(r, _, _, _, _, _, _)
  | ERecordFieldAccess(r, _, _, _)
  | ELambda(r, _, _, _, _)
  | ERecord(r, _, _, _, _)
  | EDict(r, _, _, _, _)
  | ERecordUpdate(r, _, _, _, _, _)
  | EEnum(r, _, _, _, _)
  | EMatch(r, _, _, _, _)
  | EPipe(r, _, _)
  | EStatement(r, _, _)
  | EError r -> r

let typeReferenceRange (t : TypeReference) : Range =
  match t with
  | TUnit r
  | TBool r
  | TInt r
  | TInt8 r
  | TUInt8 r
  | TInt16 r
  | TUInt16 r
  | TInt32 r
  | TUInt32 r
  | TInt64 r
  | TUInt64 r
  | TInt128 r
  | TUInt128 r
  | TFloat r
  | TChar r
  | TString r
  | TDateTime r
  | TUuid r
  | TBlob r
  | TList(r, _, _, _, _)
  | TDict(r, _, _, _, _, _, _)
  | TVariable(r, _, _)
  | TTuple(r, _, _, _, _, _, _)
  | TFn(r, _, _) -> r
  | TCustom q -> q.range


// ============================================================================
// Normalized package IR + declaration normalization
//
// The layers below are execution-only (Cli / Package / TestModule -> WT2PT -> PT).
// They are never serialized for highlighting, so synthesized nodes may use
// `synthRange`. They normalize the raw parser tree (rich decls above) into the
// module-qualified package shapes the lowering consumes.
// ============================================================================

/// A bound in the normalized package IR: the param name and the trait as a type
/// reference (`TCustom`), so lowering resolves it like any other custom type.
type Bound = { param : string; trait_ : QualifiedTypeIdentifier }

let boundNorm (b : TypeParamBound) : Bound = { param = b.param; trait_ = b.trait_ }

module TypeDeclaration =
  type RecordField = { name : string; typ : TypeReference; description : string }

  type EnumField =
    { typ : TypeReference; label : Option<string>; description : string }

  type EnumCase = { name : string; fields : List<EnumField>; description : string }

  type Definition =
    | Alias of TypeReference
    | Record of NEList<RecordField>
    | Enum of NEList<EnumCase>

  type T =
    { typeParams : List<string>; bounds : List<Bound>; definition : Definition }


module PackageType =
  type Name = { owner : string; modules : List<string>; name : string }

  type PackageType =
    { name : Name; declaration : TypeDeclaration.T; description : string }

module PackageValue =
  type Name = { owner : string; modules : List<string>; name : string }

  type PackageValue = { name : Name; description : string; body : Expr }

module PackageFn =
  type Name = { owner : string; modules : List<string>; name : string }

  type Parameter = { name : string; typ : TypeReference; description : string }

  type PackageFn =
    {
      name : Name
      body : Expr
      typeParams : List<string>
      bounds : List<Bound>
      parameters : NEList<Parameter>
      returnType : TypeReference
      /// The declared permission ceiling (effect case names); see `FnDecl`.
      effects : Option<List<string>>
      description : string
    }


/// A trait in package form: its own item, like a type.
module PackageTrait =
  type Name = { owner : string; modules : List<string>; name : string }

  type Method =
    { name : string
      typeParams : List<string>
      bounds : List<Bound>
      parameters : NEList<PackageFn.Parameter>
      returnType : TypeReference
      effects : Option<List<string>>
      description : string }

  type PackageTrait =
    { name : Name
      typeParams : List<string>
      bounds : List<Bound>
      methods : List<Method>
      description : string }


/// A resolvable name, as the impl's method targets are written.
type MethodTarget = Name

/// An impl in package form: its own item at `<module>[.<Type>].<Trait>`, whose
/// method fns are ordinary package fns beneath it.
module PackageImpl =
  type Name = { owner : string; modules : List<string>; name : string }

  type PackageImpl =
    {
      name : Name
      trait_ : QualifiedTypeIdentifier
      forType : TypeReference
      typeParams : List<string>
      bounds : List<Bound>
      /// method name, and the fn that implements it as written: a method declared
      /// in the block (resolved from the impl's own module) or an alias target.
      methods : List<string * MethodTarget>
      description : string
    }


module DB =
  type T = { name : string; version : int; typ : TypeReference }


// --- normalization: raw parser syntax → package IR ---
//
// The parser produces one range-complete syntax tree. The package form is the
// shape execution wants: names pulled out of `(range, name)` pairs, no ranges.
// Field descriptions default to ""; declaration descriptions keep their `///`
// doc comments.

let private fnParamNorm (p : FnParam) : PackageFn.Parameter =
  match p with
  // A unit parameter is named "_".
  | FPUnit _ -> { name = "_"; typ = TUnit synthRange; description = "" }
  | FPNormal(_, name, typ, _, _, _, description) ->
    { name = name.name; typ = typ; description = description }

let private recordFieldNorm (f : RecordFieldSyntax) : TypeDeclaration.RecordField =
  { name = snd f.name; typ = f.typ; description = f.description }

let private enumFieldNorm (f : EnumFieldSyntax) : TypeDeclaration.EnumField =
  { typ = f.typ; label = f.label |> Option.map snd; description = "" }

let private enumCaseNorm (c : EnumCaseSyntax) : TypeDeclaration.EnumCase =
  { name = snd c.name
    fields = c.fields |> List.map enumFieldNorm
    description = c.description }

let typeDefinitionNorm (d : TypeDefinition) : TypeDeclaration.Definition =
  match d with
  | TDAlias t -> TypeDeclaration.Alias t
  | TDRecord fields ->
    fields
    |> List.map (fst >> recordFieldNorm)
    |> NEList.ofListWithDefault (
      { name = "_"; typ = TUnit synthRange; description = "" }
      : TypeDeclaration.RecordField
    )
    |> TypeDeclaration.Record
  | TDEnum cases ->
    cases
    |> List.map (snd >> enumCaseNorm)
    |> NEList.ofListWithDefault (
      { name = "_"; fields = []; description = "" } : TypeDeclaration.EnumCase
    )
    |> TypeDeclaration.Enum


// --- build owner-qualified package items from declarations ---
//
// A fn `map` inside `module Darklang.Stdlib.List` becomes `Darklang.Stdlib.List.map`:
// the accumulated path's first segment is the owner, the rest the modules.

/// The dotted `module A.B.C` header split into its path segments.
let moduleNameParts (m : ModuleDecl) : List<string> =
  let dotted = snd m.name
  dotted.Split('.') |> Array.toList |> List.filter (fun s -> s <> "")

let packageFn
  (owner : string)
  (modules : List<string>)
  (fn : FnDecl)
  : PackageFn.PackageFn =
  let parameters =
    fn.parameters
    |> List.map fnParamNorm
    |> NEList.ofListWithDefault (
      { name = "_"; typ = TUnit synthRange; description = "" } : PackageFn.Parameter
    )
  { name = { owner = owner; modules = modules; name = fn.name.name }
    body = fn.body
    typeParams = fn.typeParams |> List.map fst
    bounds = fn.bounds |> List.map boundNorm
    parameters = parameters
    returnType = fn.returnType
    effects = fn.effects |> Option.map (List.map (fun id -> id.name))
    description = fn.description }

let packageType
  (owner : string)
  (modules : List<string>)
  (t : TypeDecl)
  : PackageType.PackageType =
  { name = { owner = owner; modules = modules; name = t.name.name }
    declaration =
      { typeParams = t.typeParams |> List.map fst
        bounds = t.bounds |> List.map boundNorm
        definition = typeDefinitionNorm t.definition }
    description = t.description }

let packageValue
  (owner : string)
  (modules : List<string>)
  (v : ValueDecl)
  : PackageValue.PackageValue =
  { name = { owner = owner; modules = modules; name = v.name.name }
    description = v.description
    body = v.body }


// --- traits and impls: desugaring to types, fns and values ---
//
// A trait is a record type whose fields are fn types; an impl is one package fn per
// method plus a package value of the trait's record type, found at runtime by
// type. Nothing downstream of the parser has a trait or impl kind: these two
// functions turn the declarations into ordinary ones, and `SourceFile.items` calls
// them. The pretty printer recognises the shapes and prints `trait` / `impl` back.

/// The name a type reference dispatches on: the head of `List<'a>` is "List", of
/// `Acme.Point` is "Point", of `Int64` is "Int64". Used to place an impl's members
/// under `<module>.<TypeName>.<TraitName>`.
let typeReferenceHeadName (t : TypeReference) : string =
  match t with
  | TUnit _ -> "Unit"
  | TBool _ -> "Bool"
  | TInt _ -> "Int"
  | TInt8 _ -> "Int8"
  | TUInt8 _ -> "UInt8"
  | TInt16 _ -> "Int16"
  | TUInt16 _ -> "UInt16"
  | TInt32 _ -> "Int32"
  | TUInt32 _ -> "UInt32"
  | TInt64 _ -> "Int64"
  | TUInt64 _ -> "UInt64"
  | TInt128 _ -> "Int128"
  | TUInt128 _ -> "UInt128"
  | TFloat _ -> "Float"
  | TChar _ -> "Char"
  | TString _ -> "String"
  | TDateTime _ -> "DateTime"
  | TUuid _ -> "Uuid"
  | TBlob _ -> "Blob"
  | TList _ -> "List"
  | TDict _ -> "Dict"
  | TTuple _ -> "Tuple"
  | TFn _ -> "Fn"
  | TVariable(_, _, (_, name)) -> name
  | TCustom qti -> qti.typ.name

/// The impl's own module path: `<module>[.<Type>].<Trait>`, the type segment
/// dropped when the module is already named for the type.
let implMemberPath (currentPath : List<string>) (impl : ImplDecl) : List<string> =
  let typeName = typeReferenceHeadName impl.forType
  let withType =
    match List.tryLast currentPath with
    | Some last when last = typeName -> currentPath
    | _ -> currentPath @ [ typeName ]
  withType @ [ impl.trait_.typ.name ]

let packageTrait
  (owner : string)
  (modules : List<string>)
  (t : TraitDecl)
  : PackageTrait.PackageTrait =
  { name = { owner = owner; modules = modules; name = t.name.name }
    typeParams = t.typeParams |> List.map fst
    bounds = t.bounds |> List.map boundNorm
    methods =
      t.methods
      |> List.map (fun m ->
        { name = m.name.name
          typeParams = m.typeParams |> List.map fst
          bounds = m.bounds |> List.map boundNorm
          parameters =
            m.parameters
            |> List.map fnParamNorm
            |> NEList.ofListWithDefault (
              { name = "_"; typ = TUnit synthRange; description = "" }
              : PackageFn.Parameter
            )
          returnType = m.returnType
          effects = m.effects |> Option.map (List.map (fun id -> id.name))
          description = m.description })
    description = t.description }

/// The method fns an impl declares, as package fns under the impl's own path, with
/// the impl's type params and bounds prepended (a conditional impl's methods are
/// generic over the impl's params).
let implMethodFns
  (owner : string)
  (memberPath : List<string>)
  (impl : ImplDecl)
  : List<PackageFn.PackageFn> =
  impl.methods
  |> List.map (fun m ->
    packageFn
      owner
      memberPath
      { m with
          typeParams = impl.typeParams @ m.typeParams
          bounds = impl.bounds @ m.bounds })

/// The impl item itself. Its location is the member path: the module the method
/// fns live in IS the impl's name.
let packageImpl
  (owner : string)
  (memberPath : List<string>)
  (impl : ImplDecl)
  : PackageImpl.PackageImpl =
  let location =
    match List.rev memberPath with
    | name :: revModules ->
      ({ owner = owner; modules = List.rev revModules; name = name }
      : PackageImpl.Name)
    | [] -> { owner = owner; modules = []; name = impl.trait_.typ.name }
  let declared =
    impl.methods
    |> List.map (fun m -> (m.name.name, Unresolved(NEList.singleton m.name.name)))
  let aliased =
    impl.aliases
    |> List.map (fun a ->
      let target =
        match a.body with
        | EFnName(_, q) ->
          Unresolved(
            NEList.ofListUnsafe
              "alias"
              []
              ((q.modules |> List.map (fun (m, _) -> m.name)) @ [ q.fn.name ])
          )
        | EVariable(_, n) -> Unresolved(NEList.singleton n)
        | _ -> Unresolved(NEList.singleton a.name.name)
      (a.name.name, target))
  { name = location
    trait_ = impl.trait_
    forType = impl.forType
    typeParams = impl.typeParams |> List.map fst
    bounds = impl.bounds |> List.map boundNorm
    methods = declared @ aliased
    description = impl.description }
