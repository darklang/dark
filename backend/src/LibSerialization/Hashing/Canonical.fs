/// Canonical serializers for content-addressable hashing.
///
/// Produces deterministic bytes by skipping identity-irrelevant fields
/// (AST node IDs, description, deprecated, originalName).
/// Re-uses leaf serializers from the existing binary format.
module rec LibSerialization.Hashing.Canonical

open System.IO
open Prelude
open LibExecution.ProgramTypes
module PT = LibExecution.ProgramTypes

// Re-use leaf serializers from existing binary serializers
module Common = LibSerialization.Binary.Serializers.Common
module PTC = LibSerialization.Binary.Serializers.PT.Common
module ExprS = LibSerialization.Binary.Serializers.PT.Expr


/// Controls how package references are written during hashing.
type HashRefMode =
  {
    /// Deps from already-processed SCCs (topological order);
    /// their hashes are final and can be used directly.
    resolvedDeps : Map<Hash, Hash>

    /// Items within the current SCC whose hashes can't be known yet
    /// (circular dependency); use FQN strings instead of hashes to
    /// break the circularity.
    sccNames : Map<Hash, string>
  }

let Normal = { resolvedDeps = Map.empty; sccNames = Map.empty }

/// Resolve a Hash: first apply finalized deps, then check SCC names.
let private resolveHash (mode : HashRefMode) (hash : Hash) : Hash =
  mode.resolvedDeps |> Map.tryFind hash |> Option.defaultValue hash

let private isSccRef (mode : HashRefMode) (hash : Hash) : Option<string> =
  let resolved = resolveHash mode hash
  Map.tryFind resolved mode.sccNames


// =====================
// Name resolution writers
// =====================

/// Skip originalName, only write resolved value (or error)
let writeNameResolution
  (writeValue : BinaryWriter -> 'a -> unit)
  (w : BinaryWriter)
  (nr : PT.NameResolution<'a>)
  =
  match nr.resolved with
  | Ok value ->
    w.Write(0uy)
    writeValue w value
  | Error error ->
    w.Write(1uy)
    PTC.NameResolutionError.write w error


/// Write FQTypeName, resolving deps and checking SCC substitution
let writeFQTypeName
  (mode : HashRefMode)
  (w : BinaryWriter)
  (name : PT.FQTypeName.FQTypeName)
  =
  match name with
  | PT.FQTypeName.Package p ->
    match isSccRef mode p with
    | Some fqn ->
      w.Write(1uy) // SCC name-ref tag
      Common.String.write w fqn
    | None ->
      w.Write(0uy)
      PTC.FQTypeName.Package.write w (resolveHash mode p)


/// Write FQFnName, resolving deps and checking SCC substitution
let writeFQFnName
  (mode : HashRefMode)
  (w : BinaryWriter)
  (name : PT.FQFnName.FQFnName)
  =
  match name with
  | PT.FQFnName.Builtin b ->
    w.Write(0uy)
    PTC.FQFnName.Builtin.write w b
  | PT.FQFnName.Package p ->
    match isSccRef mode p with
    | Some fqn ->
      w.Write(2uy) // SCC name-ref tag
      Common.String.write w fqn
    | None ->
      w.Write(1uy)
      PTC.FQFnName.Package.write w (resolveHash mode p)


/// Write FQValueName, resolving deps and checking SCC substitution
let writeFQValueName
  (mode : HashRefMode)
  (w : BinaryWriter)
  (name : PT.FQValueName.FQValueName)
  =
  match name with
  | PT.FQValueName.Builtin b ->
    w.Write(0uy)
    PTC.FQValueName.Builtin.write w b
  | PT.FQValueName.Package p ->
    match isSccRef mode p with
    | Some fqn ->
      w.Write(2uy) // SCC name-ref tag
      Common.String.write w fqn
    | None ->
      w.Write(1uy)
      PTC.FQValueName.Package.write w (resolveHash mode p)


// =====================
// Type reference writers
// =====================

let writeTypeReference
  (mode : HashRefMode)
  (w : BinaryWriter)
  (t : PT.TypeReference)
  : unit =
  match t with
  | PT.TInt64 -> w.Write 0uy
  | PT.TUInt64 -> w.Write 1uy
  | PT.TInt8 -> w.Write 2uy
  | PT.TUInt8 -> w.Write 3uy
  | PT.TInt16 -> w.Write 4uy
  | PT.TUInt16 -> w.Write 5uy
  | PT.TInt32 -> w.Write 6uy
  | PT.TUInt32 -> w.Write 7uy
  | PT.TInt128 -> w.Write 8uy
  | PT.TUInt128 -> w.Write 9uy
  | PT.TFloat -> w.Write 10uy
  | PT.TBool -> w.Write 11uy
  | PT.TUnit -> w.Write 12uy
  | PT.TString -> w.Write 13uy
  | PT.TList inner ->
    w.Write 14uy
    writeTypeReference mode w inner
  | PT.TDict inner ->
    w.Write 15uy
    writeTypeReference mode w inner
  | PT.TDB inner ->
    w.Write 16uy
    writeTypeReference mode w inner
  | PT.TDateTime -> w.Write 17uy
  | PT.TChar -> w.Write 18uy
  | PT.TUuid -> w.Write 19uy
  | PT.TCustomType(typeName, typeArgs) ->
    w.Write 20uy
    writeNameResolution (writeFQTypeName mode) w typeName
    Common.List.write w (writeTypeReference mode) typeArgs
  | PT.TVariable name ->
    w.Write 21uy
    Common.String.write w name
  | PT.TFn(paramTypes, returnType) ->
    w.Write 22uy
    Common.NEList.write (writeTypeReference mode) w paramTypes
    writeTypeReference mode w returnType
  | PT.TTuple(first, second, rest) ->
    w.Write 23uy
    writeTypeReference mode w first
    writeTypeReference mode w second
    Common.List.write w (writeTypeReference mode) rest
  | PT.TBlob -> w.Write 24uy
  | PT.TStream inner ->
    w.Write 25uy
    writeTypeReference mode w inner


// =====================
// Pattern writers
// =====================

let writeLetPattern (w : BinaryWriter) (pattern : PT.LetPattern) =
  match pattern with
  | PT.LPVariable(_id, name) ->
    w.Write 0uy
    Common.String.write w name
  | PT.LPUnit _id -> w.Write 1uy
  | PT.LPTuple(_id, first, second, rest) ->
    w.Write 2uy
    writeLetPattern w first
    writeLetPattern w second
    Common.List.write w writeLetPattern rest
  | PT.LPWildcard _id -> w.Write 3uy


let writeMatchPattern (w : BinaryWriter) (pattern : PT.MatchPattern) =
  match pattern with
  | PT.MPVariable(_id, name) ->
    w.Write 0uy
    Common.String.write w name
  | PT.MPEnum(_id, caseName, fieldPats) ->
    w.Write 1uy
    Common.String.write w caseName
    Common.List.write w writeMatchPattern fieldPats
  | PT.MPInt64(_id, value) ->
    w.Write 2uy
    w.Write value
  | PT.MPUInt64(_id, value) ->
    w.Write 3uy
    w.Write value
  | PT.MPInt8(_id, value) ->
    w.Write 4uy
    w.Write value
  | PT.MPUInt8(_id, value) ->
    w.Write 5uy
    w.Write value
  | PT.MPInt16(_id, value) ->
    w.Write 6uy
    w.Write value
  | PT.MPUInt16(_id, value) ->
    w.Write 7uy
    w.Write value
  | PT.MPInt32(_id, value) ->
    w.Write 8uy
    w.Write value
  | PT.MPUInt32(_id, value) ->
    w.Write 9uy
    w.Write value
  | PT.MPInt128(_id, value) ->
    w.Write 10uy
    Common.String.write w (string value)
  | PT.MPUInt128(_id, value) ->
    w.Write 11uy
    Common.String.write w (string value)
  | PT.MPBool(_id, value) ->
    w.Write 12uy
    w.Write value
  | PT.MPChar(_id, value) ->
    w.Write 13uy
    Common.String.write w value
  | PT.MPString(_id, value) ->
    w.Write 14uy
    Common.String.write w value
  | PT.MPFloat(_id, sign, whole, fractional) ->
    w.Write 15uy
    PTC.Sign.write w sign
    Common.String.write w whole
    Common.String.write w fractional
  | PT.MPUnit _id -> w.Write 16uy
  | PT.MPTuple(_id, first, second, rest) ->
    w.Write 17uy
    writeMatchPattern w first
    writeMatchPattern w second
    Common.List.write w writeMatchPattern rest
  | PT.MPList(_id, patterns) ->
    w.Write 18uy
    Common.List.write w writeMatchPattern patterns
  | PT.MPListCons(_id, head, tail) ->
    w.Write 19uy
    writeMatchPattern w head
    writeMatchPattern w tail
  | PT.MPOr(_id, patterns) ->
    w.Write 20uy
    Common.NEList.write writeMatchPattern w patterns


// =====================
// Expression writers
// =====================

let writeStringSegment
  (mode : HashRefMode)
  (w : BinaryWriter)
  (segment : PT.StringSegment)
  =
  match segment with
  | PT.StringText text ->
    w.Write 0uy
    Common.String.write w text
  | PT.StringInterpolation expr ->
    w.Write 1uy
    writeExpr mode w expr


let writeMatchCase (mode : HashRefMode) (w : BinaryWriter) (case : PT.MatchCase) =
  writeMatchPattern w case.pat
  Common.Option.write w (writeExpr mode) case.whenCondition
  writeExpr mode w case.rhs


let writePipeExpr (mode : HashRefMode) (w : BinaryWriter) (pipeExpr : PT.PipeExpr) =
  match pipeExpr with
  | PT.EPipeVariable(_id, name, args) ->
    w.Write 0uy
    Common.String.write w name
    Common.List.write w (writeExpr mode) args
  | PT.EPipeLambda(_id, pats, body) ->
    w.Write 1uy
    Common.NEList.write writeLetPattern w pats
    writeExpr mode w body
  | PT.EPipeInfix(_id, infix, expr) ->
    w.Write 2uy
    ExprS.Infix.write w infix
    writeExpr mode w expr
  | PT.EPipeFnCall(_id, fnName, typeArgs, args) ->
    w.Write 3uy
    writeNameResolution (writeFQFnName mode) w fnName
    Common.List.write w (writeTypeReference mode) typeArgs
    Common.List.write w (writeExpr mode) args
  | PT.EPipeEnum(_id, typeName, caseName, fields) ->
    w.Write 4uy
    writeNameResolution (writeFQTypeName mode) w typeName
    Common.String.write w caseName
    Common.List.write w (writeExpr mode) fields


let writeExpr (mode : HashRefMode) (w : BinaryWriter) (expr : PT.Expr) =
  match expr with
  | PT.EInt64(_id, value) ->
    w.Write 0uy
    w.Write value
  | PT.EUInt64(_id, value) ->
    w.Write 1uy
    w.Write value
  | PT.EInt8(_id, value) ->
    w.Write 2uy
    w.Write value
  | PT.EUInt8(_id, value) ->
    w.Write 3uy
    w.Write value
  | PT.EInt16(_id, value) ->
    w.Write 4uy
    w.Write value
  | PT.EUInt16(_id, value) ->
    w.Write 5uy
    w.Write value
  | PT.EInt32(_id, value) ->
    w.Write 6uy
    w.Write value
  | PT.EUInt32(_id, value) ->
    w.Write 7uy
    w.Write value
  | PT.EInt128(_id, value) ->
    w.Write 8uy
    Common.String.write w (string value)
  | PT.EUInt128(_id, value) ->
    w.Write 9uy
    Common.String.write w (string value)
  | PT.EBool(_id, value) ->
    w.Write 10uy
    w.Write value
  | PT.EString(_id, segments) ->
    w.Write 11uy
    Common.List.write w (writeStringSegment mode) segments
  | PT.EChar(_id, value) ->
    w.Write 12uy
    Common.String.write w value
  | PT.EFloat(_id, sign, whole, fractional) ->
    w.Write 13uy
    PTC.Sign.write w sign
    Common.String.write w whole
    Common.String.write w fractional
  | PT.EUnit _id -> w.Write 14uy
  | PT.EValue(_id, nameRes) ->
    w.Write 15uy
    writeNameResolution (writeFQValueName mode) w nameRes
  | PT.ELet(_id, pattern, rhs, body) ->
    w.Write 16uy
    writeLetPattern w pattern
    writeExpr mode w rhs
    writeExpr mode w body
  | PT.EIf(_id, cond, thenExpr, elseExpr) ->
    w.Write 17uy
    writeExpr mode w cond
    writeExpr mode w thenExpr
    Common.Option.write w (writeExpr mode) elseExpr
  | PT.ELambda(_id, pats, body) ->
    w.Write 18uy
    Common.NEList.write writeLetPattern w pats
    writeExpr mode w body
  | PT.ERecordFieldAccess(_id, expr, field) ->
    w.Write 19uy
    writeExpr mode w expr
    Common.String.write w field
  | PT.EVariable(_id, name) ->
    w.Write 20uy
    Common.String.write w name
  | PT.EApply(_id, fn, typeArgs, args) ->
    w.Write 21uy
    writeExpr mode w fn
    Common.List.write w (writeTypeReference mode) typeArgs
    Common.NEList.write (writeExpr mode) w args
  | PT.EList(_id, exprs) ->
    w.Write 22uy
    Common.List.write w (writeExpr mode) exprs
  | PT.ERecord(_id, typeName, typeArgs, fields) ->
    w.Write 23uy
    writeNameResolution (writeFQTypeName mode) w typeName
    Common.List.write w (writeTypeReference mode) typeArgs
    Common.List.write
      w
      (fun w (name, expr) ->
        Common.String.write w name
        writeExpr mode w expr)
      fields
  | PT.ERecordUpdate(_id, record, updates) ->
    w.Write 24uy
    writeExpr mode w record
    Common.NEList.write
      (fun w (name, expr) ->
        Common.String.write w name
        writeExpr mode w expr)
      w
      updates
  | PT.EPipe(_id, expr, pipes) ->
    w.Write 25uy
    writeExpr mode w expr
    Common.List.write w (writePipeExpr mode) pipes
  | PT.EEnum(_id, typeName, typeArgs, caseName, fields) ->
    w.Write 26uy
    writeNameResolution (writeFQTypeName mode) w typeName
    Common.List.write w (writeTypeReference mode) typeArgs
    Common.String.write w caseName
    Common.List.write w (writeExpr mode) fields
  | PT.EMatch(_id, expr, cases) ->
    w.Write 27uy
    writeExpr mode w expr
    Common.List.write w (writeMatchCase mode) cases
  | PT.ETuple(_id, first, second, rest) ->
    w.Write 28uy
    writeExpr mode w first
    writeExpr mode w second
    Common.List.write w (writeExpr mode) rest
  | PT.EInfix(_id, op, left, right) ->
    w.Write 29uy
    ExprS.Infix.write w op
    writeExpr mode w left
    writeExpr mode w right
  | PT.EDict(_id, pairs) ->
    w.Write 30uy
    Common.List.write
      w
      (fun w (key, value) ->
        Common.String.write w key
        writeExpr mode w value)
      pairs
  | PT.EFnName(_id, nameRes) ->
    w.Write 31uy
    writeNameResolution (writeFQFnName mode) w nameRes
  | PT.EStatement(_id, first, next) ->
    w.Write 32uy
    writeExpr mode w first
    writeExpr mode w next
  | PT.ESelf _id -> w.Write 33uy
  | PT.EArg(_id, index) ->
    w.Write 34uy
    w.Write index


// =====================
// Type declaration writers
// =====================

let writeParameter
  (mode : HashRefMode)
  (w : BinaryWriter)
  (p : PT.PackageFn.Parameter)
  =
  Common.String.write w p.name
  writeTypeReference mode w p.typ

let writeRecordField
  (mode : HashRefMode)
  (w : BinaryWriter)
  (f : PT.TypeDeclaration.RecordField)
  =
  Common.String.write w f.name
  writeTypeReference mode w f.typ

let writeEnumField
  (mode : HashRefMode)
  (w : BinaryWriter)
  (f : PT.TypeDeclaration.EnumField)
  =
  writeTypeReference mode w f.typ
  Common.Option.write w Common.String.write f.label

let writeEnumCase
  (mode : HashRefMode)
  (w : BinaryWriter)
  (c : PT.TypeDeclaration.EnumCase)
  =
  Common.String.write w c.name
  Common.List.write w (writeEnumField mode) c.fields

let writeTypeDeclaration
  (mode : HashRefMode)
  (w : BinaryWriter)
  (d : PT.TypeDeclaration.T)
  =
  Common.List.write w Common.String.write d.typeParams
  match d.definition with
  | PT.TypeDeclaration.Alias typeRef ->
    w.Write(0uy)
    writeTypeReference mode w typeRef
  | PT.TypeDeclaration.Record fields ->
    w.Write(1uy)
    Common.NEList.write (writeRecordField mode) w fields
  | PT.TypeDeclaration.Enum cases ->
    w.Write(2uy)
    Common.NEList.write (writeEnumCase mode) w cases


// =====================
// Top-level item writers (used by both compute*Hash and SCC batch hashing)
// =====================

/// Write a PackageType's hash-relevant content (skip hash, description, deprecated)
let writeType
  (mode : HashRefMode)
  (w : BinaryWriter)
  (t : PT.PackageType.PackageType)
  =
  w.Write(0uy) // tag: type
  writeTypeDeclaration mode w t.declaration

/// Write a PackageFn's hash-relevant content (skip hash, description, deprecated)
let writeFn (mode : HashRefMode) (w : BinaryWriter) (fn : PT.PackageFn.PackageFn) =
  w.Write(1uy) // tag: fn
  writeExpr mode w fn.body
  Common.List.write w Common.String.write fn.typeParams
  Common.NEList.write (writeParameter mode) w fn.parameters
  writeTypeReference mode w fn.returnType

/// Write a PackageValue's hash-relevant content (skip hash, description, deprecated)
let writeValue
  (mode : HashRefMode)
  (w : BinaryWriter)
  (v : PT.PackageValue.PackageValue)
  =
  w.Write(2uy) // tag: value
  writeExpr mode w v.body
