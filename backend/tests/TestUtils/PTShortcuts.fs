/// Collection of helpful "shortcut" functions to create Dark values quickly
module TestUtils.PTShortcuts

open Prelude
open LibExecution.ProgramTypes


let typeNamePkg id = FQTypeName.fqPackage id


let eUnit () : Expr = EUnit(gid ())

let eBool (b : bool) : Expr = EBool(gid (), b)

let eInt8 (i : int8) : Expr = EInt8(gid (), i)
let euInt8 (i : uint8) : Expr = EUInt8(gid (), i)
let eInt16 (i : int16) : Expr = EInt16(gid (), i)
let euInt16 (i : uint16) : Expr = EUInt16(gid (), i)
let eInt32 (i : int32) : Expr = EInt32(gid (), i)
let euInt32 (i : uint32) : Expr = EUInt32(gid (), i)
let eInt64 (i : int64) : Expr = EInt64(gid (), i)
let euInt64 (i : uint64) : Expr = EUInt64(gid (), i)
let eInt128 (i : System.Int128) : Expr = EInt128(gid (), i)
let euInt128 (i : System.UInt128) : Expr = EUInt128(gid (), i)

let eFloat (sign : Sign) (whole : string) (fraction : string) : Expr =
  EFloat(gid (), sign, whole, fraction)

let eChar (c : string) : Expr = EChar(gid (), c)

let strText (str : string) : StringSegment = StringText str
let strInterp (expr : Expr) : StringSegment = StringInterpolation expr
let eStr (segments : List<StringSegment>) : Expr = EString(gid (), segments)

let eList (elems : Expr list) : Expr = EList(gid (), elems)
let eDict (entries : List<string * Expr>) : Expr = EDict(gid (), entries)
let eTuple (first : Expr) (second : Expr) (theRest : Expr list) : Expr =
  ETuple(gid (), first, second, theRest)


let lpUnit () : LetPattern = LPUnit(gid ())
let lpVar (name : string) : LetPattern = LPVariable(gid (), name)
let lpTuple
  (first : LetPattern)
  (second : LetPattern)
  (theRest : LetPattern list)
  : LetPattern =
  LPTuple(gid (), first, second, theRest)
let eLet (pat : LetPattern) (value : Expr) (body : Expr) : Expr =
  ELet(gid (), pat, value, body)
let eVar (name : string) : Expr = EVariable(gid (), name)

let eIf (cond : Expr) (thenBranch : Expr) (elseBranch : Option<Expr>) : Expr =
  EIf(gid (), cond, thenBranch, elseBranch)

let eMatch (expr : Expr) (cases : List<MatchCase>) : Expr =
  EMatch(gid (), expr, cases)

let eRecord
  (typeName : FQTypeName.FQTypeName)
  (typeArgs : List<TypeReference>)
  (fields : List<string * Expr>)
  : Expr =
  ERecord(gid (), Ok typeName, typeArgs, fields)

let eFieldAccess (expr : Expr) (fieldName : string) : Expr =
  ERecordFieldAccess(gid (), expr, fieldName)

let eRecordUpdate (expr : Expr) (updates : List<string * Expr>) : Expr =
  ERecordUpdate(gid (), expr, NEList.ofListUnsafe "" [] updates)

let eEnum
  (typeName : FQTypeName.FQTypeName)
  (typeArgs : List<TypeReference>)
  (caseName : string)
  (args : Expr list)
  : Expr =
  EEnum(gid (), Ok typeName, typeArgs, caseName, args)


let eInfix (op : Infix) (left : Expr) (right : Expr) : Expr =
  EInfix(gid (), op, left, right)

let eBuiltinValue (name : string) (version : int) : Expr =
  EValue(gid (), Ok(FQValueName.fqBuiltIn name version))

let ePackageValue (hash : Hash) : Expr =
  EValue(gid (), Ok(FQValueName.fqPackage hash))

let eBuiltinFn (name : string) (version : int) : Expr =
  EFnName(gid (), Ok(FQFnName.fqBuiltIn name version))

let ePackageFn (hash : Hash) : Expr = EFnName(gid (), Ok(FQFnName.fqPackage hash))

let eLambda id (pats : List<LetPattern>) (body : Expr) : Expr =
  let pats = NEList.ofListUnsafe "eLambda" [] pats
  ELambda(id, pats, body)

let eApply
  (target : Expr)
  (typeArgs : List<TypeReference>)
  (args : List<Expr>)
  : Expr =
  let args = NEList.ofListUnsafe "eApply" [] args
  EApply(gid (), target, typeArgs, args)

let ePipe (expr : Expr) (parts : List<PipeExpr>) : Expr = EPipe(gid (), expr, parts)

let eStatement (first : Expr) (next : Expr) : Expr = EStatement(gid (), first, next)

let pLambda id (pats : List<LetPattern>) (body : Expr) : PipeExpr =
  EPipeLambda(id, NEList.ofListUnsafe "pLambda" [] pats, body)

let pInfix id (op : Infix) (expr : Expr) : PipeExpr = EPipeInfix(id, op, expr)

let pFnCall
  id
  (fn : FQFnName.FQFnName)
  (typeArgs : List<TypeReference>)
  (args : List<Expr>)
  : PipeExpr =
  EPipeFnCall(id, Ok fn, typeArgs, args)

let pEnum
  id
  (typeName : FQTypeName.FQTypeName)
  (caseName : string)
  (fields : List<Expr>)
  : PipeExpr =
  EPipeEnum(id, Ok typeName, caseName, fields)

let pVariable id (varName : string) (args : List<Expr>) : PipeExpr =
  EPipeVariable(id, varName, args)
