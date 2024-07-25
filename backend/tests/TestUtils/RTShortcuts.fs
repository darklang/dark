/// Collection of helpful "shortcut" functions to create Dark values quickly
module TestUtils.RTShortcuts

open Prelude
open LibExecution.RuntimeTypes

module PT = LibExecution.ProgramTypes
module PT2RT = LibExecution.ProgramTypesToRuntimeTypes

// let eUnit () : Expr = EUnit(gid ())

// let eBool (b : bool) : Expr = EBool(gid (), b)

// let eInt8 (i : int8) : Expr = EInt8(gid (), i)
// let euInt8 (i : uint8) : Expr = EUInt8(gid (), i)
// let eInt16 (i : int16) : Expr = EInt16(gid (), i)
// let euInt16 (i : uint16) : Expr = EUInt16(gid (), i)
// let eInt32 (i : int32) : Expr = EInt32(gid (), i)
// let euInt32 (i : uint32) : Expr = EUInt32(gid (), i)
// let eInt64 (i : int64) : Expr = EInt64(gid (), i)
// let euInt64 (i : uint64) : Expr = EUInt64(gid (), i)
// let eInt128 (i : System.Int128) : Expr = EInt128(gid (), i)
// let euInt128 (i : System.UInt128) : Expr = EUInt128(gid (), i)

// let eFloat (sign : Sign) (whole : string) (fraction : string) : Expr =
//   EFloat(gid (), makeFloat sign whole fraction)

//let eChar (c : string) : Expr = EChar(gid (), c)
// let eStr (str : string) : Expr = EString(gid (), [ StringText str ])




// let eList (elems : Expr list) : Expr = EList(gid (), elems)

// let eVar (name : string) : Expr = EVariable(gid (), name)

// let eFieldAccess (expr : Expr) (fieldName : string) : Expr =
//   EFieldAccess(gid (), expr, fieldName)

// let eLambda (pats : List<LetPattern>) (body : Expr) : Expr =
//   let pats = NEList.ofListUnsafe "eLambda" [] pats
//   ELambda(gid (), pats, body)

// let eEnum
//   (typeName : FQTypeName.FQTypeName)
//   (name : string)
//   (args : Expr list)
//   : Expr =
//   EEnum(gid (), typeName, name, args)


// let eBuiltinFnName (name : string) (version : int) : Expr =
//   PT.FQFnName.fqBuiltIn name version
//   |> PT2RT.FQFnName.toRT
//   |> fun x -> EFnName(gid (), x)


// let eFn'
//   (function_ : string)
//   (version : int)
//   (typeArgs : List<TypeReference>)
//   (args : List<Expr>)
//   : Expr =
//   let args = NEList.ofListUnsafe "eFn'" [] args
//   EApply(gid (), (eBuiltinFnName function_ version), typeArgs, args)

// let eFn
//   (function_ : string)
//   (version : int)
//   (typeArgs : List<TypeReference>)
//   (args : List<Expr>)
//   : Expr =
//   eFn' function_ version typeArgs args


// let eApply
//   (target : Expr)
//   (typeArgs : List<TypeReference>)
//   (args : List<Expr>)
//   : Expr =
//   let args = NEList.ofListUnsafe "eApply" [] args
//   EApply(gid (), target, typeArgs, args)

// let eTuple (first : Expr) (second : Expr) (theRest : Expr list) : Expr =
//   ETuple(gid (), first, second, theRest)


// let customTypeRecord (fields : List<string * TypeReference>) : TypeDeclaration.T =
//   let fields =
//     fields
//     |> List.map (fun (name, typ) ->
//       { name = name; typ = typ } : TypeDeclaration.RecordField)
//   match fields with
//   | [] -> Exception.raiseInternal "userRecord must have at least one field" []
//   | hd :: rest ->
//     { typeParams = []; definition = TypeDeclaration.Record(NEList.ofList hd rest) }
