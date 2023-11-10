/// Collection of helpful "shortcut" functions to create Dark values quickly
module TestUtils.RTShortcuts

open Prelude
open LibExecution.RuntimeTypes

module PT = LibExecution.ProgramTypes
module PT2RT = LibExecution.ProgramTypesToRuntimeTypes
module PTParser = LibExecution.ProgramTypesParser

let eStdFnName (modules : List<string>) (name : string) (version : int) : Expr =
  PT.FnName.fqBuiltIn modules name version
  |> PT2RT.FnName.toRT
  |> fun x -> EFnName(gid (), x)

let eUserFnName (name : string) : Expr =
  PT.FnName.fqUserProgram [] name 0
  |> PT2RT.FnName.toRT
  |> fun x -> EFnName(gid (), x)


let eFn'
  (modules : List<string>)
  (function_ : string)
  (version : int)
  (typeArgs : List<TypeReference>)
  (args : List<Expr>)
  : Expr =
  let args = NEList.ofListUnsafe "eFn'" [] args
  EApply(gid (), (eStdFnName modules function_ version), typeArgs, args)

let eFn
  (modules : List<string>)
  (function_ : string)
  (version : int)
  (typeArgs : List<TypeReference>)
  (args : List<Expr>)
  : Expr =
  eFn' modules function_ version typeArgs args

let eUserFn
  (function_ : string)
  (typeArgs : List<TypeReference>)
  (args : List<Expr>)
  : Expr =
  let args = NEList.ofListUnsafe "eUserFn" [] args
  EApply(gid (), (eUserFnName function_), typeArgs, args)

let eApply
  (target : Expr)
  (typeArgs : List<TypeReference>)
  (args : List<Expr>)
  : Expr =
  let args = NEList.ofListUnsafe "eApply" [] args
  EApply(gid (), target, typeArgs, args)

let eStr (str : string) : Expr = EString(gid (), [ StringText str ])

let eChar (c : string) : Expr = EChar(gid (), c)

let eInt (i : int) : Expr = EInt(gid (), int64 i)

let eInt8 (i : int8) : Expr = EInt8(gid (), i)

let euInt8 (i : uint8) : Expr = EUInt8(gid (), i)

let eInt16 (i : int16) : Expr = EInt16(gid (), i)

let euInt16 (i : uint16) : Expr = EUInt16(gid (), i)

let eInt32 (i : int32) : Expr = EInt32(gid (), i)

let euInt32 (i : uint32) : Expr = EUInt32(gid (), i)

let eInt128 (i : System.Int128) : Expr = EInt128(gid (), i)

let euInt128 (i : System.UInt128) : Expr = EUInt128(gid (), i)

let eBool (b : bool) : Expr = EBool(gid (), b)

let eFloat (sign : Sign) (whole : string) (fraction : string) : Expr =
  EFloat(gid (), makeFloat sign whole fraction)

let eUnit () : Expr = EUnit(gid ())

let eList (elems : Expr list) : Expr = EList(gid (), elems)

let eVar (name : string) : Expr = EVariable(gid (), name)

let eFieldAccess (expr : Expr) (fieldName : string) : Expr =
  EFieldAccess(gid (), expr, fieldName)

let eLambda (pats : List<LetPattern>) (body : Expr) : Expr =
  let pats = NEList.ofListUnsafe "eLambda" [] pats
  ELambda(gid (), pats, body)

let eEnum (typeName : TypeName.TypeName) (name : string) (args : Expr list) : Expr =
  EEnum(gid (), typeName, name, args)

let userTypeName
  (modules : List<string>)
  (name : string)
  (version : int)
  : TypeName.UserProgram =
  { modules = modules; name = TypeName.TypeName name; version = version }

let fqUserTypeName (modules : List<string>) (name : string) (version : int) =
  FQName.UserProgram(userTypeName modules name version)

let eTuple (first : Expr) (second : Expr) (theRest : Expr list) : Expr =
  ETuple(gid (), first, second, theRest)

let userTypeReference
  (modules : List<string>)
  (name : string)
  (version : int)
  : TypeReference =
  TCustomType(Ok(fqUserTypeName modules name version), [])

let customTypeRecord (fields : List<string * TypeReference>) : TypeDeclaration.T =
  let fields =
    fields
    |> List.map (fun (name, typ) ->
      { name = name; typ = typ } : TypeDeclaration.RecordField)
  match fields with
  | [] -> Exception.raiseInternal "userRecord must have at least one field" []
  | hd :: rest ->
    { typeParams = []; definition = TypeDeclaration.Record(NEList.ofList hd rest) }

let userTypeRecord
  (modules : List<string>)
  (name : string)
  (version : int)
  (fields : List<string * TypeReference>)
  : UserType.T =
  { tlid = gid ()
    name = { modules = modules; name = TypeName.TypeName name; version = version }
    declaration = customTypeRecord fields }
