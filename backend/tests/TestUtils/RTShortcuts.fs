/// Collection of helpful "shortcut" functions to create Dark values quickly
module TestUtils.RTShortcuts

open Prelude
open LibExecution.RuntimeTypes

module PT = LibExecution.ProgramTypes
module PT2RT = LibExecution.ProgramTypesToRuntimeTypes
module PTParser = LibExecution.ProgramTypesParser

let eStdFnName (module_ : string) (function_ : string) (version : int) : FnTarget =
  PT.FQFnName.stdlibFqName module_ function_ version
  |> PT2RT.FQFnName.toRT
  |> FnName

let eUserFnName (function_ : string) : FnTarget =
  PT.FQFnName.userFqName function_ |> PT2RT.FQFnName.toRT |> FnName


let eFn'
  (module_ : string)
  (function_ : string)
  (version : int)
  (typeArgs : List<TypeReference>)
  (args : List<Expr>)
  : Expr =
  EApply(gid (), (eStdFnName module_ function_ version), typeArgs, args)

let eFn
  (module_ : string)
  (function_ : string)
  (version : int)
  (typeArgs : List<TypeReference>)
  (args : List<Expr>)
  : Expr =
  eFn' module_ function_ version typeArgs args

let eUserFn
  (function_ : string)
  (typeArgs : List<TypeReference>)
  (args : List<Expr>)
  : Expr =
  EApply(gid (), (eUserFnName function_), typeArgs, args)

let eApply'
  (target : FnTarget)
  (typeArgs : List<TypeReference>)
  (args : List<Expr>)
  : Expr =
  EApply(gid (), target, typeArgs, args)

let eApply
  (target : Expr)
  (typeArgs : List<TypeReference>)
  (args : List<Expr>)
  : Expr =
  eApply' (FnTargetExpr target) typeArgs args


let eStr (str : string) : Expr = EString(gid (), [ StringText str ])

let eChar (c : string) : Expr = EChar(gid (), c)

let eInt (i : int) : Expr = EInt(gid (), int64 i)

let eBool (b : bool) : Expr = EBool(gid (), b)

let eFloat (sign : Sign) (whole : string) (fraction : string) : Expr =
  EFloat(gid (), makeFloat sign whole fraction)

let eUnit () : Expr = EUnit(gid ())

let eList (elems : Expr list) : Expr = EList(gid (), elems)

let eVar (name : string) : Expr = EVariable(gid (), name)

let eFieldAccess (expr : Expr) (fieldName : string) : Expr =
  EFieldAccess(gid (), expr, fieldName)

let eLambda (varNames : string list) (body : Expr) : Expr =
  ELambda(gid (), List.map (fun name -> (gid (), name)) varNames, body)

let eConstructor
  (typeName : Option<FQTypeName.T>)
  (name : string)
  (args : Expr list)
  : Expr =
  EConstructor(gid (), typeName, name, args)

let userTypeName (name : string) (version : int) : FQTypeName.T =
  FQTypeName.User { typ = name; version = version }

let userTypeReference (name : string) (version : int) : TypeReference =
  TCustomType(userTypeName name version, [])

let customTypeRecord (fields : List<string * TypeReference>) : CustomType.T =
  let fields =
    fields
    |> List.map (fun (name, typ) ->
      { id = gid (); name = name; typ = typ } : CustomType.RecordField)
  match fields with
  | [] -> Exception.raiseInternal "userRecord must have at least one field" []
  | hd :: rest -> CustomType.Record(hd, rest)

let userTypeRecord
  (name : string)
  (version : int)
  (fields : List<string * TypeReference>)
  : UserType.T =
  { tlid = gid ()
    name = { typ = name; version = version }
    definition = customTypeRecord fields }
