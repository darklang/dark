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
  (typeArgs : List<DType>)
  (args : List<Expr>)
  : Expr =
  EApply(gid (), (eStdFnName module_ function_ version), typeArgs, args)

let eFn
  (module_ : string)
  (function_ : string)
  (version : int)
  (typeArgs : List<DType>)
  (args : List<Expr>)
  : Expr =
  eFn' module_ function_ version typeArgs args

let eUserFn
  (function_ : string)
  (typeArgs : List<DType>)
  (args : List<Expr>)
  : Expr =
  EApply(gid (), (eUserFnName function_), typeArgs, args)

let eApply' (target : FnTarget) (typeArgs : List<DType>) (args : List<Expr>) : Expr =
  EApply(gid (), target, typeArgs, args)

let eApply (target : Expr) (typeArgs : List<DType>) (args : List<Expr>) : Expr =
  eApply' (FnTargetExpr target) typeArgs args


let eStr (str : string) : Expr = EString(gid (), [ StringText str ])

let eChar (c : string) : Expr = ECharacter(gid (), c)

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
