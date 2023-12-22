module BuiltinExecution.Libs.DarkEditor

open Prelude
open LibExecution.RuntimeTypes
open LibExecution.Builtin.Shortcuts

module VT = ValueType
module Dval = LibExecution.Dval
module Interpreter = LibExecution.Interpreter
module TypeChecker = LibExecution.TypeChecker
module DvalReprDeveloper = LibExecution.DvalReprDeveloper



let modules = [ "DarkEditor" ]
let fn = fn modules
let constant = constant modules

let types : List<BuiltInType> = []
let constants : List<BuiltInConstant> = []

let typ
  (addlModules : List<string>)
  (name : string)
  (version : int)
  : TypeName.TypeName =
  TypeName.fqPackage "Darklang" ([ "DarkEditor" ] @ addlModules) name version


let fns : List<BuiltInFn> =
  [ { name = fn "builtinExists" 0
      typeParams = []
      parameters = [ Param.make "fnName" TString "" ]
      returnType = TypeReference.option TString
      description =
        "Returns the Some name of the builtin function if it exists, otherwise None."
      fn =
        (function
        | state, _, [ DString fnName ] ->
          let fnsMap =
            state.builtIns.fns

          let exists = Map.exists (fun key _ -> FnName.builtinToString key = fnName) fnsMap
          if exists then Dval.optionSome KTString (DString fnName) |> Ply
          else Dval.optionNone KTString |> Ply

        | _ -> incorrectArgs ())
      sqlSpec = NotQueryable
      previewable = Impure
      deprecated = NotDeprecated }

    ]

let contents = (fns, types, constants)
