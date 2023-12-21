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
          let fnsSeq =
            state.builtIns.fns
            |> Map.toSeq
            |> Seq.filter (fun (key, data) ->
              not (FnName.isInternalFn key) && data.deprecated = NotDeprecated)
            |> Seq.map (fun (key, _) -> FnName.builtinToString key)

          let result =
            fnsSeq |> Seq.tryFind (fun name -> name = fnName) |> Option.map DString

          match result with
          | Some name -> Dval.optionSome KTString (name) |> Ply
          | None -> Dval.optionNone KTString |> Ply

        | _ -> incorrectArgs ())
      sqlSpec = NotQueryable
      previewable = Impure
      deprecated = NotDeprecated }

    ]

let contents = (fns, types, constants)
