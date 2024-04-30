module BuiltinExecution.Libs.LanguageTools

open Prelude
open LibExecution.RuntimeTypes
open LibExecution.Builtin.Shortcuts

module VT = ValueType
module Dval = LibExecution.Dval
module Interpreter = LibExecution.Interpreter
module TypeChecker = LibExecution.TypeChecker
module DvalReprDeveloper = LibExecution.DvalReprDeveloper


let typ
  (addlModules : List<string>)
  (name : string)
  (version : int)
  : FQTypeName.FQTypeName =
  FQTypeName.fqPackage "Darklang" ([ "LanguageTools" ] @ addlModules) name version


let typeNameToStr = DvalReprDeveloper.typeName

let fns : List<BuiltInFn> =
  [ { name = fn "languageToolsAllBuiltinConstants" 0
      typeParams = []
      parameters = [ Param.make "unit" TUnit "" ]
      returnType = TList(TCustomType(Ok(typ [] "BuiltinConstant" 0), []))
      description =
        "Returns a list of the Builtin constants (usually not to be accessed directly)."
      fn =
        (function
        | state, _, [ DUnit ] ->
          let constTypeName = typ [] "BuiltinConstant" 0

          let consts =
            state.builtins.constants
            |> Map.toList
            |> List.map (fun (key, data) ->
              let fields =
                [ "name", DString(FQConstantName.builtinToString key)
                  "description", DString data.description
                  "returnType", DString(typeNameToStr data.typ) ]

              DRecord(constTypeName, constTypeName, [], Map fields))

          DList(VT.customType constTypeName [], consts) |> Ply
        | _ -> incorrectArgs ())
      sqlSpec = NotQueryable
      previewable = Impure
      deprecated = NotDeprecated }


    { name = fn "languageToolsAllBuiltinFns" 0
      typeParams = []
      parameters = [ Param.make "unit" TUnit "" ]
      returnType = TList(TCustomType(Ok(typ [] "BuiltinFunction" 0), []))
      description =
        "Returns a list of the Builtin functions (usually not to be accessed directly)."
      fn =
        (function
        | state, _, [ DUnit ] ->

          let fnParamTypeName = typ [] "BuiltinFunctionParameter" 0
          let fnTypeName = typ [] "BuiltinFunction" 0

          let fns =
            state.builtins.fns
            |> Map.toList
            |> List.map (fun (key, data) ->
              let parameters =
                data.parameters
                |> List.map (fun p ->
                  let fields =
                    [ "name", DString p.name
                      "type", DString(typeNameToStr p.typ) ]
                  DRecord(fnParamTypeName, fnParamTypeName, [], Map fields))
                |> Dval.list (KTCustomType(fnParamTypeName, []))

              let fields =
                [ "name", DString(FQFnName.builtinToString key)
                  "description", DString data.description
                  "parameters", parameters
                  "returnType", DString(typeNameToStr data.returnType) ]

              DRecord(fnTypeName, fnTypeName, [], Map fields))

          DList(VT.customType fnTypeName [], fns) |> Ply
        | _ -> incorrectArgs ())
      sqlSpec = NotQueryable
      previewable = Impure
      deprecated = NotDeprecated }


    // This exists because the above-defined fn returns a big list,
    // and List.filter fails on such a big list due to stack overflow
    // (a bug to fix...) TODO
    { name = fn "languageToolsBuiltinFnExists" 0
      typeParams = []
      parameters = [ Param.make "name" TString ""; Param.make "version" TInt64 "" ]
      returnType = TBool
      description = "Returns whether or not some builtin fn exists"
      fn =
        (function
        | state, _, [ DString name; DInt64 version ] ->
          let name : FQFnName.Builtin = { name = name; version = int version }

          let found = state.builtins.fns |> Map.find name |> Option.isSome

          DBool found |> Ply
        | _ -> incorrectArgs ())
      sqlSpec = NotQueryable
      previewable = Impure
      deprecated = NotDeprecated } ]


let builtins = LibExecution.Builtin.make [] fns
