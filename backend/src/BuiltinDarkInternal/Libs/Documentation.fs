/// Builtin functions for documentation
module BuiltinDarkInternal.Libs.Documentation

open System.Threading.Tasks

open Prelude

open LibExecution.RuntimeTypes
open LibExecution.Builtin.Shortcuts

module VT = ValueType
module Dval = LibExecution.Dval

let fn = fn [ "DarkInternal"; "Documentation" ]
let packageType (addlModules : List<string>) (name : string) =
  TypeName.fqPackage
    "Darklang"
    ([ "Internal"; "Documentation" ] @ addlModules)
    name
    0

let types : List<BuiltInType> = []

let constants : List<BuiltInConstant> = []

let fns : List<BuiltInFn> =
  [ { name = fn "list" 0
      typeParams = []
      parameters = [ Param.make "unit" TUnit "" ]
      returnType = TList(TCustomType(Ok(packageType [] "BuiltinFunction"), []))
      description =
        "Returns a list of Function records, representing the functions available in the standard library. Does not return DarkInternal functions"
      fn =
        (function
        | state, _, [ DUnit ] ->
          uply {
            let typeNameToStr = LibExecution.DvalReprDeveloper.typeName

            let fnParamTypeName = packageType [] "BuiltinFunctionParameter"
            let fnTypeName = packageType [] "BuiltinFunction"

            let! fns =
              state.builtIns.fns
              |> Map.toList
              |> List.filter (fun (key, data) ->
                (not (FnName.isInternalFn key)) && data.deprecated = NotDeprecated)
              |> Ply.List.mapSequentially (fun (key, data) ->
                uply {
                  let parameters =
                    data.parameters
                    |> List.map (fun p ->
                      let fields =
                        [ "name", DString p.name
                          "type", DString(typeNameToStr p.typ) ]
                      DRecord(fnParamTypeName, fnParamTypeName, [], Map fields))
                    |> Dval.list (KTCustomType(fnParamTypeName, []))

                  let fields =
                    [ "name", DString(FnName.builtinToString key)
                      "description", DString data.description
                      "parameters", parameters
                      "returnType", DString(typeNameToStr data.returnType) ]

                  return DRecord(fnTypeName, fnTypeName, [], Map fields)
                })

            return DList(VT.customType fnTypeName [], fns)
          }
        | _ -> incorrectArgs ())
      sqlSpec = NotQueryable
      previewable = Impure
      deprecated = NotDeprecated } ]

let contents = (fns, types, constants)
