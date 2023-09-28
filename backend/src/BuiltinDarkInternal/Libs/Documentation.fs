/// Builtin functions for documentation
module BuiltinDarkInternal.Libs.Documentation

open System.Threading.Tasks

open Prelude

open LibExecution.RuntimeTypes
open LibExecution.Builtin.Shortcuts

module VT = ValueType
module Dval = LibExecution.Dval

let modules = [ "DarkInternal"; "Documentation" ]

let typ = typ modules
let fn = fn modules



let types : List<BuiltInType> =
  [ { name = typ "Function" 0
      declaration =
        { typeParams = []
          definition =
            TypeDeclaration.Record(
              NEList.ofList
                { name = "name"; typ = TString }
                [ { name = "description"; typ = TString }
                  { name = "parameters"
                    typ =
                      TList(TCustomType(Ok(FQName.BuiltIn(typ "Parameter" 0)), [])) }
                  { name = "returnType"; typ = TString } ]
            ) }
      deprecated = NotDeprecated
      description = "A Darklang builtin function" }
    { name = typ "Parameter" 0
      declaration =
        { typeParams = []
          definition =
            TypeDeclaration.Record(
              NEList.ofList
                { name = "name"; typ = TString }
                [ { name = "type"; typ = TString } ]
            ) }
      deprecated = NotDeprecated
      description = "A function parameter" } ]

let constants : List<BuiltInConstant> = []

let fns : List<BuiltInFn> =
  [ { name = fn "list" 0
      typeParams = []
      parameters = [ Param.make "unit" TUnit "" ]
      returnType = TList(TCustomType(Ok(FQName.BuiltIn(typ "Function" 0)), []))
      description =
        "Returns a list of Function records, representing the functions available in the standard library. Does not return DarkInternal functions"
      fn =
        (function
        | state, _, [ DUnit ] ->
          uply {
            let typeNameToStr = LibExecution.DvalReprDeveloper.typeName

            let fnParamTypeName = TypeName.fqBuiltIn modules "Parameter" 0
            let fnTypeName = TypeName.fqBuiltIn modules "Function" 0

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
                    |> Dval.list VT.unknownTODO

                  let fields =
                    [ "name", DString(FnName.builtinToString key)
                      "description", DString data.description
                      "parameters", parameters
                      "returnType", DString(typeNameToStr data.returnType) ]

                  return DRecord(fnTypeName, fnTypeName, [], Map fields)
                })

            return Dval.list VT.unknownTODO fns
          }
        | _ -> incorrectArgs ())
      sqlSpec = NotQueryable
      previewable = Impure
      deprecated = NotDeprecated } ]

let contents = (fns, types, constants)
