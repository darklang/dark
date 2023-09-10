/// StdLib functions for documentation
module BuiltinDarkInternal.Libs.Documentation

open System.Threading.Tasks

open Prelude

open LibExecution.RuntimeTypes
open LibExecution.Builtin.Shortcuts

module DvalUtils = LibExecution.DvalUtils

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
      description = "A Darklang stdlib function" }
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
          let typeNameToStr = LibExecution.DvalReprDeveloper.typeName
          state.builtIns.fns
          |> Map.toList
          |> List.filter (fun (key, data) ->
            (not (FnName.isInternalFn key)) && data.deprecated = NotDeprecated)
          |> List.map (fun (key, data) ->
            let typeName = TypeName.fqBuiltIn modules "Function" 0
            let alist =
              let returnType = typeNameToStr data.returnType
              let paramTypeName = TypeName.fqBuiltIn modules "Parameter" 0
              let parameters =
                data.parameters
                |> List.map (fun p ->
                  DvalUtils.record
                    paramTypeName
                    [ ("name", DString p.name)
                      ("type", DString(typeNameToStr p.typ)) ])
              [ ("name", DString(FnName.builtinToString key))
                ("description", DString data.description)
                ("parameters", DvalUtils.list valueTypeTODO parameters)
                ("returnType", DString returnType) ]
            DvalUtils.record typeName alist)
          |> DvalUtils.list valueTypeTODO
          |> Ply
        | _ -> incorrectArgs ())
      sqlSpec = NotQueryable
      previewable = Impure
      deprecated = NotDeprecated } ]

let contents = (fns, types, constants)
