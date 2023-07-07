/// StdLib functions for documentation
module StdLibDarkInternal.Libs.Documentation

open System.Threading.Tasks

open Prelude

open LibExecution.RuntimeTypes
open LibExecution.StdLib.Shortcuts

let modules = [ "DarkInternal"; "Documentation" ]

let typ = typ modules
let fn = fn modules



let types : List<BuiltInType> =
  [ { name = typ "Function" 0
      declaration =
        { typeParams = []
          definition =
            TypeDeclaration.Record(
              { name = "name"; typ = TString; description = "" },
              [ { name = "description"; typ = TString; description = "" }
                { name = "parameters"; typ = TList(TString); description = "" }
                { name = "returnType"; typ = TString; description = "" } ]
            ) }
      deprecated = NotDeprecated
      description = "A Darklang stdlib function" }
    { name = typ "Parameter" 0
      declaration =
        { typeParams = []
          definition =
            TypeDeclaration.Record(
              { name = "name"; typ = TString; description = "" },
              [ { name = "type"; typ = TString; description = "" } ]
            ) }
      deprecated = NotDeprecated
      description = "A function parameter" } ]


let fns : List<BuiltInFn> =
  [ { name = fn "list" 0
      typeParams = []
      parameters = [ Param.make "unit" TUnit "" ]
      returnType = TList(TCustomType(FQName.BuiltIn(typ "Function" 0), []))
      description =
        "Returns a list of Function records, representing the functions available in the standard library. Does not return DarkInternal functions"
      fn =
        (function
        | state, _, [ DUnit ] ->
          let typeNameToStr = LibExecution.DvalReprDeveloper.typeName
          state.libraries.builtInFns
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
                  Dval.record
                    paramTypeName
                    [ ("name", DString p.name)
                      ("type", DString(typeNameToStr p.typ)) ])
              [ ("name", DString(FnName.builtinToString key))
                ("documentation", DString data.description)
                ("parameters", DList parameters)
                ("returnType", DString returnType) ]
            Dval.record typeName alist)
          |> DList
          |> Ply
        | _ -> incorrectArgs ())
      sqlSpec = NotQueryable
      previewable = Impure
      deprecated = NotDeprecated } ]

let contents = (fns, types)
