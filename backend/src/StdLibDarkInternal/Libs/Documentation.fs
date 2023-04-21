/// StdLib functions for documentation
module StdLibDarkInternal.Libs.Documentation

open System.Threading.Tasks

open Prelude

open LibExecution.RuntimeTypes


let fn = FQFnName.stdlibFnName
let typ = FQTypeName.stdlibTypeName

let incorrectArgs = LibExecution.Errors.incorrectArgs

let types : List<BuiltInType> =
  [ { name = typ "DarkInternal" "DocFunction" 0
      typeParams = []
      definition =
        CustomType.Record(
          { id = 1UL; name = "space"; typ = TString },
          [ { id = 3UL; name = "path"; typ = TString }
            { id = 4UL; name = "modifier"; typ = TString }
            { id = 5UL; name = "timestamp"; typ = TDateTime }
            { id = 6UL; name = "traceID"; typ = TUuid } ]
        )
      description = "A 404 trace" } ]


let fns : List<BuiltInFn> =
  [ { name = fn "DarkInternal" "allFunctions" 0
      typeParams = []
      parameters = []
      returnType =
        TList(TCustomType(FQTypeName.Stdlib(typ "DarkInternal" "DocFunction" 0), []))
      description =
        "Returns a list of DocFunction records, representing the functions available in the standard library. Does not return DarkInternal functions"
      fn =
        (function
        | state, _, [] ->
          let typeName = LibExecution.DvalReprDeveloper.typeName
          state.libraries.stdlibFns
          |> Map.toList
          |> List.filter (fun (key, data) ->
            (not (FQFnName.isInternalFn key)) && data.deprecated = NotDeprecated)
          |> List.map (fun (key, data) ->
            let alist =
              let returnType = typeName data.returnType
              let parameters =
                data.parameters
                |> List.map (fun p ->
                  Dval.record [ ("name", DString p.name)
                                ("type", DString(typeName p.typ)) ])
              [ ("name", DString(FQFnName.toString key))
                ("documentation", DString data.description)
                ("parameters", DList parameters)
                ("returnType", DString returnType) ]
            Dval.record alist)
          |> DList
          |> Ply
        | _ -> incorrectArgs ())
      sqlSpec = NotQueryable
      previewable = Impure
      deprecated = NotDeprecated } ]
