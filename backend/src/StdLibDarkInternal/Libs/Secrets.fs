/// StdLib functions for building Secrets functionality via Dark canvases
module StdLibDarkInternal.Libs.Secrets

open System.Threading.Tasks

open Prelude
open LibExecution.RuntimeTypes
open LibExecution.StdLib.Shortcuts

module Secret = LibBackend.Secret


let modules = [ "DarkInternal"; "Canvas"; "Secret" ]

let typ = typ modules
let fn = fn modules

let types : List<BuiltInType> =
  [ { name = typ "Secret" 0
      declaration =
        { typeParams = []
          definition =
            TypeDeclaration.Record(
              { name = "name"; typ = TString; description = "" },
              [ { name = "value"; typ = TString; description = "" }
                { name = "version"; typ = TInt; description = "" } ]
            ) }
      description = "A secret"
      deprecated = NotDeprecated } ]


let fns : List<BuiltInFn> =
  [ { name = fn "getAll" 0
      typeParams = []
      parameters = [ Param.make "canvasID" TUuid "" ]
      returnType = TList(TCustomType(FQName.BuiltIn(typ "Secret" 0), []))
      description = "Get all secrets in the canvas"
      fn =
        (function
        | _, _, [ DUuid canvasID ] ->
          uply {
            let! secrets = Secret.getCanvasSecrets canvasID
            let typeName = FQName.BuiltIn(typ "Secret" 0)
            return
              secrets
              |> List.map (fun s ->
                Dval.record
                  typeName
                  [ "name", DString s.name
                    "value", DString s.value
                    "version", DInt s.version ])
              |> DList
          }
        | _ -> incorrectArgs ())
      sqlSpec = NotQueryable
      previewable = Impure
      deprecated = NotDeprecated }


    { name = fn "delete" 0
      typeParams = []
      parameters =
        [ Param.make "canvasID" TUuid ""
          Param.make "name" TString ""
          Param.make "version" TInt "" ]
      returnType = TUnit
      description = "Delete a secret"
      fn =
        (function
        | _, _, [ DUuid canvasID; DString name; DInt version ] ->
          uply {
            do! Secret.delete canvasID name (int version)
            return DUnit
          }
        | _ -> incorrectArgs ())
      sqlSpec = NotQueryable
      previewable = Impure
      deprecated = NotDeprecated }


    { name = fn "insert" 0
      typeParams = []
      parameters =
        [ Param.make "canvasID" TUuid ""
          Param.make "name" TString ""
          Param.make "value" TString ""
          Param.make "version" TInt "" ]
      returnType = TypeReference.result TUnit TString
      description = "Add a secret"
      fn =
        (function
        | _, _, [ DUuid canvasID; DString name; DString value; DInt version ] ->
          uply {
            try
              do! Secret.insert canvasID name value (int version)
              return Dval.resultOk DUnit
            with _ ->
              return Dval.resultError (DString "Error inserting secret")
          }
        | _ -> incorrectArgs ())
      sqlSpec = NotQueryable
      previewable = Impure
      deprecated = NotDeprecated } ]

let contents = (fns, types)
