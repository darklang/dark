/// StdLib functions for building Secrets functionality via Dark canvases
module StdLibDarkInternal.Libs.Secrets

open System.Threading.Tasks

open Prelude
open LibExecution.RuntimeTypes
open LibExecution.StdLib.Shortcuts

module Secret = LibBackend.Secret


let types : List<BuiltInType> = []


let fns : List<BuiltInFn> =
  [ { name = fn "DarkInternal" "getSecrets" 0
      typeParams = []
      parameters = [ Param.make "canvasID" TUuid "" ]
      returnType = TDict TString
      description = "Get list of secrets in the canvas"
      fn =
        (function
        | _, _, [ DUuid canvasID ] ->
          uply {
            let! secrets = Secret.getCanvasSecrets canvasID
            return
              secrets
              |> List.map (fun s ->
                DTuple(DString s.name, DString s.value, [ DInt s.version ]))
              |> DList
          }
        | _ -> incorrectArgs ())
      sqlSpec = NotQueryable
      previewable = Impure
      deprecated = NotDeprecated }


    { name = fn "DarkInternal" "deleteSecret" 0
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


    { name = fn "DarkInternal" "insertSecret" 0
      typeParams = []
      parameters =
        [ Param.make "canvasID" TUuid ""
          Param.make "name" TString ""
          Param.make "value" TString ""
          Param.make "version" TString "" ]
      returnType = TResult(TUnit, TString)
      description = "Add a secret"
      fn =
        (function
        | _, _, [ DUuid canvasID; DString name; DString value; DInt version ] ->
          uply {
            try
              do! Secret.insert canvasID name value (int version)
              return DResult(Ok DUnit)
            with
            | _ -> return DResult(Error(DString "Error inserting secret"))
          }
        | _ -> incorrectArgs ())
      sqlSpec = NotQueryable
      previewable = Impure
      deprecated = NotDeprecated } ]

let contents = (fns, types)
