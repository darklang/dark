/// Builtin functions for building Secrets functionality via Dark canvases
module BuiltinDarkInternal.Libs.Secrets

open System.Threading.Tasks

open Prelude
open LibExecution.RuntimeTypes
open LibExecution.Builtin.Shortcuts

module VT = ValueType
module Dval = LibExecution.Dval
module Secret = LibCloud.Secret


let packageSecretType (addlModules : List<string>) (name : string) =
  FQTypeName.fqPackage "Darklang" ("Internal" :: "Canvas" :: addlModules) name

let fns : List<BuiltInFn> =
  [ { name = fn "darkInternalCanvasSecretGetAll" 0
      typeParams = []
      parameters = [ Param.make "canvasID" TUuid "" ]
      returnType = TList(TCustomType(Ok(packageSecretType [] "Secret"), []))
      description = "Get all secrets in the canvas"
      fn =
        (function
        | _, _, [ DUuid canvasID ] ->
          uply {
            let! secrets = Secret.getCanvasSecrets canvasID
            let typeName = packageSecretType [] "Secret"

            return
              secrets
              |> List.map (fun s ->
                let fields =
                  [ "name", DString s.name
                    "value", DString s.value
                    "version", DInt64 s.version ]
                DRecord(typeName, typeName, [], Map fields))
              |> Dval.list (KTCustomType(typeName, []))
          }
        | _ -> incorrectArgs ())
      sqlSpec = NotQueryable
      previewable = Impure
      deprecated = NotDeprecated }


    { name = fn "darkInternalCanvasSecretDelete" 0
      typeParams = []
      parameters =
        [ Param.make "canvasID" TUuid ""
          Param.make "name" TString ""
          Param.make "version" TInt64 "" ]
      returnType = TUnit
      description = "Delete a secret"
      fn =
        (function
        | _, _, [ DUuid canvasID; DString name; DInt64 version ] ->
          uply {
            do! Secret.delete canvasID name (int version)
            return DUnit
          }
        | _ -> incorrectArgs ())
      sqlSpec = NotQueryable
      previewable = Impure
      deprecated = NotDeprecated }


    { name = fn "darkInternalCanvasSecretInsert" 0
      typeParams = []
      parameters =
        [ Param.make "canvasID" TUuid ""
          Param.make "name" TString ""
          Param.make "value" TString ""
          Param.make "version" TInt64 "" ]
      returnType = TypeReference.result TUnit TString
      description = "Add a secret"
      fn =
        let resultOk = Dval.resultOk KTUnit KTString
        let resultError = Dval.resultError KTUnit KTString
        (function
        | _, _, [ DUuid canvasID; DString name; DString value; DInt64 version ] ->
          uply {
            try
              do! Secret.insert canvasID name value (int version)
              return resultOk DUnit
            with _ ->
              return resultError (DString "Error inserting secret")
          }
        | _ -> incorrectArgs ())
      sqlSpec = NotQueryable
      previewable = Impure
      deprecated = NotDeprecated } ]

let builtins = LibExecution.Builtin.make [] fns
