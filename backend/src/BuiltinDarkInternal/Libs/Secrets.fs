/// Builtin functions for building Secrets functionality via Dark canvases
module BuiltinDarkInternal.Libs.Secrets

open System.Threading.Tasks

open Prelude
open LibExecution.RuntimeTypes
open LibExecution.Builtin.Shortcuts

module VT = ValueType
module Dval = LibExecution.Dval
module Secret = LibCloud.Secret


let modules = [ "DarkInternal"; "Canvas"; "Secret" ]

let typ = typ modules
let fn = fn modules

let types : List<BuiltInType> =
  [ { name = typ "Secret" 0
      declaration =
        { typeParams = []
          definition =
            TypeDeclaration.Record(
              NEList.ofList
                { name = "name"; typ = TString }
                [ { name = "value"; typ = TString }
                  { name = "version"; typ = TInt } ]
            ) }
      description = "A secret"
      deprecated = NotDeprecated } ]


let fns : List<BuiltInFn> =
  [ { name = fn "getAll" 0
      typeParams = []
      parameters = [ Param.make "canvasID" TUuid "" ]
      returnType = TList(TCustomType(Ok(FQName.BuiltIn(typ "Secret" 0)), []))
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
                let fields =
                  [ "name", DString s.name
                    "value", DString s.value
                    "version", DInt s.version ]
                DRecord(typeName, typeName, [], Map fields))
              |> Dval.list (ValueType.Known(KTCustomType(typeName, [])))
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
        let resultOk = Dval.resultOk VT.unit VT.string
        let resultError = Dval.resultError VT.unit VT.string
        (function
        | _, _, [ DUuid canvasID; DString name; DString value; DInt version ] ->
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

let constants : List<BuiltInConstant> = []

let contents = (fns, types, constants)
