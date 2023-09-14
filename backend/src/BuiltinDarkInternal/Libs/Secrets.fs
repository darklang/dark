/// StdLib functions for building Secrets functionality via Dark canvases
module BuiltinDarkInternal.Libs.Secrets

open System.Threading.Tasks

open Prelude
open LibExecution.RuntimeTypes
open LibExecution.Builtin.Shortcuts

module Dval = LibExecution.Dval
module VT = ValueType
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
                Dval.record
                  typeName
                  [ "name", DString s.name
                    "value", DString s.value
                    "version", DInt s.version ])
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
        (function
        | _, _, [ DUuid canvasID; DString name; DString value; DInt version ] ->
          let okType = VT.unit
          let errType = VT.string
          uply {
            try
              do! Secret.insert canvasID name value (int version)
              return Dval.resultOk okType errType DUnit
            with _ ->
              return
                Dval.resultError okType errType (DString "Error inserting secret")
          }
        | _ -> incorrectArgs ())
      sqlSpec = NotQueryable
      previewable = Impure
      deprecated = NotDeprecated } ]

let constants : List<BuiltInConstant> = []

let contents = (fns, types, constants)
