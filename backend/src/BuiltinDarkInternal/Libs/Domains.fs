/// StdLib functions for building Dark functionality via Dark canvases
module BuiltinDarkInternal.Libs.Domains

open System.Threading.Tasks

open Prelude
open LibExecution.RuntimeTypes
open LibExecution.Builtin.Shortcuts

module Canvas = LibCloud.Canvas

let modules = [ "DarkInternal"; "Canvas"; "Domain" ]

let typ = typ modules
let fn = fn modules


let types : List<BuiltInType> = []
let constants : List<BuiltInConstant> = []

let fns : List<BuiltInFn> =
  [ { name = fn "get" 0
      typeParams = []
      parameters = [ Param.make "canvasID" TUuid "" ]
      returnType = TList TString
      description = "Returns the domain for a canvas if it exists"
      fn =
        (function
        | _, _, [ DUuid canvasID ] ->
          uply {
            let! name = Canvas.domainsForCanvasID canvasID
            return name |> List.map DString |> DList
          }
        | _ -> incorrectArgs ())
      sqlSpec = NotQueryable
      previewable = Impure
      deprecated = NotDeprecated }


    { name = fn "toCanvasID" 0
      typeParams = []
      parameters = [ Param.make "domain" TString "" ]
      returnType = TypeReference.result TUuid TString
      description = "Returns the canvasID for a domain if it exists"
      fn =
        (function
        | _, _, [ DString domain ] ->
          uply {
            let! name = Canvas.canvasIDForDomain domain
            match name with
            | Some name -> return Dval.resultOk (DUuid name)
            | None -> return Dval.resultError (DString "Canvas not found")
          }
        | _ -> incorrectArgs ())
      sqlSpec = NotQueryable
      previewable = Impure
      deprecated = NotDeprecated } ]

let contents = (fns, types, constants)
