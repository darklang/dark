module ApiServer.Secrets

// API endpoints for Secrets

open Microsoft.AspNetCore.Http
open Giraffe

open System.Threading.Tasks
open FSharp.Control.Tasks

open Prelude
open Tablecloth

module PT = LibExecution.ProgramTypes
module OT = LibExecution.OCamlTypes
module ORT = LibExecution.OCamlTypes.RuntimeT
module AT = LibExecution.AnalysisTypes
module Convert = LibExecution.OCamlTypes.Convert

module Insert =
  type Secret = { secret_name : string; secret_value : string }
  type Params = Secret
  type T = { secrets : List<Secret> }

  let insert (ctx : HttpContext) : Task<T> =
    task {
      try
        let t = Middleware.startTimer ctx
        let canvasInfo = Middleware.loadCanvasInfo ctx
        let! p = ctx.BindModelAsync<Params>()
        t "read-api"

        do! LibBackend.Secret.insert canvasInfo.id p.secret_name p.secret_value
        t "insert-secret"

        let! secrets = LibBackend.Secret.getCanvasSecrets canvasInfo.id
        t "get-secrets"

        let result =
          { secrets =
              List.map
                (fun (s : LibBackend.Secret.Secret) ->
                  { secret_name = s.name; secret_value = s.value })
                secrets }

        t "write-api"

        return result

      with e ->
        let msg = e.ToString()

        // FSTODO: does this error trigger correctly
        if String.includes "duplicate key value violates unique constraint" msg then
          failwith "The secret's name is already defined for this canvas"
        else
          raise e

        return { secrets = [] }
    }

module Delete =
  type Secret = { secret_name : string; secret_value : string }
  type Params = { secret_name : string }
  type T = { secrets : List<Secret> }

  let delete (ctx : HttpContext) : Task<T> =
    task {
      let t = Middleware.startTimer ctx
      let canvasInfo = Middleware.loadCanvasInfo ctx
      let! p = ctx.BindModelAsync<Params>()
      t "read-api"

      // CLEANUP: only do this if the secret is not used on the canvas
      do! LibBackend.Secret.delete canvasInfo.id p.secret_name
      t "delete-secret"

      let! secrets = LibBackend.Secret.getCanvasSecrets canvasInfo.id
      t "get-secrets"

      let result =
        { secrets =
            List.map
              (fun (s : LibBackend.Secret.Secret) ->
                { secret_name = s.name; secret_value = s.value })
              secrets }

      t "write-api"

      return result
    }
