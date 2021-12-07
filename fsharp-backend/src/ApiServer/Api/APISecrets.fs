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
      let t = Middleware.startTimer "read-api" ctx
      try
        t.next "read-api"
        let canvasInfo = Middleware.loadCanvasInfo ctx
        let! p = ctx.BindModelAsync<Params>()

        t.next "insert-secret"
        do! LibBackend.Secret.insert canvasInfo.id p.secret_name p.secret_value

        t.next "get-secrets"
        let! secrets = LibBackend.Secret.getCanvasSecrets canvasInfo.id

        t.next "write-api"
        let result =
          { secrets =
              List.map
                (fun (s : LibBackend.Secret.Secret) ->
                  { secret_name = s.name; secret_value = s.value })
                secrets }

        t.stop ()

        return result

      with
      | e ->
        let msg = e.ToString()

        // FSTODO: does this error trigger correctly
        if String.includes "duplicate key value violates unique constraint" msg then
          failwith "The secret's name is already defined for this canvas"
        else
          raise e

        t.stop ()
        return { secrets = [] }
    }

module Delete =
  type Secret = { secret_name : string; secret_value : string }
  type Params = { secret_name : string }
  type T = { secrets : List<Secret> }

  let delete (ctx : HttpContext) : Task<T> =
    task {
      let t = Middleware.startTimer "read-api" ctx
      let canvasInfo = Middleware.loadCanvasInfo ctx
      let! p = ctx.BindModelAsync<Params>()

      // CLEANUP: only do this if the secret is not used on the canvas
      t.next "delete-secret"
      do! LibBackend.Secret.delete canvasInfo.id p.secret_name

      t.next "get-secrets"
      let! secrets = LibBackend.Secret.getCanvasSecrets canvasInfo.id

      t.next "write-api"
      let result =
        { secrets =
            List.map
              (fun (s : LibBackend.Secret.Secret) ->
                { secret_name = s.name; secret_value = s.value })
              secrets }

      t.stop ()
      return result
    }
