/// API endpoints to manage Secrets
module ApiServer.Secrets

open System.Threading.Tasks
open FSharp.Control.Tasks
open Microsoft.AspNetCore.Http

open Prelude
open Tablecloth
open Http

module PT = LibExecution.ProgramTypes
module OT = LibExecution.OCamlTypes
module ORT = LibExecution.OCamlTypes.RuntimeT
module AT = LibExecution.AnalysisTypes
module Convert = LibExecution.OCamlTypes.Convert
module Telemetry = LibService.Telemetry

open LibService.Exception

module Insert =

  type Secret = { secret_name : string; secret_value : string }

  type Params = Secret

  type T = { secrets : List<Secret> }

  /// API endpoint to insert a Secret within a canvas
  let insert (ctx : HttpContext) : Task<T> =
    task {
      use t = startTimer "read-api" ctx
      try
        t.next "read-api"
        let canvasInfo = loadCanvasInfo ctx
        let! p = ctx.ReadJsonAsync<Params>()
        Telemetry.addTags [ "secret_name", p.secret_name ]

        t.next "insert-secret"
        do! LibBackend.Secret.insert canvasInfo.id p.secret_name p.secret_value

        t.next "get-secrets"
        let! secrets = LibBackend.Secret.getCanvasSecrets canvasInfo.id

        t.next "write-api"
        return
          { secrets =
              List.map
                (fun (s : PT.Secret.T) ->
                  { secret_name = s.name; secret_value = s.value })
                secrets }
      with
      | e ->
        let msg = e.ToString()

        if String.includes "duplicate key value violates unique constraint" msg then
          Exception.raiseEditor
            "The secret's name is already defined for this canvas"
        else
          e.Reraise()

        return { secrets = [] }
    }

module Delete =
  type Secret = { secret_name : string; secret_value : string }

  type Params = { secret_name : string }

  type T = { secrets : List<Secret> }

  /// API endpoint to delete a specific Secret
  let delete (ctx : HttpContext) : Task<T> =
    task {
      use t = startTimer "read-api" ctx
      let canvasInfo = loadCanvasInfo ctx
      let! p = ctx.ReadJsonAsync<Params>()
      Telemetry.addTags [ "secret_name", p.secret_name ]

      // TODO: only do this if the secret is not used on the canvas
      t.next "delete-secret"
      do! LibBackend.Secret.delete canvasInfo.id p.secret_name

      t.next "get-secrets"
      let! secrets = LibBackend.Secret.getCanvasSecrets canvasInfo.id

      t.next "write-api"
      return
        { secrets =
            List.map
              (fun (s : PT.Secret.T) ->
                { secret_name = s.name; secret_value = s.value })
              secrets }
    }
