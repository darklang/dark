/// API endpoints to manage Secrets
module ApiServer.Secrets

open System.Threading.Tasks
open FSharp.Control.Tasks
open Microsoft.AspNetCore.Http

open Prelude
open Tablecloth
open Http

module PT = LibExecution.ProgramTypes
module AT = LibExecution.AnalysisTypes
module CTApi = ClientTypes.Api
module Telemetry = LibService.Telemetry

open LibService.Exception

module InsertV1 =
  /// API endpoint to insert a Secret within a canvas
  let insert (ctx : HttpContext) : Task<CTApi.Secrets.InsertV1.Response> =
    task {
      use t = startTimer "read-api" ctx
      try
        t.next "read-api"
        let canvasInfo = loadCanvasInfo ctx
        let! p = ctx.ReadVanillaJsonAsync<CTApi.Secrets.InsertV1.Request>()
        Telemetry.addTags [ "secretName", p.name ]

        t.next "insert-secret"
        do! LibBackend.Secret.insert canvasInfo.id p.name p.value

        t.next "get-secrets"
        let! secrets = LibBackend.Secret.getCanvasSecrets canvasInfo.id

        t.next "write-api"
        return
          { secrets =
              List.map
                (fun (s : PT.Secret.T) -> { name = s.name; value = s.value })
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

module DeleteV1 =
  /// API endpoint to delete a specific Secret
  let delete (ctx : HttpContext) : Task<CTApi.Secrets.DeleteV1.Response> =
    task {
      use t = startTimer "read-api" ctx
      let canvasInfo = loadCanvasInfo ctx
      let! p = ctx.ReadVanillaJsonAsync<CTApi.Secrets.DeleteV1.Request>()
      Telemetry.addTags [ "secretName", p.name ]

      // TODO: only do this if the secret is not used on the canvas
      t.next "delete-secret"
      do! LibBackend.Secret.delete canvasInfo.id p.name

      t.next "get-secrets"
      let! secrets = LibBackend.Secret.getCanvasSecrets canvasInfo.id

      t.next "write-api"
      return
        { secrets = secrets |> List.map (fun s -> { name = s.name; value = s.value }) }
    }
