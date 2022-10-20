/// API endpoints to manage Tunnels
module ApiServer.Tunnels

open System.Threading.Tasks
open FSharp.Control.Tasks
open Microsoft.AspNetCore.Http

open Prelude
open Tablecloth
open Http

module CTApi = ClientTypes.Api

module Telemetry = LibService.Telemetry

module Register =

  /// API endpoint to set or remove a tunnel
  let register (ctx : HttpContext) : Task<CTApi.Tunnels.Register.Response> =
    task {
      use t = startTimer "read-api" ctx
      t.next "read-api"
      let userInfo = loadUserInfo ctx
      let! p = ctx.ReadVanillaJsonAsync<CTApi.Tunnels.Register.Request>()
      Telemetry.addTags [ "tunnel", p.tunnelHost ]

      t.next "save-tunnel"
      let valid =
        match p.tunnelHost with
        | None -> true // deleted
        | Some host -> LibBackend.Account.validateTunnelHost host

      if valid then
        do! LibBackend.Account.setTunnelHostFor userInfo.id p.tunnelHost
        return { success = true }
      else
        return { success = false }
    }
