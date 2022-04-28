module BwdServer.Compression

open FSharp.Control.Tasks
open System.Threading.Tasks

open System
open Microsoft.AspNetCore
open Microsoft.AspNetCore.Builder
open Microsoft.AspNetCore.Hosting
open Microsoft.AspNetCore.Http
open Microsoft.AspNetCore.Http.Extensions
open Microsoft.AspNetCore.Http.Abstractions
open Microsoft.Extensions.Logging
open Microsoft.Extensions.Hosting
open Microsoft.Extensions.DependencyInjection
open Microsoft.AspNetCore.ResponseCompression
open Microsoft.Extensions.DependencyInjection.Extensions

type CustomCompressionProvider(services, options) =
  inherit ResponseCompressionProvider(services, options)
  override this.ShouldCompressResponse(ctx : HttpContext) : bool =
    // Compress responses unless they're too small
    let default_ = base.ShouldCompressResponse ctx
    let tooSmall =
      // This was the setting we had in the ocaml nginx
      if ctx.Response.ContentLength.HasValue then
        ctx.Response.ContentLength.Value < 1024
      else
        false
    default_ && not tooSmall

let configureServices (services : IServiceCollection) : IServiceCollection =
  let configureOptions (options : ResponseCompressionOptions) : unit =
    // CLEANUP: This is set to the same values as we used in nginx for the ocaml
    // bwdserver. By default, .net also had a few others: text/javascript,
    // application/xml, text/xml, text/json, application/wasm. They aren't that
    // interesting to us right now.
    options.MimeTypes <-
      [ "text/html"
        "text/plain"
        "text/css"
        "application/javascript"
        "application/json" ]
  services.Configure(configureOptions) |> ignore<IServiceCollection>
  services.TryAddSingleton<IResponseCompressionProvider, CustomCompressionProvider>()
  services

let addToApp (app : IApplicationBuilder) : IApplicationBuilder =
  // FSTODO do we need to do anything to use our custom provider with the default middleware?
  // https://github.com/dotnet/aspnetcore/tree/c85baf8db0c72ae8e68643029d514b2e737c9fae/src/Middleware/ResponseCompression/src
  app.UseResponseCompression()
