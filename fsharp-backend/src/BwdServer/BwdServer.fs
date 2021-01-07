module BwdServer

// This is the webserver for builtwithdark.com. It uses ASP.NET directly,
// instead of a web framework, so we can tuen the exact behaviour of headers
// and such.

(* open Microsoft.AspNetCore.Http *)
open FSharp.Control.Tasks

open System
(* open System.Security.Claims *)
open System.Threading.Tasks
open Microsoft.AspNetCore
open Microsoft.AspNetCore.Builder
open Microsoft.AspNetCore.Hosting
open Microsoft.AspNetCore.Http
open Microsoft.AspNetCore.Http.Extensions
(* open Microsoft.AspNetCore.Http.Features *)
(* open Microsoft.Extensions.Configuration *)
open Microsoft.Extensions.Logging
open Microsoft.Extensions.DependencyInjection
open Prelude
open FSharpx

module PT = LibBackend.ProgramSerialization.ProgramTypes
module RT = LibExecution.RuntimeTypes

// This boilerplate is copied from Giraffe. I elected not to use Giraffe
// because we don't need any of its feature, but the types it uses are very
// nice.
// https://github.com/giraffe-fsharp/Giraffe/blob/9598682f4f68e23217c4199a48f30ca3457b037e/src/Giraffe/Core.fs

type HttpFuncResult = Task<HttpContext option>

type HttpFunc = HttpContext -> HttpFuncResult
type HttpHandler = HttpFunc -> HttpContext -> HttpFuncResult

let compose (handler1 : HttpHandler) (handler2 : HttpHandler) : HttpHandler =
  fun (final : HttpFunc) ->
    let func = final |> handler2 |> handler1

    fun (ctx : HttpContext) ->
      match ctx.Response.HasStarted with
      | true -> final ctx
      | false -> func ctx

let (>=>) = compose

type BwdMiddleware(next : RequestDelegate, handler : HttpHandler) =
  let earlyReturn = Some >> Task.FromResult
  let func : HttpFunc = handler earlyReturn

  member __.Invoke(ctx : HttpContext) =
    task {
      let! result = func ctx
      if (result.IsNone) then return! next.Invoke ctx
    }

// End stuff copied from Giraffe

let recordEventMiddleware : HttpHandler =
  fun next ctx ->
    // FSTODO
    next ctx

let record404Middleware : HttpHandler =
  fun next ctx ->
    // FSTODO
    next ctx

let recordHeapioMiddleware : HttpHandler =
  fun next ctx ->
    // FSTODO
    next ctx

let recordHoneycombMiddleware : HttpHandler =
  fun next ctx ->
    // FSTODO
    next ctx

let sanitizeUrlPath (path : string) : string =
  path
  |> FsRegEx.replace "//+" "/"
  |> String.trimEnd [| '/' |]
  |> fun str -> if str = "" then "/" else str

let normalizeRequest : HttpHandler =
  fun next ctx ->
    ctx.Request.Path <- ctx.Request.Path.Value |> sanitizeUrlPath |> PathString
    next ctx

let canvasNameFromHost (host : string) : Task<Option<string>> =
  task {
    match host.Split [| '.' |] with
    // Route *.darkcustomdomain.com same as we do *.builtwithdark.com - it's
    // just another load balancer
    | [| a; "darkcustomdomain"; "com" |]
    | [| a; "builtwithdark"; "localhost" |]
    | [| a; "builtwithdark"; "com" |] -> return Some a
    | [| "builtwithdark"; "localhost" |]
    | [| "builtwithdark"; "com" |] -> return Some "builtwithdark"
    | _ -> return! LibBackend.Canvas.canvasNameFromCustomDomain host
  }

let runDarkHandler : HttpHandler =
  fun (next : HttpFunc) (ctx : HttpContext) ->
    task {
      let addHeader (ctx : HttpContext) (name : string) (value : string) : unit =
        ctx.Response.Headers.Add(
          name,
          Microsoft.Extensions.Primitives.StringValues([| value |])
        )

      let e500 (msg : string) =
        task {
          let bytes = System.Text.Encoding.UTF8.GetBytes msg
          ctx.Response.StatusCode <- 500
          addHeader ctx "server" "darklang"
          if bytes.Length > 0 then addHeader ctx "Content-Type" "text/plain"
          addHeader ctx "Content-Length" (bytes.Length.ToString())
          do! ctx.Response.Body.WriteAsync(bytes, 0, bytes.Length)
          return Some ctx
        }

      let requestPath = ctx.Request.Path.Value

      let exprs : Task<List<PT.Toplevel>> =
        task {
          let executionID = gid ()
          let logger = ctx.RequestServices.GetService(typeof<ILogger>) :?> ILogger

          match! canvasNameFromHost ctx.Request.Host.Host with
          | Some canvasName ->
              let ownerName = LibBackend.Canvas.ownerNameFromHost canvasName
              let! userID = LibBackend.Account.userIDForUserName ownerName
              let! canvasID = LibBackend.Canvas.canvasIDForCanvas userID canvasName
              let method = ctx.Request.Method

              return!
                LibBackend.ProgramSerialization.SQL.loadHttpHandlersFromCache
                  canvasName
                  canvasID
                  userID
                  requestPath
                  method
          | None -> return []
        }

      match! exprs with
      | [ PT.TLHandler { spec = PT.Handler.HTTP (route = route)
                         ast = expr
                         tlid = tlid } ] ->
          let ms = new IO.MemoryStream()
          do! ctx.Request.Body.CopyToAsync(ms)
          let url = ctx.Request.GetEncodedUrl()
          let body = ms.ToArray()
          let expr = expr.toRuntimeType ()
          let fns = LibExecution.StdLib.StdLib.fns @ LibBackend.StdLib.StdLib.fns
          let vars = LibExecution.Http.routeInputVars route requestPath

          match vars with
          | None ->
              return!
                e500
                  $"The request ({requestPath}) does not match the route ({route})"
          | Some vars ->
              let symtable = Map.ofList vars

              let! result =
                LibExecution.Execution.runHttp tlid url symtable body fns expr

              printfn $"result of runHttp is {result}"

              match result with
              | RT.DHttpResponse (status, headers, RT.DBytes body) ->
                  ctx.Response.StatusCode <- status
                  List.iter (fun (k, v) -> addHeader ctx k v) headers
                  do! ctx.Response.Body.WriteAsync(body, 0, body.Length)
                  return! next ctx
              // TODO: maybe not the right thing, but this is what the OCaml does
              // FSTODO: move this to LibExecution so it can be available in the client
              | RT.DFakeVal (RT.DErrorRail (RT.DResult (Error _)))
              | RT.DFakeVal (RT.DErrorRail (RT.DOption None)) ->
                  ctx.Response.StatusCode <- 404
                  addHeader ctx "server" "darklang"
                  return Some ctx
              | RT.DFakeVal (RT.DIncomplete _) ->
                  return!
                    e500
                      "Error calling server code: Handler returned an \
                                incomplete result. Please inform the owner of this \
                                site that their code is broken."
              | other ->
                  printfn $"Not a HTTP response: {other}"
                  return! e500 "body is not a HttpResponse"
      | [] ->
          ctx.Response.StatusCode <- 404
          addHeader ctx "server" "darklang"
          return Some ctx
      | _ -> return! e500 "More than one handler found for this URL"
    }

let webApp : HttpHandler =
  // FSTODO use giraffe's builtin httpsRedirect
  normalizeRequest
  >=> recordEventMiddleware
  >=> record404Middleware
  >=> recordHeapioMiddleware
  >=> recordHoneycombMiddleware
  >=> runDarkHandler

let configureApp (app : IApplicationBuilder) =
  app.UseDeveloperExceptionPage().UseMiddleware<BwdMiddleware>(webApp) |> ignore

let configureLogging (builder : ILoggingBuilder) =
  let filter (l : LogLevel) : bool = true

  // Configure the logging factory
  builder
    .AddFilter(filter) // Optional filter
    .AddConsole() // Set up the Console logger
    .AddDebug() // Set up the Debug logger
  // Add additional loggers if wanted...
  |> ignore

let configureServices (services : IServiceCollection) = ()

let webserver (port : int) =
  WebHost.CreateDefaultBuilder()
  |> fun wh -> wh.UseKestrel(fun kestrel -> kestrel.AddServerHeader <- false)
  |> fun wh -> wh.ConfigureServices(configureServices)
  |> fun wh -> wh.Configure(configureApp)
  |> fun wh -> wh.ConfigureLogging(configureLogging)
  |> fun wh -> wh.UseUrls($"http://*:{port}")
  |> fun wh -> wh.Build()


[<EntryPoint>]
let main _ =
  LibBackend.ProgramSerialization.OCamlInterop.Binary.init ()
  (webserver 9001).Run()
  0
