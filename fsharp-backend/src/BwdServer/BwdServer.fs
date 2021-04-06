module BwdServer

// This is the webserver for builtwithdark.com. It uses ASP.NET directly,
// instead of a web framework, so we can tuen the exact behaviour of headers
// and such.

(* open Microsoft.AspNetCore.Http *)
open FSharp.Control.Tasks
open System.Threading.Tasks

open System
(* open System.Security.Claims *)
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
open Tablecloth
open FSharpx

module PT = LibBackend.ProgramTypes
module RT = LibExecution.RuntimeTypes
module RealExe = LibBackend.RealExecution
module Interpreter = LibExecution.Interpreter

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

let canvasNameFromHost (host : string) : Task<Option<CanvasName.T>> =
  task {
    match host.Split [| '.' |] with
    // Route *.darkcustomdomain.com same as we do *.builtwithdark.com - it's
    // just another load balancer
    | [| a; "darkcustomdomain"; "com" |]
    | [| a; "builtwithdark"; "localhost" |]
    | [| a; "builtwithdark"; "com" |] -> return Some(CanvasName.create a)
    | [| "builtwithdark"; "localhost" |]
    | [| "builtwithdark"; "com" |] -> return Some(CanvasName.create "builtwithdark")
    | _ -> return! LibBackend.Canvas.canvasNameFromCustomDomain host
  }

let extractErrorRail (result : RT.Dval) : RT.Dval =
  match result with
  | RT.DErrorRail (RT.DOption None)
  | RT.DErrorRail (RT.DResult (Error _)) ->
      (RT.DHttpResponse(
        RT.Response(404, [ "Content-length", "9"; "server", "darklang" ]),
        RT.DBytes(toBytes "Not found")
      ))
  | RT.DErrorRail _ ->
      (RT.DHttpResponse(
        RT.Response(500, [ "Content-length", "32"; "server", "darklang" ]),
        RT.DBytes(toBytes "Invalid conversion from errorrail")
      ))
  | dv -> dv


let runHttp
  (c : LibBackend.Canvas.T)
  (tlid : tlid)
  (traceID : LibExecution.AnalysisTypes.TraceID)
  (url : string)
  (body : byte [])
  (inputVars : RT.Symtable)
  (expr : RT.Expr)
  : Task<RT.Dval> =
  task {
    let program = LibBackend.Canvas.toProgram c
    let! state = RealExe.createState traceID tlid program
    let symtable = Interpreter.withGlobals state inputVars

    let! result =
      Interpreter.applyFnVal
        state
        (RT.Expr.toID expr)
        (RT.FnName(RT.FQFnName.stdlibFqName "Http" "middleware" 0))
        [ RT.DStr url
          RT.DBytes body
          RT.DObj Map.empty
          RT.DFnVal(
            RT.Lambda
              { parameters = [ gid (), "request" ]
                symtable = symtable
                body = expr }
          ) ]
        RT.NotInPipe
        RT.NoRail
      |> TaskOrValue.toTask

    return extractErrorRail result

  }

let runDarkHandler : HttpHandler =
  fun (next : HttpFunc) (ctx : HttpContext) ->
    task {
      let addHeader (ctx : HttpContext) (name : string) (value : string) : unit =
        ctx.Response.Headers.Add(
          name,
          Microsoft.Extensions.Primitives.StringValues([| value |])
        )

      let msg (code : int) (msg : string) =
        task {
          let bytes = System.Text.Encoding.UTF8.GetBytes msg
          ctx.Response.StatusCode <- code
          addHeader ctx "server" "darklang"
          if bytes.Length > 0 then addHeader ctx "Content-Type" "text/plain"
          addHeader ctx "Content-Length" (bytes.Length.ToString())
          do! ctx.Response.Body.WriteAsync(bytes, 0, bytes.Length)
          return Some ctx
        }

      let requestPath = ctx.Request.Path.Value

      let loggerFactory = ctx.RequestServices.GetService<ILoggerFactory>()
      let logger = loggerFactory.CreateLogger("logger")
      let log msg (v : 'a) = logger.LogError("{msg}: {v}", msg, v)

      match! canvasNameFromHost ctx.Request.Host.Host with
      | Some canvasName ->

          let ownerName = LibBackend.Account.ownerNameFromCanvasName canvasName
          let ownerUsername = UserName.create (ownerName.ToString())
          let! ownerID = LibBackend.Account.userIDForUserName ownerUsername
          let! canvasID = LibBackend.Canvas.canvasIDForCanvasName ownerID canvasName

          let meta : LibBackend.Canvas.Meta =
            { id = canvasID; owner = ownerID; name = canvasName }

          let traceID = System.Guid.NewGuid()
          let method = ctx.Request.Method

          let! c =
            LibBackend.Canvas.loadHttpHandlersFromCache meta requestPath method
            |> Task.map Result.unwrapUnsafe

          match Map.values c.handlers with
          | [ { spec = PT.Handler.HTTP (route = route); ast = expr; tlid = tlid } ] ->
              let url = ctx.Request.GetEncodedUrl()
              let vars = LibBackend.Routing.routeInputVars route requestPath

              match vars with
              | Some vars ->
                  let symtable = Map.ofList vars

                  let ms = new IO.MemoryStream()
                  do! ctx.Request.Body.CopyToAsync(ms)
                  let body = ms.ToArray()
                  let expr = expr.toRuntimeType ()

                  let! result = runHttp c tlid traceID url body symtable expr

                  match result with
                  | RT.DHttpResponse (RT.Redirect url, _) ->
                      ctx.Response.Redirect(url, false)
                      return! next ctx
                  | RT.DHttpResponse (RT.Response (status, headers), RT.DBytes body) ->
                      ctx.Response.StatusCode <- status
                      List.iter (fun (k, v) -> addHeader ctx k v) headers
                      do! ctx.Response.Body.WriteAsync(body, 0, body.Length)
                      return! next ctx
                  | RT.DIncomplete _ ->
                      return!
                        msg
                          500
                          "Error calling server code: Handler returned an \
                           incomplete result. Please inform the owner of this \
                           site that their code is broken."
                  | other ->
                      printfn $"Not a HTTP response: {other}"
                      return! msg 500 "body is not a HttpResponse"
              | None -> // vars didnt parse
                  return!
                    msg
                      500
                      $"The request ({requestPath}) does not match the route ({route})"
          | [] -> return! msg 404 "No handler was found for this URL"
          | _ -> return! msg 500 "More than one handler found for this URL"
      | None -> return! msg 404 "No handler was found for this URL"
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

let configureLogging (shouldLog : bool) (builder : ILoggingBuilder) =
  // We want to disable this by default for tests because it clogs the output
  let filter (l : LogLevel) : bool = shouldLog

  // Configure the logging factory
  builder
    .AddFilter(filter) // Optional filter
    .AddConsole() // Set up the Console logger
    .AddDebug() // Set up the Debug logger
  // Add additional loggers if wanted...
  |> ignore

let configureServices (services : IServiceCollection) = ()

let webserver (shouldLog : bool) (port : int) =
  WebHost.CreateDefaultBuilder()
  |> fun wh -> wh.UseKestrel(fun kestrel -> kestrel.AddServerHeader <- false)
  |> fun wh -> wh.ConfigureServices(configureServices)
  |> fun wh -> wh.Configure(configureApp)
  |> fun wh -> wh.ConfigureLogging(configureLogging shouldLog)
  |> fun wh -> wh.UseUrls($"http://*:{port}")
  |> fun wh -> wh.Build()


[<EntryPoint>]
let main _ =
  printfn "Starting BwdServer"
  LibBackend.Init.init ()
  (webserver true 9001).Run()
  0
