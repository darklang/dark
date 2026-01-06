/// Rollbar error tracking - STUBBED OUT
/// Rollbar has been removed. This module provides no-op implementations.
module LibService.Rollbar

open System.Threading.Tasks

open Microsoft.AspNetCore.Http

open Prelude

/// Optional person data
type Person = Option<UserID>

/// Debug Rollbar config - no-op
let debugRollbarConfig () = ()

/// Notify that a non-error happened - prints to console only
let notify (message : string) (metadata : Metadata) : unit =
  print $"[notify] {message}"

/// Notify that an error (but not an exception) happened - prints to console only
let sendError (message : string) (metadata : Metadata) : unit =
  print $"[error] {message}"
  print System.Environment.StackTrace

/// Sends exception to console (Rollbar removed)
let sendException (person : Person) (metadata : Metadata) (e : exn) : unit =
  printException "[exception]" metadata e

/// Last ditch error reporting - prints and returns -1
let lastDitchBlockAndPage (msg : string) (e : exn) : int =
  printException msg [] e
  Task.Delay(1000).Wait()
  -1

module AspNet =
  open Microsoft.Extensions.DependencyInjection
  open Microsoft.AspNetCore.Builder

  type RollbarContext =
    { ctxMetadataFn : HttpContext -> Person * Metadata
      ignoreStartupPath : Option<string> }

  /// Middleware that catches exceptions and logs them
  type DarkRollbarMiddleware
    (nextRequestProcessor : RequestDelegate, rollbarCtx : RollbarContext) =
    member this._nextRequestProcessor : RequestDelegate = nextRequestProcessor
    member this.Invoke(ctx : HttpContext) : Task =
      task {
        try
          do! this._nextRequestProcessor.Invoke(ctx)
        with e ->
          if Some(string ctx.Request.Path) = rollbarCtx.ignoreStartupPath then
            ()
          else
            printException "[http exception]" [] e
          Exception.reraise e
      }

  let addRollbarToServices (services : IServiceCollection) : IServiceCollection =
    services

  let addRollbarToApp
    (app : IApplicationBuilder)
    (ctxMetadataFn : HttpContext -> Person * Metadata)
    (ignoreStartupPath : Option<string>)
    : IApplicationBuilder =
    app.UseMiddleware<DarkRollbarMiddleware>(
      { ctxMetadataFn = ctxMetadataFn; ignoreStartupPath = ignoreStartupPath }
    )

/// Initialize Rollbar - no-op
let init (serviceName : string) : unit =
  printTime "Rollbar disabled (stubbed out)"
  Exception.sendRollbarError <- fun msg metadata -> sendError msg metadata
