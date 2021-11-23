module BwdServer.MiddlewareV0

// This is the webserver for builtwithdark.com. It uses ASP.NET directly,
// instead of a web framework, so we can tune the exact behaviour of headers
// and such.

open FSharp.Control.Tasks
open System.Threading.Tasks

open Prelude
open Tablecloth

module PT = LibExecution.ProgramTypes
module RT = LibExecution.RuntimeTypes
module RealExe = LibRealExecution.RealExecution
module Exe = LibExecution.Execution
module Interpreter = LibExecution.Interpreter
module Account = LibBackend.Account
module Canvas = LibBackend.Canvas
module Routing = LibBackend.Routing
module Pusher = LibBackend.Pusher
module TI = LibBackend.TraceInputs
module Req = LibExecution.HttpRequest
module Resp = LibExecution.HttpResponse


// ---------------
// Urls
// ---------------

// Proxies that terminate HTTPs should give us X-Forwarded-Proto: http
// or X-Forwarded-Proto: https.
// Return the URI, adding the scheme to the URI if there is an X-Forwarded-Proto.
let canonicalizeURL (toHttps : bool) (url : string) =
  if toHttps then
    let uri = System.UriBuilder url
    uri.Port <- 443
    uri.Scheme <- "https" // Switch to 443 or it will print :80
    string uri.Uri // Use .Uri or it will slip in a port number
  else
    url


// ---------------
// Handle builtwithdark request
// ---------------
let runHttpRequest
  (c : Canvas.T)
  (tlid : tlid)
  (traceID : LibExecution.AnalysisTypes.TraceID)
  (routeVars : List<string * RT.Dval>)
  (request : RT.Dval)
  (expr : RT.Expr)
  : Task<Resp.HttpResponse * HashSet.T<tlid>> =
  task {
    let program = Canvas.toProgram c
    let! state, touchedTLIDs = RealExe.createState traceID tlid program

    // Build request
    let symtable =
      Map.ofList routeVars
      |> Interpreter.withGlobals state
      |> Map.add "request" request

    // Execute
    let! result = Interpreter.eval state symtable expr

    return (Resp.toHttpResponse result, touchedTLIDs)
  }

