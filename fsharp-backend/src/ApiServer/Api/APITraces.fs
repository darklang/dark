module ApiServer.Traces

// API endpoints for Traces

open Microsoft.AspNetCore.Http
open Giraffe
open Giraffe.EndpointRouting

open System.Threading.Tasks
open FSharp.Control.Tasks
open FSharpPlus
open Prelude
open Tablecloth

module PT = LibBackend.ProgramTypes
module ORT = LibBackend.OCamlInterop.OCamlTypes.RuntimeT
module AT = LibExecution.AnalysisTypes

module Traces = LibBackend.Traces
module Auth = LibBackend.Authorization
module Canvas = LibBackend.Canvas
module Convert = LibBackend.OCamlInterop.Convert

module TraceData =
  type Params = { tlid : tlid; trace_id : AT.TraceID }

  // CLEANUP: this uses ORT.dval instead of RT.Dval
  type InputVars = List<string * ORT.dval>
  type FunctionArgHash = string
  type HashVersion = int
  type FnName = string
  type FunctionResult = FnName * id * FunctionArgHash * HashVersion * ORT.dval

  type TraceData =
    { input : InputVars
      timestamp : System.DateTime
      function_results : List<FunctionResult> }

  type Trace = AT.TraceID * TraceData
  type TraceResult = { trace : Trace }

  type T = Option<TraceResult>

  let getTraceData (ctx : HttpContext) : Task<T> =
    task {
      let t = Middleware.startTimer ctx
      let canvasInfo = Middleware.loadCanvasInfo ctx
      t "loadCanvasInfo"

      let! args = ctx.BindModelAsync<Params>()
      t "readBody"

      let! (c : Canvas.T) =
        Canvas.loadTLIDsFromCache canvasInfo [ args.tlid ]
        |> Task.map Result.unwrapUnsafe

      t "loadCanvas"

      // TODO: we dont need the handlers or functions at all here, just for the sample
      // values which we can do on the client instead
      let handler = c.handlers |> Map.get args.tlid

      let! trace =
        match handler with
        | Some h -> Traces.handlerTrace c.meta.id args.trace_id h |> Task.map Some
        | None ->
            match c.userFunctions |> Map.get args.tlid with
            | Some u -> Traces.userfnTrace c.meta.id args.trace_id u |> Task.map Some
            | None -> task { return None }

      // CLEANUP, this is shimming an RT.Dval into an ORT.dval. Nightmare.
      let (trace : Option<Trace>) =
        match trace with
        | Some (id, (traceData : AT.TraceData)) ->
            Some(
              id,
              { input =
                  List.map
                    (fun (s, dv) -> (s, Convert.rt2ocamlDval dv))
                    traceData.input
                timestamp = traceData.timestamp
                function_results =
                  List.map
                    (fun (r1, r2, r3, r4, dv) ->
                      (r1, r2, r3, r4, Convert.rt2ocamlDval dv))
                    traceData.function_results }
            )
        | None -> None

      t "loadTraces"
      return Option.map (fun t -> { trace = t }) trace
    }

module AllTraces =

  type T = { traces : List<tlid * AT.TraceID> }

  let fetchAllTraces (ctx : HttpContext) : Task<T> =
    task {
      let t = Middleware.startTimer ctx
      let canvasInfo = Middleware.loadCanvasInfo ctx

      // FSTODO we only need the HTTP handler paths here, so we can remove the loadAll
      // FSTODO don't load traces for deleted handlers
      let! (c : Canvas.T) = Canvas.loadAll canvasInfo |> Task.map Result.unwrapUnsafe
      t "loadCanvas"

      let! hTraces =
        c.handlers
        |> Map.values
        |> List.map
             (fun h ->
               Traces.traceIDsForHandler c h
               |> Task.map (List.map (fun traceid -> (h.tlid, traceid))))
        |> Task.flatten
        |> Task.map List.concat

      t "fetchHandlerTraces"

      let! ufTraces =
        c.userFunctions
        |> Map.values
        |> List.map
             (fun uf ->
               Traces.traceIDsForUserFn c.meta.id uf.tlid
               |> Task.map (List.map (fun traceID -> (uf.tlid, traceID))))
        |> Task.flatten
        |> Task.map List.concat

      t "fetchUserFnTraces"

      return { traces = hTraces @ ufTraces }
    }

let endpoints : Endpoint list =
  let h = Middleware.apiHandler
  let oh = Middleware.apiOptionHandler

  [ POST [ routef "/api/%s/get_trace_data" (oh TraceData.getTraceData Auth.Read)
           routef "/api/%s/all_traces" (h AllTraces.fetchAllTraces Auth.Read) ] ]
