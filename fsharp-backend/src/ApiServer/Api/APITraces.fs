/// API endpoints to fetch Traces
module ApiServer.Traces

open System.Threading.Tasks
open FSharp.Control.Tasks
open Microsoft.AspNetCore.Http

open Prelude
open Tablecloth
open Http

module PT = LibExecution.ProgramTypes
module ORT = LibExecution.OCamlTypes.RuntimeT
module AT = LibExecution.AnalysisTypes

module Traces = LibBackend.Traces
module Canvas = LibBackend.Canvas
module Convert = LibExecution.OCamlTypes.Convert
module Telemetry = LibService.Telemetry

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
      timestamp : NodaTime.Instant
      function_results : List<FunctionResult> }

  type Trace = AT.TraceID * TraceData

  type T = { trace : Trace }

  /// API endpoint to fetch data for a specific Trace
  ///
  /// Data returned includes input, timestamp, and results
  let getTraceData (ctx : HttpContext) : Task<Option<T>> =
    task {
      use t = startTimer "read-api" ctx
      let canvasInfo = loadCanvasInfo ctx
      let! p = ctx.ReadJsonAsync<Params>()
      Telemetry.addTags [ "tlid", p.tlid; "trace_id", p.trace_id ]

      t.next "load-canvas"
      let! c = Canvas.loadTLIDs canvasInfo [ p.tlid ]


      // CLEANUP: we dont need the handlers or functions at all here, just for the sample
      // values which we could do on the client instead
      t.next "load-trace"
      let handler = c.handlers |> Map.get p.tlid

      let! trace =
        match handler with
        | Some h -> Traces.handlerTrace c.meta.id p.trace_id h |> Task.map Some
        | None ->
          match c.userFunctions |> Map.get p.tlid with
          | Some u -> Traces.userfnTrace c.meta.id p.trace_id u |> Task.map Some
          | None -> Task.FromResult None


      t.next "write-api"
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

      return Option.map (fun t -> { trace = t }) trace
    }

module AllTraces =

  type T = { traces : List<tlid * AT.TraceID> }

  /// API endpoint to fetch a list of Traces for a Toplevel
  ///
  /// Only returns metadata - does not include inputs/outputs
  let fetchAll (ctx : HttpContext) : Task<T> =
    task {
      use t = startTimer "read-api" ctx
      let canvasInfo = loadCanvasInfo ctx

      // CLEANUP we only need the HTTP handler paths here, so we can remove the loadAll
      // CLEANUP don't load traces for deleted handlers
      t.next "load-canvas"
      let! (c : Canvas.T) = Canvas.loadAll canvasInfo

      t.next "fetch-handler-traces"
      let! hTraces =
        c.handlers
        |> Map.values
        |> List.map (fun h ->
          Traces.traceIDsForHandler c h
          |> Task.map (List.map (fun traceid -> (h.tlid, traceid))))
        |> Task.flatten
        |> Task.map List.concat


      t.next "fetch-userfn-traces"
      let! ufTraces =
        c.userFunctions
        |> Map.values
        |> List.map (fun uf ->
          Traces.traceIDsForUserFn c.meta.id uf.tlid
          |> Task.map (List.map (fun traceID -> (uf.tlid, traceID))))
        |> Task.flatten
        |> Task.map List.concat

      return { traces = hTraces @ ufTraces }
    }
