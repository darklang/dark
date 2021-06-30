namespace Wasm

open System.Threading.Tasks
open FSharp.Control.Tasks

open Prelude
open Tablecloth

module RT = LibExecution.RuntimeTypes
module Exe = LibExecution.Execution
module PT = LibExecution.ProgramTypes
module AT = LibExecution.AnalysisTypes
module OT = LibExecution.OCamlTypes
module ORT = LibExecution.OCamlTypes.RuntimeT
module DvalRepr = LibExecution.DvalRepr

module OCamlInterop =
  type handler_analysis_param =
    { handler : ORT.fluidExpr ORT.HandlerT.handler
      trace_id : AT.TraceID
      trace_data : AT.TraceData
      (* dont use a trace as this isn't optional *)
      dbs : ORT.fluidExpr ORT.DbT.db list
      user_fns : ORT.fluidExpr ORT.user_fn list
      user_tipes : ORT.user_tipe list
      secrets : OT.secret list }

  type function_analysis_param =
    { func : ORT.fluidExpr ORT.user_fn
      trace_id : AT.TraceID
      trace_data : AT.TraceData
      (* dont use a trace as this isn't optional *)
      dbs : ORT.fluidExpr ORT.DbT.db list
      user_fns : ORT.fluidExpr ORT.user_fn list
      user_tipes : ORT.user_tipe list
      secrets : OT.secret list }

  type ExecutionResult =
    | ExecutedResult of ORT.dval
    | NonExecutedResult of ORT.dval

  // Dictionarys are mutable
  type AnalysisResults = System.Collections.Generic.Dictionary<id, ExecutionResult>

  type AnalysisEnvelope = AT.TraceID * AnalysisResults

module Eval =

  let loadFromTrace
    (results : AT.FunctionResult list)
    ((_tlid, fnName, callerID) : RT.FunctionRecord)
    (args : List<RT.Dval>)
    : Option<RT.Dval * System.DateTime> =
    let hashes =
      DvalRepr.supportedHashVersions
      |> List.map (fun key -> (key, DvalRepr.hash key args))
      |> Map

    results
    |> List.filterMap
         (fun (rFnName, rCallerID, hash, hashVersion, dval) ->
           if string fnName = rFnName
              && callerID = rCallerID
              && hash = ((Map.tryFind hashVersion hashes) |> Option.unwrapUnsafe) then
             Some dval
           else
             None)
    |> List.head
    (* We don't use the time, so just hack it to get the interface right. *)
    |> Option.map (fun dv -> (dv, System.DateTime.Now))


  let stdlib =
    LibExecution.StdLib.StdLib.fns
    |> Tablecloth.Map.fromListBy (fun fn -> RT.FQFnName.Stdlib fn.name)


  // call this from JS with DotNet.invokeMethod('Wasm', 'run', 7)
  // or DotNet.invokeMethodAsync('Wasm', 'run', 8)
  [<Microsoft.JSInterop.JSInvokable>]
  let performHandlerAnalysis (str : string) : Task<string> =
    task {
      try
        let args =
          Json.OCamlCompatible.deserialize<OCamlInterop.handler_analysis_param> str

        let traceID = args.trace_id

        let program : RT.ProgramContext =
          { accountID = System.Guid.NewGuid()
            canvasID = System.Guid.NewGuid()
            userFns =
              args.user_fns
              |> List.map OT.Convert.ocamlUserFunction2PT
              |> List.map PT.UserFunction.toRuntimeType
              |> List.map (fun fn -> fn.name, fn)
              |> Map
            userTypes =
              args.user_tipes
              |> List.map OT.Convert.ocamlUserType2PT
              |> List.map PT.UserType.toRuntimeType
              |> List.map (fun t -> (t.name, t.version), t)
              |> Map
            dbs =
              args.dbs
              |> List.map (OT.Convert.ocamlDB2PT { x = 0; y = 0 })
              |> List.map PT.DB.toRuntimeType
              |> List.map (fun t -> t.name, t)
              |> Map
            secrets = [] }

        // FSTODO: get packages from caller
        let libraries : RT.Libraries = { stdlib = stdlib; packageFns = Map.empty }
        let dvalResults, traceDvalFn = Exe.traceDvals ()
        let functionResults = args.trace_data.function_results

        let tracing =
          { LibExecution.Execution.noTracing RT.Preview with
              traceDval = traceDvalFn
              loadFnResult = loadFromTrace functionResults }

        let state = Exe.createState libraries tracing args.handler.tlid program

        let ast = (args.handler.ast |> OT.Convert.ocamlExpr2PT).toRuntimeType ()
        let inputVars = Map args.trace_data.input
        let! (_result : RT.Dval) = Exe.executeExpr state inputVars ast


        let ocamlResults =
          dvalResults
          |> Dictionary.toList
          |> List.map
               (fun (k, v) ->
                 k,
                 match v with
                 | AT.ExecutedResult dv ->
                     OCamlInterop.ExecutedResult(OT.Convert.rt2ocamlDval dv)
                 | AT.NonExecutedResult dv ->
                     OCamlInterop.NonExecutedResult(OT.Convert.rt2ocamlDval dv))
          |> Dictionary.fromList

        let results : OCamlInterop.AnalysisEnvelope = (traceID, ocamlResults)
        let serialized = Json.OCamlCompatible.serialize results
        return serialized
      with e ->
        System.Console.WriteLine(string e)
        return "error"
    }
