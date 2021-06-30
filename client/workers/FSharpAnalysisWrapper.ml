open Prelude

type event = (Types.performAnalysisParams Js.nullable)

let stringifyInput (event : event) : string =
  match Js.Nullable.toOption event with
  | None ->
      (* When we sent too much data, the event just won't have data in it. *)
      (* reportError "Trace was too big to load into analysis" () ; *)
      Js.Console.log2 "Got null instead of an event" event ;
      "Error: got null instead of an event"
  | Some (AnalyzeHandler hParams) ->
      Js.Json.stringify (Encoders.performHandlerAnalysisParams hParams)
  | Some (AnalyzeFunction fParams) ->
      Js.Json.stringify (Encoders.performFunctionAnalysisParams fParams)

let decodeOutput (str) =
  Belt.Result.Ok (Decoders.analysisEnvelope (Json.parseOrRaise str))
