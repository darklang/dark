open Prelude
module Decode = Json_decode_extended

type event = Js.nullable<Types.performAnalysisParams>

type response = {
  responseType: string,
  json: string,
}

let stringifyInput = (event: event): response =>
  switch Js.Nullable.toOption(event) {
  | None =>
    // When we sent too much data, the event just won't have data in it.
    // reportError "Trace was too big to load into analysis" () ;
    Js.Console.log2("Got null instead of an event", event)
    {responseType: "error", json: "Error: got null instead of an event"}
  | Some(params) => {
      responseType: "success",
      json: Js.Json.stringify(Encoders.performAnalysisParams(params)),
    }
  }

let decodeOutput = str => {
  let result = Decode.result(Decoders.analysisEnvelope, Decode.string, Json.parseOrRaise(str))
  switch result {
  | Belt.Result.Ok(msg) => Belt.Result.Ok(msg)
  | Belt.Result.Error(msg) =>
    ErrorReporting.Rollbar.send(msg, None, Js.Json.null)
    Belt.Result.Error(Types.AnalysisParseError(msg))
  }
}
