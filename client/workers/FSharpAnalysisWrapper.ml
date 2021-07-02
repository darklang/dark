open Prelude
module Decode = Json_decode_extended

type event = Types.performAnalysisParams Js.nullable

type response =
  { responseType : string
  ; json : string }

let stringifyInput (event : event) : response =
  match Js.Nullable.toOption event with
  | None ->
      (* When we sent too much data, the event just won't have data in it. *)
      (* reportError "Trace was too big to load into analysis" () ; *)
      Js.Console.log2 "Got null instead of an event" event ;
      {responseType = "error"; json = "Error: got null instead of an event"}
  | Some params ->
      { responseType = "success"
      ; json = Js.Json.stringify (Encoders.performAnalysisParams params) }


let decodeOutput str =
  Decode.result Decoders.analysisEnvelope Decode.string (Json.parseOrRaise str)
