/// Payloads that we send to the client via Pusher.com
module ClientTypes.Pusher

open Prelude

module Payload =
  type NewTrace = Analysis.TraceID * tlid list
