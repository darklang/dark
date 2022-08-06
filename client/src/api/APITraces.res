module TraceData = {
  module Params = {
    @ppx.deriving(show({with_path: false}))
    type rec t = {tlid: TLID.t, traceID: TraceID.t}
    let decode = (j): t => {
      open Json_decode_extended
      {
        traceID: field("traceID", TraceID.decode, j),
        tlid: field("tlid", TLID.decode, j),
      }
    }
    let encode = (params: t): Js.Json.t => {
      open Json_encode_extended
      object_(list{("tlid", TLID.encode(params.tlid)), ("traceID", TraceID.encode(params.traceID))})
    }
  }

  @ppx.deriving(show({with_path: false}))
  type rec t = {trace: AnalysisTypes.Trace.t}
  let decode = (j): t => {
    open Json_decode_extended
    {trace: field("trace", AnalysisTypes.Trace.decode, j)}
  }
  let encode = (trace: t): Js.Json.t => {
    open Json_encode_extended
    object_(list{("trace", AnalysisTypes.Trace.encode(trace.trace))})
  }
}

module AllTraces = {
  @ppx.deriving(show({with_path: false}))
  type rec t = {traces: list<(TLID.t, TraceID.t)>}

  let decode = (j): t => {
    open Json_decode_extended
    {
      traces: field("traces", list(pair(TLID.decode, TraceID.decode)), j),
    }
  }
  let encode = (traces: t): Js.Json.t => {
    open Json_encode_extended
    object_(list{("traces", list(pair(TLID.encode, TraceID.encode), traces.traces))})
  }
}
