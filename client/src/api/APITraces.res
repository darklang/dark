module TraceData = {
  @ppx.deriving(show({with_path: false}))
  type rec t = {trace: AnalysisTypes.Trace.t}
  let decode = (j): t => {
    open Json_decode_extended
    {trace: field("trace", AnalysisTypes.Trace.decode, j)}
  }

  module Params = {
    @ppx.deriving(show({with_path: false}))
    type rec t = {gtdrpTlid: TLID.t, gtdrpTraceID: AnalysisTypes.TraceID.t}
    let encode = (params: t): Js.Json.t => {
      open Json_encode_extended
      object_(list{
        ("tlid", TLID.encode(params.gtdrpTlid)),
        ("trace_id", AnalysisTypes.TraceID.encode(params.gtdrpTraceID)),
      })
    }
  }

  //CLEANUP RENAME FIELDS
}

module AllTraces = {
  @ppx.deriving(show({with_path: false}))
  type rec t = {traces: list<(TLID.t, AnalysisTypes.TraceID.t)>}

  let decode = (j): t => {
    open Json_decode_extended
    {
      traces: field("traces", list(pair(TLID.decode, AnalysisTypes.TraceID.decode)), j),
    }
  }
}
