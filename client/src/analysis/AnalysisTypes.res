module RT = RuntimeTypes
module PT = ProgramTypes

open BaseTypes

// CLEANUP: find a better place for this
@ppx.deriving(show({with_path: false}))
type rec unlockedDBs = TLID.Set.t

module InputValueDict = {
  @ppx.deriving(show({with_path: false}))
  type rec t = Belt.Map.String.t<RT.Dval.t>

  let decode = (j): t => {
    open Json.Decode

    array(tuple2(string, RT.Dval.decode), j) |> Belt.Map.String.fromArray
  }
}

module FunctionResult = {
  // CLEANUP make the type match the encoding
  @ppx.deriving(show({with_path: false}))
  type rec t = {
    fnName: string,
    callerID: ID.t,
    argHash: string,
    argHashVersion: int,
    value: RT.Dval.t,
  }

  let decode = (j): t => {
    open Json_decode_extended
    let (fnName, callerID, argHash, argHashVersion, value) = tuple5(
      string,
      ID.decode,
      string,
      int,
      RT.Dval.decode,
      j,
    )

    {
      fnName: fnName,
      callerID: callerID,
      argHash: argHash,
      argHashVersion: argHashVersion,
      value: value,
    }
  }

  let encode = (fr: t): Js.Json.t => {
    open Json_encode_extended
    list(
      Tc.Fun.identity,
      list{
        string(fr.fnName),
        ID.encode(fr.callerID),
        string(fr.argHash),
        int(fr.argHashVersion),
        RT.Dval.encode(fr.value),
      },
    )
  }
}

module TraceData = {
  @ppx.deriving(show({with_path: false}))
  type rec t = {
    input: Belt.Map.String.t<RT.Dval.t>,
    timestamp: string,
    functionResults: list<FunctionResult.t>,
  }

  let decode = (j): t => {
    open Json_decode_extended
    {
      input: field("input", InputValueDict.decode, j),
      timestamp: field("timestamp", string, j),
      functionResults: field("function_results", list(FunctionResult.decode), j),
    }
  }

  let encode = (t: t): Js.Json.t => {
    open Json_encode_extended
    object_(list{
      ("input", list(tuple2(string, RT.Dval.encode), Belt.Map.String.toList(t.input))),
      ("timestamp", string(t.timestamp)),
      ("function_results", list(FunctionResult.encode, t.functionResults)),
    })
  }
}

module TraceError = {
  @ppx.deriving(show({with_path: false}))
  type rec t =
    | NoneYet
    | MaximumCallStackError // unrecoverable - don't try again
}

module Trace = {
  @ppx.deriving(show({with_path: false}))
  type rec t = (TraceID.t, Tc.Result.t<TraceData.t, TraceError.t>)

  let decode = (j): t => {
    open Json_decode_extended
    pair(TraceID.decode, optional(TraceData.decode), j) |> (
      ((id, traceData)) =>
        switch traceData {
        | None => (id, Error(TraceError.NoneYet))
        | Some(traceData) => (id, Ok(traceData))
        }
    )
  }

  let encode = (t: t): Js.Json.t => {
    open Json_encode_extended
    let data = v =>
      Tc.Result.map(~f=TraceData.encode, v) |> Tc.Result.toOption |> Tc.Option.unwrap(~default=null)

    pair(TraceID.encode, data, t)
  }
}

module Traces = {
  @ppx.deriving(show({with_path: false}))
  type rec t = TLID.Dict.t<list<Trace.t>>

  let decode = (j): t => {
    open Json_decode_extended
    j |> list(tuple2(TLID.decode, list(Trace.decode))) |> TLID.Dict.fromList
  }
  // CLEANUP TYPE DOES NOT MATCH ENCODING
}

module ExecutionResult = {
  @ppx.deriving(show({with_path: false}))
  type rec t =
    | ExecutedResult(RT.Dval.t)
    | NonExecutedResult(RT.Dval.t)

  let decode = (j: Js.Json.t): t => {
    open Json_decode_extended
    variants(
      list{
        ("ExecutedResult", variant1(a => ExecutedResult(a), RT.Dval.decode)),
        ("NonExecutedResult", variant1(a => NonExecutedResult(a), RT.Dval.decode)),
      },
      j,
    )
  }
}

// map from expression ids to symbol table, which maps from varname strings to
// the ids of the expressions that represent their values

module PerformAnalysis = {
  module Handler = {
    module Params = {
      @ppx.deriving(show({with_path: false}))
      type rec t = {
        handler: PT.Handler.t,
        traceID: TraceID.t,
        traceData: TraceData.t,
        dbs: list<PT.DB.t>,
        userFns: list<PT.UserFunction.t>,
        userTypes: list<PT.UserType.t>,
        secrets: list<SecretTypes.t>,
      }

      let encode = (params: t): Js.Json.t => {
        open Json_encode_extended
        object_(list{
          ("handler", PT.Handler.encode(params.handler)),
          ("traceID", TraceID.encode(params.traceID)),
          ("traceData", TraceData.encode(params.traceData)),
          ("dbs", list(PT.DB.encode, params.dbs)),
          ("userFns", list(PT.UserFunction.encode, params.userFns)),
          ("userTypes", list(PT.UserType.encode, params.userTypes)),
          ("secrets", list(SecretTypes.encode, params.secrets)),
        })
      }
    }
  }

  module Function = {
    module Params = {
      @ppx.deriving(show({with_path: false}))
      type rec t = {
        func: PT.UserFunction.t,
        traceID: TraceID.t,
        traceData: TraceData.t,
        dbs: list<PT.DB.t>,
        userFns: list<PT.UserFunction.t>,
        userTypes: list<PT.UserType.t>,
        secrets: list<SecretTypes.t>,
      }

      let encode = (params: t): Js.Json.t => {
        open Json_encode_extended
        object_(list{
          ("func", PT.UserFunction.encode(params.func)),
          ("traceID", TraceID.encode(params.traceID)),
          ("traceData", TraceData.encode(params.traceData)),
          ("dbs", list(PT.DB.encode, params.dbs)),
          ("userFns", list(PT.UserFunction.encode, params.userFns)),
          ("userTypes", list(PT.UserType.encode, params.userTypes)),
          ("secrets", list(SecretTypes.encode, params.secrets)),
        })
      }
    }
  }

  module Params = {
    @ppx.deriving(show({with_path: false}))
    type rec t =
      | AnalyzeHandler(Handler.Params.t)
      | AnalyzeFunction(Function.Params.t)

    let encode = (params: t): Js.Json.t => {
      open Json_encode_extended
      let ev = variant
      switch params {
      | AnalyzeHandler(h) => ev("AnalyzeHandler", list{Handler.Params.encode(h)})
      | AnalyzeFunction(h) => ev("AnalyzeFunction", list{Function.Params.encode(h)})
      }
    }
  }

  module IntermediateResultStore = {
    @ppx.deriving(show({with_path: false}))
    type rec t = ID.Map.t<ExecutionResult.t>

    let decode = (j: Js.Json.t): t => ID.Map.decode(ExecutionResult.decode, j)
  }

  module Envelope = {
    @ppx.deriving(show({with_path: false}))
    type rec t = (TraceID.t, IntermediateResultStore.t)

    let decode = (j: Js.Json.t): t => {
      open Json_decode_extended
      tuple2(TraceID.decode, IntermediateResultStore.decode)(j)
    }
  }

  module Error = {
    @ppx.deriving(show({with_path: false}))
    type rec t =
      | ExecutionError(Params.t, string)
      | ParseError(string)
  }

  @ppx.deriving(show({with_path: false}))
  type rec t = Tc.Result.t<Envelope.t, Error.t>
}

module FourOhFour = {
  module Spec = {
    @ppx.deriving(show({with_path: false}))
    type rec t = {
      space: string,
      path: string,
      modifier: string,
    }

    let decode = (j): t => {
      open Json_decode_extended
      {
        space: field("space", string, j),
        path: field("path", string, j),
        modifier: field("modifier", string, j),
      }
    }

    let encode = (fof: t): Js.Json.t => {
      open Json_encode_extended
      object_(list{
        ("space", string(fof.space)),
        ("path", string(fof.path)),
        ("modifier", string(fof.modifier)),
      })
    }
  }

  @ppx.deriving(show({with_path: false}))
  type rec t = {
    space: string,
    path: string,
    modifier: string,
    timestamp: string,
    traceID: string,
  }

  let decode = (j): t => {
    open Json_decode_extended
    {
      space: index(0, string, j),
      path: index(1, string, j),
      modifier: index(2, string, j),
      timestamp: index(3, string, j),
      traceID: index(4, TraceID.decode, j),
    }
  }

  let encode = (fof: t): Js.Json.t => {
    open Json_encode_extended
    tuple5(
      string,
      string,
      string,
      string,
      TraceID.encode,
      (fof.space, fof.path, fof.modifier, fof.timestamp, fof.traceID),
    )
  }
}
module WorkerState = {
  @ppx.deriving(show({with_path: false}))
  type rec t = Running | Blocked | Paused
  let encode = (ws: t): Js.Json.t => {
    open Json_encode_extended
    switch ws {
    | Running => variant("Running", list{})
    | Blocked => variant("Blocked", list{})
    | Paused => variant("Paused", list{})
    }
  }
  let decode = (j: Js.Json.t): t => {
    open Json_decode_extended
    variants(
      list{
        ("Running", variant0(Running)),
        ("Blocked", variant0(Blocked)),
        ("Paused", variant0(Paused)),
      },
      j,
    )
  }
}

module WorkerStats = {
  @ppx.deriving(show({with_path: false}))
  type rec t = {
    count: int,
    schedule: option<WorkerState.t>,
  }
  let default: t = {count: 0, schedule: None}
}

@ppx.deriving(show({with_path: false}))
type rec avDict = ID.Map.t<Tc.Map.String.t<ID.t>>

and analysisStore = Types.loadable<PerformAnalysis.IntermediateResultStore.t>

// indexed by traceID
and analyses = Tc.Map.String.t<analysisStore>

and traces = TLID.Dict.t<list<Trace.t>>

and dbStats = {
  count: int,
  example: option<(RT.Dval.t, string)>,
}

and dbStatsStore = Tc.Map.String.t<dbStats>

and traceOpt = (TraceID.t, option<TraceData.t>)
