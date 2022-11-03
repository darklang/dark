module RT = RuntimeTypes
module PT = ProgramTypes

open Belt_extended

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
      functionResults: field("functionResults", list(FunctionResult.decode), j),
    }
  }

  let encode = (t: t): Js.Json.t => {
    open Json_encode_extended
    object_(list{
      ("input", list(tuple2(string, RT.Dval.encode), Belt.Map.String.toList(t.input))),
      ("timestamp", string(t.timestamp)),
      ("functionResults", list(FunctionResult.encode, t.functionResults)),
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

module NewTrace = {
  @ppx.deriving(show({with_path: false}))
  type rec t = (TraceID.t, list<TLID.t>)

  let decode = (j): t => {
    open Json_decode_extended
    tuple2(TraceID.decode, list(TLID.decode), j)
  }
  let encode = (nt: t): Js.Json.t => {
    open Json_encode_extended
    pair(TraceID.encode, list(TLID.encode), nt)
  }
}

module Traces = {
  @ppx.deriving(show({with_path: false}))
  type rec t = TLID.Dict.t<list<Trace.t>>

  let decode = (j): t => {
    open Json_decode_extended
    j |> list(tuple2(TLID.decode, list(Trace.decode))) |> TLID.Dict.fromList
  }
  // CLEANUP type does not match encoding
}

module ExecutionResult = {
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
  let encode = (er: t): Js.Json.t => {
    open Json_encode_extended
    let ev = variant
    switch er {
    | ExecutedResult(dv) => ev("ExecutedResult", list{RT.Dval.encode(dv)})
    | NonExecutedResult(dv) => ev("NonExecutedResult", list{RT.Dval.encode(dv)})
    }
  }
}

module PerformAnalysis = {
  module Handler = {
    module Params = {
      @ppx.deriving(show({with_path: false}))
      type rec t = {
        requestID: ID.t,
        requestTime: Js.Date.t,
        handler: PT.Handler.t,
        traceID: TraceID.t,
        traceData: TraceData.t,
        dbs: list<PT.DB.t>,
        userFns: list<PT.UserFunction.t>,
        userTypes: list<PT.UserType.t>,
        packageFns: list<PT.Package.Fn.t>,
        secrets: list<SecretTypes.t>,
      }

      let encode = (params: t): Js.Json.t => {
        open Json_encode_extended
        object_(list{
          ("requestID", ID.encode(params.requestID)),
          ("requestTime", Json_encode_extended.date(params.requestTime)),
          ("handler", PT.Handler.encode(params.handler)),
          ("traceID", TraceID.encode(params.traceID)),
          ("traceData", TraceData.encode(params.traceData)),
          ("dbs", list(PT.DB.encode, params.dbs)),
          ("userFns", list(PT.UserFunction.encode, params.userFns)),
          ("userTypes", list(PT.UserType.encode, params.userTypes)),
          ("packageFns", list(PT.Package.Fn.encode, params.packageFns)),
          ("secrets", list(SecretTypes.encode, params.secrets)),
        })
      }

      let decode = (j): t => {
        open Json_decode_extended
        {
          requestID: field("requestID", ID.decode, j),
          requestTime: field("requestTime", date, j),
          handler: field("handler", PT.Handler.decode, j),
          traceID: field("traceID", TraceID.decode, j),
          traceData: field("traceData", TraceData.decode, j),
          dbs: field("dbs", list(PT.DB.decode), j),
          userFns: field("userFns", list(PT.UserFunction.decode), j),
          userTypes: field("userTypes", list(PT.UserType.decode), j),
          packageFns: field("packageFns", list(PT.Package.Fn.decode), j),
          secrets: field("secrets", list(SecretTypes.decode), j),
        }
      }
    }
  }

  module Function = {
    module Params = {
      @ppx.deriving(show({with_path: false}))
      type rec t = {
        requestID: ID.t,
        requestTime: Js.Date.t,
        func: PT.UserFunction.t,
        traceID: TraceID.t,
        traceData: TraceData.t,
        dbs: list<PT.DB.t>,
        userFns: list<PT.UserFunction.t>,
        userTypes: list<PT.UserType.t>,
        packageFns: list<PT.Package.Fn.t>,
        secrets: list<SecretTypes.t>,
      }

      let encode = (params: t): Js.Json.t => {
        open Json_encode_extended
        object_(list{
          ("requestID", ID.encode(params.requestID)),
          ("requestTime", Json_encode_extended.date(params.requestTime)),
          ("func", PT.UserFunction.encode(params.func)),
          ("traceID", TraceID.encode(params.traceID)),
          ("traceData", TraceData.encode(params.traceData)),
          ("dbs", list(PT.DB.encode, params.dbs)),
          ("userFns", list(PT.UserFunction.encode, params.userFns)),
          ("userTypes", list(PT.UserType.encode, params.userTypes)),
          ("packageFns", list(PT.Package.Fn.encode, params.packageFns)),
          ("secrets", list(SecretTypes.encode, params.secrets)),
        })
      }

      let decode = (j): t => {
        open Json_decode_extended
        {
          requestID: field("requestID", ID.decode, j),
          requestTime: field("requestTime", date, j),
          func: field("func", PT.UserFunction.decode, j),
          traceID: field("traceID", TraceID.decode, j),
          traceData: field("traceData", TraceData.decode, j),
          dbs: field("dbs", list(PT.DB.decode), j),
          userFns: field("userFns", list(PT.UserFunction.decode), j),
          userTypes: field("userTypes", list(PT.UserType.decode), j),
          packageFns: field("packageFns", list(PT.Package.Fn.decode), j),
          secrets: field("secrets", list(SecretTypes.decode), j),
        }
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

    let decode = (j: Js.Json.t): t => {
      open Json_decode_extended
      variants(
        list{
          ("AnalyzeHandler", variant1(a => AnalyzeHandler(a), Handler.Params.decode)),
          ("AnalyzeFunction", variant1(a => AnalyzeFunction(a), Function.Params.decode)),
        },
        j,
      )
    }
  }

  module IntermediateResultStore = {
    type rec t = ID.Map.t<ExecutionResult.t>

    let decode = (j: Js.Json.t): t => ID.Map.decode(ExecutionResult.decode, j)

    let encode = (store: t): Js.Json.t => {
      ID.Map.encode(ExecutionResult.encode, store)
    }

    let toDebugString = (store: t): string => {
      store->encode->Js.Json.stringify
    }
  }

  module Envelope = {
    // ID: something to identify the specific request
    // Date: when the analysis request was made
    type rec t = (TraceID.t, IntermediateResultStore.t, ID.t, Js.Date.t)

    let decode = (j: Js.Json.t): t => {
      open Json_decode_extended
      tuple4(TraceID.decode, IntermediateResultStore.decode, ID.decode, Json_decode_extended.date)(
        j,
      )
    }

    let encode = (envelope: t): Js.Json.t => {
      open Json_encode_extended
      tuple4(
        TraceID.encode,
        IntermediateResultStore.encode,
        ID.encode,
        Json_encode_extended.date,
        envelope,
      )
    }
  }

  module Error = {
    type rec t =
      | ExecutionError(Params.t, string)
      | ParseError(string)
  }

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

  // The backend has some serializers that make this hard to use the natural
  // serialization for this
  let encode = (ws: t): Js.Json.t => {
    open Json_encode_extended
    switch ws {
    | Running => string("run")
    | Blocked => string("block")
    | Paused => string("pause")
    }
  }
  let decode = (j: Js.Json.t): t => {
    open Json_decode_extended
    switch string(j) {
    | "run" => Running
    | "block" => Blocked
    | "pause" => Paused
    | _ => Recover.recover("Invalid WorkerState encoding", Running)
    }
  }
}

module WorkerStats = {
  type rec t = {
    count: int,
    schedule: option<WorkerState.t>,
  }
  let default: t = {count: 0, schedule: None}
}

type rec avDict = ID.Map.t<Tc.Map.String.t<ID.t>>

type analysisStore = Loadable.t<PerformAnalysis.IntermediateResultStore.t, string>

let analysisStoreToDebugString = (store: analysisStore): string => {
  switch store {
  | Loadable.Loading(_) => "Loading"
  | Loadable.Success(result) =>
    "Success(" ++ PerformAnalysis.IntermediateResultStore.toDebugString(result) ++ ")"
  | Loadable.Error(msg) => "Error: " ++ msg
  | NotInitialized => "NotInitialized"
  }
}

// indexed by traceID
type analyses = Tc.Map.String.t<(Js.Date.t, analysisStore)>

let analysesToDebugString = (a: analyses): string => {
  a
  ->Tc.Map.toList
  ->Tc.List.map(~f=((traceID, (_, store))) => traceID ++ ": " ++ analysisStoreToDebugString(store))
  ->Tc.String.join(~sep=", ")
}

type traces = TLID.Dict.t<list<Trace.t>>

type dbStats = {
  count: int,
  example: option<(RT.Dval.t, string)>,
}

type dbStatsStore = Tc.Map.String.t<dbStats>

type traceOpt = (TraceID.t, option<TraceData.t>)
