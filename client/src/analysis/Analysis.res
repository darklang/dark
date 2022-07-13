open Prelude

// Tea
module Cmd = Tea.Cmd

// Dark
module B = BlankOr
module P = Pointer
module RT = Runtime
module TL = Toplevel
module TD = TLID.Dict
module E = FluidExpression

// ----------------------
// Analyses
// ----------------------

@ocaml.doc(" [defaultTraceIDForTL ~tlid] returns the id of the \"default\" trace
 * for the top level with the given [tlid].
 *
 * This default trace exists in order to eg populate the autocomplete
 * with parameters of functions that haven't been called yet. ")
let defaultTraceIDForTL = (~tlid: TLID.t) =>
  BsUuid.Uuid.V5.create(
    ~name=TLID.toString(tlid),
    ~namespace=#Uuid("00000000-0000-0000-0000-000000000000"),
  ) |> BsUuid.Uuid.V5.toString

let getTraces = (m: model, tlid: TLID.t): list<trace> =>
  Map.get(~key=tlid, m.traces) |> Option.unwrapLazy(~default=() => list{
    (defaultTraceIDForTL(~tlid), Error(NoneYet)),
  })

let getTrace = (m: model, tlid: TLID.t, traceID: traceID): option<trace> =>
  getTraces(m, tlid) |> List.find(~f=((id, _)) => id == traceID)

let getStoredAnalysis = (m: model, traceID: traceID): analysisStore =>
  // only handlers have analysis results, but lots of stuff expect this
  // data to exist. It may be better to not do that, but this is fine
  // for now.
  Map.get(~key=traceID, m.analyses) |> Option.unwrap(~default=LoadableNotInitialized)

let record = (old: analyses, id: traceID, result: analysisStore): analyses =>
  Map.add(~key=id, ~value=result, old)

let replaceFunctionResult = (
  m: model,
  tlid: TLID.t,
  traceID: traceID,
  callerID: id,
  fnName: string,
  hash: dvalArgsHash,
  hashVersion: int,
  dval: dval,
): model => {
  let newResult = {
    fnName: fnName,
    callerID: callerID,
    argHash: hash,
    argHashVersion: hashVersion,
    value: dval,
  }

  let traces = m.traces |> Map.update(~key=tlid, ~f=ml =>
    ml
    |> Option.unwrap(
      ~default=list{
        (
          traceID,
          Ok({
            input: Belt.Map.String.empty,
            timestamp: "",
            functionResults: list{newResult},
          }),
        ),
      },
    )
    |> List.map(~f=((tid, tdata) as t) =>
      if tid == traceID {
        (tid, Result.map(~f=tdata => {
            ...tdata,
            functionResults: list{newResult, ...tdata.functionResults},
          }, tdata))
      } else {
        t
      }
    )
    |> (x => Some(x))
  )

  {...m, traces: traces}
}

let getLiveValueLoadable = (analysisStore: analysisStore, id: id): loadable<executionResult> =>
  switch analysisStore {
  | LoadableSuccess(dvals) =>
    Map.get(~key=id, dvals)
    |> Option.map(~f=dv => LoadableSuccess(dv))
    |> Option.unwrap(~default=LoadableNotInitialized)
  | LoadableNotInitialized => LoadableNotInitialized
  | LoadableLoading(oldDvals) =>
    oldDvals
    |> Option.andThen(~f=m => Map.get(~key=id, m))
    |> Option.map(~f=dv => LoadableSuccess(dv))
    |> Option.unwrap(~default=LoadableLoading(None))
  | LoadableError(error) => LoadableError(error)
  }

let getLiveValue' = (analysisStore: analysisStore, id: id): option<dval> =>
  switch analysisStore {
  | LoadableSuccess(dvals) =>
    switch Map.get(~key=id, dvals) {
    | Some(ExecutedResult(dval)) | Some(NonExecutedResult(dval)) => Some(dval)
    | _ => None
    }
  | _ => None
  }

let getLiveValue = (m: model, id: id, traceID: traceID): option<dval> =>
  getLiveValue'(getStoredAnalysis(m, traceID), id)

let getTipeOf' = (analysisStore: analysisStore, id: id): option<tipe> =>
  getLiveValue'(analysisStore, id) |> Option.map(~f=RT.typeOf)

let getTipeOf = (m: model, id: id, traceID: traceID): option<tipe> =>
  getLiveValue(m, id, traceID) |> Option.map(~f=RT.typeOf)

let getArguments = (m: model, tl: toplevel, callerID: id, traceID: traceID): option<list<dval>> =>
  switch TL.getAST(tl) {
  | Some(ast) =>
    let argIDs = ast |> AST.getArguments(callerID) |> List.map(~f=E.toID)
    let dvals = List.filterMap(argIDs, ~f=id => getLiveValue(m, id, traceID))

    if List.length(dvals) == List.length(argIDs) {
      Some(dvals)
    } else {
      None
    }
  | None => None
  }

@ocaml.doc(" [getAvailableVarnames m tl id traceID] gets a list of (varname, dval option)s that are in scope
 * at an expression with the given [id] within the ast of the [tl]. The dval for a given varname
 * comes from the trace with [traceID]. ")
let getAvailableVarnames = (m: model, tl: toplevel, id: id, traceID: traceID): list<(
  string,
  option<dval>,
)> => {
  /* TODO: Calling out is so slow that calculating on the fly is faster.
   * But we can also cache this so that's it's not in the display hot-path */
  let tlid = TL.id(tl)
  let traceDict =
    getTrace(m, tlid, traceID)
    |> Option.andThen(~f=((_tid, td)) => td |> Result.toOption)
    |> Option.andThen(~f=t => Some(t.input))
    |> Option.unwrap(~default=Belt.Map.String.empty)

  let varsFor = (ast: FluidAST.t) =>
    ast
    |> FluidAST.toExpr
    |> AST.variablesIn
    |> Map.get(~key=id)
    |> Option.unwrap(~default=Map.String.empty)
    |> Map.toList
    |> List.map(~f=((varname, id)) => (varname, getLiveValue(m, id, traceID)))

  let glob = TL.allGloballyScopedVarnames(m.dbs) |> List.map(~f=v => (v, Some(DDB(v))))

  let inputVariables =
    RT.inputVariables(tl) |> List.map(~f=varname => (
      varname,
      Belt.Map.String.get(traceDict, varname),
    ))

  switch tl {
  | TLHandler(h) => Belt.List.concatMany([varsFor(h.ast), glob, inputVariables])
  | TLFunc(fn) => Belt.List.concatMany([varsFor(fn.ufAST), glob, inputVariables])
  | TLPmFunc(_) | TLDB(_) | TLTipe(_) => list{}
  }
}

// ----------------------
// Which trace is selected
// ----------------------

let selectedTraceID = (tlTraceIDs: tlTraceIDs, traces: list<trace>, tlid: TLID.t): option<
  traceID,
> =>
  // We briefly do analysis on a toplevel which does not have an
  // analysis available, so be careful here.
  switch Map.get(~key=tlid, tlTraceIDs) {
  | Some(c) => Some(c)
  | None =>
    // if we don't have it, pick the first trace
    List.head(traces) |> Option.map(~f=Tuple2.first)
  }

let selectedTrace = (tlTraceIDs: tlTraceIDs, traces: list<trace>, tlid: TLID.t): option<trace> =>
  selectedTraceID(tlTraceIDs, traces, tlid) |> Option.andThen(~f=traceID =>
    List.find(~f=((id, _)) => id == traceID, traces)
  )

let setSelectedTraceID = (m: model, tlid: TLID.t, traceID: traceID): model => {
  let newCursors = Map.add(~key=tlid, ~value=traceID, m.tlTraceIDs)
  {...m, tlTraceIDs: newCursors}
}

let getSelectedTraceID = (m: model, tlid: TLID.t): option<traceID> => {
  let traces = getTraces(m, tlid)
  selectedTraceID(m.tlTraceIDs, traces, tlid)
}

// ----------------------
// Communication with server
// ----------------------
module ReceiveAnalysis = {
  let decode: Tea.Json.Decoder.t<Js.Json.t, performAnalysisResult> = {
    open Tea.Json.Decoder
    map(msg => msg, field("detail", Decoder(json => Tea_result.Ok(Obj.magic(json)))))
  }

  let listen = (~key, tagger) =>
    BrowserListeners.registerGlobal("receiveAnalysis", key, tagger, decode)
}

module ReceiveFetch = {
  let decode: Tea.Json.Decoder.t<Js.Json.t, fetchResult> = {
    open Tea.Json.Decoder
    map(msg => msg, field("detail", Decoder(json => Tea_result.Ok(Obj.magic(json)))))
  }

  let listen = (~key, tagger) =>
    BrowserListeners.registerGlobal("receiveFetch", key, tagger, decode)
}

module NewTracePush = {
  let decode = {
    open Tea.Json.Decoder
    let traceID = map((id): traceID => id, string)
    let tlids = list(map((tlid) => TLID.fromInt(tlid), Tea.Json.Decoder.int))
    field("detail", Native.Decoder.tuple2(traceID, tlids))
  }

  let listen = (~key, tagger) =>
    BrowserListeners.registerGlobal("newTracePush", key, tagger, decode)
}

module New404Push = {
  let decode = {
    open Tea.Json.Decoder
    field("detail", Decoders.wrapDecoder(Decoders.fof))
  }

  let listen = (~key, tagger) => BrowserListeners.registerGlobal("new404Push", key, tagger, decode)
}

module NewPresencePush = {
  let decode = {
    open Tea.Json.Decoder
    field("detail", list(Decoders.wrapDecoder(Decoders.presenceMsg)))
  }

  let listen = (~key, tagger) =>
    BrowserListeners.registerGlobal("newPresencePush", key, tagger, decode)
}

module AddOps = {
  let decode = {
    open Tea.Json.Decoder
    field("detail", Decoders.wrapDecoder(Decoders.addOpAPIPusherMsg))
  }

  let listen = (~key, tagger) => BrowserListeners.registerGlobal("addOp", key, tagger, decode)
}

module WorkerStatePush = {
  let decode = {
    open Tea.Json.Decoder
    field("detail", Decoders.wrapDecoder(Decoders.updateWorkerScheduleAPIResult))
  }

  let listen = (~key, tagger) =>
    BrowserListeners.registerGlobal("workerStatePush", key, tagger, decode)
}

// Request analysis
module RequestAnalysis = {
  @val @scope(("window", "Dark", "analysis"))
  external send: performAnalysisParams => unit = "requestAnalysis"
}

module Fetcher = {
  @val @scope(("window", "Dark", "fetcher"))
  external request: ((fetchContext, fetchRequest)) => unit = "fetch"
}

@val @scope(("window", "location")) external origin: string = "origin"

let contextFromModel = (m: model): fetchContext => {
  canvasName: m.canvasName,
  csrfToken: m.csrfToken,
  origin: origin,
}

let updateDBStats = (m, tlid) =>
  Sync.attempt(
    ~key="update-db-stats-" ++ TLID.toString(tlid),
    m,
    Tea_cmd.call(_ =>
      Fetcher.request((contextFromModel(m), DbStatsFetch({dbStatsTlids: list{tlid}})))
    ),
  )

let getWorkerStats = (m, tlid) =>
  Sync.attempt(
    ~key="get-worker-stats-" ++ TLID.toString(tlid),
    m,
    Tea_cmd.call(_ =>
      Fetcher.request((contextFromModel(m), WorkerStatsFetch({workerStatsTlid: tlid})))
    ),
  )

/* [mergeTraces ~selectedTraceIDs ~onConflict ~oldTraces ~newTraces]
 * returns the results of "merging" [oldTraces] and [newTraces] by merging
 * the list of traces for each handler in the following manner:
 *
 * - Start with the handler's old traces, replacing any that share the id of a new trace using the [onConflict] function
 * - For any remaining new traces for that handler, prepend them.
 * - Drop any traces in excess of a hardcoded limit (currently 10 to match stored_event.load_events,
 *   plus 1 for any selected trace),
 *   keeping the newest traces and any [selectedTraceIDs]
 *
 * Note that this preserves the order of the newTraces relative to the oldTraces
 * (except for newTraces that share an old id -- we preserve the order of those in oldTraces)
 * and we also preserve the relative order of the selected traces.
 */
let mergeTraces = (
  ~selectedTraceIDs: tlTraceIDs,
  ~onConflict: (trace, trace) => trace,
  ~oldTraces: traces,
  ~newTraces: traces,
): traces => {
  let maxTracesPerHandler = 10 /* shared with stored_event.load_events */ + 1
  Map.merge(oldTraces, newTraces, ~f=(tlid, oldList, newList) =>
    switch (oldList, newList) {
    | (None, None) => None
    | (Some(o), None) => Some(o)
    | (None, Some(n)) => Some(n)
    | (Some(o), Some(n)) =>
      /* Algorithm overview:
       * 1. merge the lists, updating the trace in the same position
       * if present, and adding it to the front otherwise.
       *
       * 2. drop any traces in excess of [maxTracesPerHandler]
       * from the back, making sure to preserve any selected traces.
       *
       ***
       * Example:
       *
       * o = [o1,o2,o3,o4,o5,o6,o7,o8,o9]
       * n = [n1,n2,n_3,n4,n5,n6]
       * n_3 shares an id with o3
       * selectedTrace in this handler = o8
       *
       * Pass 1 produces:
       * [n1,n2,n4,n5, o1,o2,n_3,o4,o5,o6,o7,o8,o9]
       * Pass 2 produces:
       * [n1,n2,n4,n5, o1,o2,n_3,o4,o5,o6, o8]
       *
       ***
       */
      // Pass 1: merge the lists, using foldr to preserve the order of n
      let merged = List.foldRight(n, ~initial=o, ~f=(acc, (newID, newData) as new_) => {
        let found = ref(false)
        let updated = List.map(acc, ~f=((oldID, oldData) as old) =>
          if oldID == newID {
            found := true
            onConflict(old, new_)
          } else {
            (oldID, oldData)
          }
        )

        if found.contents /* deref, not "not" */ {
          updated
        } else {
          list{(newID, newData), ...acc}
        }
      })

      /* Pass 2: preserve up to [maxTracesPerHandler] traces
       * guaranteeing that we won't duplicate the selected trace and
       * that we preserve the relative order
       */
      let selectedTraceID = Map.get(~key=tlid, selectedTraceIDs)

      let maxNonSelectedTraces = maxTracesPerHandler - 1
      let (preserved, _) = List.fold(merged, ~initial=(list{}, 0), ~f=(
        (acc, nonSelectedCount),
        (id, _) as currTrace,
      ) =>
        if selectedTraceID == Some(id) {
          (list{currTrace, ...acc}, nonSelectedCount)
        } else if nonSelectedCount < maxNonSelectedTraces {
          (list{currTrace, ...acc}, nonSelectedCount + 1)
        } else {
          (acc, nonSelectedCount)
        }
      )

      // We have to reverse because the fold prepended in reverse order
      Some(List.reverse(preserved))
    }
  )
}

let requestTrace = (~force=false, m, tlid, traceID): (model, Cmd.t<msg>) => {
  let should =
    // DBs + Types dont have traces
    TL.get(m, tlid)
    |> Option.map(~f=tl => !(TL.isDB(tl) || TL.isUserTipe(tl)))
    |> Option.unwrap(~default=false)

  if should {
    Sync.attempt(
      ~force,
      ~key="tracefetch-" ++ traceID,
      m,
      Tea_cmd.call(_ =>
        Fetcher.request((contextFromModel(m), TraceFetch({gtdrpTlid: tlid, gtdrpTraceID: traceID})))
      ),
    )
  } else {
    (m, Cmd.none)
  }
}

let requestAnalysis = (m, tlid, traceID): Cmd.t<msg> => {
  let dbs = Map.values(m.dbs)
  let userFns = Map.values(m.userFunctions)
  let userTipes = Map.values(m.userTipes)
  let trace = getTrace(m, tlid, traceID)
  let tl = TL.get(m, tlid)
  let secrets = m.secrets
  switch (tl, trace) {
  | (Some(TLHandler(h)), Some(_, Ok(traceData))) =>
    Tea_cmd.call(_ =>
      RequestAnalysis.send(
        AnalyzeHandler({
          handler: h,
          traceID: traceID,
          traceData: traceData,
          dbs: dbs,
          userFns: userFns,
          userTipes: userTipes,
          secrets: secrets,
        }),
      )
    )
  | (Some(TLFunc(f)), Some(_, Ok(traceData))) =>
    Tea_cmd.call(_ =>
      RequestAnalysis.send(
        AnalyzeFunction({
          func: f,
          traceID: traceID,
          traceData: traceData,
          dbs: dbs,
          userFns: userFns,
          userTipes: userTipes,
          secrets: secrets,
        }),
      )
    )
  | _ => Cmd.none
  }
}

let updateTraces = (m: model, traces: traces): model => {
  let newTraces = mergeTraces(
    ~selectedTraceIDs=m.tlTraceIDs,
    ~onConflict=((oldID, oldData), (newID, newData)) =>
      /* Update if:
       * - new data is ok (successful fetch)
       * - old data is an error, so is new data, but different errors
       * */
      if Result.isOk(newData) || (!Result.isOk(oldData) && oldData != newData) {
        (newID, newData)
      } else {
        (oldID, oldData)
      },
    ~oldTraces=m.traces,
    ~newTraces=traces,
  )

  {...m, traces: newTraces}
}

let analyzeFocused = (m: model): (model, Cmd.t<msg>) =>
  switch CursorState.tlidOf(m.cursorState) {
  | Some(tlid) =>
    let trace =
      getSelectedTraceID(m, tlid) |> Option.andThen(~f=traceID =>
        getTrace(m, tlid, traceID) |> Option.orElse(Some(traceID, Error(NoneYet)))
      )

    switch trace {
    | Some(
        _,
        Error(MaximumCallStackError),
      ) => /* Don't attempt to refetch if we blew the stack trying
       * to decode it */
      (m, Cmd.none)
    | Some(traceID, Error(NoneYet)) =>
      // Fetch the trace data if it's missing.
      requestTrace(m, tlid, traceID)
    | Some(traceID, Ok(_)) => // Run the analysis, if missing
      (m, requestAnalysis(m, tlid, traceID))
    | None => (m, Cmd.none)
    }
  | None => (m, Cmd.none)
  }
