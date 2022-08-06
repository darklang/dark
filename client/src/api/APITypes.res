@ppx.deriving(show({with_path: false}))
type rec unlockedDBs = TLID.Set.t

and fetchRequest =
  | TraceFetch(APITraces.TraceData.Params.t)
  | DbStatsFetch(APIDBs.DBStats.Params.t)
  | WorkerStatsFetch(APIWorkers.WorkerStats.Params.t)

// traces/db_stats fetching
and fetchResult =
  | TraceFetchSuccess(APITraces.TraceData.Params.t, APITraces.TraceData.t)
  | TraceFetchFailure(APITraces.TraceData.Params.t, string, string)
  | TraceFetchMissing(APITraces.TraceData.Params.t)
  | DbStatsFetchSuccess(APIDBs.DBStats.Params.t, APIDBs.DBStats.t)
  | DbStatsFetchFailure(APIDBs.DBStats.Params.t, string, string)
  | DbStatsFetchMissing(APIDBs.DBStats.Params.t)
  | WorkerStatsFetchSuccess(APIWorkers.WorkerStats.Params.t, APIWorkers.WorkerStats.t)
  | WorkerStatsFetchFailure(APIWorkers.WorkerStats.Params.t, string, string)
  | WorkerStatsFetchMissing(APIWorkers.WorkerStats.Params.t)

and fetchContext = {
  canvasName: string,
  csrfToken: string,
  origin: string,
}
