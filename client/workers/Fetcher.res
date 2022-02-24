open Prelude

@val external rollbarConfig: string = "rollbarConfig"

@val external buildHash: string = "buildHash"

let () = Rollbar.init(Json.parseOrRaise(rollbarConfig))

type event = {@get "data": (Types.fetchContext, Types.fetchRequest)}

type self

@val external self: self = "self"

@set external onmessage: (self, event => unit) => unit = "onmessage"

type pushResult = Types.fetchResult

@bs.send external postMessage: (self, pushResult) => unit = "postMessage"

exception NoneFound

exception BadAuthorization(Fetch.response)

let fetch_ = (
  ~decoder,
  ~on_success,
  ~on_missing,
  ~on_failure,
  url,
  context: Types.fetchContext,
  data,
) => {
  open Js.Promise
  Fetch.fetchWithInit(
    url,
    Fetch.RequestInit.make(
      ~method_=Post,
      ~body=Fetch.BodyInit.make(Js.Json.stringify(data)),
      ~headers=Fetch.HeadersInit.makeWithDict(
        Js.Dict.fromList(list{
          ("Content-Type", "application/json"),
          ("X-CSRF-TOKEN", context.csrfToken),
          (Header.client_version, buildHash),
        }),
      ),
      (),
    ),
  )
  |> then_((resp: Fetch.response) =>
    /* The result not be there because we haven't saved the handler yet.
     * In that case, return TraceFetchMissing so we can try again. */
    if Fetch.Response.status(resp) == 404 {
      reject(NoneFound)
    } else if Fetch.Response.status(resp) == 401 {
      reject(BadAuthorization(resp))
    } else {
      resolve(resp)
    }
  )
  |> then_(Fetch.Response.json)
  |> then_(resp => {
    let result = decoder(resp)
    resolve(postMessage(self, on_success(result)))
  })
  |> catch(err =>
    /* Js.Promise.error is opaque, and we just put this in here */
    switch Obj.magic(err) {
    | NoneFound =>
      /* Note: there's no user facing error here, we just want to try
       * again, which is triggered by on_missing. So we don't want a
       * reportError call here - that'll cause a rollbar flood. See comment
       * above re: 404s. */
      resolve(postMessage(self, on_missing(Obj.magic(err)["message"])))
    | BadAuthorization(resp) =>
      Fetch.Response.text(resp) |> then_(body => resolve(postMessage(self, on_failure(body))))
    | _ =>
      let message = Obj.magic(err)["message"]
      let message = if String.endsWith(~suffix="Maximum call stack size exceeded", message) {
        "Selected trace too large for the editor to load, maybe try another?"
      } else {
        message
      }

      /* data here is jsonified params; ex: for a get_trace_data failure,
       * it contains a tlid and a trace id */
      reportError("fetch error", (url, err, data))
      resolve(postMessage(self, on_failure(message)))
    }
  )
}

let fetch = (context: Types.fetchContext, request: Types.fetchRequest) => {
  let urlRoot = context.origin ++ context.apiRoot ++ context.canvasName
  switch request {
  | TraceFetch(gdtp) =>
    let url = urlRoot ++ "/get_trace_data"

    fetch_(
      ~decoder=Decoders.getTraceDataAPIResult,
      ~on_success=r => TraceFetchSuccess(gdtp, r),
      ~on_missing=_ => TraceFetchMissing(gdtp),
      ~on_failure=r => TraceFetchFailure(gdtp, url, r),
      url,
      context,
      Encoders.getTraceDataAPIParams(gdtp),
    )
  | DbStatsFetch(dbsParams) =>
    let url = urlRoot ++ "/get_db_stats"

    fetch_(
      ~decoder=Decoders.dbStatsAPIResult,
      ~on_success=r => DbStatsFetchSuccess(dbsParams, r),
      ~on_missing=_ => DbStatsFetchMissing(dbsParams),
      ~on_failure=r => DbStatsFetchFailure(dbsParams, url, r),
      url,
      context,
      Encoders.dbStatsAPIParams(dbsParams),
    )
  | WorkerStatsFetch(workerParams) =>
    let url = urlRoot ++ "/get_worker_stats"

    fetch_(
      ~decoder=Decoders.workerStatsAPIResult,
      ~on_success=r => WorkerStatsFetchSuccess(workerParams, r),
      ~on_missing=_ => WorkerStatsFetchMissing(workerParams),
      ~on_failure=r => WorkerStatsFetchFailure(workerParams, url, r),
      url,
      context,
      Encoders.workerStatsAPIParams(workerParams),
    )
  }
}

let () = onmessage(self, e => {
  let (context, request) = e["data"]
  ignore(fetch(context, request))
})
