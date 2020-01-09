open Prelude

external rollbarConfig : string = "rollbarConfig" [@@bs.val]

let () = Rollbar.init (Json.parseOrRaise rollbarConfig)

type event = < data : Types.fetchContext * Types.fetchRequest [@bs.get] > Js.t

type self

external self : self = "self" [@@bs.val]

external onmessage : self -> (event -> unit) -> unit = "onmessage" [@@bs.set]

type pushResult = Types.fetchResult

external postMessage : self -> pushResult -> unit = "postMessage" [@@bs.send]

exception NoneFound

exception BadAuthorization of Fetch.response

let fetch_
    ~decoder
    ~on_success
    ~on_missing
    ~on_failure
    url
    (context : Types.fetchContext)
    data =
  let open Js.Promise in
  Fetch.fetchWithInit
    url
    (Fetch.RequestInit.make
       ~method_:Post
       ~body:(Fetch.BodyInit.make (Js.Json.stringify data))
       ~headers:
         (Fetch.HeadersInit.makeWithDict
            (Js.Dict.fromList
               [ ("Content-Type", "application/json")
               ; ("X-CSRF-TOKEN", context.csrfToken) ]))
       ())
  |> then_ (fun (resp : Fetch.response) ->
         (* The result not be there because we haven't saved the handler yet.
          * In that case, return TraceFetchMissing so we can try again. *)
         if Fetch.Response.status resp = 404
         then reject NoneFound
         else if Fetch.Response.status resp = 401
         then reject (BadAuthorization resp)
         else resolve resp)
  |> then_ Fetch.Response.json
  |> then_ (fun resp ->
         let result = decoder resp in
         resolve (postMessage self (on_success result)))
  |> catch (fun err ->
         (* Js.Promise.error is opaque, and we just put this in here *)
         match Obj.magic err with
         | NoneFound ->
             (* Note: there's no user facing error here, we just want to try
            * again, which is triggered by on_missing. So we don't want a
            * reportError call here - that'll cause a rollbar flood. See comment
            * above re: 404s. *)
             resolve (postMessage self (on_missing (Obj.magic err)##message))
         | BadAuthorization resp ->
             Fetch.Response.text resp
             |> then_ (fun body -> resolve (postMessage self (on_failure body)))
         | _ ->
             let message = (Obj.magic err)##message in
             let message =
               if String.endsWith
                    ~suffix:"Maximum call stack size exceeded"
                    message
               then
                 "Selected trace too large for the editor to load, maybe try another?"
               else message
             in
             (* data here is jsonified params; ex: for a get_trace_data failure,
              * it contains a tlid and a trace id *)
             reportError "fetch error" (url, err, data) ;
             resolve (postMessage self (on_failure message)))


let fetch (context : Types.fetchContext) (request : Types.fetchRequest) =
  match request with
  | TraceFetch gdtp ->
      let url =
        context.prefix
        ^ context.origin
        ^ "/api/"
        ^ context.canvasName
        ^ "/get_trace_data"
      in
      fetch_
        ~decoder:Decoders.getTraceDataAPIResult
        ~on_success:(fun r -> TraceFetchSuccess (gdtp, r))
        ~on_missing:(fun _ -> TraceFetchMissing gdtp)
        ~on_failure:(fun r -> TraceFetchFailure (gdtp, url, r))
        url
        context
        (Encoders.getTraceDataAPIParams gdtp)
  | DbStatsFetch dbsParams ->
      let url =
        context.prefix
        ^ context.origin
        ^ "/api/"
        ^ context.canvasName
        ^ "/get_db_stats"
      in
      fetch_
        ~decoder:Decoders.dbStatsAPIResult
        ~on_success:(fun r -> DbStatsFetchSuccess (dbsParams, r))
        ~on_missing:(fun _ -> DbStatsFetchMissing dbsParams)
        ~on_failure:(fun r -> DbStatsFetchFailure (dbsParams, url, r))
        url
        context
        (Encoders.dbStatsAPIParams dbsParams)
  | WorkerStatsFetch workerParams ->
      let url =
        context.prefix
        ^ context.origin
        ^ "/api/"
        ^ context.canvasName
        ^ "/get_worker_stats"
      in
      fetch_
        ~decoder:Decoders.workerStatsAPIResult
        ~on_success:(fun r -> WorkerStatsFetchSuccess (workerParams, r))
        ~on_missing:(fun _ -> WorkerStatsFetchMissing workerParams)
        ~on_failure:(fun r -> WorkerStatsFetchFailure (workerParams, url, r))
        url
        context
        (Encoders.workerStatsAPIParams workerParams)


let () =
  onmessage self (fun e ->
      let context, request = e##data in
      ignore (fetch context request))
