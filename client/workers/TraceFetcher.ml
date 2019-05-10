external rollbarConfig : string = "rollbarConfig" [@@bs.val]

let () = Rollbar.init (Json.parseOrRaise rollbarConfig)

type event = < data : Types.fetchContext * Types.fetchRequest [@bs.get] > Js.t

type self

external self : self = "self" [@@bs.val]

external onmessage : self -> (event -> unit) -> unit = "" [@@bs.set]

type pushResult = Types.traceFetchResult

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
         else resolve resp )
  |> then_ Fetch.Response.json
  |> then_ (fun resp ->
         let result = decoder resp in
         resolve (postMessage self (on_success result)) )
  |> catch (fun err ->
         (* Js.Promise.error is opaque, and we just put this in here *)
         match Obj.magic err with
         | NoneFound ->
             Js.log "Received no response from fetch" ;
             resolve (postMessage self (on_missing (Obj.magic err)##message))
         | BadAuthorization resp ->
             Fetch.Response.text resp
             |> then_ (fun body -> resolve (postMessage self (on_failure body)))
         | _ ->
             Js.log2 "fetch error" err ;
             resolve (postMessage self (on_failure (Obj.magic err)##message))
     )


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
        ~decoder:Decoders.getTraceDataRPCResult
        ~on_success:(fun r -> TraceFetchSuccess (gdtp, r))
        ~on_missing:(fun _ -> TraceFetchMissing gdtp)
        ~on_failure:(fun r -> TraceFetchFailure (gdtp, url, r))
        url
        context
        (Encoders.getTraceDataRPCParams gdtp)
  | DbStatsFetch dbsParams ->
      let url =
        context.prefix
        ^ context.origin
        ^ "/api/"
        ^ context.canvasName
        ^ "/db_stats"
      in
      fetch_
        ~decoder:Decoders.dbStatsRPCResult
        ~on_success:(fun r -> DbStatsFetchSuccess (dbsParams, r))
        ~on_missing:(fun r -> DbStatsFetchFailure (dbsParams, url, r))
        ~on_failure:(fun r -> DbStatsFetchFailure (dbsParams, url, r))
        url
        context
        (Encoders.dbStatsRPCParams dbsParams)


let () =
  onmessage self (fun e ->
      let context, request = e##data in
      ignore (fetch context request) )
