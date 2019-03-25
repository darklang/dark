external rollbarConfig : string = "rollbarConfig" [@@bs.val]

let () = Rollbar.init (Json.parseOrRaise rollbarConfig)

type event =
  < data : Types.traceFetchContext * Types.getTraceDataRPCParams [@bs.get] >
  Js.t

type self

external self : self = "self" [@@bs.val]

external onmessage : self -> (event -> unit) -> unit = "" [@@bs.set]

type pushResult = Types.traceFetchResult

external postMessage : self -> pushResult -> unit = "postMessage" [@@bs.send]

exception NoneFound

exception BadAuthorization of Fetch.response

let fetch (context : Types.traceFetchContext) params =
  let open Js.Promise in
  let url =
    context.prefix
    ^ context.origin
    ^ "/api/"
    ^ context.canvasName
    ^ "/get_trace_data"
  in
  Fetch.fetchWithInit
    url
    (Fetch.RequestInit.make
       ~method_:Post
       ~body:
         (Fetch.BodyInit.make
            (Js.Json.stringify (Encoders.getTraceDataRPCParams params)))
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
         let result = Decoders.getTraceDataRPCResult resp in
         resolve (postMessage self (TraceFetchSuccess (params, result))) )
  |> catch (fun err ->
         (* Js.Promise.error is opaque, and we just put this in here *)
         match Obj.magic err with
         | NoneFound ->
             Js.log "Received no trace" ;
             resolve (postMessage self (TraceFetchMissing params))
         | BadAuthorization resp ->
             Fetch.Response.text resp
             |> then_ (fun body ->
                    resolve
                      (postMessage self (TraceFetchFailure (params, url, body)))
                )
         | _ ->
             Js.log2 "traceFetch error" err ;
             resolve
               (postMessage
                  self
                  (TraceFetchFailure (params, url, (Obj.magic err)##message)))
     )


let () =
  onmessage self (fun e ->
      let context, params = e##data in
      ignore (fetch context params) )
