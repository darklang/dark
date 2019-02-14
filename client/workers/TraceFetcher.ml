type event =
  < data : Types.traceFetchContext * Types.getTraceDataRPCParams [@bs.get] >
  Js.t

type self

external self : self = "self" [@@bs.val]

external onmessage : self -> (event -> unit) -> unit = "" [@@bs.set]

type pushResult = Types.traceFetchResult

external postMessage : self -> pushResult -> unit = "postMessage" [@@bs.send]

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
  |> then_ Fetch.Response.json
  |> then_ (fun resp ->
         let result = Decoders.getTraceDataRPCResult resp in
         resolve (postMessage self (TraceFetchSuccess {params; result})) )
  |> catch (fun err ->
         Js.log2 "traceFetch error" err ;
         resolve
           (postMessage self (TraceFetchFailure ("Failure fetching: " ^ url)))
     )


let () =
  onmessage self (fun e ->
      let context, params = e##data in
      ignore (fetch context params) )
