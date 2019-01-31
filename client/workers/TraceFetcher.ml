type event = < data : Types.rpcContext * Types.analysisParams [@bs.get] > Js.t

type self

external self : self = "self" [@@bs.val]

external onmessage : self -> (event -> unit) -> unit = "" [@@bs.set]

type pushResult =
  { params : Types.analysisParams
  ; result : Types.getAnalysisResult }

external postMessage : self -> pushResult -> unit = "postMessage" [@@bs.send]

let fetch (context : Types.rpcContext) params =
  let open Js.Promise in
  let url = context.origin ^ "/api/" ^ context.canvasName ^ "/get_analysis" in
  Fetch.fetchWithInit
    url
    (Fetch.RequestInit.make
       ~method_:Post
       ~body:
         (Fetch.BodyInit.make
            (Js.Json.stringify (Encoders.analysisParams params)))
       ~headers:
         (Fetch.HeadersInit.makeWithDict
            (Js.Dict.fromList
               [ ("Content-Type", "application/json")
               ; ("X-CSRF-TOKEN", context.csrfToken) ]))
       ())
  |> then_ Fetch.Response.json
  |> then_ (fun resp -> resolve (Decoders.getAnalysisRPC resp))
  |> then_ (fun res -> resolve (postMessage self {params; result = res}))


let () =
  onmessage self (fun e ->
      let context, params = e##data in
      let _ = fetch context params in
      () )
