open Prelude
open Json.Encode

// Dark

let httpError = (e: Tea.Http.error<string>): Js.Json.t => {
  module Http = Tea.Http
  let responseBody = (r: Http.responseBody) =>
    switch r {
    | NoResponse => object_(list{("noResponse", null)})
    | StringResponse(str) => string(str)
    | ArrayBufferResponse() => object_(list{("arrayBufferResponse", null)})
    | BlobResponse() => object_(list{("blobResponse", null)})
    | DocumentResponse(_) => object_(list{("documentResponse", string("<documentResponse>"))})
    | JsonResponse(json) => json
    | TextResponse(text) => string(text)
    | RawResponse(str, ()) => object_(list{("rawResponse", string(str))})
    }

  let response = (r: Http.response) => {
    module StringMap = Belt.Map.String
    object_(list{
      ("url", string(r.url)),
      (
        "status",
        object_(list{("code", int(r.status.code)), ("message", string(r.status.message))}),
      ),
      (
        "headers",
        r.headers |> StringMap.toList |> List.map(~f=((k, v)) => (k, string(v))) |> object_,
      ),
      ("body", responseBody(r.body)),
    })
  }

  switch e {
  | Http.BadUrl(url) => object_(list{("type", string("BadUrl")), ("url", string(url))})
  | Http.Timeout => object_(list{("type", string("Timeout"))})
  | Http.NetworkError => object_(list{("type", string("NetworkError"))})
  | Http.BadStatus(r) => object_(list{("type", string("BadStatus")), ("response", response(r))})
  | Http.BadPayload(msg, r) =>
    object_(list{
      ("type", string("BadPayload")),
      ("message", string(msg)),
      ("response", response(r)),
    })
  | Http.Aborted => object_(list{("type", string("Aborted"))})
  }
}
