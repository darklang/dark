open Prelude
open Json.Encode

// Dark

let ops = (ops: list<PT.Op.t>): Js.Json.t =>
  list(
    PT.Op.encode,
    switch ops {
    | list{UndoTL(_)} => ops
    | list{RedoTL(_)} => ops
    | list{} => ops
    | _ =>
      let savepoints = List.map(~f=op => PT.Op.TLSavepoint(PT.Op.tlidOf(op)), ops)

      Belt.List.concat(savepoints, ops)
    },
  )

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
    module StringMap = Caml.Map.Make(Tc.Caml.String)
    object_(list{
      ("url", string(r.url)),
      (
        "status",
        object_(list{("code", int(r.status.code)), ("message", string(r.status.message))}),
      ),
      (
        "headers",
        r.headers |> StringMap.bindings |> List.map(~f=((k, v)) => (k, string(v))) |> object_,
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
