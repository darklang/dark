/// StdLib functions to manage and retrieve static assets of Dark users
module BackendOnlyStdLib.LibStaticAssets

open System.Threading.Tasks
open FSharp.Control.Tasks

open LibExecution.RuntimeTypes
open Prelude

module SA = LibBackend.StaticAssets
module Errors = LibExecution.Errors

let fn = FQFnName.stdlibFnName

let err (str : string) = Ply(Dval.errStr str)

let incorrectArgs = Errors.incorrectArgs

open System.IO
open System.IO.Compression
open System.Net.Http

let httpClient =
  let socketHandler : HttpMessageHandler =
    let handler = new SocketsHttpHandler()

    // Cookies shouldn't be necessary
    handler.UseCookies <- false
    handler

  let client = new HttpClient(socketHandler, disposeHandler = false)
  client.Timeout <- System.TimeSpan.FromSeconds 30.0
  // Can't find what this was in OCaml/Curl, but 100MB seems a reasonable default
  client.MaxResponseContentBufferSize <- 1024L * 1024L * 100L
  client




/// Replaces legacy HttpClientv0. Returns bytes, and throws on non-200s or if
/// anything goes wrong.
let getV0 (url : string) : Task<byte []> =
  task {
    try
      use req = new HttpRequestMessage(HttpMethod.Get, url)
      let! response = httpClient.SendAsync req
      let code = int response.StatusCode
      let! body = response.Content.ReadAsByteArrayAsync()
      if code < 200 || code >= 300 then
        return Exception.raiseCode $"Bad HTTP response ({code}) in call to {url}"
      else
        return body
    with
    | e -> return Exception.raiseCode $"Internal HTTP-stack exception: {e.Message}"
  }

/// Replaces legacy HttpClientv1. Returns bytes, headers, and status code, and throws
/// on non-200s or if anything goes wrong.
let getV1 (url : string) : Task<byte [] * List<string * string> * int> =
  task {
    try
      use req = new HttpRequestMessage(HttpMethod.Get, url)
      let! response = httpClient.SendAsync req
      let code = int response.StatusCode
      let headers = HttpHeaders.headersForAspNetResponse response
      let! body = response.Content.ReadAsByteArrayAsync()
      if code < 200 || code >= 300 then
        return Exception.raiseCode $"Bad HTTP response ({code}) in call to {url}"
      else
        return body, headers, code
    with
    | e -> return Exception.raiseCode $"Internal HTTP-stack exception: {e.Message}"
  }

/// Replaces legacy HttpClientv2. Returns bytes, headers, and status code, and throws
/// if the request has issues. Does not raise on non-200 status code.
let getV2 (url : string) : Task<byte [] * List<string * string> * int> =
  task {
    try
      use req = new HttpRequestMessage(HttpMethod.Get, url)
      let! response = httpClient.SendAsync req
      let code = int response.StatusCode
      let headers = HttpHeaders.headersForAspNetResponse response
      let! body = response.Content.ReadAsByteArrayAsync()
      return body, headers, code
    with
    | e -> return Exception.raiseCode $"Internal HTTP-stack exception: {e.Message}"

  }


let fns : List<BuiltInFn> =
  [ { name = fn "StaticAssets" "baseUrlFor" 0
      parameters = [ Param.make "deploy_hash" TStr "" ]
      returnType = TStr
      description = "Return the baseUrl for the specified deploy hash"
      fn =
        (function
        | state, [ DStr deployHash ] ->
          SA.url state.program.canvasName deployHash SA.Short |> DStr |> Ply
        | _ -> incorrectArgs ())
      sqlSpec = NotQueryable
      // CLEANUP may be marked as ImpurePreviewable
      previewable = Impure
      deprecated = NotDeprecated }


    { name = fn "StaticAssets" "baseUrlForLatest" 0
      parameters = []
      returnType = TStr
      description = "Return the baseUrl for the latest deploy"
      fn =
        (function
        | state, [] ->
          uply {
            // CLEANUP calling this with no deploy hash generates an error
            let! deployHash = SA.latestDeployHash state.program.canvasID
            let url = SA.url state.program.canvasName deployHash SA.Short
            return DStr url
          }
        | _ -> incorrectArgs ())
      sqlSpec = NotQueryable
      previewable = Impure
      deprecated = NotDeprecated }


    { name = fn "StaticAssets" "urlFor" 0
      parameters = [ Param.make "deploy_hash" TStr ""; Param.make "file" TStr "" ]
      returnType = TStr
      description = "Return a url for the specified file and deploy hash"
      fn =
        (function
        | state, [ DStr deployHash; DStr file ] ->
          SA.urlFor state.program.canvasName deployHash SA.Short file |> DStr |> Ply
        | _ -> incorrectArgs ())
      sqlSpec = NotQueryable
      // CLEANUP may be marked as ImpurePreviewable
      previewable = Impure
      deprecated = NotDeprecated }


    { name = fn "StaticAssets" "urlForLatest" 0
      parameters = [ Param.make "file" TStr "" ]
      returnType = TStr
      description = "Return a url for the specified file and latest deploy"
      fn =
        (function
        | state, [ DStr file ] ->
          uply {
            let! deployHash = SA.latestDeployHash state.program.canvasID
            let url = SA.urlFor state.program.canvasName deployHash SA.Short file
            return DStr url
          }
        | _ -> incorrectArgs ())
      sqlSpec = NotQueryable
      previewable = Impure
      deprecated = NotDeprecated }


    { name = fn "StaticAssets" "fetch" 0
      parameters = [ Param.make "deploy_hash" TStr ""; Param.make "file" TStr "" ]
      returnType = TResult(TStr, TStr)
      description =
        "Return the specified file from the deploy_hash - only works on UTF8-safe files for now"
      fn =
        (function
        | state, [ DStr deployHash; DStr file ] ->
          uply {
            let url = SA.urlFor state.program.canvasName deployHash SA.Short file

            let! response = getV0 url
            match UTF8.ofBytesOpt response with
            | Some dv -> return DResult(Ok(DStr dv))
            | None -> return DResult(Error(DStr "Response was not UTF-8 safe"))
          }
        | _ -> incorrectArgs ())
      sqlSpec = NotQueryable
      previewable = Impure
      deprecated = ReplacedBy(fn "StaticAssets" "fetch" 1) }


    { name = fn "StaticAssets" "fetch" 1
      parameters = [ Param.make "deploy_hash" TStr ""; Param.make "file" TStr "" ]
      returnType = TResult(TStr, TStr)
      description =
        "Return the specified file from the deploy_hash - only works on UTF8-safe files for now"
      fn =
        (function
        | state, [ DStr deployHash; DStr file ] ->
          uply {
            let url = SA.urlFor state.program.canvasName deployHash SA.Short file
            let! response = getV0 url
            match UTF8.ofBytesOpt response with
            | Some dv -> return DResult(Ok(DStr dv))
            | None -> return (DResult(Error(DStr "Response was not UTF-8 safe")))
          }
        | _ -> incorrectArgs ())
      sqlSpec = NotQueryable
      previewable = Impure
      deprecated = NotDeprecated }


    { name = fn "StaticAssets" "fetchBytes" 0
      parameters = [ Param.make "deploy_hash" TStr ""; Param.make "file" TStr "" ]
      returnType = TResult(TBytes, TStr)
      description = "Return the bytes of the specified file from the deploy_hash"
      fn =
        (function
        | state, [ DStr deployHash; DStr file ] ->
          uply {
            let url = SA.urlFor state.program.canvasName deployHash SA.Short file
            let! response, _, _ = getV1 url
            return DResult(Ok(DBytes response))
          }
        | _ -> incorrectArgs ())
      sqlSpec = NotQueryable
      previewable = Impure
      deprecated = NotDeprecated }


    { name = fn "StaticAssets" "fetchLatest" 0
      parameters = [ Param.make "file" TStr "" ]
      returnType = TResult(TStr, TStr)
      description =
        "Return the specified file from the latest deploy - only works on UTF8-safe files for now"
      fn =
        (function
        | state, [ DStr file ] ->
          uply {
            let! deployHash = SA.latestDeployHash state.program.canvasID
            let url = SA.urlFor state.program.canvasName deployHash SA.Short file
            let! response = getV0 url
            match UTF8.ofBytesOpt response with
            | Some str -> return DResult(Ok(DStr str))
            | None -> return DResult(Error(DStr "Response was not UTF-8 safe"))
          }
        | _ -> incorrectArgs ())
      sqlSpec = NotQueryable
      previewable = Impure
      deprecated = ReplacedBy(fn "StaticAssets" "fetchLatest" 1) }


    { name = fn "StaticAssets" "fetchLatest" 1
      parameters = [ Param.make "file" TStr "" ]
      returnType = TResult(TStr, TStr)
      description =
        "Return the specified file from the latest deploy - only works on UTF8-safe files for now"
      fn =
        (function
        | state, [ DStr file ] ->
          uply {
            let! deployHash = SA.latestDeployHash state.program.canvasID
            let url = SA.urlFor state.program.canvasName deployHash SA.Short file
            let! response = getV0 url
            match UTF8.ofBytesOpt response with
            | Some str -> return Dval.resultOk (DStr str)
            | None -> return Dval.resultError (DStr "Response was not UTF-8 safe")
          }
        | _ -> incorrectArgs ())
      sqlSpec = NotQueryable
      previewable = Impure
      deprecated = NotDeprecated }


    { name = fn "StaticAssets" "fetchLatestBytes" 0
      parameters = [ Param.make "file" TStr "" ]
      returnType = TResult(TBytes, TStr)
      description = "Return the bytes of the specified file from the latest deploy"
      fn =
        (function
        | state, [ DStr file ] ->
          uply {
            let! deployHash = SA.latestDeployHash state.program.canvasID
            let url = SA.urlFor state.program.canvasName deployHash SA.Short file
            let! response, _, _ = getV1 url
            return DResult(Ok(DBytes(response)))
          }
        | _ -> incorrectArgs ())
      sqlSpec = NotQueryable
      previewable = Impure
      deprecated = NotDeprecated }


    { name = fn "StaticAssets" "serve" 0
      parameters = [ Param.make "deploy_hash" TStr ""; Param.make "file" TStr "" ]
      returnType = TResult(THttpResponse TStr, TStr)
      description =
        "Return the specified file from the latest deploy - only works on UTF8-safe files for now"
      fn =
        (function
        | state, [ DStr deployHash; DStr file ] ->
          uply {
            let url = SA.urlFor state.program.canvasName deployHash SA.Short file
            let! (body, headers, code) = getV2 url
            let headers =
              headers
              |> List.map (fun (k, v) -> (k, v.Trim()))
              |> List.filter (fun (k, v) -> not (k.Contains("Content-Length")))
              |> List.filter (fun (k, v) -> not (k.Contains("Transfer-Encoding")))
              |> List.filter (fun (k, v) -> not (k.Contains("Cache-Control")))
              |> List.filter (fun (k, v) -> not (k.Trim() = ""))
              |> List.filter (fun (k, v) -> not (v.Trim() = ""))
            match UTF8.ofBytesOpt body with
            | Some str ->
              return DResult(Ok(DHttpResponse(Response(code, headers, DStr str))))
            | None -> return DResult(Error(DStr "Response was not UTF-8 safe"))
          }
        | _ -> incorrectArgs ())
      sqlSpec = NotQueryable
      previewable = Impure
      deprecated = ReplacedBy(fn "StaticAssets" "serve" 1) }


    { name = fn "StaticAssets" "serve" 1
      parameters = [ Param.make "deploy_hash" TStr ""; Param.make "file" TStr "" ]
      returnType = TResult(THttpResponse TBytes, TStr)
      description = "Return the specified file from the latest deploy"
      fn =
        (function
        | state, [ DStr deployHash; DStr file ] ->
          uply {
            let url = SA.urlFor state.program.canvasName deployHash SA.Short file
            let! (body, headers, code) = getV2 url
            let headers =
              headers
              |> List.map (fun (k, v) -> (k, v.Trim()))
              |> List.filter (fun (k, v) -> not (k.Contains "Content-Length"))
              |> List.filter (fun (k, v) -> not (k.Contains "Transfer-Encoding"))
              |> List.filter (fun (k, v) -> not (k.Contains "Cache-Control"))
              |> List.filter (fun (k, v) -> not (k.Trim() = ""))
              |> List.filter (fun (k, v) -> not (v.Trim() = ""))
            return DResult(Ok(DHttpResponse(Response(code, headers, DBytes(body)))))
          }
        | _ -> incorrectArgs ())
      sqlSpec = NotQueryable
      previewable = Impure
      deprecated = NotDeprecated }


    { name = fn "StaticAssets" "serveLatest" 0
      parameters = [ Param.make "file" TStr "" ]
      returnType = TResult(TStr, TStr)
      description =
        "Return the specified file from the latest deploy - only works on UTF8-safe files for now"
      fn =
        (function
        | state, [ DStr file ] ->
          uply {
            let! deployHash = SA.latestDeployHash state.program.canvasID
            let url = SA.urlFor state.program.canvasName deployHash SA.Short file
            let! body, headers, code = getV2 url
            let headers =
              headers
              |> List.map (fun (k, v) -> (k, v.Trim()))
              |> List.filter (fun (k, v) -> not (k.Contains "Content-Length"))
              |> List.filter (fun (k, v) -> not (k.Contains "Transfer-Encoding"))
              |> List.filter (fun (k, v) -> not (k.Contains "Cache-Control"))
              |> List.filter (fun (k, v) -> not (k.Trim() = ""))
              |> List.filter (fun (k, v) -> not (v.Trim() = ""))
            return DResult(Ok(DHttpResponse(Response(code, headers, DBytes body))))
          }
        | _ -> incorrectArgs ())
      sqlSpec = NotQueryable
      previewable = Impure
      deprecated = ReplacedBy(fn "StaticAssets" "serveLatest" 1) }


    { name = fn "StaticAssets" "serveLatest" 1
      parameters = [ Param.make "file" TStr "" ]
      returnType = TResult(THttpResponse(TBytes), TStr)
      description = "Return the specified file from the latest deploy"
      fn =
        (function
        | state, [ DStr file ] ->
          uply {
            let! deployHash = SA.latestDeployHash state.program.canvasID
            let url = SA.urlFor state.program.canvasName deployHash SA.Short file
            let! body, headers, code = getV2 url
            let headers =
              headers
              |> List.map (fun (k, v) -> (k, v.Trim()))
              |> List.filter (fun (k, v) -> not (k.Contains "Content-Length"))
              |> List.filter (fun (k, v) -> not (k.Contains "Transfer-Encoding"))
              |> List.filter (fun (k, v) -> not (k.Contains "Cache-Control"))
              |> List.filter (fun (k, v) -> not (k.Trim() = ""))
              |> List.filter (fun (k, v) -> not (v.Trim() = ""))
            return DResult(Ok(DHttpResponse(Response(code, headers, DBytes(body)))))
          }
        | _ -> incorrectArgs ())
      sqlSpec = NotQueryable
      previewable = Impure
      deprecated = NotDeprecated } ]
