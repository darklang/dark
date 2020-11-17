module LibExecution.LibMiddleware

open System.Threading.Tasks
open FSharp.Control.Tasks
open LibExecution.Runtime
open FSharpPlus
open Prelude

let fn = FnDesc.stdFnDesc

let varA = TVariable "a"
let varB = TVariable "b"

// Middlewares are based on Ring: https://github.com/mmcgrana/ring/blob/master/SPEC

// Middlewares are typed functions that contribute a small, composible part of
// decoding a web request for the handler to use. Middlewares receive a
// request, and then based on the request, may choose to call the next
// middleware or simply return a response instead. As such, middlewares
// receive as parameters both the request so far, as well as the next
// middleware to call. They are responsible for calling the next middleware, possibly changing the request first and possible altering the response as well.
// This leads to middlewares having the following shape:

// let myMiddleware (arg : myMiddlewareArgType) next =
//   fun (req : 'req) ->
//     let doSomethingToRequest req = { req with someExtraField = someFunction req }
//     let doSomethingToResponse res = { res with someExtraField = someFunction res }
//     let shortCircuitResponse = { status = 404, body = "", headers = [] }
//     if someCondition req
//     then shortCircuitResponse
//     else
//       req
//       |> doSomethingToRequest
//       |> nextMiddleware
//       |> doSomethingToResponse

// A middleware returns a function which takes a request. A middleware takes
// whatever arguments it needs, as well as the next middleware to call. As
// such, a middleware stack looks like this:

// let middleware =
//   (\ctx -> handler ctx) // shown like this for clarity
//   |> addQueryParams url
//   |> addHeaders headers
//   |> readVarsFromURL
//   |> addJsonBody headers body
//   |> addFormBody headers body
//   |> addCookies headers
//   |> processErrorRail
//   |> optionsHanderMiddleware
//   |> headHandlerMiddleware
//   |> textPingMiddleware
//   |> sitemapFaviconMiddleware
// middleware emptyRequest

// Each middleware wraps the previous one, so the outermost middleware is last,
// and the handler comes first.

// EmptyRequest is an empty record, and each middleware adds fields to it until
// the request has the shape required by the handler. It then returns a
// response, which can also have fields added to it by middleware wishing to
// send those fields to other middlewares.

// As such, the types of the entire middleware have to add up to the type of
// the handler.


let middlewareNextParameter =
  Param.make "next" (TFn([ TVariable "ctx2" ], THTTPResponse)) ""

let middlewareReturnType = TFn([ TVariable "ctx" ], THTTPResponse)

let fns : List<BuiltInFn> =
  [ { name = fn "Http" "emptyRequest" 0
      parameters = []
      returnType = TAny
      description = "An empty HTTP request, with no fields"
      fn =
        (function
        | state, [] -> Value(DObj(Map []))
        | args -> incorrectArgs ())
      sqlSpec = NotYetImplementedTODO
      previewable = Pure
      deprecated = NotDeprecated }
    { name = fn "Http" "parseQueryString" 0
      parameters = [ Param.make "url" TStr "" ]
      returnType = TDict TStr // This is always a string
      description =
        "Parse the query string into a dict. If there are copies of the same query param, the last one wins"
      fn =
        (function
        | state, [ DStr url ] ->
            let queryString = System.Uri(url).Query
            let nvc = System.Web.HttpUtility.ParseQueryString(queryString)
            nvc.AllKeys
            |> Seq.map (fun key ->
                 let values = nvc.GetValues(key)
                 (key, DStr(values.[values.Length - 1])))
            |> Seq.toList
            |> Map
            |> DObj
            |> Value
        | args -> incorrectArgs ())
      sqlSpec = NotYetImplementedTODO
      previewable = Pure
      deprecated = NotDeprecated }
    { name = fn "Http" "parseQueryParamsMiddleware" 0
      parameters =
        [ Param.make "url" TStr ""
          Param.make "req" (TVariable "a") ""
          middlewareNextParameter ]
      returnType = middlewareReturnType
      description = ""
      fn =
        (function
        // FSTODO
        | state, [] -> Value(DObj(Map []))
        | args -> incorrectArgs ())
      sqlSpec = NotYetImplementedTODO
      previewable = Pure
      deprecated = NotDeprecated }
    { name = fn "Http" "parseHeaders" 0
      parameters = [ Param.make "headerString" TStr "" ]
      returnType = TDict TStr
      description =
        "Parse the headers string into a dict. If multiple headers of the same key exist, the latest one wins."
      fn =
        (function
        // FSTODO
        | state, [] -> Value(DObj(Map []))
        | args -> incorrectArgs ())
      sqlSpec = NotYetImplementedTODO
      previewable = Pure
      deprecated = NotDeprecated }
    { name = fn "Http" "parseHeadersMiddleware" 0
      parameters =
        [ Param.make "url" TStr ""
          Param.make "req" (TVariable "a") ""
          middlewareNextParameter ]
      returnType = middlewareReturnType
      description =
        "Parse the headers string into a dict. If multiple headers of the same key exist, the latest one wins."
      fn =
        (function
        // FSTODO
        | state, [] -> Value(DObj(Map []))
        | args -> incorrectArgs ())
      sqlSpec = NotYetImplementedTODO
      previewable = Pure
      deprecated = NotDeprecated }
    { name = fn "Http" "addJSONBodyMiddleware" 0
      parameters =
        [ Param.make "url" TStr ""
          Param.make "req" (TVariable "a") ""
          middlewareNextParameter ]
      returnType = middlewareReturnType
      description =
        "Parse the headers string into a dict. If multiple headers of the same key exist, the latest one wins."
      fn =
        (function
        // FSTODO
        | state, [] -> Value(DObj(Map []))
        | args -> incorrectArgs ())
      sqlSpec = NotYetImplementedTODO
      previewable = Pure
      deprecated = NotDeprecated }
    { name = fn "Http" "middleware" 0
      parameters =
        [ Param.make "url" TBytes ""
          Param.make "body" TBytes ""
          Param.make "headers" TBytes ""
          middlewareNextParameter ]
      returnType = THTTPResponse
      description =
        "Call the middleware stack, returning a response which can be sent to the browser"
      fn =
        (function
        // FSTODO
        | state, [] -> Value(DObj(Map []))
        | args -> incorrectArgs ())
      sqlSpec = NotYetImplementedTODO
      previewable = Pure
      deprecated = NotDeprecated } ]

// let httpMiddleware_v0 (body : string)
//                       (headers : string)
//                       (url : string)
//                       (handler)
//                       : response =
//   handler
//   |> addQueryParams url
//   |> parseRawBody
//   |> addHeaders headers
//   |> addJsonBody body
//   |> addFormBody body
//   |> addCookies headers
//   |> processErrorRail
//   |> optionsHanderMiddleware
//   |> headHandlerMiddleware
//   |> textPingMiddleware
//   |> sitemapFaviconMiddleware
//
