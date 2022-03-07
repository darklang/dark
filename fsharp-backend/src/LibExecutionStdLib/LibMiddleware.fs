module LibExecutionStdLib.LibMiddleware

open System.Threading.Tasks
open FSharp.Control.Tasks

open Prelude
open LibExecution.VendoredTablecloth
open LibExecution.RuntimeTypes

module Interpreter = LibExecution.Interpreter
module Errors = LibExecution.Errors
module DvalReprExternal = LibExecution.DvalReprExternal

let fn = FQFnName.stdlibFnName

let err (str : string) = Ply(Dval.errStr str)

let incorrectArgs = Errors.incorrectArgs

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

// This is returned by a middlware and passed into the next middleware

// CLEANUP: this started as an attempt to rewrite the http framework using
// middleware. It was sort-of working, but it was quite difficult to make work.
// Specifically, it was really hard to make it work exactly like the old OCaml code,
// and also quite hard to write and to figure out what it did. A better approach
// might be to try to write it in Dark using the package manager.

// let middlewareReturnType = TFn([ TVariable "req" ], TVariable "resp")

// let middlewareNextParameter = Param.make "next" middlewareReturnType ""


// let fns : List<BuiltInFn> =
//   [ { name = fn "Http" "parseQueryString" 0
//       parameters = [ Param.make "url" TStr "" ]
//       returnType = TDict TStr // This is always a string
//       description =
//         "Parse the query string into a dict. If there are copies of the same query param, the last one wins"
//       fn =
//         (function
//         | _, [ DStr url ] ->
//           // TODO: could do with some fuzzing here
//           let queryString = System.Uri(url).Query
//           let nvc = System.Web.HttpUtility.ParseQueryString queryString

//           nvc.AllKeys
//           |> Seq.map
//                (fun key ->
//                  let values = nvc.GetValues key

//                  let value =
//                    let split = values[values.Length - 1] |> String.split ","

//                    match split with
//                    | [] -> DNull
//                    | [ "" ] -> DNull // CLEANUP this should be a string
//                    | [ v ] -> DStr v
//                    | list -> DList(List.map DStr list)

//                  if isNull key then
//                    // All the values with no key are by GetValues, so make each one a value
//                    values |> Array.toList |> List.map (fun k -> (k, DNull))
//                  else
//                    [ (key, value) ])
//           |> List.concat
//           |> Map
//           |> DObj
//           |> Ply
//         | _ -> incorrectArgs ())
//       sqlSpec = NotYetImplementedTODO
//       previewable = Pure
//       deprecated = NotDeprecated }
//     { name = fn "Http" "addQueryParamsMiddleware" 0
//       parameters = [ middlewareNextParameter ]
//       returnType = middlewareReturnType
//       description = ""
//       fn =
//         // fun req ->
//         //   let url = Http.parseQueryString_v0 req.url
//         //   let req = Dict.set_v0 req "queryParams" url
//         //   req |> next
//         let code =
//           eLambda
//             [ "req" ]
//             (eLet
//               "url"
//               (eFn "Http" "parseQueryString" 0 [ eFieldAccess (eVar "req") "url" ])
//               (eLet
//                 "req"
//                 (eFn "Dict" "set" 0 [ eVar "req"; eStr "queryParams"; eVar "url" ])
//                 (ePipeApply (eVar "next") [ eVar "req" ])))

//         (function
//         | state, [ DFnVal _ as next ] ->
//           let st = Map.empty |> Map.add "next" next
//           Interpreter.eval state st code
//         | _ -> incorrectArgs ())
//       sqlSpec = NotYetImplementedTODO
//       previewable = Pure
//       deprecated = NotDeprecated }
//     { name = fn "Http" "urlEncodeQueryParamString" 0
//       parameters = [ Param.make "url" TStr "" ]
//       returnType = TStr
//       // Uses the same conditions as the old OCaml implementation
//       description = "URL encode query param strings"
//       fn =
//         let urlSafe =
//           (let allowed =
//             // TODO: this adds extra safe characters ("=,&") to avoid having
//             // to parse the query string (using the .NET utilities to do so
//             // might throw away info we need, as well.) This is wrong and
//             // probably has edge cases. Needs fuzzing at least.
//             "/?:@ABCDEFGHIJKLMNOPQRSTUVWXYZabcdefghijklmnopqrstuvwxyz0123456789_.-~=,&"

//            let mutable safe = (Array.init 256 (fun _ -> false))
//            Array.iter (fun b -> safe[int b] <- true) (UTF8.toBytes allowed)
//            safe)

//         (function
//         | _, [ DStr queryParams ] ->
//           let result = System.Text.StringBuilder()
//           // What if there;s a urlencoded ',' or '=' in the values?
//           queryParams
//           |> UTF8.toBytes
//           |> Array.iter
//                (fun b ->
//                  let (_ : System.Text.StringBuilder) =
//                    if urlSafe[int b] then
//                      result.Append(char b)
//                    else
//                      result.AppendFormat("%{0:X2}", b)

//                  ())

//           result |> string |> DStr |> Ply
//         | _ -> incorrectArgs ())
//       sqlSpec = NotYetImplementedTODO
//       previewable = Pure
//       deprecated = NotDeprecated }
//     { name = fn "Http" "canonicalizeUrlMiddleware" 0
//       parameters = [ middlewareNextParameter ]
//       returnType = middlewareReturnType
//       description = "Lowercases the url and removes queryparams from it"
//       fn =
//         // fun req ->
//         //   let parts = req.url |> String.split_v0 "?"
//         //   let host = parts |> List.head_v2 |> Option.withDefault_v0 req.url |> String.toLowercase_v1
//         //   let query = parts |> List.getAt_v1 1 |> Option.map (fun q -> "?" ++ Http.urlEncodeQueryParamString_v0 q) |> Option.withDefault_v0 ""
//         //   let req = Dict.set_v0 req "url" (host ++ query)
//         //   req |> next
//         let code =
//           eLambda
//             [ "req" ]
//             (eLet
//               "parts"
//               (ePipeApply
//                 (eStdFnVal "String" "split" 0)
//                 [ eFieldAccess (eVar "req") "url"; eStr "?" ])
//               (eLet
//                 "host"
//                 (ePipeApply
//                   (eStdFnVal "String" "toLowercase" 1)
//                   [ ePipeApply
//                       (eStdFnVal "Option" "withDefault" 0)
//                       [ ePipeApply (eStdFnVal "List" "head" 2) [ eVar "parts" ]
//                         eFieldAccess (eVar "req") "url" ] ])
//                 (eLet
//                   "query"
//                   (ePipeApply
//                     (eStdFnVal "Option" "withDefault" 0)
//                     [ ePipeApply
//                         (eStdFnVal "Option" "map" 0)
//                         [ ePipeApply
//                             (eStdFnVal "List" "getAt" 1)
//                             [ eVar "parts"; eInt 1 ]
//                           eLambda
//                             [ "q" ]
//                             (eFn
//                               ""
//                               "++"
//                               0
//                               [ eStr "?"
//                                 eFn "Http" "urlEncodeQueryParamString" 0 [ eVar "q" ] ]) ]
//                       eStr "" ])
//                   (eLet
//                     "req"
//                     (eFn
//                       "Dict"
//                       "set"
//                       0
//                       [ eVar "req"
//                         eStr "url"
//                         eFn "" "++" 0 [ eVar "host"; eVar "query" ] ])
//                     (ePipeApply (eVar "next") [ eVar "req" ])))))

//         (function
//         | state, [ DFnVal _ as next ] ->
//           let st = Map.empty |> Map.add "next" next
//           Interpreter.eval state st code
//         | _ -> incorrectArgs ())
//       sqlSpec = NotYetImplementedTODO
//       previewable = Pure
//       deprecated = NotDeprecated }
//     { name = fn "Http" "parseHeadersMiddleware" 0
//       parameters =
//         [ Param.make "url" TStr ""
//           Param.make "req" (TVariable "a") ""
//           middlewareNextParameter ]
//       returnType = middlewareReturnType
//       description =
//         "Parse the headers string into a dict. If multiple headers of the same key exist, the latest one wins."
//       fn =
//         (function
//         // TODO
//         | _, [] -> Ply(DObj(Map []))
//         | _ -> incorrectArgs ())
//       sqlSpec = NotYetImplementedTODO
//       previewable = Pure
//       deprecated = NotDeprecated }
//     { name = fn "Http" "addJSONBodyMiddleware" 0
//       parameters =
//         [ Param.make "url" TStr ""
//           Param.make "req" (TVariable "a") ""
//           middlewareNextParameter ]
//       returnType = middlewareReturnType
//       description =
//         "Parse the headers string into a dict. If multiple headers of the same key exist, the latest one wins."
//       fn =
//         (function
//         // TODO
//         | _, [] -> Ply(DObj(Map []))
//         | _ -> incorrectArgs ())
//       sqlSpec = NotYetImplementedTODO
//       previewable = Pure
//       deprecated = NotDeprecated }
//     { name = fn "Http" "setHeader" 0
//       parameters =
//         [ Param.make "response" (THttpResponse varA) ""
//           Param.make "name" TStr ""
//           Param.make "value" TStr "" ]
//       returnType = THttpResponse varA
//       description = "Set a header in the HTTP response"
//       fn =
//         (function
//         | _, [ DHttpResponse response; DStr name; DStr value ] ->
//           match response with
//           | Response (code, headers, responseVal) ->
//             Response(code, headers @ [ name, value ], responseVal)
//             |> DHttpResponse
//             |> Ply
//           | Redirect _ -> Ply(DHttpResponse response)
//         | _ -> incorrectArgs ())
//       sqlSpec = NotYetImplementedTODO
//       previewable = Pure
//       deprecated = NotDeprecated }
//     { name = fn "Http" "setHeaderUnlessPresent" 0
//       parameters =
//         [ Param.make "response" (THttpResponse varA) ""
//           Param.make "name" TStr ""
//           Param.make "value" TStr "" ]
//       returnType = THttpResponse varA
//       description = "Set a header in the HTTP response"
//       fn =
//         (function
//         | _, [ DHttpResponse response; DStr headerName; DStr value ] ->
//           match response with
//           | Response (code, headers, responseVal) ->
//             let existingHeader =
//               headers
//               |> List.tryFind (fun (name, _) -> String.toLowercase name = headerName)

//             let headers =
//               if existingHeader = None then
//                 (headerName, value) :: headers
//               else
//                 headers

//             Response(code, headers, responseVal) |> DHttpResponse |> Ply
//           | Redirect _ -> Ply(DHttpResponse response)
//         | _ -> incorrectArgs ())
//       sqlSpec = NotYetImplementedTODO
//       previewable = Pure
//       deprecated = NotDeprecated }

//     { name = fn "Http" "responseBody" 0
//       parameters = [ Param.make "response" (THttpResponse varA) "" ]
//       returnType = varA
//       description = "Return the body of a HTTP response"
//       fn =
//         (function
//         | _, [ DHttpResponse response ] ->
//           match response with
//           | Redirect _ -> Ply DNull
//           | Response (_, _, dv) -> Ply dv
//         | _ -> incorrectArgs ())
//       sqlSpec = NotYetImplementedTODO
//       previewable = Pure
//       deprecated = NotDeprecated }
//     { name = fn "Http" "addServerHeaderMiddleware" 0
//       parameters = [ middlewareNextParameter ]
//       returnType = middlewareReturnType
//       description = "Add the darklang server header."
//       fn =
//         let code =
//           // (fun req ->
//           //   let response = req |> next in
//           //   Http.setHeader_v0 response "server" "darklang")
//           eLambda
//             [ "req" ]
//             (eLet
//               "response"
//               (ePipeApply (eVar "next") [ eVar "req" ])
//               (eFn
//                 "Http"
//                 "setHeaderUnlessPresent"
//                 0
//                 [ eVar "response"; eStr "server"; eStr "darklang" ]))

//         (function
//         | state, [ DFnVal _ as next ] ->
//           let st = Map.empty |> Map.add "next" next
//           Interpreter.eval state st code
//         | _ -> incorrectArgs ())
//       sqlSpec = NotYetImplementedTODO
//       previewable = Pure
//       deprecated = NotDeprecated }
//     { name = fn "Http" "createRequest" 0
//       parameters =
//         [ Param.make "url" TStr ""
//           Param.make "headers" (TDict TStr) ""
//           Param.make "body" TBytes "" ]
//       returnType = TRecord [ "url", TStr; "headers", TDict TStr; "body", TBytes ]
//       description = "Creates a request"
//       fn =
//         (function
//         | _, [ DStr _ as url; DObj headers; DBytes bodyBytes as body ] ->
//           let body = if bodyBytes.Length = 0 then DNull else body
//           let cookies = DObj Map.empty // TODO
//           let formBody = DNull // TODO
//           let fullBody = DStr ""
//           let jsonBody = DNull // TODO

//           let headers =
//             headers
//             |> Map.toList
//             |> List.map (fun (k, v) -> (String.toLowercase k, v))
//             |> (@) [ "user-agent", DStr "ocaml-cohttp/1.2.0" ] // TODO wtf
//             |> Map.ofList
//             |> DObj

//           Map.empty
//           |> Map.add "body" body
//           |> Map.add "url" url
//           |> Map.add "headers" headers
//           |> Map.add "cookies" cookies
//           |> Map.add "formBody" formBody
//           |> Map.add "fullBody" fullBody
//           |> Map.add "jsonBody" jsonBody

//           |> DObj
//           |> Ply
//         | _ -> incorrectArgs ())
//       sqlSpec = NotYetImplementedTODO
//       previewable = Pure
//       deprecated = NotDeprecated }
//     { name = fn "Http" "convertToResponseValue" 0
//       parameters =
//         [ Param.make
//             "response"
//             varA
//             "A HTTP response to be returned to the client. May be any type, and will automatically be converted to an appropriate HTTP response" ]
//       returnType = THttpResponse TBytes
//       description = "Return a HTTPResponse based on the input"
//       // This does a lot more than we'd hope it does, and ideally we'd have
//       // split this into multiple middleware functions.  However, there is a
//       // need to make it match the OCaml version.  In OCaml, if the value was
//       // not wrapped in a response, we would always use toPrettyMachineJson to
//       // display it, regardless of the inferred headers. As a result, we cannot
//       // separate these from each other: if we convert to bytes we lose type to
//       // infer the content-type from. If we add the content-type first, we lose
//       // whether it's a result or not.
//       fn =
//         (function
//         | _, [ response ] ->
//           // let req ->
//           //   if not wrapped in a response
//           //     use machinejson
//           //     infer header based on type (list/obj uses JSON, else use textplain)
//           //   if in a response:
//           //      use existing header or infer from type.
//           //      if output is bytes, print direct
//           //      if text/html or text/plain or applcation/xml, use enduser_readable_text
//           //      if application/json, convert to pretty_machine_json_v1
//           //   note: string with no response: machine printout and text/plain (dev mode)
//           //         string in http response: enduser_readable_text and text/plain
//           let inferContentType dv =
//             match dv with
//             | DObj _
//             | DList _ -> "application/json; charset=utf-8"
//             | _ -> "text/plain; charset=utf-8"

//           match response with
//           | DHttpResponse (Response (code, headers, dv)) ->
//             let inferredCT = inferContentType dv

//             let existingContentType =
//               headers
//               |> List.tryFind
//                    (fun (name, _) -> String.toLowercase name = "content-type")

//             let headers =
//               if existingContentType = None then
//                 ("Content-type", inferredCT) :: headers
//               else
//                 headers

//             let contentType =
//               existingContentType
//               |> Option.map (fun (k, v) -> v)
//               |> Option.defaultValue inferredCT
//               |> String.split ";"
//               |> Seq.tryHead

//             let asBytes =
//               match dv, contentType with
//               | DBytes bytes, _ -> bytes
//               | _, Some "text/plain"
//               | _, Some "application/xml"
//               | _, Some "text/html" ->
//                 dv |> DvalReprExternal.toEnduserReadableTextV0 |> UTF8.toBytes
//               | _ -> dv |> DvalReprExternal.toPrettyMachineJsonStringV1 |> UTF8.toBytes

//             Ply(DHttpResponse(Response(code, headers, DBytes asBytes)))
//           | DHttpResponse (Redirect _) as resp -> Ply resp
//           | response ->
//             let bytes =
//               response |> DvalReprExternal.toPrettyMachineJsonStringV1 |> UTF8.toBytes

//             let headers = [ "content-type", inferContentType response ]
//             Ply(DHttpResponse(Response(200L, headers, DBytes bytes)))
//         | _ -> incorrectArgs ())
//       sqlSpec = NotYetImplementedTODO
//       previewable = Pure
//       deprecated = NotDeprecated }
//     { name = fn "Http" "convertToResponseMiddleware" 0
//       parameters = [ middlewareNextParameter ]
//       returnType = middlewareReturnType
//       description =
//         "Takes a value that is expected to be returned to an end-user via HTTP.
//         If it is not a HttpResponse, it converts it into one."
//       fn =
//         let code =
//           // (fun req -> Http.convertToResponseValue (req |> next))
//           eLambda
//             [ "req" ]
//             (eFn
//               "Http"
//               "convertToResponseValue"
//               0
//               [ (ePipeApply (eVar "next") [ eVar "req" ]) ])

//         (function
//         | state, [ DFnVal _ as next ] ->
//           let st = Map.empty |> Map.add "next" next
//           Interpreter.eval state st code
//         | _ -> incorrectArgs ())
//       sqlSpec = NotYetImplementedTODO
//       previewable = Pure
//       deprecated = NotDeprecated }
//     { name = fn "Http" "addContentLengthResponseHeaderMiddleware" 0
//       parameters = [ middlewareNextParameter ]
//       returnType = middlewareReturnType
//       description = "Take the HTTP result, and add a Content-length header to it."
//       fn =
//         let code =
//           // (fun req ->
//           //   let response = req |> next in
//           //   let body = Http.responseBody_v0 response in
//           //   Http.setHeader_v0 response "content-length" (toString (Bytes.length_v0 body)))
//           eLambda
//             [ "req" ]
//             (eLet
//               "response"
//               (ePipeApply (eVar "next") [ eVar "req" ])
//               (eLet
//                 "body"
//                 (eFn "Http" "responseBody" 0 [ eVar "response" ])
//                 (eFn
//                   "Http"
//                   "setHeader" // always set it
//                   0
//                   [ eVar "response"
//                     eStr "Content-Length"
//                     eFn "" "toString" 0 [ eFn "Bytes" "length" 0 [ eVar "body" ] ] ])))

//         (function
//         | state, [ DFnVal _ as next ] ->
//           let st = Map.empty |> Map.add "next" next
//           Interpreter.eval state st code
//         | _, _ -> incorrectArgs ())
//       sqlSpec = NotYetImplementedTODO
//       previewable = Pure
//       deprecated = NotDeprecated }
//     { name = fn "Http" "respondToTextPingMiddleware" 0
//       parameters = [ middlewareNextParameter ]
//       returnType = middlewareReturnType
//       description =
//         "If the header has content-type of `text/ping`, return a status code of 418"
//       fn =
//         let code =
//           // (fun req ->
//           //   let contentType  = Dict.get_v2 req.headers "content-type"
//           //   if contentType = "text/ping"
//           //   then
//           //     Http::response_v0 Bytes.empty_v0 418
//           //   else
//           //     req |> next
//           eLambda
//             [ "req" ]
//             (eLet
//               "contentType"
//               (eFn
//                 "Dict"
//                 "get"
//                 2
//                 [ eFieldAccess (eVar "req") "headers"; eStr "content-type" ])
//               (eIf
//                 (eFn
//                   ""
//                   "=="
//                   0
//                   [ eVar "contentType"; eConstructor "Just" [ eStr "text/ping" ] ])
//                 (eFn "Http" "response" 0 [ eFn "Bytes" "empty" 0 []; eInt 418 ])
//                 (ePipeApply (eVar "next") [ eVar "req" ])))

//         (function
//         | state, [ DFnVal _ as next ] ->
//           let st = Map.empty |> Map.add "next" next
//           Interpreter.eval state st code
//         | _, _ -> incorrectArgs ())
//       sqlSpec = NotYetImplementedTODO
//       previewable = Pure
//       deprecated = NotDeprecated }
//     { name = fn "Http" "middleware" 0
//       parameters =
//         [ Param.make "url" TStr ""
//           Param.make "body" TBytes ""
//           Param.make "headers" (TDict TStr) ""
//           middlewareNextParameter ]
//       returnType = THttpResponse TBytes
//       description =
//         "Call the middleware stack, returning a response which can be sent
//         to the browser. Each function in the middleware stack receives the next
//         middleware element, and returns a function to be called on the request,
//         returning a response. The stack is executed with the last elements executed
//         first on the request and last on the response. "
//       fn =
//         let code =
//           // let fns = [Http.wrapInResponseValue_v0 ; Http.addserverHeaderMiddleware_v0]
//           // let app = List.fold_v0 fns handler (fun accum curr -> accum |> curr)
//           // request |> app
//           eLet
//             "fns"
//             (eList [ eStdFnVal "Http" "convertToResponseMiddleware" 0
//                      eStdFnVal "Http" "canonicalizeUrlMiddleware" 0
//                      eStdFnVal "Http" "addQueryParamsMiddleware" 0 // must be after canonicalizeUrlMiddleware
//                      // Shortcircuit
//                      eStdFnVal "Http" "respondToTextPingMiddleware" 0
//                      // Everything gets `Content-Length` and `Server` headers
//                      eStdFnVal "Http" "addServerHeaderMiddleware" 0
//                      eStdFnVal "Http" "addContentLengthResponseHeaderMiddleware" 0 ])
//             (eLet
//               "app"
//               (eFn
//                 "List"
//                 "fold"
//                 0
//                 [ eVar "fns"
//                   eVar "handler"
//                   eLambda
//                     [ "accum"; "curr" ]
//                     (ePipeApply (eVar "curr") [ eVar "accum" ]) ])
//               (eLet
//                 "initialRequest"
//                 (eFn
//                   "Http"
//                   "createRequest"
//                   0
//                   [ eVar "url"; eVar "headers"; eVar "body" ])

//                 (ePipeApply (eVar "app") [ eVar "initialRequest" ])))


//         (function
//         | state, [ DStr _ as url; DBytes _ as body; headers; DFnVal _ as handler ] ->
//           let st =
//             Map.empty
//             |> Map.add "url" url
//             |> Map.add "body" body
//             |> Map.add "headers" headers
//             |> Map.add "handler" handler

//           Interpreter.eval state st code
//         | _ -> incorrectArgs ())
//       sqlSpec = NotYetImplementedTODO
//       previewable = Pure
//       deprecated = NotDeprecated }
//     // CLEANUP: move into LibBytes. It's here to help with testing
//     { name = fn "Bytes" "empty" 0
//       parameters = []
//       returnType = TBytes
//       description = "Returns an empty Bytes value"
//       fn =
//         (function
//         | _, [] -> Ply(DBytes [||])
//         | _ -> incorrectArgs ())
//       sqlSpec = NotYetImplementedTODO
//       previewable = Pure
//       deprecated = NotDeprecated } ]

// let httpMiddleware_v0 (body : string)
//                       (headers : string)
//                       (url : string)
//                       (handler)
//                       : response =
//   handler
//   |> addQueryParams
//   |> parseRawBody
//   |> addJsonBody body
//   |> addFormBody body
//   |> addCookies headers
//   |> processErrorRail
//   |> optionsHanderMiddleware
//   |> textPingMiddleware
//   |> sitemapFaviconMiddleware
