module LibExecution.StdLib.LibMiddleware

open System.Threading.Tasks
open FSharp.Control.Tasks
open FSharpPlus

open Prelude
open LibExecution.RuntimeTypes
open LibExecution.Shortcuts

module Interpreter = LibExecution.Interpreter
module Errors = LibExecution.Errors

let fn = FQFnName.stdlibName

let err (str : string) = Value(Dval.errStr str)

let incorrectArgs = LibExecution.Errors.incorrectArgs

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
let middlewareReturnType = TFn([ TVariable "req" ], TVariable "resp")

let middlewareNextParameter = Param.make "next" middlewareReturnType ""


let fns : List<BuiltInFn> =
  [ { name = fn "Http" "emptyRequest" 0
      parameters = []
      returnType = TAny
      description = "An empty HTTP request, with no fields"
      fn =
        (function
        | state, [] -> Value(DObj(Map []))
        | _ -> incorrectArgs ())
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
            |> Seq.map
                 (fun key ->
                   let values = nvc.GetValues(key)
                   (key, DStr(values.[values.Length - 1])))
            |> Seq.toList
            |> Map
            |> DObj
            |> Value
        | _ -> incorrectArgs ())
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
        | _ -> incorrectArgs ())
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
        | _ -> incorrectArgs ())
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
        | _ -> incorrectArgs ())
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
        | _ -> incorrectArgs ())
      sqlSpec = NotYetImplementedTODO
      previewable = Pure
      deprecated = NotDeprecated }
    { name = fn "Http" "setHeader" 0
      parameters =
        [ Param.make "response" (THttpResponse varA) ""
          Param.make "name" TStr ""
          Param.make "value" TStr "" ]
      returnType = THttpResponse varA
      description = "Set a header in the HTTP response"
      fn =
        (function
        | state,
          [ DHttpResponse (Response (code, headers), responseVal); DStr name;
            DStr value ] ->
            Value(
              DHttpResponse(Response(code, headers ++ [ name, value ]), responseVal)
            )
        | _ -> incorrectArgs ())
      sqlSpec = NotYetImplementedTODO
      previewable = Pure
      deprecated = NotDeprecated }
    { name = fn "Http" "responseBody" 0
      parameters = [ Param.make "response" (THttpResponse varA) "" ]
      returnType = varA
      description = "Return the body of a HTTP response"
      fn =
        (function
        | state, [ DHttpResponse (_, responseVal) ] -> Value responseVal
        | _ -> incorrectArgs ())
      sqlSpec = NotYetImplementedTODO
      previewable = Pure
      deprecated = NotDeprecated }
    { name = fn "Http" "addServerHeaderMiddleware" 0
      parameters = [ middlewareNextParameter ]
      returnType = middlewareReturnType
      description = "Add the darklang server header."
      fn =
        let code =
          // (fun req ->
          //   let response = req |> next in
          //   Http.setHeader_v0 response "server" "darklang")
          eLambda
            [ "req" ]
            (eLet
              "response"
              (ePipeApply (eVar "next") [ eVar "req" ])
              (eFn
                "Http"
                "setHeader"
                0
                [ eVar "response"; eStr "server"; eStr "darklang" ]))

        (function
        | state, [ DFnVal _ as next ] ->
            let st = Map.empty |> Map.add "next" next
            Interpreter.eval state st code
        | _ -> incorrectArgs ())
      sqlSpec = NotYetImplementedTODO
      previewable = Pure
      deprecated = NotDeprecated }
    { name = fn "Http" "convertToResponseValue" 0
      parameters =
        [ Param.make
            "response"
            varA
            "A HTTP response to be returned to the client. May be any type, and will automatically be converted to an appropriate HTTP response" ]
      returnType = THttpResponse varA
      description = "Return a HTTPResponse based on the input."
      fn =
        (function
        | state, [ response ] ->
            match response with
            | DHttpResponse _ -> Value response
            | _ ->
                let contentType =
                  match response with
                  | DObj _
                  | DList _ -> "application/json; charset-utf-8"
                  | _ -> "text/plain; charset=utf-8"

                let bytified =
                  response
                  |> LibExecution.DvalRepr.toPrettyMachineJsonV1
                  |> toBytes
                  |> DBytes

                Value(
                  DHttpResponse(
                    Response(200, [ "content-type", contentType ]),
                    bytified
                  )
                )
        | _ -> incorrectArgs ())
      sqlSpec = NotYetImplementedTODO
      previewable = Pure
      deprecated = NotDeprecated }
    { name = fn "Http" "wrapInResponseValue" 0
      parameters = [ middlewareNextParameter ]
      returnType = middlewareReturnType
      description = "Takes a value that is expected to be returned to an end-user via HTTP.
        If it is not a HttpResponse, it converts it into one. The output is
        converted to bytes. If it needs to be stringified first, it is
        stringified to a representation suitable for consuming machine-readable
        JSON. A header is added based on type: Dicts, Records and Lists have
        JSON content-type, all others have text/plain."
      fn =
        let code =
          // (fun req -> Http.convertToResponseValue (req |> next))
          eLambda
            [ "req" ]
            (eFn
              "Http"
              "convertToResponseValue"
              0
              [ (ePipeApply (eVar "next") [ eVar "req" ]) ])

        (function
        | state, [ DFnVal _ as next ] ->
            let st = Map.empty |> Map.add "next" next
            Interpreter.eval state st code
        | _ -> incorrectArgs ())
      sqlSpec = NotYetImplementedTODO
      previewable = Pure
      deprecated = NotDeprecated }
    { name = fn "Http" "addContentLengthResponseHeader" 0
      parameters = [ middlewareNextParameter ]
      returnType = middlewareReturnType
      description = "Take the HTTP result, and add a Content-length header to it."
      fn =
        let code =
          // (fun req ->
          //   let response = req |> next in
          //   let body = Http.responseBody_v0 response in
          // FSTODO: should be bytes, not a string
          //   Http.setHeader_v0 response "content-length" (toString (Bytes.length_v0 body)))
          eLambda
            [ "req" ]
            (eLet
              "response"
              (ePipeApply (eVar "next") [ eVar "req" ])
              (eLet
                "body"
                (eFn "Http" "responseBody" 0 [ eVar "response" ])
                (eFn
                  "Http"
                  "setHeader"
                  0
                  [ eVar "response"
                    eStr "content-length"
                    eFn "" "toString" 0 [ eFn "Bytes" "length" 0 [ eVar "body" ] ] ])))

        (function
        | state, [ DFnVal _ as next ] ->
            let st = Map.empty |> Map.add "next" next
            Interpreter.eval state st code
        | _, _ -> incorrectArgs ())

      sqlSpec = NotYetImplementedTODO
      previewable = Pure
      deprecated = NotDeprecated }
    // let req ->
    //   if not wrapped in a response, use machinejson, and infer header based on type (list/obj uses JSON, else use textplain)
    //   if in a response:
    //      use existing header or infer from type.
    //      if output is bytes, print direct
    //      if text/html or text/plain or applcation/xml, use enduser_readable_text
    //      if application/json, convert to pretty_machine_json_v1
    //   note: string with no response: machine printout and text/plain (dev mode)
    //         string in http response: enduser_readable_text and text/plain
    { name = fn "Http" "middleware" 0
      parameters =
        [ Param.make "url" TBytes ""
          Param.make "body" TBytes ""
          Param.make "headers" TBytes ""
          middlewareNextParameter ]
      returnType = THttpResponse TBytes
      description =
        "Call the middleware stack, returning a response which can be sent to the browser"
      fn =
        // eStdFnVal "Http" "wrapInResponseValue" 0
        // eStdFnVal "Http" "addServerHeaderMiddleware" 0
        // eStdFnVal "Http" "addContentLengthResponseHeader" 0
        let code =
          // let fns = [Http.wrapInResponseValue_v0 ; Http.addserverHeaderMiddleware_v0]
          // let app = List.fold_v0 fns handler (fun accum curr -> accum |> curr)
          // handler |> app
          eLet
            "fns"
            (eList [ eStdFnVal "Http" "wrapInResponseValue" 0
                     eStdFnVal "Http" "addContentLengthResponseHeader" 0
                     eStdFnVal "Http" "addServerHeaderMiddleware" 0 ])
            (eLet
              "app"
              (eFn
                "List"
                "fold"
                0
                [ eVar "fns"
                  eVar "handler"
                  eLambda
                    [ "accum"; "curr" ]
                    (ePipeApply (eVar "curr") [ eVar "accum" ]) ])
              (ePipeApply (eVar "app") [ eVar "handler" ]))


        (function
        | state, [ DStr _ as url; DBytes _ as body; headers; DFnVal _ as handler ] ->
            let st =
              Map.empty
              |> Map.add "url" url
              |> Map.add "body" body
              |> Map.add "headers" headers
              |> Map.add "handler" handler

            Interpreter.eval state st code
        | _ -> incorrectArgs ())
      sqlSpec = NotYetImplementedTODO
      previewable = Pure
      deprecated = NotDeprecated } ]


// let httpMiddleware_v0 (body : string)
//                       (headers : string)
//                       (url : string)
//                       (handler)
//                       : response =
//   handler
//   |> \nextMW -> addQueryParams url nextMW
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
