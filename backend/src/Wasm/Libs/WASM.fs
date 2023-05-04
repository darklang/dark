/// StdLib for handling JS-WASM interactions via WASM'd Darklang code
module Wasm.LibWASM

open System

open Prelude
open Tablecloth

open LibExecution.RuntimeTypes

open LibExecution.StdLib.Shortcuts

module PT = LibExecution.ProgramTypes
module Exe = LibExecution.Execution

open System.IO
open System.Net.Http

open System.Threading.Tasks
open FSharp.Control.Tasks

open LibExecution
open VendoredTablecloth


module HttpClient =
  type Method = HttpMethod

  module Headers =
    type Header = string * string
    type T = List<Header>

  type Body = byte array

  type HttpRequest =
    { url : string
      method : Method
      headers : Headers.T
      body : Body }

  type HttpResult = { statusCode : int; headers : Headers.T; body : Body }

  // forked from Elm's HttpError type
  // https://package.elm-lang.org/packages/elm/http/latest/Http#Error

  type HttpRequestError =
    | BadUrl of details : string
    | Timeout
    | NetworkError
    | Other of details : string

  type HttpRequestResult = Result<HttpResult, HttpRequestError>


  let private httpClient : HttpClient =
    new HttpClient(
      new HttpClientHandler(AllowAutoRedirect = false),
      disposeHandler = false,
      Timeout = System.TimeSpan.FromSeconds 30.0,
      MaxResponseContentBufferSize = 1024L * 1024L * 100L // 100MB
    )

  let request (httpRequest : HttpRequest) : Task<HttpRequestResult> =
    task {
      // use _ =
      // Telemetry.child
      //   "HttpClient.call"
      //   [ "request.url", httpRequest.url; "request.method", httpRequest.method ]
      try
        let uri = System.Uri(httpRequest.url, System.UriKind.Absolute)

        // currently we only support http(s) requests
        if uri.Scheme <> "https" && uri.Scheme <> "http" then
          return Error(BadUrl "Unsupported Protocol")
        else
          let reqUri =
            System.UriBuilder(
              Scheme = uri.Scheme,
              Host = uri.Host,
              Port = uri.Port,
              Path = uri.AbsolutePath,
              Query = uri.Query
            )
            |> string

          use req =
            new HttpRequestMessage(
              httpRequest.method,
              reqUri,
              // Content = new ByteArrayContent(httpRequest.body),

              // Support both Http 2.0 and 3.0
              // https://learn.microsoft.com/en-us/dotnet/api/system.net.http.httpversionpolicy?view=net-7.0
              // TODO: test this (against requestbin or something that allows us to control the HTTP protocol version)
              Version = System.Net.HttpVersion.Version30,
              VersionPolicy = System.Net.Http.HttpVersionPolicy.RequestVersionOrLower
            )

          // headers
          httpRequest.headers
          |> List.iter (fun (k, v) ->
            // .NET handles "content headers" separately from other headers.
            // They're put into `req.Content.Headers` rather than `req.Headers`
            // https://docs.microsoft.com/en-us/dotnet/api/system.net.http.headers.httpcontentheaders?view=net-6.0
            if String.equalsCaseInsensitive k "content-type" then
              try
                req.Content.Headers.ContentType <-
                  Headers.MediaTypeHeaderValue.Parse(v)
              with
              | :? System.FormatException ->
                Exception.raiseCode "Invalid content-type header"
            else
              let added = req.Headers.TryAddWithoutValidation(k, v)

              // Headers are split between req.Headers and req.Content.Headers so just try both
              if not added then req.Content.Headers.Add(k, v))

          // send request
          // Telemetry.addTag "request.content_type" req.Content.Headers.ContentType
          // Telemetry.addTag "request.content_length" req.Content.Headers.ContentLength
          use! response = httpClient.SendAsync req

          // Telemetry.addTags [ "response.status_code", response.StatusCode
          //                     "response.version", response.Version ]
          use! responseStream = response.Content.ReadAsStreamAsync()
          use memoryStream = new MemoryStream()
          do! responseStream.CopyToAsync(memoryStream)
          let respBody = memoryStream.ToArray()

          let headers =
            response
            |> HttpHeaders.headersForAspNetResponse
            |> List.map (fun (k, v) -> (String.toLowercase k, String.toLowercase v))

          return
            { statusCode = int response.StatusCode
              headers = headers
              body = respBody }
            |> Ok

      with
      | :? TaskCanceledException ->
        // Telemetry.addTags [ "error", true; "error.msg", "Timeout" ]
        return Error Timeout

      | :? System.ArgumentException as e ->
        // We know of one specific case indicating Unsupported Protocol
        // If we get this otherwise, return generic error
        //
        // TODO: would this be better as a guard (i.e. `when e.Message = ...`),
        //   leaving other cases un-caught?
        if e.Message = "Only 'http' and 'https' schemes are allowed. (Parameter 'value')" then
          // Telemetry.addTags [ "error", true; "error.msg", "Unsupported Protocol" ]
          return Error(BadUrl "Unsupported Protocol")
        else
          // Telemetry.addTags [ "error", true; "error.msg", e.Message ]
          return Error(Other e.Message)

      | :? System.UriFormatException ->
        // Telemetry.addTags [ "error", true; "error.msg", "Invalid URI" ]
        return Error(BadUrl "Invalid URI")

      | :? IOException as e -> return Error(Other e.Message)

      | :? HttpRequestException as e ->
        // This is a bit of an awkward case. I'm unsure how it fits into our model.
        // We've made a request, and _potentially_ (according to .NET) have a status
        // code. That should return some sort of Error - but our Error case type
        // doesn't have a good slot to include the status code. We could have a new
        // case of `| ErrorHandlingResponse of statusCode: int` but that feels wrong.
        let statusCode = if e.StatusCode.HasValue then int e.StatusCode.Value else 0

        // Telemetry.addException [ "error.status_code", statusCode ] e

        return Error(Other(Exception.getMessages e |> String.concat " "))
    }

let headersType = TList(TTuple(TString, TString, []))

type HeaderError =
  | BadInput of string
  | TypeMismatch of string

let types : List<BuiltInType> =
  [ { name = typ "HttpClient" "Response" 0
      typeParams = []
      definition =
        CustomType.Record(
          { name = "statusCode"; typ = TInt },
          [ { name = "headers"; typ = headersType }
            { name = "body"; typ = TBytes } ]
        )
      description = "The response from a HTTP request"
      deprecated = NotDeprecated } ]


let fns : List<BuiltInFn> =
  [ { name = fn "WASM" "callJSFunction" 0
      typeParams = []
      parameters =
        [ Param.make "functionName" TString ""

          // TODO: maybe instead of a `TList TString`,
          // we require a `TList(TCustomType SimpleJSON)`,
          // where `type SimpleJSON = | JNull | JBool | JList ...`
          Param.make "serializedArgs" (TList TString) "" ]
      returnType = TResult(TUnit, TString)
      description =
        "Calls a function exposed in JS host, i.e. `console.log`, or a user-defined function globally available"
      fn =
        (function
        | _, _, [ DString functionName; DList args ] ->
          let args =
            args
            |> List.fold (Ok []) (fun agg item ->
              match agg, item with
              | (Error err, _) -> Error err
              | (Ok l, DString arg) ->
                // Should empty args be allowed?
                if arg = "" then
                  "Empty request header key provided" |> Error
                else
                  Ok(arg :: l)

              | (_, notAString) ->
                // this should be a DError, not a "normal" error
                $"Expected args to be a `List<String>`, but got: {LibExecution.DvalReprDeveloper.toRepr notAString}"
                |> Error)
            |> Result.map (fun pairs -> List.rev pairs)

          match args with
          | Ok args ->
            uply {
              try
                do WasmHelpers.callJSFunction functionName args
                return DResult(Ok DUnit)
              with
              | e ->
                return
                  $"Error calling {functionName} with provided args: {e.Message}"
                  |> DString
                  |> Error
                  |> DResult
            }
          | Error argsError -> Ply(DResult(Error(DString argsError)))
        | _ -> incorrectArgs ())
      sqlSpec = NotQueryable
      previewable = Impure
      deprecated = NotDeprecated }


    { name = fn' ["WASM"; "HttpClient"]  "requestFromWasm" 0
      typeParams = []
      parameters =
        [ Param.make "method" TString ""
          Param.make "uri" TString ""
          Param.make "headers" headersType ""
          Param.make "body" TBytes "" ]
      returnType =
        TResult(
          TCustomType(FQTypeName.Stdlib(typ "HttpClient" "Response" 0), []),
          TString
        )
      description =
        "Make blocking HTTP call to <param uri>. Returns a <type Result> where
        the response is wrapped in {{ Ok }} if a response was successfully
        received and parsed, and is wrapped in {{ Error }} otherwise"
      fn =
        (function
        | _, _, [ DString method; DString uri; DList reqHeaders; DBytes reqBody ] ->
          let reqHeaders : Result<List<string * string>, HeaderError> =
            reqHeaders
            |> List.fold (Ok []) (fun agg item ->
              match agg, item with
              | (Error err, _) -> Error err
              | (Ok pairs, DTuple (DString k, DString v, [])) ->
                // TODO: what about whitespace? What else can break?
                if k = "" then
                  BadInput "Empty request header key provided" |> Error
                else
                  Ok((k, v) :: pairs)

              | (_, notAPair) ->
                // this should be a DError, not a "normal" error
                TypeMismatch
                  $"Expected request headers to be a `List<String*String>`, but got: {DvalReprDeveloper.toRepr notAPair}"
                |> Error)
            |> Result.map (fun pairs -> List.rev pairs)

          let method =
            try
              Some(HttpMethod method)
            with
            | _ -> None

          match reqHeaders, method with
          | Ok reqHeaders, Some method ->
            uply {
              let request : HttpClient.HttpRequest =
                { url = uri; method = method; headers = reqHeaders; body = reqBody }

              let! (response : HttpClient.HttpRequestResult) =
                HttpClient.request request

              match response with
              | Ok response ->
                let responseHeaders =
                  response.headers
                  |> List.map (fun (k, v) ->
                    DTuple(
                      DString(String.toLowercase k),
                      DString(String.toLowercase v),
                      []
                    ))
                  |> DList

                let typ =
                  FQTypeName.Stdlib(
                    FQTypeName.stdlibTypeName "HttpClient" "Response" 0
                  )

                return
                  [ ("statusCode", DInt(int64 response.statusCode))
                    ("headers", responseHeaders)
                    ("body", DBytes response.body) ]
                  |> Dval.record typ
                  |> Ok
                  |> DResult

              | Error (HttpClient.BadUrl details) ->
                // TODO: include a DvalSource rather than SourceNone
                return DResult(Error(DString $"Bad URL: {details}"))

              | Error (HttpClient.Timeout) ->
                return DResult(Error(DString $"Request timed out"))

              | Error (HttpClient.NetworkError) ->
                return DResult(Error(DString $"Network error"))

              | Error (HttpClient.Other details) ->
                return DResult(Error(DString details))
            }

          | Error reqHeadersErr, _ ->
            uply {
              match reqHeadersErr with
              | BadInput details -> return DResult(Error(DString details))
              | TypeMismatch details -> return DError(SourceNone, details)
            }

          | _, None ->
            let error = "Expected valid HTTP method (e.g. 'get' or 'POST')"
            uply { return DResult(Error(DString error)) }

        | _ -> incorrectArgs ())
      sqlSpec = NotQueryable
      previewable = Impure
      deprecated = NotDeprecated } ]

let contents = (fns, types)
