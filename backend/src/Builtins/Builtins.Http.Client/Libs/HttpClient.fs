/// HTTP client builtins.
///
/// Two builtins, two API surfaces:
/// - `httpClientRequest` returns a Response with `body : Blob` —
///   buffers the whole body up front; the simple/common case.
/// - `httpClientStream` returns a StreamResponse with
///   `body : Stream<UInt8>` — lazy/chunked; for large bodies, SSE, etc.
///
/// The network machinery lives behind the checked host boundary
/// (`LibExecution.HostHttp`): these builtins parse Dvals, build a
/// `Host.Operation`, and map the structured result back to the
/// Stdlib.HttpClient types. Guest requests run under the host's guest HTTP
/// configuration (`HostTypes.HttpProfile.Guest`); the trusted sync pull
/// under `Sync`.
///
/// TODO collapse into a single builtin: `httpClientStream` becomes the only F#
/// builtin, and `Stdlib.HttpClient.request` a Dark-side wrapper that streams,
/// drains via `Stream.toBlob`, and repacks into a `Response`. Four gates before
/// the collapse stops regressing existing callers:
///   (1) a `body : Blob` param on the stream builtin (today it always sends `[||]`);
///   (2) a body-read timeout (`ResponseHeadersRead` means the cancel token only
///       covers header arrival, so a drain can hang indefinitely);
///   (3) drain-time error translation (an IOException during the drain must become
///       `Result.Error NetworkError`, not an uncaught RuntimeError);
///   (4) telemetry parity with `makeRequest` (one span through the drain, same tags).
/// Until those land, the buffered builtin stays; request construction is shared via
/// the helpers below, so the remaining duplication is small.
module Builtins.Http.Client.Libs.HttpClient

open System.Threading.Tasks
open FSharp.Control.Tasks

open Prelude
open LibExecution
open LibExecution.RuntimeTypes
open LibExecution.Effects
module VT = ValueType
module RTE = RuntimeError
module NR = LibExecution.RuntimeTypes.NameResolution
module Blob = LibExecution.Blob
module Stream = LibExecution.Stream
module Host = LibExecution.Host
module PermissionCheck = LibExecution.PermissionCheck
module Interpreter = LibExecution.Interpreter

let responseOKType () =
  FQTypeName.fqPackage (PackageRefs.Type.Stdlib.HttpClient.response ())
let responseErrorType () =
  FQTypeName.fqPackage (PackageRefs.Type.Stdlib.HttpClient.requestError ())
let streamResponseType () =
  FQTypeName.fqPackage (PackageRefs.Type.Stdlib.HttpClient.streamResponse ())


module BadHeader =
  let toDT (err : HostTypes.HttpBadHeader) : Dval =
    let (caseName, fields) =
      match err with
      | HostTypes.HttpBadHeader.EmptyKey -> "EmptyKey", []
      | HostTypes.HttpBadHeader.InvalidContentType -> "InvalidContentType", []
    let typeName =
      FQTypeName.fqPackage (PackageRefs.Type.Stdlib.HttpClient.badHeader ())
    DEnum(typeName, typeName, [], caseName, fields)

module BadUrl =
  let toDT (err : HostTypes.HttpBadUrl) : Dval =
    let (caseName, fields) =
      match err with
      | HostTypes.HttpBadUrl.UnsupportedProtocol -> "UnsupportedProtocol", []
      | HostTypes.HttpBadUrl.InvalidHost -> "InvalidHost", []
      | HostTypes.HttpBadUrl.InvalidUri -> "InvalidUri", []
      | HostTypes.HttpBadUrl.InvalidRequest -> "InvalidRequest", []

    let typeName =
      FQTypeName.fqPackage (PackageRefs.Type.Stdlib.HttpClient.badUrlDetails ())
    DEnum(typeName, typeName, [], caseName, fields)

module RequestError =
  let toDT (err : HostTypes.HttpRequestError) : Dval =
    let (caseName, fields) =
      match err with
      | HostTypes.HttpRequestError.BadUrl details ->
        "BadUrl", [ BadUrl.toDT details ]
      | HostTypes.HttpRequestError.Timeout -> "Timeout", []
      | HostTypes.HttpRequestError.BadHeader err ->
        "BadHeader", [ BadHeader.toDT err ]
      | HostTypes.HttpRequestError.NetworkError -> "NetworkError", []
      | HostTypes.HttpRequestError.BadMethod -> "BadMethod", []

    let typeName =
      FQTypeName.fqPackage (PackageRefs.Type.Stdlib.HttpClient.requestError ())
    DEnum(typeName, typeName, [], caseName, fields)


let headersType = TList(TTuple(TString, TString, []))

let private headersToDval (headers : List<string * string>) : Dval =
  headers
  |> List.map (fun (k, v) ->
    DTuple(DString(String.toLowercase k), DString(String.toLowercase v), []))
  |> Dval.list (KTTuple(VT.string, VT.string, []))

/// Parse the Dval header list. A non-pair element is a type error (RTE); an
/// empty key is the guest-visible EmptyKey error.
let private parseHeaders
  (vm : VMState)
  (fnName : FQFnName.FQFnName)
  (reqHeaders : List<Dval>)
  : Result<List<string * string>, HostTypes.HttpBadHeader> =
  reqHeaders
  |> List.map (fun item ->
    match item with
    | DTuple(DString k, DString v, []) ->
      let k = String.trim k
      if k = "" then
        // CLEANUP reconsider if we should error here
        Error HostTypes.HttpBadHeader.EmptyKey
      else
        Ok((k, v))
    | notAPair ->
      RTE.Applications.FnParameterNotExpectedType(
        fnName,
        2,
        "headers",
        None,
        VT.list (VT.tuple VT.string VT.string []),
        Dval.toValueType notAPair,
        notAPair
      )
      |> RTE.Apply
      |> raiseRTE vm.threadID)
  |> Result.collect

open LibExecution.Builtin.Shortcuts


/// `Result<Response, RequestError>`, the answer of `read` and `request`.
let private responseOk (response : Dval) : Dval =
  Dval.resultOk
    (KTCustomType(responseOKType (), []))
    (KTCustomType(responseErrorType (), []))
    response

let private responseError (error : Dval) : Dval =
  Dval.resultError
    (KTCustomType(responseOKType (), []))
    (KTCustomType(responseErrorType (), []))
    error

/// One guest-profile request, named for the loop to perform: the shared body of
/// `httpClientRead` and `httpClientRequest`. `wrapper` is the Dark fn a bad header names.
let private guestRequest
  (vm : VMState)
  (wrapper : unit -> string)
  (method : string)
  (uri : string)
  (reqHeaders : List<Dval>)
  (body : byte[])
  : Ply<Dval> =
  let resultOk = responseOk
  let resultError = responseError
  match parseHeaders vm (FQFnName.fqPackage (wrapper ())) reqHeaders with
  | Error headerError ->
    Ply(
      resultError (
        RequestError.toDT (HostTypes.HttpRequestError.BadHeader headerError)
      )
    )
  | Ok headers ->
    let op =
      Host.Operation.HttpRequest(
        HostTypes.HttpProfile.Guest,
        method,
        uri,
        headers,
        body
      )
    Interpreter.requestHost vm op (fun outcome ->
      match outcome with
      | Error failure ->
        Exception.raiseInternal
          "http request failed outside the typed error surface"
          [ "message", failure.message ]
      | Ok response ->
        match Host.expectHttp response with
        | Error err -> Ply(resultError (RequestError.toDT err))
        | Ok response ->
          let typ = responseOKType ()
          let fields =
            [ ("statusCode", Dval.int (bigint response.statusCode))
              ("headers", headersToDval response.headers)
              ("body", Blob.newEphemeral response.body) ]
          Ply(resultOk (DRecord(typ, typ, [], Map fields))))

let private responseResultType =
  TypeReference.result
    (TCustomType(NR.ok (responseOKType ()), []))
    (TCustomType(NR.ok (responseErrorType ()), []))


let fns () : List<BuiltInFn> =
  [ // A GET or HEAD observes and changes nothing, so it is a read: the interpreter hands it back
    // in flight and the program runs on until it looks at the response (`docs/processes.md`,
    // "Reads are concurrent"; `Effects.readsOnly` names this builtin). `Http` stays one word in
    // the permission language; the split is what lets the table say which calls are reads.
    { name = fn "httpClientRead" 0
      typeParams = []
      parameters =
        [ Param.make "method" TString "GET or HEAD"
          Param.make "uri" TString ""
          Param.make "headers" headersType "" ]
      returnType = responseResultType
      description =
        "A GET or HEAD of <param uri>: a read, so it runs concurrently with what follows and is "
        + "waited for where the response is first looked at. Any other method is a BadMethod "
        + "error; `httpClientRequest` is for those."
      fn =
        (function
        | _, vm, _, [| DString method; DString uri; DList(_, reqHeaders) |] ->
          match method.ToUpperInvariant() with
          | "GET"
          | "HEAD" as method ->
            guestRequest
              vm
              PackageRefs.Fn.Stdlib.HttpClient.read
              method
              uri
              reqHeaders
              [||]
          | _ ->
            Ply(
              responseError (RequestError.toDT HostTypes.HttpRequestError.BadMethod)
            )
        | _ -> incorrectArgs ())
      previewable = Impure
      callEffects = set [ Effect.Http ]
      deprecated = NotDeprecated }


    { name = fn "httpClientRequest" 0
      typeParams = []
      parameters =
        [ Param.make "method" TString ""
          Param.make "uri" TString ""
          Param.make "headers" headersType ""
          Param.make "body" TBlob "" ]
      returnType = responseResultType
      description =
        "Make an HTTP call to <param uri>, in program order (a write). Returns a <type Result> "
        + "where the response is wrapped in {{ Ok }} if a response was successfully received "
        + "and parsed, and is wrapped in {{ Error }} otherwise"
      fn =
        (function
        | state,
          vm,
          _,
          [| DString method; DString uri; DList(_, reqHeaders); DBlob bodyRef |] ->
          Blob.withBytes state bodyRef (fun body ->
            guestRequest
              vm
              PackageRefs.Fn.Stdlib.HttpClient.request
              method
              uri
              reqHeaders
              body)
        | _ -> incorrectArgs ())
      previewable = Impure
      callEffects = set [ Effect.Http ]
      deprecated = NotDeprecated }



    // ——————————————————————————————————————————————————————————
    // Streaming HTTP.
    //
    // The body is not buffered into a byte[]; the host opens the response and
    // keeps it in its stream table, and the DStream pulls chunks through the
    // host-issued handle. Bulk consumers (`streamToBlob`) pull whole buffers
    // via `nextChunk`; byte-wise consumers (`streamNext`) see one `DUInt8` at
    // a time synthesised from the same buffer.
    //
    // The disposer releases the host-side response when the consumer drains
    // to EOF or calls `Builtin.streamClose`. Abandoning a stream mid-drain
    // falls back to the GC-triggered finalizer on `Dval.StreamFinalizer`,
    // which runs the same disposer chain when the DStream becomes
    // unreachable. Chunk reads and the close go directly to `HostHttp` — the
    // transfer was authorized when the stream was opened, and the handle is
    // host-issued, so no further policy decision is involved.
    // ——————————————————————————————————————————————————————————
    { name = fn "httpClientStream" 0
      typeParams = []
      parameters =
        [ Param.make "method" TString ""
          Param.make "uri" TString ""
          Param.make "headers" headersType "" ]
      returnType =
        TypeReference.result
          (TCustomType(NR.ok (streamResponseType ()), []))
          (TCustomType(NR.ok (responseErrorType ()), []))
      description =
        "Make a streaming HTTP call to <param uri>. Returns a <type "
        + "StreamResponse> whose `body` is a lazy <type Stream> that yields bytes "
        + "as they arrive. Drain with `Builtin.streamToList`/`streamToBlob`, or "
        + "compose with `streamMap`/`streamFilter`/etc. The underlying HTTP "
        + "response is released when the stream is drained to completion or "
        + "`Builtin.streamClose`d."
      fn =
        let streamTypeOk = KTCustomType(streamResponseType (), [])
        let streamTypeErr = KTCustomType(responseErrorType (), [])
        let resultOk = Dval.resultOk streamTypeOk streamTypeErr
        let resultError = Dval.resultError streamTypeOk streamTypeErr
        (function
        | _, vm, _, [| DString method; DString uri; DList(_, reqHeaders) |] ->
          let headers =
            parseHeaders
              vm
              (FQFnName.fqPackage (PackageRefs.Fn.Stdlib.HttpClient.stream ()))
              reqHeaders
          match headers with
          | Error headerError ->
            Ply(
              resultError (
                RequestError.toDT (HostTypes.HttpRequestError.BadHeader headerError)
              )
            )
          | Ok headers ->
            let op =
              Host.Operation.HttpStreamOpen(
                HostTypes.HttpProfile.Guest,
                method,
                uri,
                headers
              )
            Interpreter.requestHost vm op (fun outcome ->
              match outcome with
              | Error failure ->
                Exception.raiseInternal
                  "http stream open failed outside the typed error surface"
                  [ "message", failure.message ]
              | Ok response ->
                match Host.expectHttpStream response with
                | Error err -> Ply(resultError (RequestError.toDT err))
                | Ok head ->
                  let nextChunk (maxBytes : int) : Ply<Option<byte[]>> =
                    uply {
                      let! chunk = Host.httpStreamRead head.handle maxBytes
                      return chunk
                    }

                  let disposer () = Host.httpStreamClose head.handle

                  let body = Stream.newChunked VT.uint8 nextChunk (Some disposer)

                  let typ = streamResponseType ()
                  let fields =
                    [ ("statusCode", Dval.int (bigint head.statusCode))
                      ("headers", headersToDval head.headers)
                      ("body", body) ]
                  Ply(resultOk (DRecord(typ, typ, [], Map fields))))
        | _ -> incorrectArgs ())
      previewable = Impure
      callEffects = set [ Effect.Http ]
      deprecated = NotDeprecated } ]


let builtins () = Builtin.make [] (fns ())
