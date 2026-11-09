/// HTTP client builtins: one call, read two ways.
///
/// - `httpClientRequest` reads the whole response and returns a Response with
///   `body : Bytes`. A GET or HEAD is a read, handed back in flight
///   (`Effects.callReadsOnly`); any other method is a write, in program order.
/// - `httpClientStream` returns a StreamResponse whose `body : Stream<UInt8>`
///   is pulled as it arrives; for large bodies, server-sent events, etc.
///
/// Both take the same arguments and make the same host call: the buffered one is
/// the streaming send drained by the host (`HostHttp.send`), under one deadline and
/// with a failure mid-body reported as a typed error. Two builtins rather than one
/// because the scheduler and a resume treat them differently: a buffered response
/// is a value the log can hand back, and an open stream is not.
///
/// The network machinery lives behind the checked host boundary
/// (`LibExecution.HostHttp`): these builtins parse Dvals, build a
/// `Host.Operation`, and map the structured result back to the
/// Stdlib.HttpClient types. What makes a request safe to send (header names and
/// values, the network guard, the grant) is checked there, below any Dark code;
/// what shapes it (`Stdlib.HttpClient.request` and `stream`) is Dark.
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
module Bytes = LibExecution.Bytes
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
      | HostTypes.HttpBadHeader.InvalidName -> "InvalidName", []
      | HostTypes.HttpBadHeader.InvalidValue -> "InvalidValue", []
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
      | HostTypes.HttpRequestError.TlsError message ->
        "TlsError", [ DString message ]
      | HostTypes.HttpRequestError.BadMethod -> "BadMethod", []

    let typeName =
      FQTypeName.fqPackage (PackageRefs.Type.Stdlib.HttpClient.requestError ())
    DEnum(typeName, typeName, [], caseName, fields)


let headersType = TList(TTuple(TString, TString, []))

let private headersToDval (headers : List<string * string>) : Dval =
  headers
  |> List.map (fun (k, v) ->
    DTuple(
      DString(String.normalizeOutside (String.toLowercase k)),
      DString(String.normalizeOutside (String.toLowercase v)),
      []
    ))
  |> Dval.list (KTTuple(VT.string, VT.string, []))

/// The Dval header list, as written. A non-pair element is a type error (RTE).
/// Trimming and the empty-key check are Dark's (`Stdlib.HttpClient`); the host
/// refuses anything unsafe to send.
let private parseHeaders
  (vm : VMState)
  (fnName : FQFnName.FQFnName)
  (reqHeaders : List<Dval>)
  : List<string * string> =
  reqHeaders
  |> List.map (fun item ->
    match item with
    | DTuple(DString k, DString v, []) -> (k, v)
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

open LibExecution.Builtin.Shortcuts


let private resultOf (okType : FQTypeName.FQTypeName) =
  let ok = KTCustomType(okType, [])
  let error = KTCustomType(responseErrorType (), [])
  (Dval.resultOk ok error, Dval.resultError ok error)

let private resultTypeOf (okType : FQTypeName.FQTypeName) =
  TypeReference.result
    (TCustomType(NR.ok okType, []))
    (TCustomType(NR.ok (responseErrorType ()), []))

let private parameters =
  [ Param.make "method" TString ""
    Param.make "uri" TString ""
    Param.make "headers" headersType ""
    Param.make "body" TBytes "" ]

/// Name the request to the host and hand its answer to `answer`. `wrapper` is the Dark fn a
/// header of the wrong shape names.
let private send
  (state : ExecutionState)
  (vm : VMState)
  (wrapper : FQFnName.FQFnName)
  (args : Dval[])
  (operation : string -> string -> List<string * string> -> byte[] -> Host.Operation)
  (answer : Host.Response -> Ply<Dval>)
  : Ply<Dval> =
  match args with
  | [| DString method; DString uri; DList(_, Items(reqHeaders)); DBytes bodyRef |] ->
    let headers = parseHeaders vm wrapper reqHeaders
    Bytes.withBytes state bodyRef (fun body ->
      Interpreter.requestHost vm (operation method uri headers body) (fun outcome ->
        match outcome with
        | Error failure ->
          Exception.raiseInternal
            "http request failed outside the typed error surface"
            [ "message", failure.message ]
        | Ok response -> answer response))
  | _ -> incorrectArgs ()


let fns () : List<BuiltInFn> =
  [ { name = fn "httpClientRequest" 0
      typeParams = []
      parameters = parameters
      returnType = resultTypeOf (responseOKType ())
      description =
        "Make an HTTP call to <param uri> and read the whole response. A GET or HEAD is a "
        + "read: it runs concurrently with what follows and is waited for where the response "
        + "is first looked at. Any other method runs in program order. Returns the response in "
        + "{{ Ok }}, or why there is none in {{ Error }}."
      fn =
        let (resultOk, resultError) = resultOf (responseOKType ())
        (fun (struct (state, vm, _, args)) ->
          send
            state
            vm
            (FQFnName.fqPackage (PackageRefs.Fn.Stdlib.HttpClient.request ()))
            args
            (fun method uri headers body ->
              Host.Operation.HttpRequest(
                HostTypes.HttpProfile.Guest,
                method,
                uri,
                headers,
                body
              ))
            (fun response ->
              match Host.expectHttp response with
              | Error err -> Ply(resultError (RequestError.toDT err))
              | Ok response ->
                let typ = responseOKType ()
                let fields =
                  [ ("statusCode", Dval.dint64 (int64 response.statusCode))
                    ("headers", headersToDval response.headers)
                    ("body", Bytes.newEphemeral response.body) ]
                Ply(resultOk (DRecord(typ, typ, [], RecordFields.ofList fields)))))
      previewable = Impure
      callEffects = set [ Effect.Http ]
      deprecated = NotDeprecated }


    // ——————————————————————————————————————————————————————————
    // Streaming HTTP.
    //
    // The body is not buffered into a byte[]; the host opens the response and
    // keeps it in its stream table, and the DStream pulls chunks through the
    // host-issued handle. Bulk consumers (`streamToBytes`) pull whole buffers
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
      parameters = parameters
      returnType = resultTypeOf (streamResponseType ())
      description =
        "Make a streaming HTTP call to <param uri>, in program order. Returns a <type "
        + "StreamResponse> whose `body` is a lazy <type Stream> that yields bytes "
        + "as they arrive. Drain with `Builtin.streamToList`/`streamToBytes`, or "
        + "compose with `streamMap`/`streamFilter`/etc. The underlying HTTP "
        + "response is released when the stream is drained to completion or "
        + "`Builtin.streamClose`d."
      fn =
        let (resultOk, resultError) = resultOf (streamResponseType ())
        (fun (struct (state, vm, _, args)) ->
          send
            state
            vm
            (FQFnName.fqPackage (PackageRefs.Fn.Stdlib.HttpClient.stream ()))
            args
            (fun method uri headers body ->
              Host.Operation.HttpStreamOpen(
                HostTypes.HttpProfile.Guest,
                method,
                uri,
                headers,
                body
              ))
            (fun response ->
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
                  [ ("statusCode", Dval.dint64 (int64 head.statusCode))
                    ("headers", headersToDval head.headers)
                    ("body", body) ]
                Ply(resultOk (DRecord(typ, typ, [], RecordFields.ofList fields)))))
      previewable = Impure
      callEffects = set [ Effect.Http ]
      deprecated = NotDeprecated } ]


let builtins () = Builtin.make [] (fns ())
