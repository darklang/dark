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
/// TODO collapse into a single builtin. The intended end state is:
///   - `httpClientRequest` is gone from the F# side.
///   - `httpClientStream` is the only F# builtin, and it takes a
///     `body : Blob` (currently always sends `[||]`).
///   - `Stdlib.HttpClient.request` is a Dark-side wrapper: call
///     `HttpClient.stream`, drain the body via `Stream.toBlob`,
///     repack into a `Response`. ~5 lines of Dark.
///
/// Gates before the collapse stops regressing existing callers: a body
/// parameter on the stream operation, a body-read timeout on the drain, and
/// drain-time error translation to a typed NetworkError.
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

/// Refuse the private-network sync transport unless the host explicitly enabled
/// it for this execution and every non-root frame is bundled Darklang code.
/// Checking only the immediate caller is insufficient: an untrusted package can
/// otherwise call a bundled wrapper and turn it into a confused deputy.
let private requireBundledCaller
  (state : ExecutionState)
  (vm : VMState)
  (builtinName : string)
  : unit =
  let rec fnOf (point : ExecutionPoint) : Option<FQFnName.Package> =
    match point with
    | ExecutionPoint.Function(FQFnName.Package p) -> Some p
    | ExecutionPoint.Lambda(parent, _) -> fnOf parent
    | _ -> None
  let rec allBundled (frameID : System.Guid) (sawPackage : bool) : bool =
    match vm.callFrames.TryGetValue frameID with
    | false, _ -> false
    | true, frame ->
      let frameTrusted, sawPackage =
        match fnOf frame.executionPoint with
        | Some p -> state.isBundledPackageFn p, true
        // The root interpreter frame is Source. It is trusted only because the
        // separate host capability below distinguishes `dark sync` from a
        // guest `run`/`eval` source frame.
        | None -> frame.parent.IsNone, sawPackage
      if not frameTrusted then
        false
      else
        match frame.parent with
        | ValueNone -> sawPackage
        | ValueSome(parentID, _, _) -> allBundled parentID sawPackage
  let trusted = state.canUsePrivateNetworkHttp && allBundled vm.currentFrameID false
  if not trusted then
    RuntimeError.UncaughtException(
      $"permission denied: `{builtinName}` is restricted to trusted first-party (Darklang) code",
      []
    )
    |> raiseUntargetedRTE

open LibExecution.Builtin.Shortcuts


let fns () : List<BuiltInFn> =
  [ { name = fn "httpClientRequest" 0
      typeParams = []
      parameters =
        [ Param.make "method" TString ""
          Param.make "uri" TString ""
          Param.make "headers" headersType ""
          Param.make "body" TBlob "" ]
      returnType =
        TypeReference.result
          (TCustomType(NR.ok (responseOKType ()), []))
          (TCustomType(NR.ok (responseErrorType ()), []))
      description =
        "Make blocking HTTP call to <param uri>. Returns a <type Result> where "
        + "the response is wrapped in {{ Ok }} if a response was successfully "
        + "received and parsed, and is wrapped in {{ Error }} otherwise"
      fn =
        let responseTypeOK = KTCustomType(responseOKType (), [])
        let responseTypeErr = KTCustomType(responseErrorType (), [])
        let resultOk = Dval.resultOk responseTypeOK responseTypeErr
        let resultError = Dval.resultError responseTypeOK responseTypeErr
        (function
        | state,
          vm,
          _,
          [| DString method; DString uri; DList(_, reqHeaders); DBlob bodyRef |] ->
          uply {
            let! reqBodyBytes = Blob.readBytes state bodyRef
            let headers =
              parseHeaders
                vm
                (FQFnName.fqPackage (PackageRefs.Fn.Stdlib.HttpClient.request ()))
                reqHeaders
            match headers with
            | Error headerError ->
              return
                resultError (
                  RequestError.toDT (
                    HostTypes.HttpRequestError.BadHeader headerError
                  )
                )
            | Ok headers ->
              let op =
                Host.Operation.HttpRequest(
                  HostTypes.HttpProfile.Guest,
                  method,
                  uri,
                  headers,
                  reqBodyBytes
                )
              match! PermissionCheck.performHost state vm op with
              | Error failure ->
                return
                  Exception.raiseInternal
                    "http request failed outside the typed error surface"
                    [ "message", failure.message ]
              | Ok response ->
                match Host.expectHttp response with
                | Error err -> return resultError (RequestError.toDT err)
                | Ok response ->
                  let typ = responseOKType ()
                  let fields =
                    [ ("statusCode", Dval.int (bigint response.statusCode))
                      ("headers", headersToDval response.headers)
                      ("body", Blob.newEphemeral response.body) ]
                  return resultOk (DRecord(typ, typ, [], Map fields))
          }
        | _ -> incorrectArgs ())
      sqlSpec = NotQueryable
      previewable = Impure
      callEffects = set [ Effect.Http ]
      deprecated = NotDeprecated }


    // GET with SSRF guards OFF, returning raw BYTES — for pulling a peer's op wire over the tailnet.
    // (The safe `httpClientRequest` bans loopback/RFC-1918/tailnet, which a peer's sync server sits behind;
    // the Blob variant hands the body back as bytes for the caller to decode — `Stdlib.Blob.toString` for the
    // JSON wire.) TRUSTED-CLI use: the caller IS the code author; used by `Sync.pull` / `dark sync fetch <url>`.
    // Gated to first-party callers by `requireBundledCaller` below: though
    // registered in the general builtin set, a third-party pulled package
    // cannot call it, so its SSRF-guards-off reach is confined to the bundled
    // sync code it exists for.
    { name = fn "httpGetUnsafeBytes" 0
      typeParams = []
      parameters =
        [ Param.make
            "uri"
            TString
            "URL to GET with SSRF guards OFF (loopback/RFC-1918/tailnet reachable)" ]
      returnType = TypeReference.result TBlob TString
      description =
        "GET <param uri> with NO SSRF guards, returning the raw response body as "
        + "Bytes (Ok) or a message (Error). A non-2xx status is an Error too: a "
        + "peer's 404 page is not a store. For pulling a peer's store over the "
        + "tailnet."
      fn =
        let resultOk = Dval.resultOk KTBlob KTString
        let resultError = Dval.resultError KTBlob KTString
        (function
        | state, vm, _, [| DString uri |] ->
          uply {
            // SSRF guards off (loopback/RFC-1918/tailnet reachable): only the
            // bundled sync code may call this, not a third-party package.
            requireBundledCaller state vm "httpGetUnsafeBytes"
            let op =
              Host.Operation.HttpRequest(
                HostTypes.HttpProfile.Sync,
                "GET",
                uri,
                [],
                [||]
              )
            match! PermissionCheck.performHost state vm op with
            | Error failure ->
              return
                Exception.raiseInternal
                  "http request failed outside the typed error surface"
                  [ "message", failure.message ]
            | Ok response ->
              match Host.expectHttp response with
              | Ok r when r.statusCode >= 200 && r.statusCode < 300 ->
                return resultOk (Blob.newEphemeral r.body)
              | Ok r ->
                let snippet =
                  try
                    let t = System.Text.Encoding.UTF8.GetString(r.body)
                    if t.Length > 200 then t.Substring(0, 200) + "..." else t
                  with _ ->
                    ""
                return resultError (DString $"HTTP {r.statusCode}: {snippet}")
              | Error err ->
                let reason =
                  match err with
                  | HostTypes.HttpRequestError.BadUrl _ -> "bad url"
                  | HostTypes.HttpRequestError.Timeout -> "timeout"
                  | HostTypes.HttpRequestError.BadHeader _ -> "bad header"
                  | HostTypes.HttpRequestError.NetworkError -> "network error"
                  | HostTypes.HttpRequestError.BadMethod -> "bad method"
                return resultError (DString $"fetch failed: {reason}")
          }
        | _ -> incorrectArgs ())
      sqlSpec = NotQueryable
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
        | state, vm, _, [| DString method; DString uri; DList(_, reqHeaders) |] ->
          uply {
            let headers =
              parseHeaders
                vm
                (FQFnName.fqPackage (PackageRefs.Fn.Stdlib.HttpClient.stream ()))
                reqHeaders
            match headers with
            | Error headerError ->
              return
                resultError (
                  RequestError.toDT (
                    HostTypes.HttpRequestError.BadHeader headerError
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
              match! PermissionCheck.performHost state vm op with
              | Error failure ->
                return
                  Exception.raiseInternal
                    "http stream open failed outside the typed error surface"
                    [ "message", failure.message ]
              | Ok response ->
                match Host.expectHttpStream response with
                | Error err -> return resultError (RequestError.toDT err)
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
                  return resultOk (DRecord(typ, typ, [], Map fields))
          }
        | _ -> incorrectArgs ())
      sqlSpec = NotQueryable
      previewable = Impure
      callEffects = set [ Effect.Http ]
      deprecated = NotDeprecated } ]


let builtins () = Builtin.make [] (fns ())
