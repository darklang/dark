/// StdLib functions to manage and retrieve static assets of Dark users
module BackendOnlyStdLib.LibStaticAssets

open LibExecution.RuntimeTypes
open Prelude
open LibExecution.RuntimeTypes

module SA = LibBackend.StaticAssets
module Errors = LibExecution.Errors

let fn = FQFnName.stdlibFnName

let err (str : string) = Ply(Dval.errStr str)

let incorrectArgs = LibExecution.Errors.incorrectArgs

let varA = TVariable "a"
let varB = TVariable "b"

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
      sqlSpec = NotYetImplementedTODO
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
            let! deployHash = SA.latestDeployHash state.program.canvasID
            let url = SA.url state.program.canvasName deployHash SA.Short
            return DStr url
          }
        | _ -> incorrectArgs ())
      sqlSpec = NotYetImplementedTODO
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
      sqlSpec = NotYetImplementedTODO
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
      sqlSpec = NotYetImplementedTODO
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
            let! response = Legacy.HttpclientV0.call url Httpclient.GET [] ""
            match response with
            | Some dv -> return DResult(Ok dv)
            | None -> return DResult(Error(DStr "Response was not UTF-8 safe"))
          }
        | _ -> incorrectArgs ())
      sqlSpec = NotYetImplementedTODO
      previewable = Impure
      deprecated = ReplacedBy(fn "" "" 0) }


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
            let response = Legacy.HttpclientV0.call url Httpclient.GET [] ""
            match response with
            | Some dv -> return Dval.resultOk dv
            | None -> return (DResult(Error(DStr "Response was not UTF-8 safe")))
          }
        | _ -> incorrectArgs ())
      sqlSpec = NotYetImplementedTODO
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
            let response = Legacy.HttpclientV1.call true url Httpclient.GET [] ""
            return DResult(Ok(DBytes(response |> UTF8.toBytes)))
          }
        | _ -> incorrectArgs ())
      sqlSpec = NotYetImplementedTODO
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
            let response = Legacy.HttpclientV0.call url Httpclient.GET [] ""
            match response with
            | Some dv -> return DResult(Ok dv)
            | None -> return DResult(Error(DStr "Response was not UTF-8 safe"))
          }
        | _ -> incorrectArgs ())
      sqlSpec = NotYetImplementedTODO
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
            let response = Legacy.HttpclientV0.call url Httpclient.GET [] ""
            match response with
            | Some dv -> return Dval.resultOk dv
            | None -> return Dval.resultError (DStr "Response was not UTF-8 safe")
          }
        | _ -> incorrectArgs ())
      sqlSpec = NotYetImplementedTODO
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
            let response = Legacy.HttpclientV1.call true url Httpclient.GET [] ""
            return DResult(Ok(DBytes(response |> UTF8.toBytes)))
          }
        | _ -> incorrectArgs ())
      sqlSpec = NotYetImplementedTODO
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
            let body, code, headers, _erroreturnType =
              Legacy.HttpclientV2.http_call_with_code url [] Httpclient.GET [] ""
            let headers =
              headers
              |> List.map (fun (k, v) -> (k, String.trim v))
              |> List.filter (fun (k, v) ->
                not (String.is_substring k "Content-Length"))
              |> List.filter (fun (k, v) ->
                not (String.is_substring k "Transfer-Encoding"))
              |> List.filter (fun (k, v) ->
                not (String.is_substring k "Cache-Control"))
              |> List.filter (fun (k, v) -> not (String.trim k = ""))
              |> List.filter (fun (k, v) -> not (String.trim v = ""))
            match body with
            | Some dv ->
              return DResult(Ok(DHttpResponse(Response(code, headers, dv))))
            | None -> return DResult(Error(DStr "Response was not UTF-8 safe"))
          }
        | _ -> incorrectArgs ())
      sqlSpec = NotYetImplementedTODO
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
            let body, code, headers, _erroreturnType =
              Legacy.HttpclientV2.http_call_with_code
                true
                url
                []
                Httpclient.GET
                []
                ""
            let headers =
              headers
              |> List.map (fun (k, v) -> (k, String.trim v))
              |> List.filter (fun (k, v) ->
                not (String.is_substring k "Content-Length"))
              |> List.filter (fun (k, v) ->
                not (String.is_substring k "Transfer-Encoding"))
              |> List.filter (fun (k, v) ->
                not (String.is_substring k "Cache-Control"))
              |> List.filter (fun (k, v) -> not (String.trim k = ""))
              |> List.filter (fun (k, v) -> not (String.trim v = ""))
            return DResult(Ok(DHttpResponse(Response(code, headers, DBytes(body)))))
          }
        | _ -> incorrectArgs ())
      sqlSpec = NotYetImplementedTODO
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
            let body, code, headers, _erroreturnType =
              Legacy.HttpclientV2.http_call_with_code
                true
                url
                []
                Httpclient.GET
                []
                ""
            let headers =
              headers
              |> List.map (fun (k, v) -> (k, String.trim v))
              |> List.filter (fun (k, v) ->
                not (String.is_substring k "Content-Length"))
              |> List.filter (fun (k, v) ->
                not (String.is_substring k "Transfer-Encoding"))
              |> List.filter (fun (k, v) ->
                not (String.is_substring k "Cache-Control"))
              |> List.filter (fun (k, v) -> not (String.trim k = ""))
              |> List.filter (fun (k, v) -> not (String.trim v = ""))
            return DResult(Ok(DHttpResponse(Response(code, headers, DBytes body))))
          }
        | _ -> incorrectArgs ())
      sqlSpec = NotYetImplementedTODO
      previewable = Impure
      deprecated = ReplacedBy(fn "" "" 0) }


    { name = fn "StaticAssets" "serveLatest" 1
      parameters = [ Param.make "file" TStr "" ]
      returnType = TResult
      description = "Return the specified file from the latest deploy"
      fn =
        (function
        | state, [ DStr file ] ->
          uply {
            let! deployHash = SA.latestDeployHash state.program.canvasID
            let url = SA.urlFor state.program.canvasName deployHash SA.Short file
            let body, code, headers, _erroreturnType =
              Legacy.HttpclientV2.http_call_with_code
                true
                url
                []
                Httpclient.GET
                []
                ""
            let headers =
              headers
              |> List.map (fun (k, v) -> (k, String.trim v))
              |> List.filter (fun (k, v) ->
                not (String.is_substring k "Content-Length"))
              |> List.filter (fun (k, v) ->
                not (String.is_substring k "Transfer-Encoding"))
              |> List.filter (fun (k, v) ->
                not (String.is_substring k "Cache-Control"))
              |> List.filter (fun (k, v) -> not (String.trim k = ""))
              |> List.filter (fun (k, v) -> not (String.trim v = ""))
            return DResult(Ok(DHttpResponse(Response(code, headers, DBytes(body)))))
          }
        | _ -> incorrectArgs ())
      sqlSpec = NotYetImplementedTODO
      previewable = Impure
      deprecated = NotDeprecated } ]
