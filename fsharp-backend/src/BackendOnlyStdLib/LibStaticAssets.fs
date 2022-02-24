/// StdLib functions to manage and retrieve static assets of Dark users
module BackendOnlyStdLib.LibStaticAssets

open LibExecution.RuntimeTypes
open Prelude

module Errors = LibExecution.Errors

let fn = FQFnName.stdlibFnName

let err (str : string) = Ply(Dval.errStr str)

let incorrectArgs = LibExecution.Errors.incorrectArgs

let varA = TVariable "a"
let varB = TVariable "b"

let fns : List<BuiltInFn> = []
//   [ { name = fn "StaticAssets" "baseUrlFor" 0
//
//     ; parameters = [Param.make "deploy_hash" TStr ""]
//     ; returnType = TStr
//     ; description = "Return the baseUrl for the specified deploy hash"
//     ; fn =
//           (function
//           | state, [DStr deploy_hash] ->
//               url state.canvas_id (Unicode_string.to_string deploy_hash) `Short
//               |> DStr
//           | _ ->
//               Libexecution.Lib.incorrectArgs ())
//     ; sqlSpec = NotYetImplementedTODO
//     ; previewable = Impure
//     ; deprecated = NotDeprecated }
//   ; { name = fn "StaticAssets" "baseUrlForLatest" 0
//     ; parameters = []
//     ; returnType = TStr
//     ; description = "Return the baseUrl for the latest deploy"
//     ; fn =
//           (function
//           | state, [] ->
//               url state.canvas_id (latest_deploy_hash state.canvas_id) `Short
//               |> DStr
//           | _ ->
//               Libexecution.Lib.incorrectArgs ())
//     ; sqlSpec = NotYetImplementedTODO
//     ; previewable = Impure
//     ; deprecated = NotDeprecated }
//   ; { name = fn "StaticAssets" "urlFor" 0
//     ; parameters = [Param.make "deploy_hash" TStr ""; Param.make "file" TStr ""]
//     ; returnType = TStr
//     ; description = "Return a url for the specified file and deploy hash"
//     ; fn =
//           (function
//           | state, [DStr deploy_hash; DStr file] ->
//               url_for
//                 state.canvas_id
//                 (Unicode_string.to_string deploy_hash)
//                 `Short
//                 (Unicode_string.to_string file)
//               |> DStr
//           | _ ->
//               Libexecution.Lib.incorrectArgs ())
//     ; sqlSpec = NotYetImplementedTODO
//     ; previewable = Impure
//     ; deprecated = NotDeprecated }
//   ; { name = fn "StaticAssets" "urlForLatest" 0
//     ; parameters = [Param.make "file" TStr ""]
//     ; returnType = TStr
//     ; description = "Return a url for the specified file and latest deploy"
//     ; fn =
//           (function
//           | state, [DStr file] ->
//               url_for
//                 state.canvas_id
//                 (latest_deploy_hash state.canvas_id)
//                 `Short
//                 (Unicode_string.to_string file)
//               |> DStr
//           | _ ->
//               Libexecution.Lib.incorrectArgs ())
//     ; sqlSpec = NotYetImplementedTODO
//     ; previewable = Impure
//     ; deprecated = NotDeprecated }
//   ; { name = fn "StaticAssets" "fetch" 0
//     ; parameters = [Param.make "deploy_hash" TStr ""; Param.make "file" TStr ""]
//     ; returnType = TResult
//     ; description =
//         "Return the specified file from the deploy_hash - only works on UTF8-safe files for now"
//     ; fn =
//           (function
//           | state, [DStr deploy_hash; DStr file] ->
//               let url =
//                 url_for
//                   state.canvas_id
//                   (Unicode_string.to_string deploy_hash)
//                   `Short
//                   (Unicode_string.to_string file)
//               in
//               let response =
//                 Legacy.HttpclientV0.call url Httpclient.GET [] ""
//                 |> Dval.dstr_of_string
//               in
//               ( match response with
//               | Some dv ->
//                   DResult (Ok dv)
//               | None ->
//                   DResult
//                     (ResError
//                        (DStr "Response was not
// UTF-8 safe"))
//               )
//           | _ ->
//               Libexecution.Lib.incorrectArgs ())
//     ; sqlSpec = NotYetImplementedTODO
//     ; previewable = Impure
//     ; deprecated = ReplacedBy(fn "" "" 0) }
//   ; { name = fn "StaticAssets" "fetch" 1
//     ; parameters = [Param.make "deploy_hash" TStr ""; Param.make "file" TStr ""]
//     ; returnType = TResult
//     ; description =
//         "Return the specified file from the deploy_hash - only works on UTF8-safe files for now"
//     ; fn =
//           (function
//           | state, [DStr deploy_hash; DStr file] ->
//               let url =
//                 url_for
//                   state.canvas_id
//                   (Unicode_string.to_string deploy_hash)
//                   `Short
//                   (Unicode_string.to_string file)
//               in
//               let response =
//                 Legacy.HttpclientV0.call url Httpclient.GET [] ""
//                 |> Dval.dstr_of_string
//               in
//               ( match response with
//               | Some dv ->
//                   Dval.to_res_ok dv
//               | None ->
//                   Dval.to_res_err
//                     (DStr "Response was not UTF-8 safe") )
//           | _ ->
//               Libexecution.Lib.incorrectArgs ())
//     ; sqlSpec = NotYetImplementedTODO
//     ; previewable = Impure
//     ; deprecated = NotDeprecated }
//   ; { name = fn "StaticAssets" "fetchBytes" 0
//     ; parameters = [Param.make "deploy_hash" TStr ""; Param.make "file" TStr ""]
//     ; returnType = TResult
//     ; description =
//         "Return the bytes of the specified file from the deploy_hash"
//     ; fn =
//           (function
//           | state, [DStr deploy_hash; DStr file] ->
//               let url =
//                 url_for
//                   state.canvas_id
//                   (Unicode_string.to_string deploy_hash)
//                   `Short
//                   (Unicode_string.to_string file)
//               in
//               let response =
//                 Legacy.HttpclientV1.call
//                   ~raw_bytes:true
//                   url
//                   Httpclient.GET
//                   []
//                   ""
//               in
//               DResult (Ok (DBytes (response |> RawBytes.of_string)))
//           | _ ->
//               Libexecution.Lib.incorrectArgs ())
//     ; sqlSpec = NotYetImplementedTODO
//     ; previewable = Impure
//     ; deprecated = NotDeprecated }
//   ; { name = fn "StaticAssets" "fetchLatest" 0
//     ; parameters = [Param.make "file" TStr ""]
//     ; returnType = TResult
//     ; description =
//         "Return the specified file from the latest deploy - only works on UTF8-safe files for now"
//     ; fn =
//           (function
//           | state, [DStr file] ->
//               let url =
//                 url_for
//                   state.canvas_id
//                   (latest_deploy_hash state.canvas_id)
//                   `Short
//                   (Unicode_string.to_string file)
//               in
//               let response =
//                 Legacy.HttpclientV0.call url Httpclient.GET [] ""
//                 |> Dval.dstr_of_string
//               in
//               ( match response with
//               | Some dv ->
//                   DResult (Ok dv)
//               | None ->
//                   DResult
//                     (ResError
//                        (DStr "Response was not
// UTF-8 safe"))
//               )
//           | _ ->
//               Libexecution.Lib.incorrectArgs ())
//     ; sqlSpec = NotYetImplementedTODO
//     ; previewable = Impure
//     ; deprecated = ReplacedBy(fn "" "" 0) }
//   ; { name = fn "StaticAssets" "fetchLatest" 1
//     ; parameters = [Param.make "file" TStr ""]
//     ; returnType = TResult
//     ; description =
//         "Return the specified file from the latest deploy - only works on UTF8-safe files for now"
//     ; fn =
//           (function
//           | state, [DStr file] ->
//               let url =
//                 url_for
//                   state.canvas_id
//                   (latest_deploy_hash state.canvas_id)
//                   `Short
//                   (Unicode_string.to_string file)
//               in
//               let response =
//                 Legacy.HttpclientV0.call url Httpclient.GET [] ""
//                 |> Dval.dstr_of_string
//               in
//               ( match response with
//               | Some dv ->
//                   Dval.to_res_ok dv
//               | None ->
//                   Dval.to_res_err
//                     (DStr "Response was not
// UTF-8 safe") )
//           | _ ->
//               Libexecution.Lib.incorrectArgs ())
//     ; sqlSpec = NotYetImplementedTODO
//     ; previewable = Impure
//     ; deprecated = NotDeprecated }
//   ; { name = fn "StaticAssets" "fetchLatestBytes" 0
//     ; parameters = [Param.make "file" TStr ""]
//     ; returnType = TResult
//     ; description =
//         "Return the bytes of the specified file from the latest deploy"
//     ; fn =
//           (function
//           | state, [DStr file] ->
//               let url =
//                 url_for
//                   state.canvas_id
//                   (latest_deploy_hash state.canvas_id)
//                   `Short
//                   (Unicode_string.to_string file)
//               in
//               let response =
//                 Legacy.HttpclientV1.call
//                   ~raw_bytes:true
//                   url
//                   Httpclient.GET
//                   []
//                   ""
//               in
//               DResult (Ok (DBytes (response |> RawBytes.of_string)))
//           | _ ->
//               Libexecution.Lib.incorrectArgs ())
//     ; sqlSpec = NotYetImplementedTODO
//     ; previewable = Impure
//     ; deprecated = NotDeprecated }
//   ; { name = fn "StaticAssets" "serve" 0
//     ; parameters = [Param.make "deploy_hash" TStr ""; Param.make "file" TStr ""]
//     ; returnType = TResult
//     ; description =
//         "Return the specified file from the latest deploy - only works on UTF8-safe files for now"
//     ; fn =
//           (function
//           | state, [DStr deploy_hash; DStr file] ->
//               let url =
//                 url_for
//                   state.canvas_id
//                   (Unicode_string.to_string deploy_hash)
//                   `Short
//                   (Unicode_string.to_string file)
//               in
//               let body, code, headers, _erroreturnType =
//                 Legacy.HttpclientV2.http_call_with_code
//                   url
//                   []
//                   Httpclient.GET
//                   []
//                   ""
//               in
//               let headers =
//                 headers
//                 |> List.map (fun (k, v) -> (k, String.trim v))
//                 |> List.filter (fun (k, v) ->
//                        not
//                          (Core_kernel.String.is_substring
//                             k
//                             "Content-Length"))
//                 |> List.filter (fun (k, v) ->
//                        not
//                          (Core_kernel.String.is_substring
//                             k
//                             "Transfer-Encoding"))
//                 |> List.filter (fun (k, v) ->
//                        not
//                          (Core_kernel.String.is_substring
//                             k
//                             "Cache-Control"))
//                 |> List.filter (fun (k, v) -> not (String.trim k = ""))
//                 |> List.filter (fun (k, v) -> not (String.trim v = ""))
//               in
//               let body = Dval.dstr_of_string body in
//               ( match body with
//               | Some dv ->
//                   DResult (Ok (DResp (Response (code, headers), dv)))
//               | None ->
//                   DResult
//                     (ResError
//                        (DStr "Response was not UTF-8 safe"))
//               )
//           | _ ->
//               Libexecution.Lib.incorrectArgs ())
//     ; sqlSpec = NotYetImplementedTODO
//     ; previewable = Impure
//     ; deprecated = ReplacedBy(fn "" "" 0) }
//   ; { name = fn "StaticAssets" "serve" 1
//     ; parameters = [Param.make "deploy_hash" TStr ""; Param.make "file" TStr ""]
//     ; returnType = TResult
//     ; description = "Return the specified file from the latest deploy"
//     ; fn =
//           (function
//           | state, [DStr deploy_hash; DStr file] ->
//               let url =
//                 url_for
//                   state.canvas_id
//                   (Unicode_string.to_string deploy_hash)
//                   `Short
//                   (Unicode_string.to_string file)
//               in
//               let body, code, headers, _erroreturnType =
//                 Legacy.HttpclientV2.http_call_with_code
//                   ~raw_bytes:true
//                   url
//                   []
//                   Httpclient.GET
//                   []
//                   ""
//               in
//               let headers =
//                 headers
//                 |> List.map (fun (k, v) -> (k, String.trim v))
//                 |> List.filter (fun (k, v) ->
//                        not
//                          (Core_kernel.String.is_substring
//                             k
//                             "Content-Length"))
//                 |> List.filter (fun (k, v) ->
//                        not
//                          (Core_kernel.String.is_substring
//                             k
//                             "Transfer-Encoding"))
//                 |> List.filter (fun (k, v) ->
//                        not
//                          (Core_kernel.String.is_substring
//                             k
//                             "Cache-Control"))
//                 |> List.filter (fun (k, v) -> not (String.trim k = ""))
//                 |> List.filter (fun (k, v) -> not (String.trim v = ""))
//               in
//               DResult
//                 (Ok
//                    (DResp
//                       ( Response (code, headers)
//                       , DBytes (body |> RawBytes.of_string) )))
//           | _ ->
//               Libexecution.Lib.incorrectArgs ())
//     ; sqlSpec = NotYetImplementedTODO
//     ; previewable = Impure
//     ; deprecated = NotDeprecated }
//   ; { name = fn "StaticAssets" "serveLatest" 0
//     ; parameters = [Param.make "file" TStr ""]
//     ; returnType = TResult
//     ; description =
//         "Return the specified file from the latest deploy - only works on UTF8-safe files for now"
//     ; fn =
//           (function
//           | state, [DStr file] ->
//               let url =
//                 url_for
//                   state.canvas_id
//                   (latest_deploy_hash state.canvas_id)
//                   `Short
//                   (Unicode_string.to_string file)
//               in
//               let body, code, headers, _erroreturnType =
//                 Legacy.HttpclientV2.http_call_with_code
//                   ~raw_bytes:true
//                   url
//                   []
//                   Httpclient.GET
//                   []
//                   ""
//               in
//               let headers =
//                 headers
//                 |> List.map (fun (k, v) -> (k, String.trim v))
//                 |> List.filter (fun (k, v) ->
//                        not
//                          (Core_kernel.String.is_substring
//                             k
//                             "Content-Length"))
//                 |> List.filter (fun (k, v) ->
//                        not
//                          (Core_kernel.String.is_substring
//                             k
//                             "Transfer-Encoding"))
//                 |> List.filter (fun (k, v) ->
//                        not
//                          (Core_kernel.String.is_substring
//                             k
//                             "Cache-Control"))
//                 |> List.filter (fun (k, v) -> not (String.trim k = ""))
//                 |> List.filter (fun (k, v) -> not (String.trim v = ""))
//               in
//               DResult
//                 (Ok
//                    (DResp
//                       ( Response (code, headers)
//                       , DBytes (body |> RawBytes.of_string) )))
//           | _ ->
//               Libexecution.Lib.incorrectArgs ())
//     ; sqlSpec = NotYetImplementedTODO
//     ; previewable = Impure
//     ; deprecated = ReplacedBy(fn "" "" 0) }
//   ; { name = fn "StaticAssets" "serveLatest" 1
//     ; parameters = [Param.make "file" TStr ""]
//     ; returnType = TResult
//     ; description = "Return the specified file from the latest deploy"
//     ; fn =
//           (function
//           | state, [DStr file] ->
//               let url =
//                 url_for
//                   state.canvas_id
//                   (latest_deploy_hash state.canvas_id)
//                   `Short
//                   (Unicode_string.to_string file)
//               in
//               let body, code, headers, _erroreturnType =
//                 Legacy.HttpclientV2.http_call_with_code
//                   ~raw_bytes:true
//                   url
//                   []
//                   Httpclient.GET
//                   []
//                   ""
//               in
//               let headers =
//                 headers
//                 |> List.map (fun (k, v) -> (k, String.trim v))
//                 |> List.filter (fun (k, v) ->
//                        not
//                          (Core_kernel.String.is_substring
//                             k
//                             "Content-Length"))
//                 |> List.filter (fun (k, v) ->
//                        not
//                          (Core_kernel.String.is_substring
//                             k
//                             "Transfer-Encoding"))
//                 |> List.filter (fun (k, v) ->
//                        not
//                          (Core_kernel.String.is_substring
//                             k
//                             "Cache-Control"))
//                 |> List.filter (fun (k, v) -> not (String.trim k = ""))
//                 |> List.filter (fun (k, v) -> not (String.trim v = ""))
//               in
//               DResult
//                 (Ok
//                    (DResp
//                       ( Response (code, headers)
//                       , DBytes (body |> RawBytes.of_string) )))
//           | _ ->
//               Libexecution.Lib.incorrectArgs ())
//     ; sqlSpec = NotYetImplementedTODO
//     ; previewable = Impure
//     ; deprecated = NotDeprecated } ]
//
