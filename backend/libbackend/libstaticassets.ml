open Static_assets
open Libcommon
open Libexecution.Types.RuntimeT
open Libexecution.Runtime
open Libexecution.Lib
module Unicode_string = Libexecution.Unicode_string
module Dval = Libexecution.Dval

let fns : Libexecution.Types.fluid_expr fn list =
  [ { prefix_names = ["StaticAssets::baseUrlFor"]
    ; infix_names = []
    ; parameters = [par "deploy_hash" TStr]
    ; return_type = TStr
    ; description = "Return the baseUrl for the specified deploy hash"
    ; func =
        InProcess
          (function
          | state, [DStr deploy_hash] ->
              url state.canvas_id (Unicode_string.to_string deploy_hash) `Short
              |> Dval.dstr_of_string_exn
          | args ->
              Libexecution.Lib.fail args)
    ; preview_safety = Unsafe
    ; deprecated = false }
  ; { prefix_names = ["StaticAssets::baseUrlForLatest"]
    ; infix_names = []
    ; parameters = []
    ; return_type = TStr
    ; description = "Return the baseUrl for the latest deploy"
    ; func =
        InProcess
          (function
          | state, [] ->
              url state.canvas_id (latest_deploy_hash state.canvas_id) `Short
              |> Dval.dstr_of_string_exn
          | args ->
              Libexecution.Lib.fail args)
    ; preview_safety = Unsafe
    ; deprecated = false }
  ; { prefix_names = ["StaticAssets::urlFor"]
    ; infix_names = []
    ; parameters = [par "deploy_hash" TStr; par "file" TStr]
    ; return_type = TStr
    ; description = "Return a url for the specified file and deploy hash"
    ; func =
        InProcess
          (function
          | state, [DStr deploy_hash; DStr file] ->
              url_for
                state.canvas_id
                (Unicode_string.to_string deploy_hash)
                `Short
                (Unicode_string.to_string file)
              |> Dval.dstr_of_string_exn
          | args ->
              Libexecution.Lib.fail args)
    ; preview_safety = Unsafe
    ; deprecated = false }
  ; { prefix_names = ["StaticAssets::urlForLatest"]
    ; infix_names = []
    ; parameters = [par "file" TStr]
    ; return_type = TStr
    ; description = "Return a url for the specified file and latest deploy"
    ; func =
        InProcess
          (function
          | state, [DStr file] ->
              url_for
                state.canvas_id
                (latest_deploy_hash state.canvas_id)
                `Short
                (Unicode_string.to_string file)
              |> Dval.dstr_of_string_exn
          | args ->
              Libexecution.Lib.fail args)
    ; preview_safety = Unsafe
    ; deprecated = false }
  ; { prefix_names = ["StaticAssets::fetch"]
    ; infix_names = []
    ; parameters = [par "deploy_hash" TStr; par "file" TStr]
    ; return_type = TResult
    ; description =
        "Return the specified file from the deploy_hash - only works on UTF8-safe files for now"
    ; func =
        InProcess
          (function
          | state, [DStr deploy_hash; DStr file] ->
              let url =
                url_for
                  state.canvas_id
                  (Unicode_string.to_string deploy_hash)
                  `Short
                  (Unicode_string.to_string file)
              in
              let response =
                Legacy.HttpclientV0.call url Httpclient.GET [] ""
                |> Dval.dstr_of_string
              in
              ( match response with
              | Some dv ->
                  DResult (ResOk dv)
              | None ->
                  DResult
                    (ResError
                       (Dval.dstr_of_string_exn "Response was not
UTF-8 safe"))
              )
          | args ->
              Libexecution.Lib.fail args)
    ; preview_safety = Unsafe
    ; deprecated = true }
  ; { prefix_names = ["StaticAssets::fetch_v1"]
    ; infix_names = []
    ; parameters = [par "deploy_hash" TStr; par "file" TStr]
    ; return_type = TResult
    ; description =
        "Return the specified file from the deploy_hash - only works on UTF8-safe files for now"
    ; func =
        InProcess
          (function
          | state, [DStr deploy_hash; DStr file] ->
              let url =
                url_for
                  state.canvas_id
                  (Unicode_string.to_string deploy_hash)
                  `Short
                  (Unicode_string.to_string file)
              in
              let response =
                Legacy.HttpclientV0.call url Httpclient.GET [] ""
                |> Dval.dstr_of_string
              in
              ( match response with
              | Some dv ->
                  Dval.to_res_ok dv
              | None ->
                  Dval.to_res_err
                    (Dval.dstr_of_string_exn "Response was not UTF-8 safe") )
          | args ->
              Libexecution.Lib.fail args)
    ; preview_safety = Unsafe
    ; deprecated = false }
  ; { prefix_names = ["StaticAssets::fetchBytes"]
    ; infix_names = []
    ; parameters = [par "deploy_hash" TStr; par "file" TStr]
    ; return_type = TResult
    ; description =
        "Return the bytes of the specified file from the deploy_hash"
    ; func =
        InProcess
          (function
          | state, [DStr deploy_hash; DStr file] ->
              let url =
                url_for
                  state.canvas_id
                  (Unicode_string.to_string deploy_hash)
                  `Short
                  (Unicode_string.to_string file)
              in
              let response =
                Legacy.HttpclientV1.call
                  ~raw_bytes:true
                  url
                  Httpclient.GET
                  []
                  ""
              in
              DResult (ResOk (DBytes (response |> RawBytes.of_string)))
          | args ->
              Libexecution.Lib.fail args)
    ; preview_safety = Unsafe
    ; deprecated = false }
  ; { prefix_names = ["StaticAssets::fetchLatest"]
    ; infix_names = []
    ; parameters = [par "file" TStr]
    ; return_type = TResult
    ; description =
        "Return the specified file from the latest deploy - only works on UTF8-safe files for now"
    ; func =
        InProcess
          (function
          | state, [DStr file] ->
              let url =
                url_for
                  state.canvas_id
                  (latest_deploy_hash state.canvas_id)
                  `Short
                  (Unicode_string.to_string file)
              in
              let response =
                Legacy.HttpclientV0.call url Httpclient.GET [] ""
                |> Dval.dstr_of_string
              in
              ( match response with
              | Some dv ->
                  DResult (ResOk dv)
              | None ->
                  DResult
                    (ResError
                       (Dval.dstr_of_string_exn "Response was not
UTF-8 safe"))
              )
          | args ->
              Libexecution.Lib.fail args)
    ; preview_safety = Unsafe
    ; deprecated = true }
  ; { prefix_names = ["StaticAssets::fetchLatest_v1"]
    ; infix_names = []
    ; parameters = [par "file" TStr]
    ; return_type = TResult
    ; description =
        "Return the specified file from the latest deploy - only works on UTF8-safe files for now"
    ; func =
        InProcess
          (function
          | state, [DStr file] ->
              let url =
                url_for
                  state.canvas_id
                  (latest_deploy_hash state.canvas_id)
                  `Short
                  (Unicode_string.to_string file)
              in
              let response =
                Legacy.HttpclientV0.call url Httpclient.GET [] ""
                |> Dval.dstr_of_string
              in
              ( match response with
              | Some dv ->
                  Dval.to_res_ok dv
              | None ->
                  Dval.to_res_err
                    (Dval.dstr_of_string_exn "Response was not
UTF-8 safe") )
          | args ->
              Libexecution.Lib.fail args)
    ; preview_safety = Unsafe
    ; deprecated = false }
  ; { prefix_names = ["StaticAssets::fetchLatestBytes"]
    ; infix_names = []
    ; parameters = [par "file" TStr]
    ; return_type = TResult
    ; description =
        "Return the bytes of the specified file from the latest deploy"
    ; func =
        InProcess
          (function
          | state, [DStr file] ->
              let url =
                url_for
                  state.canvas_id
                  (latest_deploy_hash state.canvas_id)
                  `Short
                  (Unicode_string.to_string file)
              in
              let response =
                Legacy.HttpclientV1.call
                  ~raw_bytes:true
                  url
                  Httpclient.GET
                  []
                  ""
              in
              DResult (ResOk (DBytes (response |> RawBytes.of_string)))
          | args ->
              Libexecution.Lib.fail args)
    ; preview_safety = Unsafe
    ; deprecated = false }
  ; { prefix_names = ["StaticAssets::serve"]
    ; infix_names = []
    ; parameters = [par "deploy_hash" TStr; par "file" TStr]
    ; return_type = TResult
    ; description =
        "Return the specified file from the latest deploy - only works on UTF8-safe files for now"
    ; func =
        InProcess
          (function
          | state, [DStr deploy_hash; DStr file] ->
              let url =
                url_for
                  state.canvas_id
                  (Unicode_string.to_string deploy_hash)
                  `Short
                  (Unicode_string.to_string file)
              in
              let body, code, headers, _erroreturn_type =
                Legacy.HttpclientV2.http_call_with_code
                  url
                  []
                  Httpclient.GET
                  []
                  ""
              in
              let headers =
                headers
                |> List.map (fun (k, v) -> (k, String.trim v))
                |> List.filter (fun (k, v) ->
                       not
                         (Core_kernel.String.is_substring
                            k
                            ~substring:"Content-Length"))
                |> List.filter (fun (k, v) ->
                       not
                         (Core_kernel.String.is_substring
                            k
                            ~substring:"Transfer-Encoding"))
                |> List.filter (fun (k, v) ->
                       not
                         (Core_kernel.String.is_substring
                            k
                            ~substring:"Cache-Control"))
                |> List.filter (fun (k, v) -> not (String.trim k = ""))
                |> List.filter (fun (k, v) -> not (String.trim v = ""))
              in
              let body = Dval.dstr_of_string body in
              ( match body with
              | Some dv ->
                  DResult (ResOk (DResp (Response (code, headers), dv)))
              | None ->
                  DResult
                    (ResError
                       (Dval.dstr_of_string_exn "Response was not UTF-8 safe"))
              )
          | args ->
              Libexecution.Lib.fail args)
    ; preview_safety = Unsafe
    ; deprecated = true }
  ; { prefix_names = ["StaticAssets::serve_v1"]
    ; infix_names = []
    ; parameters = [par "deploy_hash" TStr; par "file" TStr]
    ; return_type = TResult
    ; description = "Return the specified file from the latest deploy"
    ; func =
        InProcess
          (function
          | state, [DStr deploy_hash; DStr file] ->
              let url =
                url_for
                  state.canvas_id
                  (Unicode_string.to_string deploy_hash)
                  `Short
                  (Unicode_string.to_string file)
              in
              let body, code, headers, _erroreturn_type =
                Legacy.HttpclientV2.http_call_with_code
                  ~raw_bytes:true
                  url
                  []
                  Httpclient.GET
                  []
                  ""
              in
              let headers =
                headers
                |> List.map (fun (k, v) -> (k, String.trim v))
                |> List.filter (fun (k, v) ->
                       not
                         (Core_kernel.String.is_substring
                            k
                            ~substring:"Content-Length"))
                |> List.filter (fun (k, v) ->
                       not
                         (Core_kernel.String.is_substring
                            k
                            ~substring:"Transfer-Encoding"))
                |> List.filter (fun (k, v) ->
                       not
                         (Core_kernel.String.is_substring
                            k
                            ~substring:"Cache-Control"))
                |> List.filter (fun (k, v) -> not (String.trim k = ""))
                |> List.filter (fun (k, v) -> not (String.trim v = ""))
              in
              DResult
                (ResOk
                   (DResp
                      ( Response (code, headers)
                      , DBytes (body |> RawBytes.of_string) )))
          | args ->
              Libexecution.Lib.fail args)
    ; preview_safety = Unsafe
    ; deprecated = false }
  ; { prefix_names = ["StaticAssets::serveLatest"]
    ; infix_names = []
    ; parameters = [par "file" TStr]
    ; return_type = TResult
    ; description =
        "Return the specified file from the latest deploy - only works on UTF8-safe files for now"
    ; func =
        InProcess
          (function
          | state, [DStr file] ->
              let url =
                url_for
                  state.canvas_id
                  (latest_deploy_hash state.canvas_id)
                  `Short
                  (Unicode_string.to_string file)
              in
              let body, code, headers, _erroreturn_type =
                Legacy.HttpclientV2.http_call_with_code
                  ~raw_bytes:true
                  url
                  []
                  Httpclient.GET
                  []
                  ""
              in
              let headers =
                headers
                |> List.map (fun (k, v) -> (k, String.trim v))
                |> List.filter (fun (k, v) ->
                       not
                         (Core_kernel.String.is_substring
                            k
                            ~substring:"Content-Length"))
                |> List.filter (fun (k, v) ->
                       not
                         (Core_kernel.String.is_substring
                            k
                            ~substring:"Transfer-Encoding"))
                |> List.filter (fun (k, v) ->
                       not
                         (Core_kernel.String.is_substring
                            k
                            ~substring:"Cache-Control"))
                |> List.filter (fun (k, v) -> not (String.trim k = ""))
                |> List.filter (fun (k, v) -> not (String.trim v = ""))
              in
              DResult
                (ResOk
                   (DResp
                      ( Response (code, headers)
                      , DBytes (body |> RawBytes.of_string) )))
          | args ->
              Libexecution.Lib.fail args)
    ; preview_safety = Unsafe
    ; deprecated = true }
  ; { prefix_names = ["StaticAssets::serveLatest_v1"]
    ; infix_names = []
    ; parameters = [par "file" TStr]
    ; return_type = TResult
    ; description = "Return the specified file from the latest deploy"
    ; func =
        InProcess
          (function
          | state, [DStr file] ->
              let url =
                url_for
                  state.canvas_id
                  (latest_deploy_hash state.canvas_id)
                  `Short
                  (Unicode_string.to_string file)
              in
              let body, code, headers, _erroreturn_type =
                Legacy.HttpclientV2.http_call_with_code
                  ~raw_bytes:true
                  url
                  []
                  Httpclient.GET
                  []
                  ""
              in
              let headers =
                headers
                |> List.map (fun (k, v) -> (k, String.trim v))
                |> List.filter (fun (k, v) ->
                       not
                         (Core_kernel.String.is_substring
                            k
                            ~substring:"Content-Length"))
                |> List.filter (fun (k, v) ->
                       not
                         (Core_kernel.String.is_substring
                            k
                            ~substring:"Transfer-Encoding"))
                |> List.filter (fun (k, v) ->
                       not
                         (Core_kernel.String.is_substring
                            k
                            ~substring:"Cache-Control"))
                |> List.filter (fun (k, v) -> not (String.trim k = ""))
                |> List.filter (fun (k, v) -> not (String.trim v = ""))
              in
              DResult
                (ResOk
                   (DResp
                      ( Response (code, headers)
                      , DBytes (body |> RawBytes.of_string) )))
          | args ->
              Libexecution.Lib.fail args)
    ; preview_safety = Unsafe
    ; deprecated = false } ]
