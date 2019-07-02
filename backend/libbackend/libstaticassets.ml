open Libexecution
open Libcommon
open Libexecution.Types.RuntimeT
open Libexecution.Runtime
module Dval = Libexecution.Dval
open Static_assets
open Lib

let fns : Lib.shortfn list =
  [ { pns = ["StaticAssets::baseUrlFor"]
    ; ins = []
    ; p = [par "deploy_hash" TStr]
    ; r = TStr
    ; d = "Return the baseUrl for the specified deploy hash"
    ; f =
        InProcess
          (function
          | state, [DStr deploy_hash] ->
              url state.canvas_id (Unicode_string.to_string deploy_hash) `Short
              |> Dval.dstr_of_string_exn
          | args ->
              Libexecution.Lib.fail args)
    ; ps = false
    ; dep = false }
  ; { pns = ["StaticAssets::baseUrlForLatest"]
    ; ins = []
    ; p = []
    ; r = TStr
    ; d = "Return the baseUrl for the latest deploy"
    ; f =
        InProcess
          (function
          | state, [] ->
              url state.canvas_id (latest_deploy_hash state.canvas_id) `Short
              |> Dval.dstr_of_string_exn
          | args ->
              Libexecution.Lib.fail args)
    ; ps = false
    ; dep = false }
  ; { pns = ["StaticAssets::urlFor"]
    ; ins = []
    ; p = [par "deploy_hash" TStr; par "file" TStr]
    ; r = TStr
    ; d = "Return a url for the specified file and deploy hash"
    ; f =
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
    ; ps = false
    ; dep = false }
  ; { pns = ["StaticAssets::urlForLatest"]
    ; ins = []
    ; p = [par "file" TStr]
    ; r = TStr
    ; d = "Return a url for the specified file and latest deploy"
    ; f =
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
    ; ps = false
    ; dep = false }
  ; { pns = ["StaticAssets::fetch"]
    ; ins = []
    ; p = [par "deploy_hash" TStr; par "file" TStr]
    ; r = TResult
    ; d =
        "Return the specified file from the deploy_hash - only works on UTF8-safe files for now"
    ; f =
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
                Httpclient.call url Httpclient.GET [] "" |> Dval.dstr_of_string
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
    ; ps = false
    ; dep = false }
  ; { pns = ["StaticAssets::fetchBytes"]
    ; ins = []
    ; p = [par "deploy_hash" TStr; par "file" TStr]
    ; r = TResult
    ; d = "Return the bytes of the specified file from the deploy_hash"
    ; f =
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
              let response = Httpclient.call url Httpclient.GET [] "" in
              DResult (ResOk (DBytes (response |> RawBytes.of_string)))
          | args ->
              Libexecution.Lib.fail args)
    ; ps = false
    ; dep = false }
  ; { pns = ["StaticAssets::fetchLatest"]
    ; ins = []
    ; p = [par "file" TStr]
    ; r = TResult
    ; d =
        "Return the specified file from the latest deploy - only works on UTF8-safe files for now"
    ; f =
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
                Httpclient.call url Httpclient.GET [] "" |> Dval.dstr_of_string
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
    ; ps = false
    ; dep = false }
  ; { pns = ["StaticAssets::fetchLatestBytes"]
    ; ins = []
    ; p = [par "file" TStr]
    ; r = TResult
    ; d = "Return the bytes of the specified file from the latest deploy"
    ; f =
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
              let response = Httpclient.call url Httpclient.GET [] "" in
              DResult (ResOk (DBytes (response |> RawBytes.of_string)))
          | args ->
              Libexecution.Lib.fail args)
    ; ps = false
    ; dep = false }
  ; { pns = ["StaticAssets::serve"]
    ; ins = []
    ; p = [par "deploy_hash" TStr; par "file" TStr]
    ; r = TResult
    ; d =
        "Return the specified file from the latest deploy - only works on UTF8-safe files for now"
    ; f =
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
              let body, code, headers, _error =
                Httpclient.http_call_with_code url [] Httpclient.GET [] ""
              in
              let headers =
                headers
                |> List.map (fun (k, v) -> (k, String.trim v))
                |> List.filter (fun (k, v) ->
                       not
                         (Core_kernel.String.is_substring
                            k
                            ~substring:"Content-Length") )
                |> List.filter (fun (k, v) ->
                       not
                         (Core_kernel.String.is_substring
                            k
                            ~substring:"Transfer-Encoding") )
                |> List.filter (fun (k, v) ->
                       not
                         (Core_kernel.String.is_substring
                            k
                            ~substring:"Cache-Control") )
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
    ; ps = false
    ; dep = true }
  ; { pns = ["StaticAssets::serve_v1"]
    ; ins = []
    ; p = [par "deploy_hash" TStr; par "file" TStr]
    ; r = TResult
    ; d = "Return the specified file from the latest deploy"
    ; f =
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
              let body, code, headers, _error =
                Httpclient.http_call_with_code url [] Httpclient.GET [] ""
              in
              let headers =
                headers
                |> List.map (fun (k, v) -> (k, String.trim v))
                |> List.filter (fun (k, v) ->
                       not
                         (Core_kernel.String.is_substring
                            k
                            ~substring:"Content-Length") )
                |> List.filter (fun (k, v) ->
                       not
                         (Core_kernel.String.is_substring
                            k
                            ~substring:"Transfer-Encoding") )
                |> List.filter (fun (k, v) ->
                       not
                         (Core_kernel.String.is_substring
                            k
                            ~substring:"Cache-Control") )
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
    ; ps = false
    ; dep = false }
  ; { pns = ["StaticAssets::serveLatest"]
    ; ins = []
    ; p = [par "file" TStr]
    ; r = TResult
    ; d =
        "Return the specified file from the latest deploy - only works on UTF8-safe files for now"
    ; f =
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
              let body, code, headers, _error =
                Httpclient.http_call_with_code url [] Httpclient.GET [] ""
              in
              let headers =
                headers
                |> List.map (fun (k, v) -> (k, String.trim v))
                |> List.filter (fun (k, v) ->
                       not
                         (Core_kernel.String.is_substring
                            k
                            ~substring:"Content-Length") )
                |> List.filter (fun (k, v) ->
                       not
                         (Core_kernel.String.is_substring
                            k
                            ~substring:"Transfer-Encoding") )
                |> List.filter (fun (k, v) ->
                       not
                         (Core_kernel.String.is_substring
                            k
                            ~substring:"Cache-Control") )
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
    ; ps = false
    ; dep = true }
  ; { pns = ["StaticAssets::serveLatest_v1"]
    ; ins = []
    ; p = [par "file" TStr]
    ; r = TResult
    ; d = "Return the specified file from the latest deploy"
    ; f =
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
              let body, code, headers, _error =
                Httpclient.http_call_with_code url [] Httpclient.GET [] ""
              in
              let headers =
                headers
                |> List.map (fun (k, v) -> (k, String.trim v))
                |> List.filter (fun (k, v) ->
                       not
                         (Core_kernel.String.is_substring
                            k
                            ~substring:"Content-Length") )
                |> List.filter (fun (k, v) ->
                       not
                         (Core_kernel.String.is_substring
                            k
                            ~substring:"Transfer-Encoding") )
                |> List.filter (fun (k, v) ->
                       not
                         (Core_kernel.String.is_substring
                            k
                            ~substring:"Cache-Control") )
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
    ; ps = false
    ; dep = false } ]
