open Libexecution
open Libcommon
open Libexecution.Types.RuntimeT
open Libexecution.Runtime
module Dval = Libexecution.Dval
open Static_assets

let replacements =
  [ ( "StaticAssets::baseUrlForLatest"
    , InProcess
        (function
        | state, [] ->
            url state.canvas_id (latest_deploy_hash state.canvas_id) `Short
            |> Dval.dstr_of_string_exn
        | args ->
            Libexecution.Lib.fail args) )
  ; ( "StaticAssets::baseUrlFor"
    , InProcess
        (function
        | state, [DStr deploy_hash] ->
            url state.canvas_id (Unicode_string.to_string deploy_hash) `Short
            |> Dval.dstr_of_string_exn
        | args ->
            Libexecution.Lib.fail args) )
  ; ( "StaticAssets::urlForLatest"
    , InProcess
        (function
        | state, [DStr file] ->
            url_for
              state.canvas_id
              (latest_deploy_hash state.canvas_id)
              `Short
              (Unicode_string.to_string file)
            |> Dval.dstr_of_string_exn
        | args ->
            Libexecution.Lib.fail args) )
  ; ( "StaticAssets::urlFor"
    , InProcess
        (function
        | state, [DStr deploy_hash; DStr file] ->
            url_for
              state.canvas_id
              (Unicode_string.to_string deploy_hash)
              `Short
              (Unicode_string.to_string file)
            |> Dval.dstr_of_string_exn
        | args ->
            Libexecution.Lib.fail args) )
  ; ( "StaticAssets::fetch"
    , InProcess
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
            Libexecution.Lib.fail args) )
  ; ( "StaticAssets::fetchLatest"
    , InProcess
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
            Libexecution.Lib.fail args) )
  ; ( "StaticAssets::serve"
    , InProcess
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
            Libexecution.Lib.fail args) )
  ; ( "StaticAssets::serveLatest"
    , InProcess
        (function
        | state, [DStr file] ->
            let url =
              "https://avatars1.githubusercontent.com/u/32852373?s=280&v=4"
              (*url_for
                state.canvas_id
                (latest_deploy_hash state.canvas_id)
                `Short
                (Unicode_string.to_string file)*)
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
              |> List.filter (fun (k, v) -> not (String.trim k = ""))
              |> List.filter (fun (k, v) -> not (String.trim v = ""))
            in
            DResult (ResOk (DResp (Response (code, headers), DBytes body)))
            (*( match body with
            | Some dv ->
                DResult (ResOk (DResp (Response (code, headers), dv)))
            | None ->
                DResult
                  (ResError
                     (Dval.dstr_of_string_exn "Response was not UTF-8 safe"))
            )*)
        | args ->
            Libexecution.Lib.fail args) ) ]
