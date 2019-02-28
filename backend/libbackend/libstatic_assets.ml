open Libexecution
open Libcommon
open Libexecution.Types.RuntimeT
open Libexecution.Runtime
module Dval = Libexecution.Dval
open Static_assets

let replacements =
  [ ( "StaticAssets::urlForLatest"
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
            Httpclient.call url Httpclient.GET [] "" |> Dval.dstr_of_string_exn
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
            Httpclient.call url Httpclient.GET [] "" |> Dval.dstr_of_string_exn
        | args ->
            Libexecution.Lib.fail args) ) ]
