open Prelude

external rollbarConfig : string = "rollbarConfig" [@@bs.val]

let () = Rollbar.init (Json.parseOrRaise rollbarConfig)

type event = < data : Types.performAnalysisParams [@bs.get] > Js.t

type self

external self : self = "self" [@@bs.val]

external onmessage : self -> (event -> unit) -> unit = "onmessage" [@@bs.set]

external postMessage : self -> Types.performAnalysisResult -> unit
  = "postMessage"
  [@@bs.send]

type darkAnalysis =
  < performHandlerAnalysis : string -> string * string [@bs.meth]
  ; performFunctionAnalysis : string -> string * string [@bs.meth] >
  Js.t

external darkAnalysis : darkAnalysis = "darkAnalysis" [@@bs.val]

let () =
  onmessage self (fun event ->
      let result =
        (* TODO: couldn't make Tc work *)
        match event##data with
        | AnalyzeHandler hParams ->
            let encoded =
              Js.Json.stringify (Encoders.performHandlerAnalysisParams hParams)
            in
            let success, msg = darkAnalysis##performHandlerAnalysis encoded in
            if success = "success"
            then Belt.Result.Ok msg
            else
              (* This is not nearly as close to the original Stack Overflow
               * error as I'd like.  I can write
               * Analysis_types.function_result_of_yojson, and
               * confirm with logs that the error occurs in the dval_of_yojson
               * part of `j |> index 3 |> dval_of_yojson`; but wrapping
               * dval_of_yojson in a try/with does not catch the error. We have
               * seen the two messages below on a large DList (because it
               * contains a list and of_yojson maybe isn't tail-recursive -
               * though to_yojson is now:
               * https://github.com/ocaml-community/yojson/issues/47), but it's
               * not impossible that other code could also cause an overflow. *)
              let handler_spec_string =
                let spec = hParams.handler.spec in
                List.map
                  ~f:(function Types.F (_, s) -> s | _ -> "-")
                  [spec.space; spec.name; spec.modifier]
                |> fun ss -> "(" ^ String.join ~sep:", " ss ^ ")"
              in
              let msg =
                if msg = "(\"Stack overflow\")"
                   || msg
                      = "(\"SyntaxError: Invalid regular expression: /maximum call stack/: Maximum call stack size exceeded\")"
                then
                  "Value is too big to send to the editor ("
                  ^ handler_spec_string
                  ^ ")"
                else msg
              in
              reportError "An execution failure occurred in a handler" msg ;
              Belt.Result.Error
                (Types.AnalysisExecutionError (event##data, msg))
        | AnalyzeFunction fParams ->
            let encoded =
              Js.Json.stringify (Encoders.performFunctionAnalysisParams fParams)
            in
            let success, msg = darkAnalysis##performFunctionAnalysis encoded in
            if success = "success"
            then Belt.Result.Ok msg
            else (
              reportError "An execution failure occurred in a function" msg ;
              Belt.Result.Error
                (Types.AnalysisExecutionError (event##data, msg)) )
      in
      let decoded =
        Tc.Result.andThen
          ~f:(fun res ->
            try
              let res = Decoders.analysisEnvelope (Json.parseOrRaise res) in
              Js.log2 "res" (show_analysisEnvelope res) ;
              Belt.Result.Ok res
            with Js.Exn.Error err ->
              let msg =
                err
                |> Js.Exn.message
                |> Tc.Option.withDefault ~default:"Unknown parse error"
              in
              reportError "Parse error in analysisWrapper" msg ;
              Belt.Result.Error (Types.AnalysisParseError msg))
          result
      in
      postMessage self decoded)


[%%raw "var sha2 = require('sha2')"]

[%%raw "module.exports = { sha2: sha2 }"]
