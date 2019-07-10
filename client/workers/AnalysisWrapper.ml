external rollbarConfig : string = "rollbarConfig" [@@bs.val]

let () = Rollbar.init (Json.parseOrRaise rollbarConfig)

type event = < data : Types.performAnalysisParams [@bs.get] > Js.t

type self

external self : self = "self" [@@bs.val]

external onmessage : self -> (event -> unit) -> unit = "onmessage" [@@bs.set]

external postMessage :
  self -> Types.performAnalysisResult -> unit
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
            else (
              Js.logMany [|"An execution failure occurred in a handler"; msg|] ;
              Belt.Result.Error
                (Types.AnalysisExecutionError (event##data, msg)) )
        | AnalyzeFunction fParams ->
            let encoded =
              Js.Json.stringify
                (Encoders.performFunctionAnalysisParams fParams)
            in
            let success, msg = darkAnalysis##performFunctionAnalysis encoded in
            if success = "success"
            then Belt.Result.Ok msg
            else (
              Js.logMany [|"An execution failure occurred in a function"; msg|] ;
              Belt.Result.Error
                (Types.AnalysisExecutionError (event##data, msg)) )
      in
      let decoded =
        Tc.Result.andThen
          ~f:(fun res ->
            try
              Belt.Result.Ok
                (Decoders.analysisEnvelope (Json.parseOrRaise res))
            with Js.Exn.Error err ->
              let msg =
                err
                |> Js.Exn.message
                |> Tc.Option.withDefault ~default:"Unknown parse error"
              in
              Belt.Result.Error (Types.AnalysisParseError msg) )
          result
      in
      postMessage self decoded )


[%%raw
"var sha2 = require('sha2')"]

[%%raw
"module.exports = { sha2: sha2 }"]
