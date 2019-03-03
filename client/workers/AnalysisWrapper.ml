(* window isn't available and rollbar isn't expecting that somehow:
 * https://github.com/rollbar/rollbar.js/pull/703. This works around it.
 * *)

[%%raw
"if (typeof window === 'undefined') { self.window = self; }"]

external rollbarConfig : string = "rollbarConfig" [@@bs.val]

let () = Rollbar.init (Json.parseOrRaise rollbarConfig)

type event = < data : Types.performAnalysisParams [@bs.get] > Js.t

type self

external self : self = "self" [@@bs.val]

external onmessage : self -> (event -> unit) -> unit = "" [@@bs.set]

external postMessage :
  self -> Types.performAnalysisResult -> unit
  = "postMessage"
  [@@bs.send]

type darkAnalysis = < performAnalysis : string -> string [@bs.meth] > Js.t

external darkAnalysis : darkAnalysis = "darkAnalysis" [@@bs.val]

let () =
  onmessage self (fun event ->
      let encoded =
        Js.Json.stringify (Encoders.performAnalysisParams event##data)
      in
      let result =
        (* TODO: couldn't make Tc work *)
        try Belt.Result.Ok (darkAnalysis##performAnalysis encoded)
        with Js.Exn.Error err ->
          let msg =
            err
            |> Js.Exn.message
            |> Tc.Option.withDefault ~default:"Unknown execution error"
          in
          Belt.Result.Error (Types.AnalysisExecutionError (event##data, msg))
      in
      let decoded =
        Tc.Result.andThen
          (fun res ->
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
