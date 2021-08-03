open Prelude

@val external rollbarConfig: string = "rollbarConfig"

let () = Rollbar.init(Json.parseOrRaise(rollbarConfig))

type event = {@get "data": Js.nullable<Types.performAnalysisParams>}

type self

@val external self: self = "self"

@set external onmessage: (self, event => unit) => unit = "onmessage"

@bs.send external postMessage: (self, Types.performAnalysisResult) => unit = "postMessage"

type darkAnalysis = {
  @meth
  "performHandlerAnalysis": string => (string, string),
  @meth
  "performFunctionAnalysis": string => (string, string),
}

@val external darkAnalysis: darkAnalysis = "darkAnalysis"

let () = onmessage(self, event => {
  let result = /* TODO: couldn't make Tc work */
  switch Js.Nullable.toOption(event["data"]) {
  | None =>
    /* When we sent too much data, the event just won't have data in it. */
    reportError("Trace was too big to load into analysis", ())
    Belt.Result.Error(Types.AnalysisParseError("Trace was too big to load into analysis"))
  | Some(AnalyzeHandler(hParams) as params) =>
    let encoded = Js.Json.stringify(Encoders.performHandlerAnalysisParams(hParams))

    let (success, msg) = darkAnalysis["performHandlerAnalysis"](encoded)
    if success == "success" {
      Belt.Result.Ok(msg)
    } else {
      /* This is not nearly as close to the original Stack Overflow
       * error as I'd like.  I can write
       * Analysis_types.function_result_of_yojson, and
       * confirm with logs that the error occurs in the dval_of_yojson
       * part of `j |> index 3 |> dval_of_yojson`; but wrapping
       * dval_of_yojson in a try/with does not catch the error. We have
       * seen the two messages below on a large DList (because it
       * contains a list and of_yojson maybe isn't tail-recursive -
       * though to_yojson is now:
       * https://github.com/ocaml-community/yojson/issues/47), but it's
       * not impossible that other code could also cause an overflow. */
      let handler_spec_string = {
        let spec = hParams.handler.spec
        list{spec.space, spec.name, spec.modifier}
        |> List.map(~f=x =>
          switch x {
          | Types.F(_, s) => s
          | _ => "_"
          }
        )
        |> List.filter(~f=\"<>"("_"))
        |> (ss => "(" ++ (String.join(~sep=", ", ss) ++ ")"))
      }

      let msg = if (
        msg == "(\"Stack overflow\")" ||
          msg == "(\"SyntaxError: Invalid regular expression: /maximum call stack/: Maximum call stack size exceeded\")"
      ) {
        "Analysis results are too big to send back to the editor " ++ handler_spec_string
      } else {
        msg
      }

      reportError("An execution failure occurred in a handler", msg)
      Belt.Result.Error(Types.AnalysisExecutionError(params, msg))
    }
  | Some(AnalyzeFunction(fParams) as params) =>
    let encoded = Js.Json.stringify(Encoders.performFunctionAnalysisParams(fParams))

    let (success, msg) = darkAnalysis["performFunctionAnalysis"](encoded)
    if success == "success" {
      Belt.Result.Ok(msg)
    } else {
      reportError("An execution failure occurred in a function", msg)
      Belt.Result.Error(Types.AnalysisExecutionError(params, msg))
    }
  }

  let decoded = Tc.Result.andThen(~f=res =>
    try Belt.Result.Ok(Decoders.analysisEnvelope(Json.parseOrRaise(res))) catch {
    | Js.Exn.Error(err) =>
      let msg = err |> Js.Exn.message |> Tc.Option.unwrap(~default="Unknown parse error")

      reportError("Parse error in analysisWrapper", msg)
      Belt.Result.Error(Types.AnalysisParseError(msg))
    }
  , result)

  postMessage(self, decoded)
})

%%raw("var sha2 = require('sha2')")

%%raw("module.exports = { sha2: sha2 }")
