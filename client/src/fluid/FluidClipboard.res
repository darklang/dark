open Prelude
module K = FluidKeyboard
module Mouse = Tea.Mouse
module TL = Toplevel
module Regex = Util.Regex

// Tea
module Cmd = Tea.Cmd
module Attrs = Tea.Html2.Attributes
module Events = Tea.Html2.Events
module AC = FluidAutocomplete
module T = FluidToken
module E = ProgramTypes.Expr
module Util = FluidUtil
module Printer = FluidTokenizer

type ast = E.t

let exprToClipboardContents = (expr: FluidExpression.t): Js.Json.t =>
  // The text here comes from the selected text
  Encoders.fluidExpr(expr)

let jsonToExpr = (jsonStr: string): option<E.t> => {
  open Js.Json
  let rec jsJsonToExpr = (j: t): E.t =>
    switch classify(j) {
    | JSONString(str) => EString(gid(), str)
    | JSONFalse => EBool(gid(), false)
    | JSONTrue => EBool(gid(), true)
    | JSONNull => ENull(gid())
    | JSONNumber(float) =>
      let str = Js.Float.toString(float)
      if Util.is63BitInt(str) {
        EInteger(gid(), str)
      } else if Regex.exactly(~re="[0-9]+\\.[0-9]+", str) {
        switch String.split(~on=".", str) {
        | list{whole, fraction} => EFloat(gid(), whole, fraction)
        | _ => recover("invalid float passed the regex", ~debug=str, E.EInteger(gid(), "0"))
        }
      } else {
        // TODO: support floats in the format 3.4e5
        recover("unsupported float in json", ~debug=str, E.EInteger(gid(), "0"))
      }
    | JSONObject(dict) =>
      dict
      |> Js_dict.entries
      |> Array.toList
      |> List.map(~f=((k, json)) => (k, jsJsonToExpr(json)))
      |> (fields => E.ERecord(gid(), fields))
    | JSONArray(arr) =>
      arr |> Array.toList |> List.map(~f=jsJsonToExpr) |> (exprs => E.EList(gid(), exprs))
    }

  try {
    let j = Json.parseOrRaise(jsonStr)
    Some(jsJsonToExpr(j))
  } catch {
  | _ => None
  }
}

let clipboardContentsToExpr = ((text, json): clipboardContents): option<E.t> =>
  switch json {
  | Some(json) =>
    try {
      let expr = Decoders.fluidExpr(json)
      Some(FluidExpression.clone(expr))
    } catch {
    | _ => recover("could not decode", ~debug=json, None)
    }
  | None => jsonToExpr(text)
  }

let clipboardContentsToString = ((text, _): clipboardContents): string => text
