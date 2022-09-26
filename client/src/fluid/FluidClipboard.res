open Prelude
module K = FluidKeyboard
module Mouse = Tea.Mouse
module TL = Toplevel

// Tea
module Cmd = Tea.Cmd
module Attrs = Tea.Html.Attributes
module Events = Tea.Html.Events
module AC = FluidAutocomplete
module T = FluidToken
module E = ProgramTypes.Expr
module Util = FluidUtil
module Printer = FluidTokenizer

type ast = E.t

let exprToClipboardContents = (expr: FluidExpression.t): Js.Json.t =>
  // The expr input comes from the selected text
  ProgramTypes.Expr.encode(expr)

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
      switch FluidPartials.parseNumberExpr(str) {
      | Some(expr) => expr
      | None =>
        // TODO: support floats in the format 3.4e5
        recover("unsupported number in json", ~debug=str, E.EInteger(gid(), 0L))
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
      let expr = ProgramTypes.Expr.decode(json)
      Some(FluidExpression.clone(expr))
    } catch {
    | _ => recover("could not decode", ~debug=json, None)
    }
  | None => jsonToExpr(text)
  }

let clipboardContentsToString = ((text, _): clipboardContents): string => text
