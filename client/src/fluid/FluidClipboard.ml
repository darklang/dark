open Prelude
module K = FluidKeyboard
module Mouse = Tea.Mouse
module TL = Toplevel
module Regex = Util.Regex

(* Tea *)
module Cmd = Tea.Cmd
module Attrs = Tea.Html2.Attributes
module Events = Tea.Html2.Events
module AC = FluidAutocomplete
module T = FluidToken
module E = FluidExpression
module Util = FluidUtil
module Printer = FluidPrinter

type viewState = ViewUtils.viewState

type ast = E.t

let exprToClipboardContents (expr : E.t) : Js.Json.t =
  (* The text here comes from the selected text *)
  Encoders.fluidExpr expr


let jsonToExpr (jsonStr : string) : E.t option =
  let open Js.Json in
  let rec jsJsonToExpr (j : t) : E.t =
    match classify j with
    | JSONString str ->
        EString (gid (), str)
    | JSONFalse ->
        EBool (gid (), false)
    | JSONTrue ->
        EBool (gid (), true)
    | JSONNull ->
        ENull (gid ())
    | JSONNumber float ->
        let str = Js.Float.toString float in
        if Util.is63BitInt str
        then EInteger (gid (), str)
        else if Regex.exactly ~re:"[0-9]+\\.[0-9]+" str
        then
          match String.split ~on:"." str with
          | [whole; fraction] ->
              EFloat (gid (), whole, fraction)
          | _ ->
              recover
                "invalid float passed the regex"
                ~debug:str
                (EInteger (gid (), "0"))
        else
          (* TODO: support floats in the format 3.4e5 *)
          recover
            "unsupported float in json"
            ~debug:str
            (EInteger (gid (), "0"))
    | JSONObject dict ->
        dict
        |> Js_dict.entries
        |> Array.toList
        |> List.map ~f:(fun (k, json) -> (k, jsJsonToExpr json))
        |> fun fields -> ERecord (gid (), fields)
    | JSONArray arr ->
        arr
        |> Array.toList
        |> List.map ~f:jsJsonToExpr
        |> fun exprs -> EList (gid (), exprs)
  in
  try
    let j = Json.parseOrRaise jsonStr in
    Some (jsJsonToExpr j)
  with _ -> None


let clipboardContentsToExpr ((text, json) : clipboardContents) : E.t option =
  match json with
  | Some json ->
    ( try
        let expr = Decoders.fluidExpr json in
        Some (E.clone expr)
      with _ -> recover "could not decode" ~debug:json None )
  | None ->
      jsonToExpr text


let clipboardContentsToString ((text, _) : clipboardContents) : string = text
