open Tc
open Types
open Prelude
module K = FluidKeyboard
module Mouse = Tea.Mouse
module TL = Toplevel
module Regex = Util.Regex

(* Tea *)
module Cmd = Tea.Cmd

module Html = struct
  include Tea.Html

  type 'a html = 'a Vdom.t
end

module Attrs = Tea.Html2.Attributes
module Events = Tea.Html2.Events
module AC = FluidAutocomplete
module T = FluidToken
module E = FluidExpression
module Util = FluidUtil
module Printer = FluidPrinter

type viewState = ViewUtils.viewState

type ast = E.t

let exprToClipboardContents (ast : ast) : clipboardContents =
  match ast with
  | EString (_, str) ->
      `Text str
  | _ ->
      `Json (Encoders.pointerData (PExpr (E.toNExpr ast)))


let jsonToExpr (jsonStr : string) : E.t =
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
        |> List.map ~f:(fun (k, json) -> (gid (), k, jsJsonToExpr json))
        |> fun fields -> ERecord (gid (), fields)
    | JSONArray arr ->
        arr
        |> Array.toList
        |> List.map ~f:jsJsonToExpr
        |> fun exprs -> EList (gid (), exprs)
  in
  try
    let j = Json.parseOrRaise jsonStr in
    jsJsonToExpr j
  with _ -> EString (gid (), jsonStr)


let clipboardContentsToExpr (data : clipboardContents) : E.t option =
  match data with
  | `Json json ->
    ( try
        let data = Decoders.pointerData json |> TL.clonePointerData in
        match data with
        | PExpr expr ->
            Some (E.fromNExpr expr)
        | _ ->
            (* We could support more but don't yet *)
            recover "not a pexpr" ~debug:data None
      with _ -> recover "could not decode" ~debug:json None )
  | `Text text ->
      Some (jsonToExpr text)
  | `None ->
      None


let clipboardContentsToString (data : clipboardContents) : string =
  match data with
  | `Json _ ->
      data
      |> clipboardContentsToExpr
      |> Option.map ~f:Printer.eToString
      |> Option.withDefault ~default:""
      |> Util.trimQuotes
  | `Text text ->
      text
  | `None ->
      ""
