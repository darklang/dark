module LibExecution.HttpHeaders

open System.Threading.Tasks
open FSharp.Control.Tasks

open Prelude
open VendoredTablecloth

type Header = string * string
type T = List<Header>

type ContentType =
  | Form
  | Xml
  | Json
  | Text
  | Html

module ContentType =
  let toString (ct : ContentType) : string =
    match ct with
    | Form -> "application/x-www-form-urlencoded"
    | Json -> "application/json; charset=utf-8"
    | Text -> "text/plain; charset=utf-8"
    | Xml -> "application/xml; charset=utf-8"
    | Html -> "text/html; charset=utf-8"

  let toHttpHeader (ct : ContentType) : Header = "Content-Type", toString ct

  let toStringNoCharset (ct : ContentType) : string =
    match ct with
    | Form -> "application/x-www-form-urlencoded"
    | Json -> "application/json"
    | Text -> "text/plain"
    | Xml -> "application/xml"
    | Html -> "text/html"

  let parse (str : string) : Option<ContentType> =
    let ctOnly =
      str |> String.split ";" |> List.tryHead |> Option.map String.toLowercase

    match ctOnly with
    | Some "application/x-www-form-urlencoded" -> Some Form
    | Some "application/json" -> Some Json
    | Some "application/xml" -> Some Xml
    | Some "text/plain" -> Some Text
    | Some "text/html" -> Some Html
    | Some _
    | None -> None

  let textHeader : Header = toHttpHeader Text
  let jsonHeader : Header = toHttpHeader Json
  let formHeader : Header = toHttpHeader Form



let getHeader (headerKey : string) (headers : T) : string option =
  headers
  |> List.tryFind
       (fun ((k : string), (_ : string)) -> String.equalsCaseInsensitive headerKey k)
  |> Option.map (fun (k, v) -> v)

let getContentType (headers : T) : Option<ContentType> =
  headers |> getHeader "Content-type" |> Option.bind ContentType.parse

let hasFormHeader (headers : T) : bool =
  // CLEANUP: this doesn't work properly if a charset is included. But also, this was
  // always false in OCaml because the string we compared against wasn't properly
  // trimmed.
  // getHeader "content-type" headers = Some formContentType
  false

let hasJsonHeader (headers : T) : bool =
  getHeader "content-type" headers
  |> Option.map (fun s -> s.Contains "application/json")
  |> Option.defaultValue false

let hasTextHeader (headers : T) : bool =
  getHeader "content-type" headers
  |> Option.map (fun s -> s.Contains "text/plain")
  |> Option.defaultValue false
