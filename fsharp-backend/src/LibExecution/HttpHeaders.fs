module LibExecution.HttpHeaders

open System.Threading.Tasks
open FSharp.Control.Tasks

open Prelude
open VendoredTablecloth

type Header = string * string
type T = List<Header>

module MediaType =
  type T =
    | Form
    | Xml
    | Json
    | Text
    | Html
    | Other of string

    override this.ToString() : string =
      match this with
      | Form -> "application/x-www-form-urlencoded"
      | Xml -> "application/xml"
      | Json -> "application/json"
      | Text -> "text/plain"
      | Html -> "text/html"
      | Other s -> s

  let parse (str : string) : T =
    match String.trim str with
    | "application/x-www-form-urlencoded" -> Form
    | "application/xml" -> Xml
    | "application/json" -> Json
    | "text/plain" -> Text
    | "text/html" -> Html
    | _ -> Other str




module Charset =
  type T =
    | Utf8
    | NotUtf8 of string

    override this.ToString() : string =
      match this with
      | Utf8 -> "utf-8"
      | NotUtf8 s -> s

  let parse (str : string) : T =
    match String.trim str with
    | "utf-8" -> Utf8
    | _ -> NotUtf8 str


module ContentType =
  type T =
    | Known of MediaType.T * Charset.T
    | KnownNoCharset of MediaType.T
    | Unknown of string // don't parse out charset or boundary or anything

    override this.ToString() : string =
      match this with
      | Known (mt, cs) -> $"{mt}; charset={cs}"
      | KnownNoCharset (mt) -> string mt
      | Unknown s -> s

  let toMediaType (ct : T) : Option<MediaType.T> =
    match ct with
    | Known (mt, _) -> Some mt
    | KnownNoCharset (mt) -> Some mt
    | Unknown s -> None

  let toHttpHeader (ct : T) : Header = "Content-Type", string ct

  let parse (str : string) : T =
    match String.split ";" str |> List.map String.trim with
    | [ mt; cs ] ->
      match String.split "=" cs |> List.map String.trim with
      | [ "charset"; cs ] -> Known(MediaType.parse mt, Charset.parse cs)
      | _ -> Unknown(str)
    | [ mt ] -> KnownNoCharset(MediaType.parse mt)
    | _ -> Unknown str

  let text = (Known(MediaType.Text, Charset.Utf8))
  let json = (Known(MediaType.Json, Charset.Utf8))
  let form = (KnownNoCharset MediaType.Form)

  let textHeader : Header = toHttpHeader text
  let jsonHeader : Header = toHttpHeader json
  let formHeader : Header = toHttpHeader form



let getHeader (headerKey : string) (headers : T) : string option =
  headers
  |> List.tryFind (fun ((k : string), (_ : string)) -> String.equalsCaseInsensitive headerKey k)
  |> Option.map (fun (k, v) -> v)

let getContentType (headers : T) : Option<ContentType.T> = headers |> getHeader "Content-type" |> Option.map ContentType.parse

let getMediaType (headers : T) : Option<MediaType.T> =
  headers
  |> getHeader "Content-type"
  |> Option.map ContentType.parse
  |> Option.bind ContentType.toMediaType

let hasFormHeader (headers : T) : bool =
  // CLEANUP: this doesn't work properly if a charset is included. But also, this was
  // always false in OCaml because the string we compared against wasn't properly
  // trimmed.
  // getHeader "content-type" headers = Some formContentType
  false

let hasJsonHeader (headers : T) : bool =
  // CLEANUP: don't use contains for this
  getHeader "content-type" headers
  |> Option.map (fun s -> s.Contains "application/json")
  |> Option.defaultValue false

let hasTextHeader (headers : T) : bool =
  // CLEANUP: don't use contains for this
  getHeader "content-type" headers
  |> Option.map (fun s -> s.Contains "text/plain")
  |> Option.defaultValue false
