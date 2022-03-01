/// <summary>
/// Header utilities for HttpMiddleware
/// </summary>
///
/// <remarks>
/// Deliberately kept separate from HttpClientHeaders,
/// which may need to work differently at some point
/// </remarks>
module HttpMiddleware.HeadersV0

open Prelude
open LibExecution.VendoredTablecloth

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

  let toHttpHeader (ct : T) : HttpHeaders.Header = "Content-Type", string ct

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

  let textHeader : HttpHeaders.Header = toHttpHeader text


let getContentType (headers : HttpHeaders.T) : Option<ContentType.T> =
  headers |> HttpHeaders.get "Content-type" |> Option.map ContentType.parse

let getMediaType (headers : HttpHeaders.T) : Option<MediaType.T> =
  headers
  |> HttpHeaders.get "Content-type"
  |> Option.map ContentType.parse
  |> Option.bind ContentType.toMediaType
