module LibExecution.HttpHeaders

open System.Threading.Tasks
open FSharp.Control.Tasks

open Prelude
open VendoredTablecloth

type T = List<string * string>

let formContentType = "application/x-www-form-urlencoded"
let jsonContentType = "application/json"
let textContentType = "text/plain"

let getHeader (headerKey : string) (headers : T) : string option =
  headers
  |> List.tryFind
       (fun ((k : string), (_ : string)) -> String.equalsCaseInsensitive headerKey k)
  |> Option.map (fun (k, v) -> v)

let getContentType (headers : T) : Option<string> =
  headers
  |> getHeader "Content-type"
  |> Option.map (fun v -> v |> (String.split ":") |> List.head)
  |> Option.map (String.split ";")
  |> Option.bind List.tryHead
  |> Option.map String.toLowercase



let hasFormHeader (headers : T) : bool =
  // CLEANUP: this doesn't work properly if a charset is included. But also, this was
  // always false in OCaml because the string we compared against wasn't properly
  // trimmed.
  // getHeader "content-type" headers = Some formContentType
  false

let hasJsonHeader (headers : T) : bool =
  getHeader "content-type" headers
  |> Option.map (fun s -> s.Contains jsonContentType)
  |> Option.defaultValue false

let hasTextHeader (headers : T) : bool =
  getHeader "content-type" headers
  |> Option.map (fun s -> s.Contains textContentType)
  |> Option.defaultValue false
