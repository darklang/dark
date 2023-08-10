module LibExecution.DvalDecoder

open Prelude
open Tablecloth

open RuntimeTypes

let unwrap = Exception.unwrapOptionInternal

let field (name : string) (m : DvalMap) : Dval =
  m |> Map.get name |> unwrap $"Expected {name}' field" []

let string (name : string) (m : DvalMap) : string =
  m
  |> field name
  |> Dval.asString
  |> unwrap $"Expected '{name}' field to be a string" []

let list (name : string) (m : DvalMap) : List<Dval> =
  m
  |> field name
  |> Dval.asList
  |> unwrap $"Expected '{name}' field to be a list" []

let stringList (name : string) (m : DvalMap) : List<string> =
  m
  |> list name
  |> List.map (fun s ->
    s |> Dval.asString |> unwrap $"Expected string values in '{name}' list" [])

let int64 (name : string) (m : DvalMap) : int64 =
  m
  |> field name
  |> Dval.asInt
  |> unwrap $"Expected '{name}' field to be an int" []

let uint64 (name : string) (m : DvalMap) : uint64 =
  m
  |> field name
  |> Dval.asInt
  |> unwrap $"Expected '{name}' field to be an int" []
  |> uint64

let int (name : string) (m : DvalMap) : int = m |> int64 name |> int

let uuid (name : string) (m : DvalMap) : System.Guid =
  m
  |> field name
  |> Dval.asUuid
  |> unwrap $"Expected '{name}' field to be a uuid" []
