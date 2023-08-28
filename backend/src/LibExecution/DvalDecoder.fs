/// Provides a set of functions for decoding Dvals
///
/// Is similar to a JsonDecoder but for Dvals
module LibExecution.DvalDecoder

open Prelude
open Tablecloth

open RuntimeTypes

let unwrap = Exception.unwrapOptionInternal

let field (name : string) (m : DvalMap) : Dval =
  m |> Map.get name |> unwrap $"Expected {name}' field" []

let stringField (name : string) (m : DvalMap) : string =
  m
  |> field name
  |> Dval.asString
  |> unwrap $"Expected '{name}' field to be a string" []

let listField (name : string) (m : DvalMap) : List<Dval> =
  m
  |> field name
  |> Dval.asList
  |> unwrap $"Expected '{name}' field to be a list" []

let stringListField (name : string) (m : DvalMap) : List<string> =
  m
  |> listField name
  |> List.map (fun s ->
    s |> Dval.asString |> unwrap $"Expected string values in '{name}' list" [])

let int64Field (name : string) (m : DvalMap) : int64 =
  m
  |> field name
  |> Dval.asInt
  |> unwrap $"Expected '{name}' field to be an int" []

let uint64Field (name : string) (m : DvalMap) : uint64 =
  m
  |> field name
  |> Dval.asInt
  |> unwrap $"Expected '{name}' field to be an int" []
  |> uint64

let intField (name : string) (m : DvalMap) : int = m |> int64Field name |> int

let uuidField (name : string) (m : DvalMap) : System.Guid =
  m
  |> field name
  |> Dval.asUuid
  |> unwrap $"Expected '{name}' field to be a uuid" []
