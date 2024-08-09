/// Provides a set of functions for decoding Dvals
///
/// Is similar to a JsonDecoder but for Dvals
module LibExecution.DvalDecoder

open Prelude

open RuntimeTypes

let unwrap = Exception.unwrapOptionInternal

let field (name : string) (m : DvalMap) : Dval =
  m |> Map.get name |> unwrap $"Expected {name}' field" []

let stringField (name : string) (m : DvalMap) : string =
  m
  |> field name
  |> Dval.asString
  |> unwrap $"Expected '{name}' field to be a string" []

// let listField (name : string) (m : DvalMap) : List<Dval> =
//   m
//   |> field name
//   |> Dval.asList
//   |> unwrap $"Expected '{name}' field to be a list" []

// let stringListField (name : string) (m : DvalMap) : List<string> =
//   m
//   |> listField name
//   |> List.map (fun s ->
//     s |> Dval.asString |> unwrap $"Expected string values in '{name}' list" [])

let int64Field (name : string) (m : DvalMap) : int64 =
  m
  |> field name
  |> Dval.asInt64
  |> unwrap $"Expected '{name}' field to be an int64" []

// let uint64Field (name : string) (m : DvalMap) : uint64 =
//   m
//   |> field name
//   |> Dval.asUInt64
//   |> unwrap $"Expected '{name}' field to be an uint64" []

let intField (name : string) (m : DvalMap) : int = m |> int64Field name |> int

// let int8Field (name : string) (m : DvalMap) : int8 =
//   m
//   |> field name
//   |> Dval.asInt8
//   |> unwrap $"Expected '{name}' field to be an int8" []

// let uint8Field (name : string) (m : DvalMap) : uint8 =
//   m
//   |> field name
//   |> Dval.asUInt8
//   |> unwrap $"Expected '{name}' field to be a uint8" []

// let int16Field (name : string) (m : DvalMap) : int16 =
//   m
//   |> field name
//   |> Dval.asInt16
//   |> unwrap $"Expected '{name}' field to be an int16" []

// let uint16Field (name : string) (m : DvalMap) : uint16 =
//   m
//   |> field name
//   |> Dval.asUInt16
//   |> unwrap $"Expected '{name}' field to be a uint16" []

// let int32Field (name : string) (m : DvalMap) : int32 =
//   m
//   |> field name
//   |> Dval.asInt32
//   |> unwrap $"Expected '{name}' field to be an int32" []

// let uint32Field (name : string) (m : DvalMap) : uint32 =
//   m
//   |> field name
//   |> Dval.asUInt32
//   |> unwrap $"Expected '{name}' field to be a uint32" []

// let int128Field (name : string) (m : DvalMap) : System.Int128 =
//   m
//   |> field name
//   |> Dval.asInt128
//   |> unwrap $"Expected '{name}' field to be an int128" []

// let uint128Field (name : string) (m : DvalMap) : System.UInt128 =
//   m
//   |> field name
//   |> Dval.asUInt128
//   |> unwrap $"Expected '{name}' field to be a uint128" []

// let uuidField (name : string) (m : DvalMap) : System.Guid =
//   m
//   |> field name
//   |> Dval.asUuid
//   |> unwrap $"Expected '{name}' field to be a uuid" []

// let mapField (name : string) (m : DvalMap) : Map<string, Dval> =
//   m
//   |> field name
//   |> Dval.asDict
//   |> unwrap $"Expected '{name}' field to be a dict" []
