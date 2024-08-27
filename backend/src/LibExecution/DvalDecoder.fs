/// Provides a set of functions for decoding Dvals
///
/// Is similar to a JsonDecoder but for Dvals
module LibExecution.DvalDecoder

open Prelude

open RuntimeTypes


let f (expected : string) (dv : Dval) = Exception.raiseInternal expected [ "dv", dv ]

let unwrap opt msg =
  match opt with
  | Some v -> v
  | None -> Exception.raiseInternal msg []

let int8 (dv : Dval) : int8 =
  match dv with
  | DInt8 i -> i
  | _ -> f "int8" dv

let uInt8 (dv : Dval) : uint8 =
  match dv with
  | DUInt8 i -> i
  | _ -> f "uint8" dv

let int16 (dv : Dval) : int16 =
  match dv with
  | DInt16 i -> i
  | _ -> f "int16" dv

let uInt16 (dv : Dval) : uint16 =
  match dv with
  | DUInt16 i -> i
  | _ -> f "uint16" dv

let int32 (dv : Dval) : int32 =
  match dv with
  | DInt32 i -> i
  | _ -> f "int32" dv

let uInt32 (dv : Dval) : uint32 =
  match dv with
  | DUInt32 i -> i
  | _ -> f "uint32" dv

let int64 (dv : Dval) : int64 =
  match dv with
  | DInt64 i -> i
  | _ -> f "int64" dv

let uInt64 (dv : Dval) : uint64 =
  match dv with
  | DUInt64 i -> i
  | _ -> f "uint64" dv

let int128 (dv : Dval) : System.Int128 =
  match dv with
  | DInt128 i -> i
  | _ -> f "System.Int128" dv

let uInt128 (dv : Dval) : System.UInt128 =
  match dv with
  | DUInt128 i -> i
  | _ -> f "System.UInt128" dv

let float (dv : Dval) : double =
  match dv with
  | DFloat f -> f
  | _ -> f "double" dv

let bool (dv : Dval) : bool =
  match dv with
  | DBool b -> b
  | _ -> f "bool" dv

let uuid (dv : Dval) : System.Guid =
  match dv with
  | DUuid u -> u
  | _ -> f "System.Guid" dv

let string (dv : Dval) : string =
  match dv with
  | DString s -> s
  | _ -> f "string" dv




let tuple2 (dv : Dval) : Dval * Dval =
  match dv with
  | DTuple(first, second, _) -> (first, second)
  | _ -> f "('a * 'b)" dv

let tuple3 (dv : Dval) : Dval * Dval * Dval =
  match dv with
  | DTuple(first, second, [ third ]) -> (first, second, third)
  | _ -> f "('a * 'b * 'c)" dv


let list (m : Dval -> 'a) (dv : Dval) : List<'a> =
  match dv with
  | DList(_, l) -> List.map m l
  | _ -> f "list" dv

let dict (dv : Dval) : Map<string, Dval> =
  match dv with
  | DDict(_, d) -> d
  | _ -> f "dict" dv


let field (name : string) (m : DvalMap) : Dval =
  match m |> Map.get name with
  | Some dv -> dv
  | None -> Exception.raiseInternal $"Expected '{name}' field" []

// let stringField (name : string) (m : DvalMap) : string =
//   m
//   |> field name
//   |> getString
//   |> unwrap $"Expected '{name}' field to be a string" []

// let listField (name : string) (m : DvalMap) : List<Dval> =
//   m
//   |> field name
//   |> getList
//   |> unwrap $"Expected '{name}' field to be a list" []

// let stringListField (name : string) (m : DvalMap) : List<string> =
//   m
//   |> listField name
//   |> List.map (fun s ->
//     s |> getString |> unwrap $"Expected string values in '{name}' list" [])


// let int8Field (name : string) (m : DvalMap) : int8 =
//   m
//   |> field name
//   |> getInt8
//   |> unwrap $"Expected '{name}' field to be an int8" []

// let uint8Field (name : string) (m : DvalMap) : uint8 =
//   m
//   |> field name
//   |> getUInt8
//   |> unwrap $"Expected '{name}' field to be a uint8" []

// let int16Field (name : string) (m : DvalMap) : int16 =
//   m
//   |> field name
//   |> getInt16
//   |> unwrap $"Expected '{name}' field to be an int16" []

// let uint16Field (name : string) (m : DvalMap) : uint16 =
//   m
//   |> field name
//   |> getUInt16
//   |> unwrap $"Expected '{name}' field to be a uint16" []

// let int32Field (name : string) (m : DvalMap) : int32 =
//   m
//   |> field name
//   |> getInt32
//   |> unwrap $"Expected '{name}' field to be an int32" []

// let uint32Field (name : string) (m : DvalMap) : uint32 =
//   m
//   |> field name
//   |> getUInt32
//   |> unwrap $"Expected '{name}' field to be a uint32" []

// let int64Field (name : string) (m : DvalMap) : int64 =
//   m
//   |> field name
//   |> getInt64
//   |> unwrap $"Expected '{name}' field to be an int64" []

// let uint64Field (name : string) (m : DvalMap) : uint64 =
//   m
//   |> field name
//   |> getUInt64
//   |> unwrap $"Expected '{name}' field to be an uint64" []

// let int128Field (name : string) (m : DvalMap) : System.Int128 =
//   m
//   |> field name
//   |> getInt128
//   |> unwrap $"Expected '{name}' field to be an int128" []

// let uint128Field (name : string) (m : DvalMap) : System.UInt128 =
//   m
//   |> field name
//   |> getUInt128
//   |> unwrap $"Expected '{name}' field to be a uint128" []

// let uuidField (name : string) (m : DvalMap) : System.Guid =
//   m
//   |> field name
//   |> getUuid
//   |> unwrap $"Expected '{name}' field to be a uuid" []

// let dictField (name : string) (m : DvalMap) : Map<string, Dval> =
//   m
//   |> field name
//   |> getDict
//   |> unwrap $"Expected '{name}' field to be a dict" []
