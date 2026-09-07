module LibSerialization.Binary.Serializers.Effects

open System.IO
open Prelude
open LibSerialization.Binary.Serializers.Common

module E = LibExecution.Effects

let write (writer : BinaryWriter) (effects : Set<E.Effect>) : unit =
  effects |> Set.toList |> List.map E.name |> List.write writer String.write

let read (reader : BinaryReader) : Set<E.Effect> =
  List.read reader String.read
  |> List.map (fun name ->
    match E.fromName name with
    | Some effect -> effect
    | None -> raiseFormatError $"Unknown effect name: {name}")
  |> Set.ofList
