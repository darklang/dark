module LibSerialization.Binary.Serializers.PT.PackageValue

open System
open System.IO
open Prelude

open LibExecution.ProgramTypes

open LibSerialization.Binary.Serializers.Common
open LibSerialization.Binary.Serializers.PT.Common

let write (w : BinaryWriter) (v : PackageValue.PackageValue) : unit =
  Hash.write w v.hash
  LibSerialization.Binary.Serializers.PT.Expr.Expr.write w v.body
  String.write w v.description
  Option.write w TypeReference.write v.typ

let read (r : BinaryReader) : PackageValue.PackageValue =
  let hash = Hash.read r
  let body = LibSerialization.Binary.Serializers.PT.Expr.Expr.read r
  let description = String.read r
  let typ = Option.read r TypeReference.read
  { hash = hash; body = body; description = description; typ = typ }
