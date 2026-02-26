module LibSerialization.Binary.Serializers.PT.PackageValue

open System
open System.IO
open Prelude

open LibExecution.ProgramTypes

open LibSerialization.Binary.Serializers.Common
open LibSerialization.Binary.Serializers.PT.Common

let write (w : BinaryWriter) (v : PackageValue.PackageValue) : unit =
  ContentHash.write w v.hash
  LibSerialization.Binary.Serializers.PT.Expr.Expr.write w v.body
  String.write w v.description
  Deprecation.write w FQValueName.write v.deprecated

let read (r : BinaryReader) : PackageValue.PackageValue =
  let hash = ContentHash.read r
  let body = LibSerialization.Binary.Serializers.PT.Expr.Expr.read r
  let description = String.read r
  let deprecated = Deprecation.read r FQValueName.read
  { hash = hash; body = body; description = description; deprecated = deprecated }
