module LibBinarySerialization.Serializers.PT.PackageValue

open System
open System.IO
open Prelude

open LibExecution.ProgramTypes

open LibBinarySerialization.BinaryFormat
open LibBinarySerialization.Serializers.Common
open LibBinarySerialization.Serializers.PT.Common

let write (w : BinaryWriter) (v : PackageValue.PackageValue) : unit =
  Guid.write w v.id
  LibBinarySerialization.Serializers.PT.Expr.Expr.write w v.body
  String.write w v.description
  Deprecation.write w FQValueName.write v.deprecated

let read (r : BinaryReader) : PackageValue.PackageValue =
  let id = Guid.read r
  let body = LibBinarySerialization.Serializers.PT.Expr.Expr.read r
  let description = String.read r
  let deprecated = Deprecation.read r FQValueName.read
  { id = id; body = body; description = description; deprecated = deprecated }
