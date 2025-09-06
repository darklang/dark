module LibBinarySerialization.Serializers.PT.PackageValue

open System
open System.IO
open Prelude

open LibExecution.ProgramTypes

open LibBinarySerialization.BinaryFormat
open LibBinarySerialization.Serializers.Common
open LibBinarySerialization.Serializers.PT.Common

module Name =
  let write (w : BinaryWriter) (name : PackageValue.Name) : unit =
    String.write w name.owner
    List.write w String.write name.modules
    String.write w name.name

  let read (r : BinaryReader) : PackageValue.Name =
    let owner = String.read r
    let modules = List.read r String.read
    let name = String.read r
    { owner = owner; modules = modules; name = name }



let write (w : BinaryWriter) (v : PackageValue.PackageValue) : unit =
  LibBinarySerialization.Serializers.Common.Hash.write w v.hash
  Name.write w v.name
  LibBinarySerialization.Serializers.PT.Expr.Expr.write w v.body
  String.write w v.description
  Deprecation.write w FQValueName.write v.deprecated

let read (r : BinaryReader) : PackageValue.PackageValue =
  let hash = LibBinarySerialization.Serializers.Common.Hash.read r
  let name = Name.read r
  let body = LibBinarySerialization.Serializers.PT.Expr.Expr.read r
  let description = String.read r
  let deprecated = Deprecation.read r FQValueName.read
  { hash = hash
    name = name
    body = body
    description = description
    deprecated = deprecated }
