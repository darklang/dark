module LibSerialization.Binary.Serializers.PT.PackageFn

open System
open System.IO
open Prelude

open LibExecution.ProgramTypes

open LibSerialization.Binary.Serializers.Common
open LibSerialization.Binary.Serializers.PT.Common


module Parameter =
  let write (w : BinaryWriter) (p : PackageFn.Parameter) : unit =
    String.write w p.name
    TypeReference.write w p.typ
    String.write w p.description

  let read (r : BinaryReader) : PackageFn.Parameter =
    let name = String.read r
    let typ = TypeReference.read r
    let description = String.read r
    { name = name; typ = typ; description = description }


let write (w : BinaryWriter) (p : PackageFn.PackageFn) : unit =
  Guid.write w p.id
  ContentHash.write w p.hash
  LibSerialization.Binary.Serializers.PT.Expr.Expr.write w p.body
  LibSerialization.Binary.Serializers.Common.List.write w String.write p.typeParams
  NEList.write Parameter.write w p.parameters
  TypeReference.write w p.returnType
  String.write w p.description
  Deprecation.write w FQFnName.write p.deprecated

let read (r : BinaryReader) : PackageFn.PackageFn =
  let id = Guid.read r
  let hash = ContentHash.read r
  let body = LibSerialization.Binary.Serializers.PT.Expr.Expr.read r
  let typeParams = LibSerialization.Binary.Serializers.Common.List.read r String.read
  let parameters = NEList.read Parameter.read r
  let returnType = TypeReference.read r
  let description = String.read r
  let deprecated = Deprecation.read r FQFnName.read
  { id = id
    hash = hash
    body = body
    typeParams = typeParams
    parameters = parameters
    returnType = returnType
    description = description
    deprecated = deprecated }
